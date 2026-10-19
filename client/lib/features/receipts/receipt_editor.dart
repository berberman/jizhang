import 'package:flutter/material.dart';
import 'package:go_router/go_router.dart';

import '../../core/api_client.dart';
import '../../core/jizhang_repository.dart';
import '../../core/models.dart';
import '../../core/session_controller.dart';
import '../../shared/form_controller.dart';
import '../../shared/widgets.dart';
import '../groups/group_screen.dart';
import '../records/record_editor.dart';

class ReceiptEditor extends StatelessWidget {
  const ReceiptEditor({super.key, required this.repository, required this.session,
    required this.groupId, this.receiptId});
  final JizhangRepository repository;
  final SessionController session;
  final String groupId;
  final String? receiptId;
  @override
  Widget build(BuildContext context) {
    final group = GroupScope.of(context).group;
    if (receiptId == null) return _ReceiptForm(repository: repository, session: session, group: group);
    return LoadView<Receipt>(load: () => repository.getReceipt(groupId, receiptId!),
      builder: (receipt) => _ReceiptForm(repository: repository, session: session, group: group, receipt: receipt));
  }
}

class _ReceiptForm extends StatefulWidget {
  const _ReceiptForm({required this.repository, required this.session, required this.group, this.receipt});
  final JizhangRepository repository;
  final SessionController session;
  final ExpenseGroup group;
  final Receipt? receipt;
  @override
  State<_ReceiptForm> createState() => _ReceiptFormState();
}

class _ReceiptFormState extends State<_ReceiptForm> {
  late final FormController _form = FormController(widget.session);
  late final TextEditingController _note = TextEditingController(text: widget.receipt?.note ?? '');
  final List<SplitFieldsController> _items = [];
  final Set<SplitFieldsController> _pendingDisposal = {};
  late int _sessionGeneration;
  @override
  void initState() {
    super.initState();
    _sessionGeneration = widget.session.generation;
    widget.session.addListener(_sessionChanged);
    for (final record in widget.receipt?.records ?? <LedgerRecord>[]) {
      if (record is ExpenseRecord) {
        _items.add(SplitFieldsController(repository: widget.repository, session: widget.session, group: widget.group, draft: SplitRecordDraft.fromRecord(record),
          savedCurrency: record.currency, savedRate: record.exchangeRate,
          currentUserId: widget.session.user?.userId, historicalUsers: [record.paidBy, ...record.splits.map((split) => split.user)]));
      }
    }
  }
  void _sessionChanged() {
    if (_sessionGeneration == widget.session.generation) return;
    _sessionGeneration = widget.session.generation;
    _note.clear();
    final previousItems = List<SplitFieldsController>.of(_items);
    if (mounted) setState(_items.clear);
    for (final item in previousItems) { _retire(item); }
  }
  @override
  void didUpdateWidget(covariant _ReceiptForm oldWidget) {
    super.didUpdateWidget(oldWidget);
    if (!identical(oldWidget.group, widget.group)) {
      for (final item in _items) { item.updateGroup(widget.group); }
    }
  }
  @override
  void dispose() {
    widget.session.removeListener(_sessionChanged);
    _form.dispose(); _note.dispose();
    for (final item in _items) { item.dispose(); }
    _drainRetired();
    super.dispose();
  }
  void _add() {
    setState(() => _items.add(SplitFieldsController(repository: widget.repository, session: widget.session, group: widget.group,
      currentUserId: widget.session.user?.userId)));
    _form.markDirty();
  }
  void _drainRetired() {
    final retired = List<SplitFieldsController>.of(_pendingDisposal);
    _pendingDisposal.clear();
    for (final item in retired) { item.dispose(); }
  }
  void _retire(SplitFieldsController item) {
    item.deactivate();
    _pendingDisposal.add(item);
    WidgetsBinding.instance.addPostFrameCallback((_) => _drainRetired());
  }
  void _remove(SplitFieldsController item) {
    if (_form.pending || !_items.contains(item)) return;
    setState(() => _items.remove(item));
    _retire(item);
    _form.markDirty();
  }
  Widget _expenseCard(SplitFieldsController item, int ordinal) =>
    Card(key: ObjectKey(item), child: ExpansionTile(
      key: PageStorageKey(item),
      initiallyExpanded: true, maintainState: true,
      title: ListenableBuilder(listenable: item, builder: (context, _) {
        final title = item.title.text.trim();
        final amount = item.amount.text.trim();
        return Column(crossAxisAlignment: CrossAxisAlignment.start, children: [
          Text(title.isEmpty ? 'Expense $ordinal' : title),
          const SizedBox(height: 4),
          Text('${amount.isEmpty ? 'Amount not set' : '${item.currency.code} $amount'} · Paid by ${item.actorUserId.isEmpty ? 'Not selected' : item.label(item.actorUserId)}',
            style: Theme.of(context).textTheme.bodyMedium),
          if (item.exchangeRate.needsConversion) _convertedSummary(item),
        ]);
      }),
      tilePadding: const EdgeInsets.symmetric(horizontal: 12),
      childrenPadding: const EdgeInsets.all(12),
      children: [
        SplitFields(key: ObjectKey(item), session: widget.session, controller: item,
          onChanged: _form.markDirty, enabled: !_form.pending),
        const SizedBox(height: 12),
        Align(alignment: Alignment.centerLeft, child: TextButton.icon(
          onPressed: _form.pending ? null : () => _remove(item),
          icon: const Icon(Icons.remove_circle_outline),
          label: Text('Remove expense $ordinal'))),
      ],
    ));
  Widget _convertedSummary(SplitFieldsController item) {
    try { return Text('Settlement: ${item.group.defaultCurrency.format(item.settlementMinor)}'); }
    on FormatException { return const Text('Settlement: rate or amount required'); }
  }
  Future<void> _save() async {
    ReceiptDraft draft;
    try {
      final records = <SplitRecordDraft>[];
      for (var i = 0; i < _items.length; i++) {
        try { records.add(_items[i].toDraft()); }
        on FormatException catch (error) { throw FormatException('Expense ${i + 1}: ${error.message}'); }
      }
      draft = ReceiptDraft(note: _note.text, records: records);
    } on FormatException catch (error) {
      setState(() => _form.error = error.message);
      return;
    }
    Receipt? saved;
    bool policyRace = false;
    final success = await _form.save(() async {
      try {
        saved = widget.receipt == null
          ? await widget.repository.createReceipt(widget.group.groupId, draft)
          : await widget.repository.updateReceipt(widget.group.groupId, widget.receipt!.receiptId, draft);
      } on ApiException catch (e) { policyRace = e.statusCode == 409; rethrow; }
    });
    if (mounted && (success || policyRace)) await GroupScope.of(context).refreshGroup();
    if (success && mounted) {
      showSuccess(context, 'Receipt saved. Its expenses and balances have been updated.');
      context.go('/groups/${widget.group.groupId}/receipts/${saved!.receiptId}');
    }
  }
  @override
  Widget build(BuildContext context) => FormLeaveGuard(controller: _form,
    child: ListenableBuilder(listenable: _form, builder: (context, _) => FormSurface(
      title: widget.receipt == null ? 'New receipt' : 'Edit receipt',
      child: Column(crossAxisAlignment: CrossAxisAlignment.stretch, children: [
        Text('Uploaded by ${widget.receipt != null ? userLabel(widget.receipt!.uploadedBy) : widget.session.user != null ? userLabel(widget.session.user!) : ''} (cannot be changed)'),
        if (widget.receipt != null) const Padding(padding: EdgeInsets.symmetric(vertical: 12),
          child: Text('Saving replaces all child expenses. Their previous record links will no longer be available.')),
        const SizedBox(height: 12),
        AbsorbPointer(absorbing: _form.pending, child: Column(crossAxisAlignment: CrossAxisAlignment.stretch, children: [
          TextField(controller: _note, maxLines: 3, decoration: const InputDecoration(labelText: 'Note (optional)'),
            onChanged: (_) => _form.markDirty()),
          const SizedBox(height: 12),
          for (var i = 0; i < _items.length; i++)
            _expenseCard(_items[i], i + 1),
          OutlinedButton.icon(onPressed: _form.pending ? null : _add,
            icon: const Icon(Icons.add), label: const Text('Add expense')),
        ])),
        if (_form.error != null) Padding(padding: const EdgeInsets.symmetric(vertical: 12),
          child: Text(_form.error!, semanticsLabel: 'Error: ${_form.error}')),
        const SizedBox(height: 12),
        Wrap(spacing: 8, runSpacing: 8, children: [
          FilledButton(onPressed: _form.pending ? null : _save, child: Text(_form.pending ? 'Saving…' : 'Save receipt')),
          TextButton(onPressed: _form.pending ? null : () => context.go(widget.receipt == null
            ? '/groups/${widget.group.groupId}/receipts'
            : '/groups/${widget.group.groupId}/receipts/${widget.receipt!.receiptId}'), child: const Text('Cancel')),
        ]),
      ]),
    )),
  );
}
