import 'package:flutter/material.dart';

import '../../core/models.dart';
import '../../core/money.dart';
import '../../core/session_controller.dart';
import '../../shared/entity_picker.dart';
import '../../shared/widgets.dart';
import '../records/record_editor.dart';

const receiptBatchExactError = 'Exact amounts require selected expenses with the same currency and total; use shares or percentages.';

class ReceiptBatchChanges {
  ReceiptBatchChanges({this.payerUserId, this.accountingDate, this.mode, List<SplitDraft>? splits})
    : splits = splits == null ? null : List.unmodifiable(splits);
  final String? payerUserId, accountingDate;
  final SplitMode? mode;
  final List<SplitDraft>? splits;
  bool get isEmpty => payerUserId == null && accountingDate == null && mode == null;

  void preflight(ExpenseGroup group, Iterable<SplitFieldsController> selected) {
    final rows = selected.toList();
    if (rows.isEmpty) throw const FormatException('Select at least one expense.');
    final active = group.members.map((member) => member.userId).toSet();
    final uuid = RegExp(r'^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$');
    if (payerUserId != null && (!active.contains(payerUserId) || !uuid.hasMatch(payerUserId!))) {
      throw const FormatException('Choose an active member as payer.');
    }
    if (accountingDate != null) validateCalendarDate(accountingDate!);
    if ((mode == null) != (splits == null)) throw const FormatException('Choose a split policy and participants.');
    if (mode != null) {
      if (splits!.any((split) => !active.contains(split.userId))) {
        throw const FormatException('Choose only active members as participants.');
      }
      BigInt total = BigInt.one;
      if (mode == SplitMode.exact) {
        try {
          total = rows.first.amountMinor;
          if (rows.any((row) => row.currency.code != rows.first.currency.code || row.amountMinor != total)) {
            throw const FormatException(receiptBatchExactError);
          }
        } on FormatException { throw const FormatException(receiptBatchExactError); }
      }
      final error = validateMoneySplits(mode!.name, total,
        [for (final split in splits!) MoneySplitValue(split.userId, split.value)]);
      if (error != null) throw FormatException(error);
    }
    for (final row in rows) {
      if (row.group.groupId != group.groupId) throw const FormatException('The selected expenses are no longer in this group.');
    }
  }

  void apply(ExpenseGroup group, Iterable<SplitFieldsController> selected, {void Function(SplitFieldsController)? onChanged}) {
    final rows = selected.toList();
    preflight(group, rows);
    if (isEmpty) return;
    for (final row in rows) {
      row.applyReceiptBatchChanges(payerUserId: payerUserId, accountingDate: accountingDate, mode: mode, splits: splits);
      onChanged?.call(row);
    }
  }
}

Future<ReceiptBatchChanges?> showReceiptBatchEditor(BuildContext context, {
  required SessionController session, required ExpenseGroup group, required List<SplitFieldsController> selected,
}) => showDialog<ReceiptBatchChanges>(context: context,
  builder: (_) => _ReceiptBatchDialog(session: session, group: group, selected: selected));

class _ReceiptBatchDialog extends StatefulWidget {
  const _ReceiptBatchDialog({required this.session, required this.group, required this.selected});
  final SessionController session;
  final ExpenseGroup group;
  final List<SplitFieldsController> selected;
  @override
  State<_ReceiptBatchDialog> createState() => _ReceiptBatchDialogState();
}

class _ReceiptBatchDialogState extends State<_ReceiptBatchDialog> {
  bool _payerEnabled = false, _dateEnabled = false, _splitsEnabled = false;
  String? _payer, _error;
  SplitMode _mode = SplitMode.equal;
  final _date = TextEditingController();
  final Set<String> _participants = {};
  final Map<(SplitMode, String), TextEditingController> _values = {};
  late final int _generation;
  TextEditingController _value(String id) => _values.putIfAbsent((_mode, id), () => TextEditingController());
  @override
  void initState() {
    super.initState();
    _generation = widget.session.generation;
    widget.session.addListener(_sessionChanged);
  }
  void _sessionChanged() {
    if (mounted && widget.session.generation != _generation) Navigator.of(context).pop();
  }
  @override
  void dispose() {
    widget.session.removeListener(_sessionChanged); _date.dispose();
    for (final input in _values.values) { input.dispose(); }
    super.dispose();
  }
  void _submit() {
    try {
      if (_payerEnabled && _payer == null) throw const FormatException('Choose an active member as payer.');
      final changes = ReceiptBatchChanges(payerUserId: _payerEnabled ? _payer : null,
        accountingDate: _dateEnabled ? _date.text : null, mode: _splitsEnabled ? _mode : null,
        splits: !_splitsEnabled ? null : [for (final id in _participants) SplitDraft(userId: id, value: switch (_mode) {
          SplitMode.equal => BigInt.one,
          SplitMode.percentage => parsePercentage(_value(id).text),
          SplitMode.exact => parseMajorAmount(_value(id).text, minorDigits: widget.selected.first.currency.minorUnits, allowZero: true),
          SplitMode.shares => _shares(_value(id).text),
        })]);
      changes.preflight(widget.group, widget.selected);
      Navigator.of(context).pop(changes);
    } on FormatException catch (error) { setState(() => _error = error.message); }
  }
  BigInt _shares(String text) {
    if (!RegExp(r'^\d+$').hasMatch(text.trim())) throw const FormatException('Shares must be integers from 0 to 32767.');
    return BigInt.parse(text.trim());
  }
  @override
  Widget build(BuildContext context) => AlertDialog(
    title: Text('Edit ${widget.selected.length} selected expenses'),
    scrollable: true,
    insetPadding: const EdgeInsets.symmetric(horizontal: 16, vertical: 24),
    content: SizedBox(width: 520, child: Column(crossAxisAlignment: CrossAxisAlignment.stretch, mainAxisSize: MainAxisSize.min, children: [
      const Text('Enable only the fields to replace. All other fields stay unchanged.'),
      CheckboxListTile(contentPadding: EdgeInsets.zero, title: const Text('Change paid by'), subtitle: Text(_payerEnabled ? 'Replace payer' : 'Leave unchanged'),
        value: _payerEnabled, onChanged: (value) => setState(() => _payerEnabled = value!)),
      if (_payerEnabled) UserPickerField(session: widget.session, label: 'Batch paid by', selectedUserId: _payer,
        knownUsers: {for (final user in widget.group.members) user.userId: user}, choices: widget.group.members,
        onChanged: (id) => setState(() => _payer = id)),
      CheckboxListTile(contentPadding: EdgeInsets.zero, title: const Text('Change date'), subtitle: Text(_dateEnabled ? 'Replace accounting date' : 'Leave unchanged'),
        value: _dateEnabled, onChanged: (value) => setState(() => _dateEnabled = value!)),
      if (_dateEnabled) TextField(controller: _date, decoration: const InputDecoration(labelText: 'Batch date (YYYY-MM-DD)')),
      CheckboxListTile(contentPadding: EdgeInsets.zero, title: const Text('Change splits'), subtitle: Text(_splitsEnabled ? 'Replace participants and split policy' : 'Leave unchanged'),
        value: _splitsEnabled, onChanged: (value) => setState(() => _splitsEnabled = value!)),
      if (_splitsEnabled) ...[
        DropdownButtonFormField<SplitMode>(initialValue: _mode, decoration: const InputDecoration(labelText: 'Batch split method'),
          isExpanded: true, itemHeight: null,
          items: [for (final mode in SplitMode.values) DropdownMenuItem(value: mode, child: Text(switch (mode) {
            SplitMode.equal => 'Equal', SplitMode.shares => 'Shares', SplitMode.percentage => 'Percentage', SplitMode.exact => 'Exact amounts',
          }))], onChanged: (mode) => setState(() => _mode = mode!)),
        if (_mode == SplitMode.exact) const Padding(padding: EdgeInsets.symmetric(vertical: 8), child: Text(receiptBatchExactError)),
        for (final user in widget.group.members) ...[
          CheckboxListTile(contentPadding: EdgeInsets.zero, title: Text(userLabel(user)), value: _participants.contains(user.userId),
            onChanged: (value) => setState(() { if (value!) { _participants.add(user.userId); } else { _participants.remove(user.userId); } })),
          if (_participants.contains(user.userId) && _mode != SplitMode.equal) TextField(
            key: ValueKey((_mode, user.userId)), controller: _value(user.userId),
            keyboardType: const TextInputType.numberWithOptions(decimal: true),
            decoration: InputDecoration(labelText: '${userLabel(user)} ${_mode == SplitMode.shares ? 'shares' : _mode == SplitMode.percentage ? 'percentage' : 'amount'}',
              suffixText: _mode == SplitMode.percentage ? '%' : _mode == SplitMode.exact ? widget.selected.first.currency.code : null,
              helperText: _mode == SplitMode.shares ? '0–32767' : null)),
        ],
      ],
      if (_error != null) Padding(padding: const EdgeInsets.only(top: 12), child: Semantics(liveRegion: true, child: Text(_error!))),
    ])),
    actions: [TextButton(onPressed: () => Navigator.of(context).pop(), child: const Text('Cancel')),
      FilledButton(onPressed: _payerEnabled || _dateEnabled || _splitsEnabled ? _submit : null, child: const Text('Apply to selected'))],
  );
}
