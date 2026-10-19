import 'package:flutter/material.dart';
import 'package:go_router/go_router.dart';

import '../../core/jizhang_repository.dart';
import '../../core/models.dart';
import '../../core/session_controller.dart';
import '../../shared/record_navigation.dart';
import '../../shared/widgets.dart';
import '../groups/group_screen.dart';

Future<bool> deleteRecordWithConfirmation(BuildContext context, JizhangRepository repository,
    String groupId, LedgerRecord record) async {
  var pending = false;
  String? error;
  return await showDialog<bool>(context: context, barrierDismissible: false, builder: (dialogContext) =>
    StatefulBuilder(builder: (context, update) => PopScope(canPop: !pending, child: AlertDialog(
      title: Text('Delete ${switch (record.kind) { RecordKind.expense => 'expense', RecordKind.income => 'income', RecordKind.transfer => 'payment' }}?'),
      content: Column(mainAxisSize: MainAxisSize.min, crossAxisAlignment: CrossAxisAlignment.start, children: [
        Text('Delete “${record.title}” (${record.currency.format(record.amountMinor)})? Group balances will change. This cannot be undone.'),
        if (error != null) Padding(padding: const EdgeInsets.only(top: 12), child: Text(error!, style: const TextStyle(color: Color(0xFFB42318)))),
      ]),
      actions: [
        TextButton(onPressed: pending ? null : () => Navigator.pop(dialogContext, false), child: const Text('Cancel')),
        FilledButton(onPressed: pending ? null : () async {
          update(() { pending = true; error = null; });
          try {
            await repository.deleteRecord(groupId, record.recordId);
            if (dialogContext.mounted) Navigator.pop(dialogContext, true);
          } catch (failure) {
            if (dialogContext.mounted) update(() { pending = false; error = errorMessage(failure); });
          }
        }, child: Text(pending ? 'Deleting…' : 'Delete')),
      ],
    )))) ?? false;
}

class RecordDetailScreen extends StatefulWidget {
  const RecordDetailScreen({super.key, required this.repository, required this.session,
    required this.groupId, required this.recordId});
  final JizhangRepository repository;
  final SessionController session;
  final String groupId, recordId;

  @override
  State<RecordDetailScreen> createState() => _RecordDetailScreenState();
}

class _RecordDetailScreenState extends State<RecordDetailScreen> {
  final _loadKey = GlobalKey<LoadViewState<LedgerRecord>>();

  Future<void> _back(BuildContext context) => returnFromRecord(context, widget.session,
    fallback: '/groups/${widget.groupId}/records');

  @override
  Widget build(BuildContext context) {
    final scope = GroupScope.of(context);
    final group = scope.group;
    return RecordReturnBoundary(
      session: widget.session, changes: scope.recordChanges,
      onReturn: (changed) async { if (changed) await _loadKey.currentState?.reload(); },
      child: Column(crossAxisAlignment: CrossAxisAlignment.stretch, children: [
      Align(alignment: Alignment.centerLeft, child: TextButton.icon(onPressed: () => _back(context), icon: const Icon(Icons.arrow_back), label: Text(context.canPop() ? 'Back' : 'Back to records'))),
      LoadView<LedgerRecord>(key: _loadKey, retainDataOnReload: true,
        load: () => widget.repository.getRecord(widget.groupId, widget.recordId), builder: (record) {
        final active = group.members.map((u) => u.userId).toSet();
        return Column(crossAxisAlignment: CrossAxisAlignment.stretch, children: [
          Text(record.title, style: Theme.of(context).textTheme.headlineSmall),
          Text(switch (record.kind) { RecordKind.expense => 'Expense', RecordKind.income => 'Income', RecordKind.transfer => 'Payment / Transfer' }),
          const SizedBox(height: 16),
          Card(child: Padding(padding: const EdgeInsets.all(12), child: Column(
            crossAxisAlignment: CrossAxisAlignment.stretch, children: [
              _RecordMetadataRow(label: 'Amount', value: RecordMoney(record: record)),
              SavedExchangeRateDetails(record: record),
              _RecordMetadataRow(label: record.kind == RecordKind.income ? 'Received by' : 'Paid by', value: IdentityLabel(user: record.actor, former: !active.contains(record.actor.userId))),
              _RecordMetadataRow(label: 'Date', value: Text(record.date)),
              _RecordMetadataRow(label: 'Created', value: Text(record.createdAt.toLocal().toString())),
              if (record.note?.trim().isNotEmpty ?? false)
                _RecordMetadataRow(label: 'Note', value: SelectableText(record.note!)),
              if (record is TransferRecord)
                _RecordMetadataRow(label: 'Paid to', value: IdentityLabel(user: record.transferTo, former: !active.contains(record.transferTo.userId))),
            ],
          ))),
          if (record is SplitRecord) ...[
            const SizedBox(height: 16),
            SectionHeading(title: record.kind == RecordKind.income ? 'Beneficiary entitlements' : 'Split amounts'),
            Text('Split method: ${splitModeLabel(record.splitMode)}'),
            const SizedBox(height: 8),
            for (final split in record.splits) Card(child: Padding(
              padding: const EdgeInsets.all(12),
              child: Column(crossAxisAlignment: CrossAxisAlignment.stretch, children: [
                IdentityLabel(user: split.user, former: !active.contains(split.user.userId)),
                const SizedBox(height: 8),
                Wrap(spacing: 16, runSpacing: 8, children: [
                  Text(splitInputLabel(record, split)),
                  Text('${record.kind == RecordKind.income ? 'Entitlement' : 'Amount'}: ${record.currency.format(split.amountMinor)}'),
                  if (record.currency.code != record.settlementCurrency.code)
                    Text('Settlement: ${record.settlementCurrency.format(split.settlementAmountMinor)}'),
                ]),
              ]),
            )),
          ],
          const SizedBox(height: 16),
          Wrap(spacing: 8, runSpacing: 8, children: [
            FilledButton(onPressed: () => context.push<void>('/groups/${widget.groupId}/${switch (record.kind) { RecordKind.expense => 'expenses', RecordKind.income => 'incomes', RecordKind.transfer => 'transfers' }}/${widget.recordId}/edit'), child: const Text('Edit')),
            OutlinedButton(onPressed: () async {
              final generation = widget.session.generation;
              final deleted = await deleteRecordWithConfirmation(context, widget.repository, widget.groupId, record);
              if (!context.mounted || !deleted || generation != widget.session.generation) return;
              GroupScope.of(context).recordChanges.markChanged();
              showSuccess(context, 'Record deleted');
              _back(context);
            }, child: const Text('Delete')),
          ]),
        ]);
      }),
    ]));
  }
}

class _RecordMetadataRow extends StatelessWidget {
  const _RecordMetadataRow({required this.label, required this.value});
  final String label;
  final Widget value;

  @override
  Widget build(BuildContext context) => Padding(
    padding: const EdgeInsets.symmetric(vertical: 4),
    child: LayoutBuilder(builder: (context, constraints) {
      final heading = Text(label, style: Theme.of(context).textTheme.labelLarge);
      if (constraints.maxWidth < 400) {
        return Column(crossAxisAlignment: CrossAxisAlignment.start, children: [
          heading, const SizedBox(height: 4), value,
        ]);
      }
      return Row(crossAxisAlignment: CrossAxisAlignment.start, children: [
        SizedBox(width: 96, child: heading),
        const SizedBox(width: 12),
        Expanded(child: value),
      ]);
    }),
  );
}
