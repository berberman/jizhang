import 'package:flutter/material.dart';
import 'package:go_router/go_router.dart';

import '../../core/jizhang_repository.dart';
import '../../core/models.dart';
import '../../core/session_controller.dart';
import '../../shared/record_navigation.dart';
import '../../shared/widgets.dart';
import '../groups/group_screen.dart';

class ReceiptDetailScreen extends StatefulWidget {
  const ReceiptDetailScreen({super.key, required this.repository, required this.session,
    required this.groupId, required this.receiptId});
  final JizhangRepository repository;
  final SessionController session;
  final String groupId, receiptId;

  @override
  State<ReceiptDetailScreen> createState() => _ReceiptDetailScreenState();
}

class _ReceiptDetailScreenState extends State<ReceiptDetailScreen> {
  final _loadKey = GlobalKey<LoadViewState<Receipt>>();

  Future<void> _delete(BuildContext context, Receipt receipt) async {
    final generation = widget.session.generation;
    var pending = false;
    String? error;
    final deleted = await showDialog<bool>(context: context, barrierDismissible: false,
      builder: (dialogContext) => StatefulBuilder(builder: (context, update) => PopScope(
        canPop: !pending,
        child: AlertDialog(
          title: const Text('Delete this receipt and its expenses?'),
          content: Column(mainAxisSize: MainAxisSize.min, crossAxisAlignment: CrossAxisAlignment.start, children: [
            Text('This permanently deletes this receipt and ${receipt.records.length} expenses. Balances will change.'),
            if (error != null) Padding(padding: const EdgeInsets.only(top: 12), child: Text(error!)),
          ]),
          actions: [
            TextButton(onPressed: pending ? null : () => Navigator.pop(dialogContext, false), child: const Text('Cancel')),
            FilledButton(onPressed: pending ? null : () async {
              update(() { pending = true; error = null; });
              try {
                await widget.repository.deleteReceipt(widget.groupId, widget.receiptId);
                if (dialogContext.mounted) Navigator.pop(dialogContext, generation == widget.session.generation);
              } catch (failure) {
                if (!dialogContext.mounted) return;
                if (generation != widget.session.generation) { Navigator.pop(dialogContext, false); return; }
                update(() { pending = false; error = errorMessage(failure); });
              }
            }, child: Text(pending ? 'Deleting…' : 'Delete')),
          ],
        ),
      )));
    if (deleted == true && context.mounted && generation == widget.session.generation) {
      showSuccess(context, 'Receipt and its expenses deleted.');
      context.go('/groups/${widget.groupId}/receipts');
    }
  }

  @override
  Widget build(BuildContext context) {
    final scope = GroupScope.of(context);
    final active = scope.group.members.map((user) => user.userId).toSet();
    return RecordReturnBoundary(
      session: widget.session, changes: scope.recordChanges,
      onReturn: (changed) async { if (changed) await _loadKey.currentState?.reload(); },
      child: Column(crossAxisAlignment: CrossAxisAlignment.stretch, children: [
        Align(alignment: Alignment.centerLeft, child: TextButton.icon(
          onPressed: () => context.go('/groups/${widget.groupId}/receipts'),
          icon: const Icon(Icons.arrow_back), label: const Text('Back to receipts'))),
        LoadView<Receipt>(key: _loadKey, retainDataOnReload: true,
          load: () => widget.repository.getReceipt(widget.groupId, widget.receiptId),
          builder: (receipt) => Column(crossAxisAlignment: CrossAxisAlignment.stretch, children: [
        Text('Receipt', style: Theme.of(context).textTheme.headlineSmall),
        const SizedBox(height: 16),
        Card(child: Padding(padding: const EdgeInsets.all(12), child: Column(
          crossAxisAlignment: CrossAxisAlignment.stretch, children: [
            _ReceiptMetadataRow(label: 'Note', value: Text(receipt.note.isEmpty ? 'No note' : receipt.note)),
            _ReceiptMetadataRow(label: 'Uploaded by', value: IdentityLabel(
              user: receipt.uploadedBy, former: !active.contains(receipt.uploadedBy.userId))),
            _ReceiptMetadataRow(label: 'Created', value: Text(receipt.createdAt.toLocal().toString())),
            _ReceiptMetadataRow(label: 'Settlement totals', value: Text(receiptTotals(receipt).join(' · '))),
          ],
        ))),
        const SizedBox(height: 12),
        Wrap(spacing: 8, runSpacing: 8, children: [
          FilledButton(onPressed: () => context.go('/groups/${widget.groupId}/receipts/${widget.receiptId}/edit'), child: const Text('Edit receipt')),
          TextButton(onPressed: () => _delete(context, receipt), child: const Text('Delete receipt')),
        ]),
        const SizedBox(height: 16),
        const SectionHeading(title: 'Expenses'),
        const SizedBox(height: 8),
        if (receipt.records.isEmpty) const EmptyState(
          title: 'No expenses', message: 'This receipt has no expenses.'),
        for (final record in receipt.records)
          Card(child: Padding(padding: const EdgeInsets.all(12), child: Column(
            crossAxisAlignment: CrossAxisAlignment.stretch, children: [
              Text(record.title, style: Theme.of(context).textTheme.titleMedium),
              if (record.note?.trim().isNotEmpty ?? false) ...[
                const SizedBox(height: 8),
                const Text('Expense note'),
                SelectableText(record.note!),
              ],
              const SizedBox(height: 8),
              Wrap(spacing: 16, runSpacing: 8, children: [
                RecordMoney(record: record),
                Text('Date: ${record.date}'),
              ]),
              SavedExchangeRateDetails(record: record),
              if (record is ExpenseRecord) ...[
                Text('Split method: ${splitModeLabel(record.splitMode)}'),
                for (final split in record.splits) Padding(
                  padding: const EdgeInsets.symmetric(vertical: 4),
                  child: Column(crossAxisAlignment: CrossAxisAlignment.start, children: [
                    IdentityLabel(user: split.user, former: !active.contains(split.user.userId)),
                    Text(splitInputLabel(record, split)),
                    Text(record.currency.format(split.amountMinor)),
                    if (record.currency.code != record.settlementCurrency.code)
                      Text('Settlement: ${record.settlementCurrency.format(split.settlementAmountMinor)}'),
                  ])),
              ],
              const SizedBox(height: 8),
              const Text('Paid by'),
              IdentityLabel(user: record.actor, former: !active.contains(record.actor.userId)),
              const SizedBox(height: 8),
              Align(alignment: Alignment.centerLeft, child: TextButton(
                onPressed: () => context.push<void>('/groups/${widget.groupId}/records/${record.recordId}'),
                child: const Text('Open expense'))),
            ],
          ))),
          ]),
        ),
      ]),
    );
  }
}

class _ReceiptMetadataRow extends StatelessWidget {
  const _ReceiptMetadataRow({required this.label, required this.value});
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
