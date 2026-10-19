import 'package:flutter/material.dart';
import 'package:go_router/go_router.dart';

import '../../core/jizhang_repository.dart';
import '../../core/models.dart';
import '../../core/pagination_controller.dart';
import '../../core/session_controller.dart';
import '../../shared/widgets.dart';

class ReceiptListScreen extends StatefulWidget {
  const ReceiptListScreen({super.key, required this.repository, required this.session, required this.groupId});
  final JizhangRepository repository;
  final SessionController session;
  final String groupId;
  @override
  State<ReceiptListScreen> createState() => _ReceiptListScreenState();
}

class _ReceiptListScreenState extends State<ReceiptListScreen> {
  late final PaginationController<Receipt> _list = PaginationController<Receipt>(
    load: (query) => widget.repository.listReceipts(widget.groupId, query),
    query: const PageQuery(sort: 'note'),
  );
  late int _sessionGeneration;
  void _sessionChanged() {
    if (_sessionGeneration == widget.session.generation) return;
    _sessionGeneration = widget.session.generation;
    _list.clear();
  }
  @override
  void initState() {
    super.initState();
    _sessionGeneration = widget.session.generation;
    widget.session.addListener(_sessionChanged);
    _list.refresh();
  }
  @override
  void dispose() {
    widget.session.removeListener(_sessionChanged);
    _list.dispose();
    super.dispose();
  }
  @override
  Widget build(BuildContext context) => ListenableBuilder(listenable: _list, builder: (context, _) => Column(
      crossAxisAlignment: CrossAxisAlignment.stretch,
      children: [
        Wrap(alignment: WrapAlignment.spaceBetween, spacing: 16, runSpacing: 8, children: [
          Text('Receipts', style: Theme.of(context).textTheme.headlineSmall),
          FilledButton.icon(onPressed: () => context.go('/groups/${widget.groupId}/receipts/new'),
            icon: const Icon(Icons.add), label: const Text('New receipt')),
        ]),
        ListToolbar(query: _list.query.query, sort: _list.query.sort, label: 'note',
          onSearch: _list.search, onSort: _list.setSort),
        if (_list.loading) const LinearProgressIndicator(),
        if (_list.error != null) ErrorPanel(error: _list.error!, onRetry: () => _list.refresh()),
        if (_list.error != null && _list.page != null) const Text('Showing previously loaded receipts.'),
        if (!_list.loading && _list.error == null && _list.page?.items.isEmpty == true)
          EmptyState(title: _list.query.query.trim().isEmpty ? 'No receipts yet' : 'No matching receipts',
            message: _list.query.query.trim().isEmpty ? 'Create a receipt to group expenses together.' : 'Try another note or clear your search.'),
        for (final receipt in _list.page?.items ?? <Receipt>[]) _receiptRow(context, receipt),
        const SizedBox(height: 16),
        PageControls(controller: _list),
      ],
    ));

  Widget _receiptRow(BuildContext context, Receipt receipt) {
    final localCreated = receipt.createdAt.toLocal();
    final localizations = MaterialLocalizations.of(context);
    final created = '${localizations.formatCompactDate(localCreated)} ${localizations.formatTimeOfDay(TimeOfDay.fromDateTime(localCreated))}';
    final title = receipt.note.isEmpty ? 'Receipt (no note)' : receipt.note;
    final totals = receiptTotals(receipt);
    final amount = Text(totals.isEmpty ? 'No expenses' : 'Batch totals: ${totals.join(' · ')}', style: Theme.of(context).textTheme.titleMedium);
    return Card(clipBehavior: Clip.antiAlias, child: InkWell(
      onTap: () => context.go('/groups/${widget.groupId}/receipts/${receipt.receiptId}'),
      child: LayoutBuilder(builder: (context, size) => Padding(
        padding: const EdgeInsets.all(12),
        child: Column(crossAxisAlignment: CrossAxisAlignment.stretch, children: [
          if (size.maxWidth >= 800) Row(crossAxisAlignment: CrossAxisAlignment.start, children: [
            Expanded(child: Text(title, style: Theme.of(context).textTheme.titleMedium)),
            const SizedBox(width: 16), Flexible(child: Align(alignment: Alignment.centerRight, child: amount)),
          ]) else ...[
            Text(title, style: Theme.of(context).textTheme.titleMedium),
            const SizedBox(height: 8), amount,
          ],
          const SizedBox(height: 8),
          Text('Receipt · ${receipt.records.length} expenses'),
          Text('Uploaded by: ${userLabel(receipt.uploadedBy)}'),
          Text('Created $created', style: Theme.of(context).textTheme.bodySmall),
        ]),
      )),
    ));
  }
}
