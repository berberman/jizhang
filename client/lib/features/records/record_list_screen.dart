import 'package:flutter/material.dart';
import 'package:go_router/go_router.dart';

import '../../core/jizhang_repository.dart';
import '../../core/models.dart';
import '../../core/pagination_controller.dart';
import '../../core/session_controller.dart';
import '../../shared/widgets.dart';
import '../../shared/record_navigation.dart';
import '../groups/group_screen.dart';
import 'record_search.dart';
import 'record_detail_screen.dart';

// Keep only view queries, never protected records, while routed editors are open.
final _recordQueries = Expando<Map<String, PageQuery>>();

class RecordListScreen extends StatefulWidget {
  const RecordListScreen({super.key, required this.repository, required this.session, required this.groupId});
  final JizhangRepository repository;
  final SessionController session;
  final String groupId;
  @override
  State<RecordListScreen> createState() => _RecordListScreenState();
}

class _RecordListScreenState extends State<RecordListScreen> {
  late String _queryKey;
  late PaginationController<LedgerRecord> _page;
  bool _deleting = false;
  bool get _currentSession => widget.session.generation.toString() == _queryKey.split(':').first;
  void _sessionChanged() {
    if (!_currentSession) _page.clear();
  }
  @override
  void initState() {
    super.initState();
    _initializePage();
  }
  void _initializePage() {
    _queryKey = '${widget.session.generation}:${widget.groupId}';
    final queries = _recordQueries[widget.session] ??= <String, PageQuery>{};
    _page = PaginationController<LedgerRecord>(load: (query) => widget.repository.listRecords(widget.groupId, query),
      query: queries[_queryKey] ?? const PageQuery(sort: '-date'));
    _page.addListener(_rememberQuery);
    widget.session.addListener(_sessionChanged);
    _page.refresh(clamp: true);
  }
  @override
  void didUpdateWidget(covariant RecordListScreen oldWidget) {
    super.didUpdateWidget(oldWidget);
    if (oldWidget.groupId != widget.groupId || oldWidget.session != widget.session) {
      oldWidget.session.removeListener(_sessionChanged);
      _page.removeListener(_rememberQuery);
      _page.dispose();
      _initializePage();
    }
  }
  void _rememberQuery() { _recordQueries[widget.session]![_queryKey] = _page.query; }
  @override
  void dispose() { widget.session.removeListener(_sessionChanged); _page.removeListener(_rememberQuery); _page.dispose(); super.dispose(); }
  Future<void> _delete(LedgerRecord record) async {
    if (_deleting) return;
    setState(() => _deleting = true);
    final deleted = await deleteRecordWithConfirmation(context, widget.repository, widget.groupId, record);
    if (!mounted) return;
    setState(() => _deleting = false);
    if (deleted) {
      showSuccess(context, 'Record deleted');
      await _page.refresh(clamp: true);
    }
  }
  @override
  Widget build(BuildContext context) => RecordReturnBoundary(
    session: widget.session, changes: GroupScope.of(context).recordChanges,
    onReturn: (changed) async { if (changed) await _page.refresh(clamp: true); },
    child: ListenableBuilder(listenable: _page, builder: (context, _) =>
      Column(crossAxisAlignment: CrossAxisAlignment.stretch, children: [
        Wrap(alignment: WrapAlignment.spaceBetween, runSpacing: 12, spacing: 16, crossAxisAlignment: WrapCrossAlignment.center, children: [
          Text('Records', style: Theme.of(context).textTheme.headlineSmall),
          Wrap(spacing: 8, runSpacing: 8, children: [
            FilledButton.icon(onPressed: () => context.go('/groups/${widget.groupId}/expenses/new'), icon: const Icon(Icons.add), label: const Text('New expense')),
            OutlinedButton.icon(onPressed: () => context.go('/groups/${widget.groupId}/incomes/new'), icon: const Icon(Icons.add), label: const Text('New income')),
            OutlinedButton.icon(onPressed: () => context.go('/groups/${widget.groupId}/transfers/new'), icon: const Icon(Icons.payments_outlined), label: const Text('Record payment')),
            IconButton(onPressed: _page.loading ? null : () => _page.refresh(clamp: true), tooltip: 'Refresh records', icon: const Icon(Icons.refresh)),
          ]),
        ]),
        RecordSearchToolbar(key: ValueKey(_queryKey), repository: widget.repository, session: widget.session,
          group: GroupScope.of(context).group, query: _page.query.query, sort: _page.query.sort,
          onSearch: (value) { if (_currentSession) _page.search(value); },
          onSort: (value) { if (_currentSession) _page.setSort(value); },
          queryError: _page.errorStatusCode == 400 ? _page.error : null),
        if (_page.loading) const LinearProgressIndicator(),
        if (_page.error != null) ...[
          if (_page.errorStatusCode != 400) ErrorPanel(error: _page.error!, onRetry: () => _page.refresh(clamp: true), backToGroups: true),
          if (_page.page != null) const Text('Previous results are shown below; the requested query could not be loaded.'),
        ],
        if (!_page.loading && _page.error == null && _page.page?.items.isEmpty == true)
          EmptyState(title: _page.query.query.trim().isEmpty ? 'No records yet' : 'No matching records',
            message: _page.query.query.trim().isEmpty ? 'Add an expense, income, or payment.' : 'Try another query or clear your filters.'),
        if (!_page.loading && _page.error == null && _page.page?.items.isEmpty == true && _page.query.query.isNotEmpty)
          TextButton(onPressed: () => _page.search(''), child: const Text('Clear filters')), 
        for (final record in _page.page?.items ?? <LedgerRecord>[]) _recordCard(context, record),
        const SizedBox(height: 16), PageControls(controller: _page),
      ])));
  Widget _recordCard(BuildContext context, LedgerRecord record) {
    final base = '/groups/${widget.groupId}';
    final localCreated = record.createdAt.toLocal();
    final localizations = MaterialLocalizations.of(context);
    final created = '${localizations.formatCompactDate(localCreated)} ${localizations.formatTimeOfDay(TimeOfDay.fromDateTime(localCreated))}';
    final amount = RecordMoney(record: record);
    final actions = Wrap(spacing: 8, runSpacing: 4, children: [
      TextButton(onPressed: () => context.go('$base/${switch (record.kind) { RecordKind.expense => 'expenses', RecordKind.income => 'incomes', RecordKind.transfer => 'transfers' }}/${record.recordId}/edit'), child: const Text('Edit')),
      TextButton(onPressed: _deleting ? null : () => _delete(record), child: Text('Delete', style: TextStyle(color: Theme.of(context).colorScheme.error))),
    ]);
    return Card(clipBehavior: Clip.antiAlias, child: InkWell(
      onTap: () => context.push<void>('$base/records/${record.recordId}'),
      child: LayoutBuilder(builder: (context, size) => Padding(
        padding: const EdgeInsets.all(12),
        child: Column(crossAxisAlignment: CrossAxisAlignment.stretch, children: [
          if (size.maxWidth >= 800) Row(crossAxisAlignment: CrossAxisAlignment.start, children: [
            Expanded(child: Text(record.title, style: Theme.of(context).textTheme.titleMedium)),
            const SizedBox(width: 16), Flexible(child: Align(alignment: Alignment.centerRight, child: amount)),
          ]) else ...[
            Text(record.title, style: Theme.of(context).textTheme.titleMedium),
            const SizedBox(height: 8), amount,
          ],
          if (record.note?.trim().isNotEmpty ?? false) ...[
            const SizedBox(height: 8),
            Text(record.note!, maxLines: 2, overflow: TextOverflow.ellipsis),
          ],
          const SizedBox(height: 8),
          Text('${switch (record.kind) { RecordKind.expense => 'Expense', RecordKind.income => 'Income', RecordKind.transfer => 'Payment / Transfer' }} · ${record.date}'),
          Text('${record.kind == RecordKind.income ? 'Received by' : 'Paid by'}: ${userLabel(record.actor)}'),
          if (record is TransferRecord) Text('Paid to: ${userLabel(record.transferTo)}'),
          Text('Created $created', style: Theme.of(context).textTheme.bodySmall),
          const SizedBox(height: 8),
          actions,
        ]),
      )),
    ));
  }
}
