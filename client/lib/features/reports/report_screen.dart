import 'package:flutter/material.dart';
import 'package:go_router/go_router.dart';

import '../../core/jizhang_repository.dart';
import '../../core/models.dart';
import '../../core/public_name_sort.dart';
import '../../core/session_controller.dart';
import '../../shared/record_navigation.dart';
import '../../shared/split_validation.dart';
import '../../shared/widgets.dart';
import '../groups/group_screen.dart';

class ReportScreen extends StatefulWidget {
  const ReportScreen({super.key, required this.repository, required this.session, required this.groupId});
  final JizhangRepository repository;
  final SessionController session;
  final String groupId;
  @override
  State<ReportScreen> createState() => _ReportScreenState();
}

class _ReportScreenState extends State<ReportScreen> {
  final _loadKey = GlobalKey<LoadViewState<GroupReport>>();
  String? _currency;
  String _sort = 'magnitude';
  bool _descending = true;
  final Set<String> _expanded = {};
  String? _recordError;
  String? _openingRecord;

  int _compare(Balance a, Balance b) {
    final comparison = switch (_sort) {
      'net' => a.totalMinor.compareTo(b.totalMinor),
      'activity' => a.breakdown.length.compareTo(b.breakdown.length),
      'name' => _compareNames(a.user, b.user),
      _ => a.totalMinor.abs().compareTo(b.totalMinor.abs()),
    };
    return comparison == 0 ? a.user.userId.compareTo(b.user.userId) : (_descending ? -comparison : comparison);
  }

  int _compareNames(User a, User b) {
    final display = caseFoldPublicName(a.displayName).compareTo(caseFoldPublicName(b.displayName));
    return display != 0 ? display : caseFoldPublicName(a.username).compareTo(caseFoldPublicName(b.username));
  }

  Future<void> _openRecord(BalanceBreakdown line) async {
    final generation = widget.session.generation;
    setState(() { _recordError = null; _openingRecord = line.recordId; });
    try {
      await widget.repository.getRecord(widget.groupId, line.recordId);
      if (!mounted || generation != widget.session.generation || !widget.session.isAuthenticated ||
          ModalRoute.of(context)?.isCurrent != true) return;
      context.push<void>('/groups/${widget.groupId}/records/${line.recordId}');
    } catch (error) {
      if (mounted) setState(() => _recordError = 'Could not open ${line.title}: $error');
    } finally {
      if (mounted) setState(() => _openingRecord = null);
    }
  }
  String _label(BigInt amount) => amount > BigInt.zero ? 'Gets back' : amount < BigInt.zero ? 'Owes' : 'Settled';
  void _payment(BuildContext context, Settlement settlement, CurrencyInfo currency) {
    final route = Uri(path: '/groups/${widget.groupId}/transfers/new', queryParameters: {
      'fromUserId': settlement.fromUser.userId, 'toUserId': settlement.toUser.userId,
      'currency': currency.code, 'amountMinor': settlement.amountMinor.toString(),
    });
    context.go(route.toString());
  }
  @override
  Widget build(BuildContext context) {
    final scope = GroupScope.of(context);
    final group = scope.group;
    return RecordReturnBoundary(
      session: widget.session, changes: scope.recordChanges,
      onReturn: (changed) async { if (changed) await _loadKey.currentState?.reload(); },
      child: LoadView<GroupReport>(
      key: _loadKey, retainDataOnReload: true,
      load: () => widget.repository.getReport(widget.groupId), builder: (report) {
        final active = group.members.map((user) => user.userId).toSet();
        final sections = report.currencies;
        final selected = sections.where((section) => section.currency.code == _currency).firstOrNull
          ?? sections.where((section) => section.currency.code == group.defaultCurrency.code).firstOrNull
          ?? sections.firstOrNull;
        final currency = selected?.currency ?? group.defaultCurrency;
        final balances = [...?selected?.balances]..sort(_compare);
        final settlements = selected?.settlements ?? <Settlement>[];
        return Column(crossAxisAlignment: CrossAxisAlignment.stretch, children: [
          Text('Balances', style: Theme.of(context).textTheme.headlineSmall),
          const SizedBox(height: 8),
          if (group.currencyMode == CurrencyMode.multi) ...[
            const Text('Currencies settle separately'),
            if (sections.isNotEmpty) DropdownButtonFormField<String>(
              initialValue: currency.code, isExpanded: true, decoration: const InputDecoration(labelText: 'Currency'),
              items: [for (final section in sections) DropdownMenuItem(value: section.currency.code, child: Text(section.currency.code))],
              onChanged: (value) => setState(() => _currency = value)),
          ] else Text('${currency.code} settlement'),
          const SizedBox(height: 16),
          Wrap(spacing: 12, runSpacing: 8, crossAxisAlignment: WrapCrossAlignment.center, children: [
            SizedBox(width: 220, child: DropdownButtonFormField<String>(
              initialValue: _sort, isExpanded: true, decoration: const InputDecoration(labelText: 'Sort members'),
              items: const [DropdownMenuItem(value: 'magnitude', child: Text('Net magnitude')), DropdownMenuItem(value: 'net', child: Text('Signed net')), DropdownMenuItem(value: 'name', child: Text('Name')), DropdownMenuItem(value: 'activity', child: Text('Record activity'))],
              onChanged: (value) { if (value != null) setState(() => _sort = value); })),
            SizedBox(width: 180, child: DropdownButtonFormField<bool>(
              initialValue: _descending, isExpanded: true, decoration: const InputDecoration(labelText: 'Direction'),
              items: const [DropdownMenuItem(value: false, child: Text('Ascending')), DropdownMenuItem(value: true, child: Text('Descending'))],
              onChanged: (value) { if (value != null) setState(() => _descending = value); })),
          ]),
          const SizedBox(height: 12),
          if (_recordError != null) Padding(padding: const EdgeInsets.only(bottom: 12), child: Text(_recordError!, semanticsLabel: _recordError)),
          if (balances.isEmpty || balances.every((balance) => balance.breakdown.isEmpty)) const EmptyState(
            title: 'No record activity', message: 'There is no record activity in this settlement currency.'),
          if (balances.isNotEmpty && balances.every((balance) => balance.totalMinor == BigInt.zero))
            const Padding(padding: EdgeInsets.only(bottom: 12), child: Text('All settled. Nobody owes a payment.')),
          for (final balance in balances)
            Card(key: ValueKey('member:${currency.code}:${balance.user.userId}'), child: ExpansionTile(
              key: PageStorageKey('balance-expansion:${currency.code}:${balance.user.userId}'),
              initiallyExpanded: _expanded.contains('${currency.code}:${balance.user.userId}'),
              onExpansionChanged: (expanded) {
                final key = '${currency.code}:${balance.user.userId}';
                if (expanded) { _expanded.add(key); } else { _expanded.remove(key); }
              },
              tilePadding: const EdgeInsets.all(12),
              title: _ReportAmountRow(
                identity: Column(crossAxisAlignment: CrossAxisAlignment.start, children: [
                  IdentityLabel(user: balance.user, former: !active.contains(balance.user.userId)),
                  Text('${balance.breakdown.length} records · All-time net'),
                  _RoleTotals(balance: balance, currency: currency),
                ]),
                amount: currency.format(balance.totalMinor.abs()),
                status: _label(balance.totalMinor),
              ),
              childrenPadding: const EdgeInsets.fromLTRB(12, 0, 12, 12),
              expandedCrossAxisAlignment: CrossAxisAlignment.stretch,
              children: [
                const Divider(height: 16),
                if (balance.breakdown.isEmpty) const Text('No record activity.')
                else _BalanceRecords(lines: balance.breakdown, currency: currency,
                  openingRecord: _openingRecord, onOpen: _openRecord),
              ],
            )),
          const SizedBox(height: 16),
          const SectionHeading(title: 'Suggested payments',
            supportingText: 'Review payment details before saving.'),
          const SizedBox(height: 8),
          if (settlements.isEmpty) const EmptyState(
            title: 'No payments needed', message: 'There are no suggested payments in this currency.'),
          for (final settlement in settlements)
            Card(child: Padding(padding: const EdgeInsets.all(12), child: Column(
              crossAxisAlignment: CrossAxisAlignment.stretch, children: [
                _ReportAmountRow(
                  identity: Column(crossAxisAlignment: CrossAxisAlignment.start, children: [
                    const Text('From →'),
                    IdentityLabel(user: settlement.fromUser, former: !active.contains(settlement.fromUser.userId)),
                    const SizedBox(height: 8),
                    const Text('To'),
                    IdentityLabel(user: settlement.toUser, former: !active.contains(settlement.toUser.userId)),
                  ]),
                  amount: currency.format(settlement.amountMinor),
                ),
                if (!active.contains(settlement.fromUser.userId) || !active.contains(settlement.toUser.userId)) ...[
                  const SizedBox(height: 8),
                  const Text('Re-add the former member before recording a payment.'),
                ],
                const SizedBox(height: 8),
                Align(alignment: Alignment.centerLeft, child: OutlinedButton(
                  onPressed: canRecordPayment(group, settlement) ? () => _payment(context, settlement, currency) : null,
                  child: const Text('Record payment'))),
              ],
            ))),
        ]);
      },
    ));
  }
}

class _ReportAmountRow extends StatelessWidget {
  const _ReportAmountRow({required this.identity, required this.amount, this.status});
  final Widget identity;
  final String amount;
  final String? status;

  @override
  Widget build(BuildContext context) => LayoutBuilder(builder: (context, constraints) {
    final total = Column(crossAxisAlignment: CrossAxisAlignment.end, children: [
      if (status != null) Text(status!, textAlign: TextAlign.end),
      Text(amount, textAlign: TextAlign.end, style: Theme.of(context).textTheme.titleMedium),
    ]);
    if (constraints.maxWidth < 600) {
      return Column(crossAxisAlignment: CrossAxisAlignment.stretch, children: [
        identity,
        const SizedBox(height: 8),
        Align(alignment: Alignment.centerRight, child: total),
      ]);
    }
    return Row(crossAxisAlignment: CrossAxisAlignment.start, children: [
      Expanded(flex: 2, child: identity),
      const SizedBox(width: 12),
      Expanded(child: total),
    ]);
  });
}

class _RoleTotals extends StatelessWidget {
  const _RoleTotals({required this.balance, required this.currency});
  final Balance balance;
  final CurrencyInfo currency;

  @override
  Widget build(BuildContext context) {
    final pairs = [
      [('Expenses paid', balance.expensesPaidMinor), ('Expense share', balance.expenseShareMinor)],
      [('Income received', balance.incomeReceivedMinor), ('Income entitlement', balance.incomeEntitlementMinor)],
      [('Payments sent', balance.paymentsSentMinor), ('Payments received', balance.paymentsReceivedMinor)],
    ];
    Widget metric((String, BigInt) value) => Padding(
      padding: const EdgeInsets.only(top: 8),
      child: Column(crossAxisAlignment: CrossAxisAlignment.start, children: [
        Text(value.$1, style: Theme.of(context).textTheme.labelMedium),
        Text(currency.format(value.$2)),
      ]));
    return LayoutBuilder(builder: (context, constraints) => Column(
      crossAxisAlignment: CrossAxisAlignment.stretch,
      children: [for (final pair in pairs)
        if (constraints.maxWidth >= 440) Row(crossAxisAlignment: CrossAxisAlignment.start, children: [
          Expanded(child: metric(pair[0])), const SizedBox(width: 16), Expanded(child: metric(pair[1])),
        ]) else ...[metric(pair[0]), metric(pair[1])],
      ],
    ));
  }
}

class _BalanceRecords extends StatefulWidget {
  const _BalanceRecords({required this.lines, required this.currency, required this.openingRecord, required this.onOpen});
  final List<BalanceBreakdown> lines;
  final CurrencyInfo currency;
  final String? openingRecord;
  final ValueChanged<BalanceBreakdown> onOpen;
  @override
  State<_BalanceRecords> createState() => _BalanceRecordsState();
}

class _BalanceRecordsState extends State<_BalanceRecords> {
  final _scroll = ScrollController();
  @override
  void dispose() { _scroll.dispose(); super.dispose(); }
  String _type(BalanceBreakdown line) => switch (line.recordType) {
    RecordKind.expense => 'Expense', RecordKind.income => 'Income', RecordKind.transfer => 'Payment / Transfer',
  };
  String _impact(BalanceBreakdown line) => '${line.amountMinor > BigInt.zero ? '+' : line.amountMinor < BigInt.zero ? '−' : ''}${widget.currency.format(line.amountMinor.abs())} (${line.amountMinor > BigInt.zero ? 'increase' : line.amountMinor < BigInt.zero ? 'decrease' : 'no change'})';
  Widget _action(BalanceBreakdown line) => TextButton(
    onPressed: widget.openingRecord == null ? () => widget.onOpen(line) : null,
    child: Semantics(label: 'Open record ${line.title}', child: Stack(alignment: Alignment.center, children: [
      Opacity(opacity: widget.openingRecord == line.recordId ? 0 : 1, child: const Text('Open record')),
      if (widget.openingRecord == line.recordId) const Positioned.fill(child: Center(child: Text('Opening…'))),
    ])));

  @override
  Widget build(BuildContext context) => LayoutBuilder(builder: (context, constraints) {
    if (MediaQuery.sizeOf(context).width >= 800) {
      return Scrollbar(controller: _scroll, thumbVisibility: true, child: SingleChildScrollView(
        key: const PageStorageKey('balance-record-scroll'),
        controller: _scroll, scrollDirection: Axis.horizontal,
        padding: const EdgeInsets.only(bottom: 16),
        child: DataTable(columns: const [
          DataColumn(label: Text('Date')), DataColumn(label: Text('Type')),
          DataColumn(label: Text('Title')), DataColumn(label: Text('Record amount'), numeric: true),
          DataColumn(label: Text('Balance impact'), numeric: true), DataColumn(label: Text('Open record')),
        ], rows: [for (final line in widget.lines) DataRow(cells: [
          DataCell(Text(line.date)), DataCell(Text(_type(line))),
          DataCell(SizedBox(width: 240, child: Text(line.title, maxLines: 2, overflow: TextOverflow.ellipsis))),
          DataCell(SelectableText(widget.currency.format(line.recordAmountMinor))),
          DataCell(SelectableText(_impact(line))), DataCell(_action(line)),
        ])]),
      ));
    }
    return Column(crossAxisAlignment: CrossAxisAlignment.stretch, children: [
      for (final line in widget.lines) Padding(padding: const EdgeInsets.symmetric(vertical: 8), child: Column(
        crossAxisAlignment: CrossAxisAlignment.stretch, children: [
          Text('Date: ${line.date}'), Text('Type: ${_type(line)}'), Text('Title: ${line.title}'),
          Text('Record amount: ${widget.currency.format(line.recordAmountMinor)}'),
          Text('Balance impact: ${_impact(line)}'),
          Align(alignment: Alignment.centerLeft, child: _action(line)), const Divider(),
        ],
      )),
    ]);
  });
}
