import 'dart:math' as math;
import 'package:flutter/material.dart';
import '../core/models.dart';
import 'widgets.dart';

// Only bounded ratios cross into floating point; amounts never do.
double spendingRatio(BigInt value, BigInt total) => total == BigInt.zero ? 0 :
    ((value * BigInt.from(1000000)) ~/ total).toInt() / 1000000;
const _palette = [Color(0xFF0F766E), Color(0xFF1D4ED8)];

class SpendingChartCard extends StatelessWidget {
  const SpendingChartCard({super.key, required this.title, required this.child});
  final String title;
  final Widget child;
  @override
  Widget build(BuildContext context) => Card(child: Padding(padding: const EdgeInsets.all(12), child:
    Column(crossAxisAlignment: CrossAxisAlignment.stretch, children: [Text(title, style: Theme.of(context).textTheme.titleLarge), const SizedBox(height: 16), child])));
}

class SpendingTimeChart extends StatefulWidget {
  const SpendingTimeChart({super.key, required this.spending, required this.personal});
  final CurrencySpending spending;
  final bool personal;
  @override
  State<SpendingTimeChart> createState() => _SpendingTimeChartState();
}
class _SpendingTimeChartState extends State<SpendingTimeChart> {
  final _scroll = ScrollController();
  int? _selected;
  @override
  void dispose() { _scroll.dispose(); super.dispose(); }
  @override
  Widget build(BuildContext context) {
    final s = widget.spending;
    final values = [for (final b in s.buckets) widget.personal ? b.shareMinor : b.totalMinor];
    final max = values.fold<BigInt>(BigInt.zero, (a, b) => a > b ? a : b);
    final selected = _selected != null && _selected! < s.buckets.length ? _selected : null;
    final step = math.max(1, (s.buckets.length / 6).ceil());
    return SpendingChartCard(title: widget.personal ? 'Expense share over time' : 'Group expenses over time', child: Column(crossAxisAlignment: CrossAxisAlignment.stretch, children: [
      Text('${s.currency.code} · zero to ${s.currency.format(max)}'),
      if (selected != null) Padding(padding: const EdgeInsets.symmetric(vertical: 8), child: Semantics(liveRegion: true, child: Text('${s.buckets[selected].start} – ${s.buckets[selected].end}: ${s.currency.format(values[selected])}'))),
      Semantics(label: '${widget.personal ? 'Allocated' : 'Group'} spending by date. Exact amounts in View data.', child: Scrollbar(controller: _scroll, thumbVisibility: true, trackVisibility: true, child: SingleChildScrollView(controller: _scroll, scrollDirection: Axis.horizontal, padding: const EdgeInsets.only(bottom: 16), child: Column(crossAxisAlignment: CrossAxisAlignment.start, children: [
        Row(crossAxisAlignment: CrossAxisAlignment.end, children: [
          for (var i = 0; i < values.length; i++) SizedBox(width: 56, child:
            Tooltip(message: '${s.buckets[i].start} – ${s.buckets[i].end}: ${s.currency.format(values[i])}', child: SizedBox(height: 176, child: TextButton(
              style: TextButton.styleFrom(padding: const EdgeInsets.symmetric(horizontal: 8), minimumSize: const Size(48, 48)),
              onPressed: () => setState(() => _selected = i),
              child: Semantics(label: '${s.buckets[i].start} to ${s.buckets[i].end}, ${s.currency.format(values[i])}', selected: selected == i, child: ExcludeSemantics(child: Align(alignment: Alignment.bottomCenter, child: Container(width: 28, height: spendingRatio(values[i], max) * 160, decoration: BoxDecoration(color: selected == i ? _palette[1] : _palette[0], borderRadius: BorderRadius.circular(3)))))))))),
        ]),
        Row(crossAxisAlignment: CrossAxisAlignment.start, children: [for (var i = 0; i < values.length; i++) SizedBox(width: 56, child: i % step == 0 || i == values.length - 1 ? Text(s.buckets[i].start.substring(5), textAlign: TextAlign.center) : const SizedBox.shrink())]),
      ])))),
      Text('0 ${s.currency.code}'),
      ExpansionTile(key: const PageStorageKey('spending-time-data'), title: const Text('View data'), tilePadding: EdgeInsets.zero, children: [
        _SpendingDataTable(storageId: 'time', headings: const ['Start', 'End', 'Group expenses', 'Expense count', 'Group income', 'Income count', 'Group net', 'Selected member share', 'Selected member paid'], numericColumns: const {2, 3, 4, 5, 6, 7, 8}, rows: [
          for (final b in s.buckets) [b.start, b.end, s.currency.format(b.totalMinor), b.expenseCount.toString(), s.currency.format(b.incomeMinor), b.incomeCount.toString(), s.currency.format(b.netMinor), s.currency.format(b.shareMinor), s.currency.format(b.paidMinor)],
        ]),
      ]),
    ]));
  }
}

class SpendingMembersChart extends StatefulWidget {
  const SpendingMembersChart({super.key, required this.spending, required this.paid, required this.onMeasureChanged, required this.activeIds});
  final CurrencySpending spending;
  final bool paid;
  final ValueChanged<bool> onMeasureChanged;
  final Set<String> activeIds;
  @override
  State<SpendingMembersChart> createState() => _SpendingMembersChartState();
}
class _SpendingMembersChartState extends State<SpendingMembersChart> {
  bool _all = false;
  String? _selected;
  @override
  Widget build(BuildContext context) {
    final s = widget.spending;
    BigInt amount(MemberSpending m) => widget.paid ? m.paidMinor : m.shareMinor;
    final rows = [...s.members]..sort((a,b) { final c = amount(b).compareTo(amount(a)); return c == 0 ? a.user.userId.compareTo(b.user.userId) : c; });
    final max = rows.isEmpty ? BigInt.zero : amount(rows.first);
    return SpendingChartCard(title: widget.paid ? 'Expenses paid by member' : 'Expense shares by member', child: Column(crossAxisAlignment: CrossAxisAlignment.stretch, children: [
      Wrap(spacing: 8, children: [ChoiceChip(label: const Text('Share'), selected: !widget.paid, onSelected: (_) => widget.onMeasureChanged(false)), ChoiceChip(label: const Text('Paid'), selected: widget.paid, onSelected: (_) => widget.onMeasureChanged(true))]),
      const SizedBox(height: 16),
      for (final row in _all ? rows : rows.take(10)) Padding(padding: const EdgeInsets.only(bottom: 12), child: InkWell(onTap: () => setState(() => _selected = row.user.userId), borderRadius: BorderRadius.circular(8), child: Padding(padding: const EdgeInsets.all(4), child: Column(crossAxisAlignment: CrossAxisAlignment.stretch, children: [
        IdentityLabel(user: row.user, former: !widget.activeIds.contains(row.user.userId)),
        Text('${widget.paid ? 'Paid' : 'Share'}: ${s.currency.format(amount(row))}${_selected == row.user.userId ? ' · Selected' : ''}'),
        const SizedBox(height: 4),
        ExcludeSemantics(child: LayoutBuilder(builder: (_, c) => Align(alignment: Alignment.centerLeft, child: Container(height: 12, width: c.maxWidth * spendingRatio(amount(row), max), color: _palette[0])))),
      ])))),
      if (rows.length > 10) TextButton(onPressed: () => setState(() => _all = !_all), child: Text(_all ? 'Show top ten' : 'Show all')),
      ExpansionTile(key: const PageStorageKey('spending-member-data'), title: const Text('View data'), tilePadding: EdgeInsets.zero, children: [
        _SpendingDataTable(storageId: 'members', headings: const ['Member', 'Allocated share', 'Paid'], numericColumns: const {1, 2}, rows: [
          for (final row in rows) ['${userLabel(row.user)}${widget.activeIds.contains(row.user.userId) ? '' : ' (former)'}', s.currency.format(row.shareMinor), s.currency.format(row.paidMinor)],
        ]),
      ]),
    ]));
  }
}

class SpendingExpenseBars extends StatelessWidget {
  const SpendingExpenseBars({super.key, required this.spending, required this.subjectLabel, required this.onExpense});
  final CurrencySpending spending;
  final String subjectLabel;
  final ValueChanged<PersonalExpense> onExpense;

  @override
  Widget build(BuildContext context) {
    final s = spending;
    final hasSpending = s.shareMinor > BigInt.zero;
    final max = [for (final e in s.topExpenses) e.amountMinor, s.otherExpensesMinor]
        .fold<BigInt>(BigInt.zero, (a, b) => a > b ? a : b);
    String percent(BigInt n) {
      final bp = hasSpending ? (n * BigInt.from(10000)) ~/ s.shareMinor : BigInt.zero;
      return '${bp ~/ BigInt.from(100)}.${(bp % BigInt.from(100)).toString().padLeft(2, '0')}%';
    }
    Widget bar(BigInt amount, {bool other = false}) => Padding(
      padding: const EdgeInsets.only(top: 8),
      child: ExcludeSemantics(child: LayoutBuilder(builder: (_, constraints) => Align(
        alignment: Alignment.centerLeft,
        child: Container(height: 12, width: constraints.maxWidth * spendingRatio(amount, max),
          color: other ? Theme.of(context).colorScheme.outline : _palette[0]),
      ))),
    );
    Widget action(PersonalExpense expense) => TextButton(
      style: TextButton.styleFrom(minimumSize: const Size(48, 48)),
      onPressed: () => onExpense(expense),
      child: Text('Open expense', semanticsLabel: 'Open expense: ${expense.title}'),
    );
    return SpendingChartCard(title: '$subjectLabel · Top five expense shares', child: Column(crossAxisAlignment: CrossAxisAlignment.stretch, children: [
      if (!hasSpending) const Text('No allocated spending in this date range') else ...[
        Text('Bar scale: 0 to ${s.currency.format(max)}'),
        for (var i = 0; i < s.topExpenses.length; i++) Padding(
          padding: const EdgeInsets.symmetric(vertical: 8),
          child: Column(crossAxisAlignment: CrossAxisAlignment.stretch, children: [
            Text('${i + 1}. ${s.topExpenses[i].title}'),
            Text('${s.topExpenses[i].date} · ${s.currency.format(s.topExpenses[i].amountMinor)} · ${percent(s.topExpenses[i].amountMinor)}'),
            bar(s.topExpenses[i].amountMinor),
            Align(alignment: Alignment.centerLeft, child: action(s.topExpenses[i])),
          ]),
        ),
        if (s.otherExpensesMinor > BigInt.zero) Padding(
          padding: const EdgeInsets.symmetric(vertical: 8),
          child: Column(crossAxisAlignment: CrossAxisAlignment.stretch, children: [
            Text('Other · ${s.currency.format(s.otherExpensesMinor)} · ${percent(s.otherExpensesMinor)}'),
            bar(s.otherExpensesMinor, other: true),
          ]),
        ),
      ],
      SelectableText('Total allocated share: ${s.currency.format(s.shareMinor)}'),
      ExpansionTile(key: const PageStorageKey('spending-expense-data'), title: const Text('View data'), tilePadding: EdgeInsets.zero, children: [
        _SpendingDataTable(storageId: 'expenses', headings: const ['Rank', 'Expense', 'Date', 'Allocated share', 'Percent', 'Action'], numericColumns: const {0, 3, 4}, rows: [
          if (hasSpending) for (var i = 0; i < s.topExpenses.length; i++) [
            '${i + 1}', s.topExpenses[i].title, s.topExpenses[i].date,
            s.currency.format(s.topExpenses[i].amountMinor), percent(s.topExpenses[i].amountMinor), action(s.topExpenses[i]),
          ],
          if (hasSpending && s.otherExpensesMinor > BigInt.zero) ['', 'Other', '', s.currency.format(s.otherExpensesMinor), percent(s.otherExpensesMinor), ''],
          ['', 'Total allocated share', '', s.currency.format(s.shareMinor), percent(s.shareMinor), ''],
        ]),
      ]),
    ]));
  }
}

class _SpendingDataTable extends StatefulWidget {
  const _SpendingDataTable({required this.storageId, required this.headings, required this.numericColumns, required this.rows});
  final String storageId;
  final List<String> headings;
  final Set<int> numericColumns;
  final List<List<Object>> rows;

  @override
  State<_SpendingDataTable> createState() => _SpendingDataTableState();
}

class _SpendingDataTableState extends State<_SpendingDataTable> {
  final _scroll = ScrollController();
  // Disclosure flags, horizontal offsets and selectable text each own storage.
  final _storage = PageStorageBucket();

  @override
  void dispose() { _scroll.dispose(); super.dispose(); }

  @override
  Widget build(BuildContext context) => PageStorage(bucket: _storage, child: Scrollbar(
    controller: _scroll, thumbVisibility: true, trackVisibility: true,
    notificationPredicate: (notification) => notification.metrics.axis == Axis.horizontal,
    child: SingleChildScrollView(
      key: PageStorageKey('${widget.storageId}:horizontal'), controller: _scroll,
      scrollDirection: Axis.horizontal, padding: const EdgeInsets.only(bottom: 16),
      child: DataTable(
        dataRowMinHeight: 56, dataRowMaxHeight: double.infinity,
        columns: [for (var column = 0; column < widget.headings.length; column++) DataColumn(
          numeric: widget.numericColumns.contains(column),
          label: Text(widget.headings[column]),
        )],
        rows: [for (var row = 0; row < widget.rows.length; row++) DataRow(cells: [
          for (var column = 0; column < widget.headings.length; column++) DataCell(
            widget.rows[row][column] is Widget ? widget.rows[row][column] as Widget :
              ConstrainedBox(constraints: const BoxConstraints(maxWidth: 240), child: SelectableText(
                widget.rows[row][column].toString(),
                key: PageStorageKey('${widget.storageId}:cell:$row:$column'),
                textAlign: widget.numericColumns.contains(column) ? TextAlign.right : TextAlign.left,
              )),
          ),
        ])],
      ),
    ),
  ));
}
