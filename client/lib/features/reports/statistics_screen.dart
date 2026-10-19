import 'package:flutter/material.dart';
import 'package:go_router/go_router.dart';
import '../../core/api_client.dart';
import '../../core/jizhang_repository.dart';
import '../../core/models.dart';
import '../../core/session_controller.dart';
import '../../shared/spending_charts.dart';
import '../../shared/record_navigation.dart';
import '../../shared/widgets.dart';
import '../groups/group_screen.dart';

String _day(DateTime d) => '${d.year.toString().padLeft(4, '0')}-${d.month.toString().padLeft(2, '0')}-${d.day.toString().padLeft(2, '0')}';

class StatisticsScreen extends StatefulWidget {
  const StatisticsScreen({super.key, required this.repository, required this.session, required this.groupId, this.parameters = const {}});
  final JizhangRepository repository;
  final SessionController session;
  final String groupId;
  final Map<String, String> parameters;
  @override
  State<StatisticsScreen> createState() => _StatisticsScreenState();
}
class _StatisticsScreenState extends State<StatisticsScreen> {
  late final TextEditingController _from, _to;
  final _fromFocus = FocusNode();
  late StatisticsQuery _query;
  StatsBucket _bucket = StatsBucket.month;
  String? _subject;
  String _view = 'group', _measure = 'share';
  String? _currency, _validation;
  String _preset = 'Custom';
  SpendingStatistics? _data;
  Object? _error;
  bool _loading = false;
  int _request = 0;
  late final int _generation;

  @override
  void initState() {
    super.initState();
    _generation = widget.session.generation;
    _from = TextEditingController(); _to = TextEditingController();
    widget.session.addListener(_sessionChanged);
    _readParameters(resetDraft: true);
    if (_validation == null && !_canonicalize()) _load();
  }
  @override
  void didUpdateWidget(covariant StatisticsScreen oldWidget) {
    super.didUpdateWidget(oldWidget);
    if (oldWidget.groupId == widget.groupId &&
        oldWidget.parameters.length == widget.parameters.length &&
        oldWidget.parameters.entries.every((entry) => widget.parameters[entry.key] == entry.value)) return;
    final queryChanged = _queryChanged(oldWidget.parameters, widget.parameters);
    final current = _parameters();
    final selfNavigation = current.length == widget.parameters.length && current.entries.every((e) => widget.parameters[e.key] == e.value);
    _readParameters(resetDraft: queryChanged || !selfNavigation);
    if (queryChanged || _validation != null) {
      _request++; _data = null; _error = null; _loading = false;
    }
    if (_validation == null && !_canonicalize() && (queryChanged || (_data == null && _error == null && !_loading))) _load();
  }
  bool _queryChanged(Map<String, String> previous, Map<String, String> next) =>
    const ['from', 'to', 'bucket', 'userId'].any((key) => previous[key] != next[key]);
  void _readParameters({required bool resetDraft}) {
    final p = widget.parameters;
    final now = DateTime.now();
    final defaults = {'from': _day(DateTime(now.year, now.month - 5, 1)), 'to': _day(now)};
    final hasDates = p.containsKey('from') || p.containsKey('to');
    final from = hasDates ? p['from'] ?? '' : defaults['from']!;
    final to = hasDates ? p['to'] ?? '' : defaults['to']!;
    final bucket = StatsBucket.values.where((b) => b.name == p['bucket']).firstOrNull ?? StatsBucket.month;
    if (resetDraft) {
      _from.text = from; _to.text = to; _bucket = bucket; _subject = p['userId'];
      _preset = hasDates ? 'Custom' : 'Last 6 months';
    }
    _currency = p['currency'];
    _view = p['view'] ?? 'group'; _measure = p['memberMeasure'] ?? 'share';
    _query = StatisticsQuery(from: from, to: to, bucket: bucket, userId: p['userId']);
    _validation = _query.error;
    if (p.containsKey('bucket') && !StatsBucket.values.any((b) => b.name == p['bucket'])) _validation = 'Choose Day, Week or Month';
    if (!['group','personal'].contains(_view)) _validation = 'Choose Group or Personal view';
    if (!['share','paid'].contains(_measure)) _validation = 'Choose Share or Paid';
    if (_currency != null && !RegExp(r'^[A-Z]{3}$').hasMatch(_currency!)) _validation = 'Choose a valid currency';
  }
  bool _canonicalize() {
    final p = widget.parameters;
    final canonical = _parameters();
    if (canonical.length == p.length && canonical.entries.every((e) => p[e.key] == e.value)) return false;
    final request = _request;
    WidgetsBinding.instance.addPostFrameCallback((_) {
      if (_current(request) && identical(widget.parameters, p) &&
          ModalRoute.of(context)?.isCurrent == true) context.replace(_location(canonical));
    });
    return true;
  }
  bool _current(int request) => mounted && request == _request && widget.session.generation == _generation && widget.session.isAuthenticated;
  void _sessionChanged() {
    if (widget.session.generation != _generation) { _request++; if (mounted) setState(() { _data = null; _error = null; _loading = false; }); }
  }
  Map<String, String> _parameters() => {'from': _query.from, 'to': _query.to, 'bucket': _query.bucket.name, if (_query.userId != null) 'userId': _query.userId!, 'view': _view, 'memberMeasure': _measure, if (_currency != null) 'currency': _currency!};
  String _location(Map<String, String> p) => Uri(path: '/groups/${widget.groupId}/statistics', queryParameters: p).toString();
  void _navigate({bool replace = false}) {
    // Presentation changes retain the loaded data and any unsaved filter draft.
    if (_queryChanged(widget.parameters, _parameters())) {
      _request++;
      setState(() { _data = null; _error = null; _loading = true; });
    } else { setState(() {}); }
    final target = _location(_parameters());
    if (GoRouterState.of(context).uri.toString() == target) { _load(); } else if (replace) { context.replace(target); } else { context.go(target); }
  }
  Future<void> _load({bool retainData = false}) async {
    if (_query.error != null || _validation != null) return;
    final request = ++_request;
    setState(() { if (!retainData) _data = null; _error = null; _loading = true; });
    try {
      final data = await widget.repository.getStatistics(widget.groupId, _query);
      if (!_current(request)) return;
      final group = GroupScope.of(context).group;
      final selected = data.currencies.where((c) => c.currency.code == _currency).firstOrNull
        ?? data.currencies.where((c) => c.currency.code == group.defaultCurrency.code).firstOrNull ?? data.currencies.firstOrNull;
      if (selected != null && selected.currency.code != _currency) {
        _currency = selected.currency.code;
        // Canonicalise the fallback without repeating the API request.
        final target = _location(_parameters());
        setState(() { _data = data; _loading = false; });
        WidgetsBinding.instance.addPostFrameCallback((_) {
          if (_current(request) && _location(_parameters()) == target &&
              ModalRoute.of(context)?.isCurrent == true) context.replace(target);
        });
        return;
      }
      setState(() { _data = data; _loading = false; });
    } catch (error) {
      if (!_current(request)) return;
      final accessError = error is ApiException &&
          (error.statusCode == 401 || error.statusCode == 403 || error.statusCode == 404);
      setState(() {
        if (!retainData || accessError) _data = null;
        _error = error; _loading = false;
      });
    }
  }
  void _apply() {
    final q = StatisticsQuery(from: _from.text, to: _to.text, bucket: _bucket, userId: _subject);
    setState(() { _validation = q.error; if (_validation != null) { _request++; _data = null; _error = null; _loading = false; } });
    if (_validation != null) return;
    _query = q; _navigate();
  }
  void _reset() {
    final n = DateTime.now();
    _from.text = _day(DateTime(n.year, n.month - 5, 1)); _to.text = _day(n);
    _bucket = StatsBucket.month; _subject = null; _currency = null; _view = 'group'; _measure = 'share'; _validation = null;
    _apply();
  }
  void _choosePreset(String value) {
    setState(() => _preset = value);
    if (value == 'Custom') return;
    final n = DateTime.now();
    final start = switch(value) { 'This month' => DateTime(n.year,n.month,1), 'Last month' => DateTime(n.year,n.month-1,1), 'This year' => DateTime(n.year,1,1), _ => DateTime(n.year,n.month-5,1) };
    _from.text = _day(start); _to.text = _day(value == 'Last month' ? DateTime(n.year,n.month,0) : n);
    _apply();
  }
  @override
  void dispose() { _request++; widget.session.removeListener(_sessionChanged); _from.dispose(); _to.dispose(); _fromFocus.dispose(); super.dispose(); }
  Widget _field(Widget child, {double width = 220}) => SizedBox(width: width, child: child);
  @override
  Widget build(BuildContext context) {
    final group = GroupScope.of(context).group;
    final data = _data;
    final section = data?.currencies.where((c) => c.currency.code == _currency).firstOrNull;
    final me = data?.user.userId == widget.session.user?.userId;
    final subject = me ? 'Your' : data == null ? 'Selected member’s' : '${userLabel(data.user)}’s';
    return RecordReturnBoundary(
      session: widget.session, changes: GroupScope.of(context).recordChanges,
      onReturn: (changed) async {
        if (changed) await _load(retainData: true);
        if (mounted && widget.session.generation == _generation &&
            ModalRoute.of(context)?.isCurrent == true && _validation == null) _canonicalize();
      },
      child: Column(crossAxisAlignment: CrossAxisAlignment.stretch, children: [
      Text('Statistics', style: Theme.of(context).textTheme.headlineSmall),
      const SizedBox(height: 16),
      Wrap(spacing: 12, runSpacing: 16, crossAxisAlignment: WrapCrossAlignment.center, children: [
        _field(DropdownButtonFormField<String>(key: ValueKey(_preset), initialValue: _preset, isExpanded: true, decoration: const InputDecoration(labelText: 'Date preset'), items: [for (final p in ['This month','Last month','Last 6 months','This year','Custom']) DropdownMenuItem(value: p, child: Text(p))], onChanged: (v) { if (v != null) _choosePreset(v); })),
        _field(TextField(controller: _from, focusNode: _fromFocus, decoration: const InputDecoration(labelText: 'From (inclusive)', hintText: 'YYYY-MM-DD'), keyboardType: TextInputType.datetime, onChanged: (_) => setState(() { _preset = 'Custom'; }))),
        _field(TextField(controller: _to, decoration: InputDecoration(labelText: 'To (inclusive)', hintText: 'YYYY-MM-DD', errorText: _validation, errorMaxLines: 4), keyboardType: TextInputType.datetime, onChanged: (_) => setState(() { _preset = 'Custom'; }))),
        _field(DropdownButtonFormField<StatsBucket>(key: ValueKey(_bucket), initialValue: _bucket, isExpanded: true, decoration: const InputDecoration(labelText: 'Bucket'), items: [for (final b in StatsBucket.values) DropdownMenuItem(value: b, child: Text(switch(b) { StatsBucket.day => 'Day', StatsBucket.week => 'Week', StatsBucket.month => 'Month' }))], onChanged: (v) => setState(() { if (v != null) _bucket = v; }))),
        FilledButton(onPressed: _apply, child: const Text('Apply')),
      ]),
      if (_validation != null) Padding(padding: const EdgeInsets.only(top: 8), child: Semantics(liveRegion: true, child: Text(_validation!, style: TextStyle(color: Theme.of(context).colorScheme.error)))),
      TextButton(onPressed: _reset, child: const Text('Reset filters')),
      if (_loading) data == null
        ? const Padding(padding: EdgeInsets.all(24), child: Center(child: CircularProgressIndicator()))
        : const LinearProgressIndicator(),
      if (_error != null) ...[
        if (data != null) const Text('Showing previously loaded data.'),
        ErrorPanel(error: _error!, onRetry: () => _load(retainData: data != null)),
      ],
      if (data != null) KeyedSubtree(key: const ValueKey('statistics-data'),
        child: Column(crossAxisAlignment: CrossAxisAlignment.stretch, children: [
        const SizedBox(height: 16),
        Wrap(spacing: 12, runSpacing: 16, crossAxisAlignment: WrapCrossAlignment.center, children: [
          _field(DropdownButtonFormField<String>(initialValue: _query.userId ?? '', isExpanded: true, decoration: const InputDecoration(labelText: 'Viewing'), items: [const DropdownMenuItem(value: '', child: Text('Me')), for (final u in data.participants) if (u.userId != widget.session.user?.userId || _query.userId == u.userId) DropdownMenuItem(value: u.userId, child: Text('${userLabel(u)}${group.members.any((m) => m.userId == u.userId) ? '' : ' (former)'}'))], onChanged: (v) { _subject = v == '' ? null : v; _query = StatisticsQuery(from: _query.from, to: _query.to, bucket: _query.bucket, userId: _subject); _navigate(); }), width: 300),
          if (group.currencyMode == CurrencyMode.multi && data.currencies.isNotEmpty) _field(DropdownButtonFormField<String>(initialValue: _currency, decoration: const InputDecoration(labelText: 'Currency'), items: [for (final c in data.currencies) DropdownMenuItem(value: c.currency.code, child: Text(c.currency.code))], onChanged: (v) { _currency = v; _navigate(); })),
          ChoiceChip(label: const Text('Group'), selected: _view == 'group', onSelected: (_) { _view = 'group'; _navigate(); }),
          ChoiceChip(label: const Text('Personal'), selected: _view == 'personal', onSelected: (_) { _view = 'personal'; _navigate(); }),
        ]),
        const SizedBox(height: 16),
        if (section == null) Column(crossAxisAlignment: CrossAxisAlignment.start, children: [const Text('No activity in this date range'), TextButton(onPressed: () { setState(() => _preset = 'Custom'); _fromFocus.requestFocus(); }, child: const Text('Change dates'))]) else ...[
          Wrap(spacing: 16, runSpacing: 12, children: [
            _Summary(label: 'Group spending', value: section.currency.format(section.totalMinor)),
            _Summary(label: 'Expense count', value: section.expenseCount.toString()),
            _Summary(label: 'Group income', value: section.currency.format(section.incomeMinor)),
            _Summary(label: 'Income count', value: section.incomeCount.toString()),
            _Summary(label: 'Net (income − expenses)', value: section.currency.format(section.netMinor)),
            _Summary(label: '$subject expense share', value: section.currency.format(section.shareMinor)),
            _Summary(label: me ? 'Expenses paid by you' : 'Expenses paid by ${userLabel(data.user)}', value: section.currency.format(section.paidMinor)),
          ]),
          const SizedBox(height: 16),
          LayoutBuilder(builder: (context, c) {
            final time = SpendingTimeChart(key: ValueKey('time:$_currency:$_view'), spending: section, personal: _view == 'personal');
            final breakdown = _view == 'personal' ? SpendingExpenseBars(spending: section, subjectLabel: subject, onExpense: (e) => context.push<void>('/groups/${widget.groupId}/records/${e.recordId}')) : SpendingMembersChart(spending: section, paid: _measure == 'paid', activeIds: group.members.map((u) => u.userId).toSet(), onMeasureChanged: (paid) { _measure = paid ? 'paid' : 'share'; _navigate(); });
            return c.maxWidth >= 900 ? Row(crossAxisAlignment: CrossAxisAlignment.start, children: [Expanded(child: time), const SizedBox(width: 16), Expanded(child: breakdown)]) : Column(crossAxisAlignment: CrossAxisAlignment.stretch, children: [time, const SizedBox(height: 16), breakdown]);
          }),
        ],
      ])),
    ]));
  }
}
class _Summary extends StatelessWidget {
  const _Summary({required this.label, required this.value});
  final String label, value;
  @override
  Widget build(BuildContext context) => ConstrainedBox(constraints: const BoxConstraints(maxWidth: 300), child: Card(child: Semantics(container: true, label: '$label: $value', excludeSemantics: true, child: Padding(padding: const EdgeInsets.all(12), child: Column(crossAxisAlignment: CrossAxisAlignment.start, children: [Text(label), SelectableText(value, style: Theme.of(context).textTheme.titleLarge)])))));
}
