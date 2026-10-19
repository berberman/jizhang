import 'package:flutter/material.dart';

import '../../core/api_client.dart';
import '../../core/exchange_rate_controller.dart';
import '../../core/jizhang_repository.dart';
import '../../core/models.dart';
import '../../core/money.dart';
import '../../core/session_controller.dart';
import '../../shared/currency_amount_fields.dart';
import '../../shared/entity_picker.dart';
import '../../shared/form_controller.dart';
import '../../shared/split_validation.dart';
import '../../shared/widgets.dart';
import '../../shared/record_navigation.dart';
import '../groups/group_screen.dart';

String calendarToday() {
  final now = DateTime.now();
  return '${now.year.toString().padLeft(4, '0')}-${now.month.toString().padLeft(2, '0')}-${now.day.toString().padLeft(2, '0')}';
}
void validateCalendarDate(String value) {
  final parsed = DateTime.tryParse(value);
  if (!RegExp(r'^\d{4}-\d{2}-\d{2}$').hasMatch(value) || parsed == null ||
      '${parsed.year.toString().padLeft(4, '0')}-${parsed.month.toString().padLeft(2, '0')}-${parsed.day.toString().padLeft(2, '0')}' != value) {
    throw const FormatException('Date must be a valid YYYY-MM-DD calendar date');
  }
}

class SplitFieldsController extends ChangeNotifier {
  SplitFieldsController({required this.repository, required this.session, required this.group, this.kind = RecordKind.expense,
    SplitRecordDraft? draft, CurrencyInfo? savedCurrency, ExchangeRate? savedRate, String? currentUserId, List<User> historicalUsers = const []})
      : title = TextEditingController(text: draft?.title ?? ''),
        note = TextEditingController(text: draft?.note ?? ''),
        amount = TextEditingController(text: draft == null ? '' : (savedCurrency ?? group.defaultCurrency).formatMajor(draft.amountMinor)),
        date = TextEditingController(text: draft?.date ?? calendarToday()),
        currency = savedCurrency ?? group.defaultCurrency,
        actorUserId = draft?.actorUserId ?? (group.members.any((u) => u.userId == currentUserId) ? currentUserId! : group.members.firstOrNull?.userId ?? ''),
        splitMode = draft?.splitMode ?? SplitMode.equal {
    if (kind == RecordKind.transfer || (draft != null && draft.kind != kind)) throw ArgumentError('Split fields require a matching expense or income kind');
    for (final user in [...historicalUsers, ...group.members]) { knownUsers[user.userId] = user; }
    selected.addAll(draft?.splits.map((s) => s.userId) ?? group.members.map((u) => u.userId));
    if (draft != null && splitMode != SplitMode.equal) {
      _visited.add(splitMode);
      for (final row in draft.splits) {
        final text = switch (splitMode) {
          SplitMode.percentage => formatMajorAmount(row.value, minorDigits: 2),
          SplitMode.exact => currency.formatMajor(row.value),
          _ => row.value.toString(),
        };
        _input(splitMode, row.userId, text);
      }
    }
    exchangeRate = ExchangeRateController(repository: repository, session: session, group: group,
      sourceCurrency: currency.code, savedRate: savedRate ?? draft?.savedExchangeRate);
    exchangeRate.addListener(notifyListeners);
    title.addListener(notifyListeners); note.addListener(notifyListeners); amount.addListener(notifyListeners); date.addListener(notifyListeners);
  }
  final JizhangRepository repository;
  final SessionController session;
  final RecordKind kind;
  ExpenseGroup group;
  CurrencyInfo currency;
  final TextEditingController title, note, amount, date;
  late final ExchangeRateController exchangeRate;
  String actorUserId;
  SplitMode splitMode;
  String? currencyReview;
  final Set<String> selected = {};
  final Set<SplitMode> _visited = {};
  final Map<SplitMode, Map<String, TextEditingController>> _inputs = {};
  final Map<String, User> knownUsers = {};
  TextEditingController _input(SplitMode mode, String id, [String? initial]) =>
    (_inputs[mode] ??= {}).putIfAbsent(id, () {
      final c = TextEditingController(text: initial ?? (mode == SplitMode.shares ? '1' : '0'));
      c.addListener(notifyListeners);
      return c;
    });
  TextEditingController splitInput(String id) => _input(splitMode, id);
  String label(String id) => knownUsers[id] == null ? 'Unavailable user ($id)' : userLabel(knownUsers[id]!);
  Set<String> get active => group.members.map((u) => u.userId).toSet();
  bool get hasFormerMembers => !active.contains(actorUserId) || selected.any((id) => !active.contains(id));
  BigInt get amountMinor => parseMajorAmount(amount.text, minorDigits: currency.minorUnits);
  BigInt get settlementMinor => exchangeRate.needsConversion ? convertMinorAmount(amountMinor,
    sourceMinorDigits: currency.minorUnits, targetMinorDigits: group.defaultCurrency.minorUnits,
    rate: parseDecimalRate(exchangeRate.validatedRate)) : amountMinor;
  List<SplitDraft> get splitDrafts => [for (final id in selected) SplitDraft(userId: id, value: switch (splitMode) {
    SplitMode.equal => BigInt.one,
    SplitMode.percentage => parsePercentage(splitInput(id).text),
    SplitMode.exact => parseMajorAmount(splitInput(id).text, minorDigits: currency.minorUnits, allowZero: true),
    SplitMode.shares => _shares(splitInput(id).text),
  })];
  BigInt _shares(String text) {
    if (!RegExp(r'^\d+$').hasMatch(text.trim())) throw const FormatException('Shares must be integers from 0 to 32767');
    final value = BigInt.parse(text.trim());
    if (value > BigInt.from(32767)) throw const FormatException('Shares must be integers from 0 to 32767');
    return value;
  }
  Map<String, BigInt> preview({bool settlement = false}) {
    final rows = [for (final s in splitDrafts) MoneySplitValue(s.userId, s.value)];
    final original = allocateSplits(splitMode.name, amountMinor, rows);
    return {for (final s in settlement ? apportion(settlementMinor, rows) : original) s.userId: s.value};
  }
  void updateGroup(ExpenseGroup value) {
    group = value; for (final user in value.members) { knownUsers[user.userId] = user; }
    exchangeRate.updateGroup(value); notifyListeners();
  }
  void setActorUserId(String value) { actorUserId = value; notifyListeners(); }
  void setCurrency(CurrencyInfo value) {
    if (currency.code == value.code) return;
    currency = value; currencyReview = 'Review amounts in ${value.code}';
    exchangeRate.setCurrency(value.code); notifyListeners();
  }
  void setSplitMode(SplitMode mode) {
    if (mode == splitMode) return;
    splitMode = mode;
    if (_visited.add(mode)) {
      if (mode == SplitMode.percentage || mode == SplitMode.exact) { splitEqually(); }
      else if (mode == SplitMode.shares) { for (final id in selected) { _input(mode, id); } }
    }
    notifyListeners();
  }
  void select(String id, bool value) {
    if (value) {
      selected.add(id);
      for (final mode in _visited) { if (mode != SplitMode.equal) _input(mode, id); }
    } else { selected.remove(id); }
    notifyListeners();
  }
  void setSplitValue(String id, String value) { splitInput(id).text = value; }
  void splitEqually() {
    if (selected.isEmpty || (splitMode != SplitMode.percentage && splitMode != SplitMode.exact)) return;
    BigInt total;
    try { total = splitMode == SplitMode.percentage ? BigInt.from(10000) : amountMinor; }
    on FormatException { total = BigInt.zero; }
    final allocations = apportion(total, [for (final id in selected) MoneySplitValue(id, BigInt.one)]);
    for (final s in allocations) {
      _input(splitMode, s.userId).text = formatMajorAmount(s.value, minorDigits: splitMode == SplitMode.percentage ? 2 : currency.minorUnits);
    }
    notifyListeners();
  }
  void assignRemainder(String id) {
    final total = splitMode == SplitMode.percentage ? BigInt.from(10000) : amountMinor;
    var others = BigInt.zero;
    for (final user in selected.where((user) => user != id)) {
      others += splitMode == SplitMode.percentage ? parsePercentage(splitInput(user).text) :
        parseMajorAmount(splitInput(user).text, minorDigits: currency.minorUnits, allowZero: true);
    }
    if (others > total) throw const FormatException('Other rows already exceed the total');
    splitInput(id).text = formatMajorAmount(total - others, minorDigits: splitMode == SplitMode.percentage ? 2 : currency.minorUnits);
    notifyListeners();
  }
  SplitRecordDraft toDraft() {
    if (title.text.trim().isEmpty) throw const FormatException('Title is required');
    final total = amountMinor;
    validateCalendarDate(date.text);
    if (hasFormerMembers) throw const FormatException('Re-add this member before saving changes');
    final splits = splitDrafts;
    final error = validateSplits(splitMode, total, splits);
    if (error != null) throw FormatException(error);
    final rate = exchangeRate.toInput();
    settlementMinor;
    return SplitRecordDraft(kind: kind, title: title.text, note: note.text, amountMinor: total, currency: currency.code, actorUserId: actorUserId,
      date: date.text, splitMode: splitMode, splits: splits, exchangeRate: rate);
  }
  void deactivate() => exchangeRate.deactivate();
  @override
  void dispose() {
    exchangeRate.removeListener(notifyListeners); exchangeRate.dispose();
    title.dispose(); note.dispose(); amount.dispose(); date.dispose();
    for (final map in _inputs.values) { for (final c in map.values) { c.dispose(); } }
    super.dispose();
  }
}

class _FormFieldPair extends StatelessWidget {
  const _FormFieldPair({required this.first, required this.second});
  final Widget first, second;
  @override
  Widget build(BuildContext context) => LayoutBuilder(builder: (context, constraints) => constraints.maxWidth >= 600
    ? Row(crossAxisAlignment: CrossAxisAlignment.start, children: [Expanded(child: first), const SizedBox(width: 12), Expanded(child: second)])
    : Column(crossAxisAlignment: CrossAxisAlignment.stretch, children: [first, const SizedBox(height: 12), second]));
}

class SplitFields extends StatefulWidget {
  const SplitFields({super.key, required this.session, required this.controller, required this.onChanged, this.enabled = true});
  final SessionController session;
  final SplitFieldsController controller;
  final VoidCallback onChanged;
  final bool enabled;
  @override
  State<SplitFields> createState() => _SplitFieldsState();
}
class _SplitFieldsState extends State<SplitFields> {
  final _search = TextEditingController();
  String? _error;
  @override
  void dispose() { _search.dispose(); super.dispose(); }
  void _action(VoidCallback action) {
    try { action(); _error = null; widget.onChanged(); }
    on FormatException catch (e) { setState(() => _error = e.message); }
  }
  @override
  Widget build(BuildContext context) => ListenableBuilder(listenable: widget.controller, builder: (context, _) {
    final c = widget.controller;
    c.exchangeRate.onPolicyConflict = () => GroupScope.of(context).refreshGroup();
    final query = _search.text.trim().toLowerCase();
    final matches = c.group.members.where((u) => query.startsWith('@') ? u.username.toLowerCase().contains(query.substring(1)) : '${u.username} ${u.displayName}'.toLowerCase().contains(query));
    Map<String, BigInt> original = {}, converted = {};
    String? previewError;
    String? assigned;
    try {
      if (c.splitMode == SplitMode.percentage || c.splitMode == SplitMode.exact) {
        final sum = c.splitDrafts.fold(BigInt.zero, (v, s) => v + s.value);
        final total = c.splitMode == SplitMode.percentage ? BigInt.from(10000) : c.amountMinor;
        final left = total - sum;
        String show(BigInt v) => c.splitMode == SplitMode.percentage ? '${formatMajorAmount(v, minorDigits: 2)}%' : c.currency.format(v);
        assigned = 'Assigned ${show(sum)} · ${show(left.abs())} ${left.isNegative ? 'excess (error)' : 'remaining'}';
      }
      original = c.preview();
      if (c.exchangeRate.needsConversion) converted = c.preview(settlement: true);
    } on FormatException catch (e) { previewError = e.message; }
    const labels = {SplitMode.equal: 'Equal', SplitMode.shares: 'Shares', SplitMode.percentage: 'Percentages', SplitMode.exact: 'Exact amounts'};
    return Column(crossAxisAlignment: CrossAxisAlignment.stretch, children: [
      TextField(controller: c.title, enabled: widget.enabled, decoration: const InputDecoration(labelText: 'Title'), onChanged: (_) => widget.onChanged()),
      const SizedBox(height: 12),
      TextField(controller: c.note, enabled: widget.enabled, minLines: 3, maxLines: null,
        keyboardType: TextInputType.multiline, decoration: const InputDecoration(labelText: 'Note (optional)'), onChanged: (_) => widget.onChanged()),
      const SizedBox(height: 12),
      CurrencyAmountFields(repository: c.repository, session: widget.session, currency: c.currency,
        amountController: c.amount, enabled: widget.enabled, onCurrencyChanged: c.setCurrency, onChanged: widget.onChanged),
      if (c.currencyReview != null) Text(c.currencyReview!),
      ExchangeRateFields(controller: c.exchangeRate, enabled: widget.enabled, onChanged: widget.onChanged),
      if (converted.isNotEmpty) Text('Settlement total: ${c.group.defaultCurrency.format(c.settlementMinor)}'),
      const SizedBox(height: 12),
      TextField(controller: c.date, enabled: widget.enabled, decoration: const InputDecoration(labelText: 'Date (YYYY-MM-DD)'), onChanged: (_) => widget.onChanged()),
      const SizedBox(height: 12),
      UserPickerField(session: widget.session, label: c.kind == RecordKind.income ? 'Received by' : 'Paid by', selectedUserId: c.actorUserId.isEmpty ? null : c.actorUserId,
        knownUsers: c.knownUsers, choices: c.group.members, enabled: widget.enabled, onChanged: (id) => _action(() => c.setActorUserId(id))),
      const SizedBox(height: 12),
      LayoutBuilder(builder: (context, size) => size.maxWidth >= 600
        ? Column(crossAxisAlignment: CrossAxisAlignment.stretch, children: [
            const Text('Split method'),
            const SizedBox(height: 8),
            SegmentedButton<SplitMode>(segments: [for (final mode in SplitMode.values) ButtonSegment(value: mode, label: Text(labels[mode]!))], selected: {c.splitMode},
              onSelectionChanged: widget.enabled ? (m) => _action(() => c.setSplitMode(m.single)) : null),
          ])
        : DropdownButtonFormField<SplitMode>(key: ValueKey(c.splitMode), initialValue: c.splitMode,
            decoration: const InputDecoration(labelText: 'Split method'), items: [for (final mode in SplitMode.values) DropdownMenuItem(value: mode, child: Text(labels[mode]!))],
            onChanged: widget.enabled ? (m) { if (m != null) _action(() => c.setSplitMode(m)); } : null)),
      const SizedBox(height: 8),
      Text('${c.selected.length} ${c.kind == RecordKind.income ? 'beneficiaries' : 'participants'} selected'),
      const SizedBox(height: 12),
      TextField(controller: _search, enabled: widget.enabled, decoration: InputDecoration(labelText: c.kind == RecordKind.income ? 'Search beneficiaries' : 'Search participants', prefixIcon: const Icon(Icons.search)), onChanged: (_) => setState(() {})),
      const SizedBox(height: 8),
      for (final user in matches) CheckboxListTile(key: ValueKey('candidate:${user.userId}'), contentPadding: EdgeInsets.zero,
        title: Text(userLabel(user)), value: c.selected.contains(user.userId), onChanged: widget.enabled ? (v) => _action(() => c.select(user.userId, v ?? false)) : null),
      if (c.hasFormerMembers) const Text('Re-add this member before saving changes', style: TextStyle(color: Color(0xFFB42318))),
      const Divider(), Text(c.kind == RecordKind.income ? 'Beneficiaries' : 'Selected participants'),
      Text(c.kind == RecordKind.income ? 'Entitlement preview' : 'Allocation preview'),
      for (final id in c.selected) Padding(key: ValueKey('selected:$id'), padding: const EdgeInsets.symmetric(vertical: 6), child: Column(crossAxisAlignment: CrossAxisAlignment.stretch, children: [
        Row(children: [Expanded(child: Text('${c.label(id)}${c.active.contains(id) ? '' : ' (former member)'}')),
          IconButton(tooltip: c.kind == RecordKind.income ? 'Remove beneficiary' : 'Remove participant', onPressed: widget.enabled && c.active.contains(id) ? () => _action(() => c.select(id, false)) : null, icon: const Icon(Icons.remove_circle_outline))]),
        if (c.splitMode != SplitMode.equal) Row(children: [
          if (c.splitMode == SplitMode.shares) IconButton(tooltip: 'Decrease shares', onPressed: widget.enabled ? () => _action(() {
            final v = c._shares(c.splitInput(id).text); if (v > BigInt.zero) c.setSplitValue(id, '${v - BigInt.one}');
          }) : null, icon: const Icon(Icons.remove)),
          Expanded(child: TextField(key: ValueKey((c, c.splitMode, id)), controller: c.splitInput(id), enabled: widget.enabled,
            keyboardType: const TextInputType.numberWithOptions(decimal: true), decoration: InputDecoration(labelText: labels[c.splitMode],
              suffixText: c.splitMode == SplitMode.percentage ? '%' : c.splitMode == SplitMode.exact ? c.currency.code : null,
              helperText: c.splitMode == SplitMode.shares ? '0–32767' : null), onChanged: (_) => widget.onChanged())),
          if (c.splitMode == SplitMode.shares) IconButton(tooltip: 'Increase shares', onPressed: widget.enabled ? () => _action(() {
            final v = c._shares(c.splitInput(id).text); if (v < BigInt.from(32767)) c.setSplitValue(id, '${v + BigInt.one}');
          }) : null, icon: const Icon(Icons.add)),
        ]),
        if (c.splitMode == SplitMode.percentage || c.splitMode == SplitMode.exact) Align(alignment: Alignment.centerLeft,
          child: TextButton(onPressed: widget.enabled ? () => _action(() => c.assignRemainder(id)) : null, child: const Text('Assign remainder'))),
        Text(original[id] == null ? 'Allocation: —' : c.currency.format(original[id]!)),
        if (converted[id] != null) Text(c.group.defaultCurrency.format(converted[id]!), style: Theme.of(context).textTheme.bodySmall),
      ])),
      if (assigned != null) Text(assigned),
      if (c.splitMode == SplitMode.percentage || c.splitMode == SplitMode.exact) Align(alignment: Alignment.centerLeft,
        child: TextButton(onPressed: widget.enabled ? () => _action(c.splitEqually) : null, child: const Text('Split equally'))),
      if (_error ?? previewError case final String error) Text(error, style: const TextStyle(color: Color(0xFFB42318))),
    ]);
  });
}

class RecordEditor extends StatefulWidget {
  const RecordEditor({super.key, required this.repository, required this.session, required this.groupId,
    this.recordId, this.kind = RecordKind.expense, this.fromUserId, this.toUserId, this.initialAmountMinor, this.initialCurrency});
  final JizhangRepository repository;
  final SessionController session;
  final String groupId;
  final String? recordId, fromUserId, toUserId, initialCurrency;
  final BigInt? initialAmountMinor;
  final RecordKind kind;
  @override
  State<RecordEditor> createState() => _RecordEditorState();
}
class _RecordEditorState extends State<RecordEditor> {
  late final FormController _form = FormController(widget.session);
  @override
  void dispose() { _form.dispose(); super.dispose(); }
  @override
  Widget build(BuildContext context) {
    final group = GroupScope.of(context).group;
    Widget fields(LedgerRecord? record) => _RecordEditorFields(repository: widget.repository, session: widget.session,
      group: group, record: record, form: _form, kind: widget.kind, fromUserId: widget.fromUserId,
      toUserId: widget.toUserId, initialAmountMinor: widget.initialAmountMinor, initialCurrency: widget.initialCurrency);
    return FormLeaveGuard(controller: _form, child: widget.recordId == null ? fields(null) :
      LoadView<LedgerRecord>(load: () => widget.repository.getRecord(widget.groupId, widget.recordId!), builder: fields));
  }
}
class _RecordEditorFields extends StatefulWidget {
  const _RecordEditorFields({required this.repository, required this.session, required this.group, required this.record,
    required this.form, required this.kind, this.fromUserId, this.toUserId, this.initialAmountMinor, this.initialCurrency});
  final JizhangRepository repository;
  final SessionController session;
  final ExpenseGroup group;
  final LedgerRecord? record;
  final FormController form;
  final RecordKind kind;
  final String? fromUserId, toUserId, initialCurrency;
  final BigInt? initialAmountMinor;
  @override
  State<_RecordEditorFields> createState() => _RecordEditorFieldsState();
}
class _RecordEditorFieldsState extends State<_RecordEditorFields> {
  SplitFieldsController? _split;
  late final RecordKind _kind = widget.record?.kind ?? widget.kind;
  bool get _transfer => _kind == RecordKind.transfer;
  late final TextEditingController _amount, _date, _note;
  late CurrencyInfo _currency;
  late String _actorUserId, _recipientId;
  final Map<String, User> _knownUsers = {};
  String? _validation;
  @override
  void initState() {
    super.initState();
    final record = widget.record;
    final names = widget.group.members.map((u) => u.userId).toList();
    _actorUserId = record?.actor.userId ?? widget.fromUserId ?? (names.contains(widget.session.user?.userId) ? widget.session.user!.userId : names.firstOrNull ?? '');
    _recipientId = record is TransferRecord ? record.transferTo.userId : widget.toUserId ?? names.where((id) => id != _actorUserId).firstOrNull ?? '';
    for (final user in [if (record != null) record.actor, if (record is TransferRecord) record.transferTo, ...widget.group.members]) { _knownUsers[user.userId] = user; }
    _currency = record?.currency ?? widget.group.defaultCurrency;
    _amount = TextEditingController(text: record != null ? _currency.formatMajor(record.amountMinor) : widget.initialAmountMinor != null && (widget.initialCurrency == null || widget.initialCurrency == _currency.code) ? _currency.formatMajor(widget.initialAmountMinor!) : '');
    _date = TextEditingController(text: record?.date ?? calendarToday());
    _note = TextEditingController(text: record?.note ?? '');
    _note.addListener(_changed);
    if (record == null && widget.initialCurrency != null && widget.initialCurrency != _currency.code) _loadInitialCurrency();
    if (!_transfer) _split = SplitFieldsController(repository: widget.repository, session: widget.session, group: widget.group, kind: _kind,
      draft: record is SplitRecord ? SplitRecordDraft.fromRecord(record) : null, savedCurrency: record?.currency,
      savedRate: record?.exchangeRate, currentUserId: widget.session.user?.userId,
      historicalUsers: record is SplitRecord ? [record.actor, ...record.splits.map((s) => s.user)] : const []);
  }
  Future<void> _loadInitialCurrency() async {
    final generation = widget.session.generation;
    try {
      final currencies = await widget.repository.getCurrencies();
      if (!mounted || generation != widget.session.generation) return;
      final currency = currencies.where((c) => c.code == widget.initialCurrency).firstOrNull;
      if (currency == null) throw const FormatException('Unsupported currency');
      if (widget.group.currencyMode == CurrencyMode.single && currency.code != widget.group.defaultCurrency.code) throw const FormatException('Payments must use the group default currency');
      setState(() { _currency = currency; _amount.text = widget.initialAmountMinor == null ? '' : currency.formatMajor(widget.initialAmountMinor!); });
    } catch (error) { if (mounted && generation == widget.session.generation) setState(() => _validation = error.toString()); }
  }
  @override
  void didUpdateWidget(covariant _RecordEditorFields oldWidget) {
    super.didUpdateWidget(oldWidget);
    if (!identical(oldWidget.group, widget.group)) {
      _split?.updateGroup(widget.group);
      for (final user in widget.group.members) { _knownUsers[user.userId] = user; }
      if (_transfer && widget.group.currencyMode == CurrencyMode.single) _currency = widget.group.defaultCurrency;
    }
  }
  @override
  void dispose() { _split?.dispose(); _amount.dispose(); _date.dispose(); _note.dispose(); super.dispose(); }
  void _changed() { _validation = null; widget.form.markDirty(); }
  Future<void> _save() async {
    if (widget.form.pending) return;
    final generation = widget.session.generation;
    SplitRecordDraft? splitDraft;
    TransferDraft? transfer;
    try {
      if (_transfer) {
        final total = parseMajorAmount(_amount.text, minorDigits: _currency.minorUnits);
        validateCalendarDate(_date.text);
        final names = widget.group.members.map((u) => u.userId).toSet();
        if (names.length < 2) throw const FormatException('Payments require at least two active members');
        if (!names.contains(_actorUserId) || !names.contains(_recipientId)) throw const FormatException('Re-add this member before saving changes');
        if (_actorUserId == _recipientId) throw const FormatException('Payer and recipient must be different');
        if (widget.group.currencyMode == CurrencyMode.single && _currency.code != widget.group.defaultCurrency.code) throw const FormatException('Payments must use the group default currency');
        transfer = TransferDraft(amountMinor: total, currency: _currency.code, byUserId: _actorUserId, toUserId: _recipientId, date: _date.text, note: _note.text);
      } else { splitDraft = _split!.toDraft(); }
    } on FormatException catch (error) { setState(() => _validation = error.message); return; }
    LedgerRecord? saved;
    bool policyRace = false;
    final success = await widget.form.save(() async {
      try {
        if (_transfer) {
          saved = widget.record == null ? await widget.repository.createTransfer(widget.group.groupId, transfer!) : await widget.repository.updateTransfer(widget.group.groupId, widget.record!.recordId, transfer!);
        } else if (_kind == RecordKind.income) {
          saved = widget.record == null ? await widget.repository.createIncome(widget.group.groupId, splitDraft!) : await widget.repository.updateIncome(widget.group.groupId, widget.record!.recordId, splitDraft!);
        } else {
          saved = widget.record == null ? await widget.repository.createExpense(widget.group.groupId, splitDraft!) : await widget.repository.updateExpense(widget.group.groupId, widget.record!.recordId, splitDraft!);
        }
      } on ApiException catch (e) { policyRace = e.statusCode == 409; rethrow; }
    });
    if (!mounted || generation != widget.session.generation) return;
    if (success || policyRace) await GroupScope.of(context).refreshGroup();
    if (!mounted || !success || generation != widget.session.generation) return;
    GroupScope.of(context).recordChanges.markChanged();
    widget.form.reset(); showSuccess(context, 'Record saved');
    await returnFromRecord(context, widget.session,
      fallback: '/groups/${widget.group.groupId}/records/${saved!.recordId}');
  }
  @override
  Widget build(BuildContext context) => ListenableBuilder(listenable: widget.form, builder: (context, _) {
    final names = widget.group.members.map((u) => u.userId).toSet();
    final insufficientMembers = _transfer && names.length < 2;
    final equalEndpoints = _transfer && _actorUserId.isNotEmpty && _recipientId.isNotEmpty && _actorUserId == _recipientId;
    final inactiveEndpoint = _transfer && (!names.contains(_actorUserId) || !names.contains(_recipientId));
    return FormSurface(title: '${widget.record == null ? 'New' : 'Edit'} ${switch (_kind) { RecordKind.expense => 'expense', RecordKind.income => 'income', RecordKind.transfer => 'payment' }}', child: Column(crossAxisAlignment: CrossAxisAlignment.stretch, children: [
      AbsorbPointer(absorbing: widget.form.pending, child: _transfer ? Column(crossAxisAlignment: CrossAxisAlignment.stretch, children: [
        CurrencyAmountFields(repository: widget.repository, session: widget.session, currency: _currency, amountController: _amount,
          locked: widget.group.currencyMode == CurrencyMode.single, enabled: !widget.form.pending,
          onCurrencyChanged: (c) => setState(() => _currency = c), onChanged: _changed),
        const SizedBox(height: 12),
        TextField(controller: _date, enabled: !widget.form.pending, decoration: const InputDecoration(labelText: 'Date (YYYY-MM-DD)'), onChanged: (_) => _changed()),
        const SizedBox(height: 12),
        TextField(controller: _note, enabled: !widget.form.pending, minLines: 3, maxLines: null,
          keyboardType: TextInputType.multiline, decoration: const InputDecoration(labelText: 'Note (optional)')),
        const SizedBox(height: 12),
        _FormFieldPair(
          first: UserPickerField(session: widget.session, label: 'Paid by', selectedUserId: _actorUserId.isEmpty ? null : _actorUserId,
            knownUsers: _knownUsers, choices: widget.group.members, enabled: !widget.form.pending,
            onChanged: (v) { setState(() => _actorUserId = v); _changed(); }),
          second: UserPickerField(session: widget.session, label: 'Paid to', selectedUserId: _recipientId.isEmpty ? null : _recipientId,
            knownUsers: _knownUsers, choices: widget.group.members, enabled: !widget.form.pending,
            onChanged: (v) { setState(() => _recipientId = v); _changed(); })),
        if (insufficientMembers) const Text('Payments require at least two active members'),
        if (equalEndpoints) const Text('Payer and recipient must be different', style: TextStyle(color: Color(0xFFB42318))),
        if (inactiveEndpoint && !insufficientMembers) const Text('Re-add this member before saving changes'),
      ]) : SplitFields(session: widget.session, controller: _split!, onChanged: _changed, enabled: !widget.form.pending)),
      const SizedBox(height: 12),
      if (_validation ?? widget.form.error case final String error) Text(error, style: const TextStyle(color: Color(0xFFB42318))),
      Wrap(spacing: 12, runSpacing: 8, children: [FilledButton(onPressed: widget.form.pending || insufficientMembers || equalEndpoints || inactiveEndpoint ? null : _save, child: Text(widget.form.pending ? 'Saving…' : 'Save')),
        TextButton(onPressed: widget.form.pending ? null : () => returnFromRecord(context, widget.session,
          fallback: '/groups/${widget.group.groupId}/records'), child: const Text('Cancel'))]),
    ]));
  });
}
