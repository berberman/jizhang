import 'package:flutter/material.dart';

import '../../core/jizhang_repository.dart';
import '../../core/models.dart';
import '../../core/session_controller.dart';
import '../../shared/entity_picker.dart';

/// A lexical token only. Matching, identity resolution and money validation belong
/// to the server. Offsets let the composer change one token without rewriting text.
class RecordQueryToken {
  const RecordQueryToken(this.start, this.end, this.field, this.value, this.quoted);
  final int start, end;
  final String? field;
  final String value;
  final bool quoted;
}

const recordQueryFields = ['title', 'note', 'type', 'payer', 'payee', 'member', 'date', 'created', 'currency', 'amount'];

List<RecordQueryToken> tokenizeRecordQuery(String text) {
  final tokens = <RecordQueryToken>[];
  var i = 0;
  while (i < text.length) {
    if (RegExp(r'\s').hasMatch(text[i])) { i++; continue; }
    final start = i;
    var value = StringBuffer();
    String? field;
    var inside = false;
    var quoted = false;
    while (i < text.length) {
      final c = text[i];
      if (!inside && RegExp(r'\s').hasMatch(c)) break;
      if (c == '"') { inside = !inside; quoted = true; i++; continue; }
      if (inside && c == r'\') {
        i++;
        if (i >= text.length || (text[i] != '"' && text[i] != r'\')) {
          throw const FormatException('Inside quotes use only \\" or \\\\ escapes.');
        }
        value.write(text[i++]);
        continue;
      }
      if (!inside && c == ':' && field == null && !quoted) {
        field = value.toString();
        if (!recordQueryFields.contains(field)) throw FormatException('Unknown filter: $field');
        value = StringBuffer();
      } else {
        value.write(c);
      }
      i++;
    }
    if (inside) throw const FormatException('Unterminated quoted value.');
    if (value.isEmpty) throw const FormatException('Search terms and filter values cannot be empty.');
    tokens.add(RecordQueryToken(start, i, field, value.toString(), quoted));
  }
  return tokens;
}

String serializeRecordQueryToken(String? field, String value, {bool quoted = false}) {
  final quote = quoted || value.isEmpty || RegExp(r'[\s"\\:]').hasMatch(value);
  final escaped = value.replaceAll(r'\', r'\\').replaceAll('"', r'\"');
  return '${field == null ? '' : '$field:'}${quote ? '"$escaped"' : value}';
}

final _draftQueries = Expando<Map<String, String>>();
final _identityLabels = Expando<Map<String, Map<String, String>>>();

class RecordSearchToolbar extends StatefulWidget {
  const RecordSearchToolbar({super.key, required this.repository, required this.session,
    required this.group, required this.query, required this.sort, required this.onSearch,
    required this.onSort, this.queryError});
  final JizhangRepository repository;
  final SessionController session;
  final ExpenseGroup group;
  final String query, sort;
  final ValueChanged<String> onSearch, onSort;
  final String? queryError;
  @override
  State<RecordSearchToolbar> createState() => _RecordSearchToolbarState();
}

class _RecordSearchToolbarState extends State<RecordSearchToolbar> {
  late TextEditingController _text;
  List<User>? _identities;
  String? _localError;
  String get _key => '${widget.session.generation}:${widget.group.groupId}';
  Map<String, String> get _memory => _draftQueries[widget.session] ??= {};
  Map<String, String> get _labels => (_identityLabels[widget.session] ??= {})[_key] ??= {};
  @override
  void initState() {
    super.initState();
    _text = TextEditingController(text: _memory[_key] ?? widget.query);
    _text.addListener(_changed);
  }
  @override
  void didUpdateWidget(covariant RecordSearchToolbar oldWidget) {
    super.didUpdateWidget(oldWidget);
    if (oldWidget.group.groupId != widget.group.groupId || oldWidget.session != widget.session) {
      _identities = null;
      _text.text = _memory[_key] ?? widget.query;
    }
    if (oldWidget.query != widget.query && widget.query.isEmpty) _text.clear();
  }
  void _changed() { _memory[_key] = _text.text; setState(() => _localError = null); }
  @override
  void dispose() { _text.dispose(); super.dispose(); }
  List<RecordQueryToken> get _tokens {
    try { return tokenizeRecordQuery(_text.text); } on FormatException { return []; }
  }
  void _search() {
    try { tokenizeRecordQuery(_text.text); } on FormatException catch (e) {
      setState(() => _localError = e.message); return;
    }
    widget.onSearch(_text.text);
  }
  void _clear() { _text.clear(); widget.onSearch(''); }
  void _replace(RecordQueryToken? token, String replacement) {
    final text = _text.text;
    _text.text = token == null ? '${text.trimRight()}${text.trim().isEmpty ? '' : ' '}$replacement'
      : text.replaceRange(token.start, token.end, replacement);
  }

  Future<User?> _pickIdentity(BuildContext dialogContext) async {
    final generation = widget.session.generation;
    final groupId = widget.group.groupId;
    try {
      if (_identities == null) {
        final report = await widget.repository.getReport(groupId);
        if (!mounted || generation != widget.session.generation || groupId != widget.group.groupId || !dialogContext.mounted) return null;
        final users = {for (final user in widget.group.members) user.userId: user};
        for (final currency in report.currencies) {
          for (final balance in currency.balances) { users[balance.user.userId] = balance.user; }
        }
        _identities = users.values.toList();
      }
      if (!dialogContext.mounted) return null;
      final active = widget.group.members.map((u) => u.userId).toSet();
      final counts = <String, int>{};
      for (final user in _identities!) { counts.update(user.displayName.toLowerCase(), (n) => n + 1, ifAbsent: () => 1); }
      return showEntityPicker<User>(dialogContext, session: widget.session,
        title: 'Choose record member', searchLabel: 'Search public names or handles',
        items: _identities!, idOf: (u) => u.userId,
        titleOf: (u) => '${u.displayName}${active.contains(u.userId) ? '' : ' (former member)'}',
        subtitleOf: (u) => '@${u.username}${counts[u.displayName.toLowerCase()]! > 1 ? ' · ${u.userId}' : ''}',
        leading: (u) => const Icon(Icons.person_outline));
    } catch (_) {
      if (mounted && dialogContext.mounted) {
        final retry = await showDialog<bool>(context: dialogContext, builder: (context) => AlertDialog(
          title: const Text('Member choices unavailable'),
          content: const Text('Could not load member choices. Raw search remains available.'),
          actions: [TextButton(onPressed: () => Navigator.pop(context, false), child: const Text('Close')),
            FilledButton(onPressed: () => Navigator.pop(context, true), child: const Text('Retry'))]));
        if (retry == true && mounted && dialogContext.mounted && generation == widget.session.generation && groupId == widget.group.groupId) return _pickIdentity(dialogContext);
      }
      return null;
    }
  }

  Future<void> _edit([RecordQueryToken? token]) async {
    try { tokenizeRecordQuery(_text.text); } on FormatException catch (e) {
      setState(() => _localError = e.message); return;
    }
    var field = token == null ? 'title' : token.field ?? 'text';
    final value = TextEditingController(text: token?.value ?? '');
    final upper = TextEditingController();
    var comparison = '=';
    if (['date', 'created', 'amount'].contains(field)) {
      final raw = value.text;
      if (raw.contains('..')) {
        comparison = 'range'; value.text = raw.split('..').first; upper.text = raw.split('..').last;
      } else {
        final prefix = RegExp(r'^(<=|>=|=|<|>)').firstMatch(raw)?.group(0);
        if (prefix != null) { comparison = prefix; value.text = raw.substring(prefix.length); }
      }
    }
    String? currency = _tokens.where((t) => t.field == 'currency').firstOrNull?.value.toUpperCase();
    String? error;
    var replaceCurrency = false;
    String? identityLabel;
    var quoteIdentity = token?.quoted ?? false;
    List<CurrencyInfo>? currencies;
    Future<List<CurrencyInfo>>? catalog;
    final route = DialogRoute<bool>(context: context, builder: (context) => StatefulBuilder(builder: (context, update) {
      final numeric = ['date', 'created', 'amount'].contains(field);
      final identity = ['payer', 'payee', 'member'].contains(field);
      if (field == 'currency' || field == 'amount') catalog ??= widget.repository.getCurrencies();
      Widget currencyPicker({required bool amount}) => FutureBuilder<List<CurrencyInfo>>(future: catalog, builder: (context, snapshot) {
        currencies = snapshot.data;
        if (snapshot.hasError) return TextButton(onPressed: () => update(() { catalog = widget.repository.getCurrencies(); }), child: const Text('Retry currency choices'));
        if (currencies == null) return const LinearProgressIndicator();
        return DropdownButtonFormField<String>(initialValue: currencies!.any((c) => c.code == (amount ? currency : value.text)) ? (amount ? currency : value.text) : null, isExpanded: true,
          decoration: InputDecoration(labelText: amount ? 'Required amount currency' : 'Currency'),
          items: [for (final c in currencies!) DropdownMenuItem(value: c.code, child: Text('${c.code} · ${c.name}', maxLines: 1, overflow: TextOverflow.ellipsis))],
          onChanged: (v) => update(() { if (amount) { currency = v; } else { value.text = v ?? ''; } }));
      });
      final controls = <Widget>[
          DropdownButtonFormField<String>(initialValue: field, isExpanded: true,
            decoration: const InputDecoration(labelText: 'Field'),
            items: [for (final f in ['text', ...recordQueryFields]) DropdownMenuItem(value: f, child: Text(f == 'created' ? 'Created date (UTC)' : f == 'text' ? 'Any text' : f == 'amount' ? 'Original amount' : f, maxLines: 1, overflow: TextOverflow.ellipsis))],
            onChanged: (v) => update(() { field = v!; value.clear(); upper.clear(); comparison = '='; identityLabel = null; quoteIdentity = false; })),
          if (numeric) DropdownButtonFormField<String>(initialValue: comparison, isExpanded: true,
            decoration: const InputDecoration(labelText: 'Comparison'),
            items: [for (final op in ['=', '<', '<=', '>', '>=', 'range']) DropdownMenuItem(value: op, child: Text(op == 'range' ? 'Inclusive range' : op, maxLines: 1, overflow: TextOverflow.ellipsis))],
            onChanged: (v) => update(() => comparison = v!)),
          if (field == 'type') DropdownButtonFormField<String>(initialValue: ['expense', 'income', 'transfer'].contains(value.text) ? value.text : null, isExpanded: true,
            decoration: const InputDecoration(labelText: 'Record type'),
            items: const [DropdownMenuItem(value: 'expense', child: Text('Expense', maxLines: 1, overflow: TextOverflow.ellipsis)), DropdownMenuItem(value: 'income', child: Text('Income', maxLines: 1, overflow: TextOverflow.ellipsis)), DropdownMenuItem(value: 'transfer', child: Text('Payment / Transfer', maxLines: 1, overflow: TextOverflow.ellipsis))],
            onChanged: (v) => value.text = v ?? '')
          else if (field == 'currency') currencyPicker(amount: false)
          else if (identity) ...[
            OutlinedButton.icon(icon: const Icon(Icons.person_search), label: Text(identityLabel ?? 'Choose member'), onPressed: () async {
              final user = await _pickIdentity(context);
              if (user != null && context.mounted) update(() {
                value.text = user.userId;
                identityLabel = '${user.displayName} (@${user.username})';
                _labels[user.userId] = identityLabel!;
                quoteIdentity = false;
              });
            }),
            TextField(controller: value, decoration: const InputDecoration(labelText: 'UUID, handle, or display name'), onChanged: (_) => update(() => identityLabel = null)),
            CheckboxListTile(value: quoteIdentity, title: const Text('Match exact display name'), onChanged: (v) => update(() => quoteIdentity = v!)),
          ] else TextField(controller: value, decoration: InputDecoration(labelText: numeric ? (comparison == 'range' ? 'Lower bound' : 'Value') : 'Contains text', hintText: field == 'date' || field == 'created' ? 'YYYY-MM-DD' : null),
            keyboardType: numeric && field == 'amount' ? const TextInputType.numberWithOptions(decimal: true) : TextInputType.text),
          if (numeric && (field == 'date' || field == 'created')) TextButton.icon(icon: const Icon(Icons.calendar_month), label: const Text('Choose date'), onPressed: () async {
            final date = await showDatePicker(context: context, initialDate: DateTime.tryParse(value.text) ?? DateTime.now(), firstDate: DateTime(1), lastDate: DateTime(9999, 12, 31));
            if (date != null) value.text = '${date.year.toString().padLeft(4, '0')}-${date.month.toString().padLeft(2, '0')}-${date.day.toString().padLeft(2, '0')}';
          }),
          if (numeric && comparison == 'range') TextField(controller: upper, decoration: InputDecoration(labelText: 'Upper bound', hintText: field == 'amount' ? null : 'YYYY-MM-DD')),
          if (numeric && comparison == 'range' && field != 'amount') TextButton.icon(icon: const Icon(Icons.calendar_month), label: const Text('Choose upper date'), onPressed: () async {
            final date = await showDatePicker(context: context, initialDate: DateTime.tryParse(upper.text) ?? DateTime.now(), firstDate: DateTime(1), lastDate: DateTime(9999, 12, 31));
            if (date != null) upper.text = '${date.year.toString().padLeft(4, '0')}-${date.month.toString().padLeft(2, '0')}-${date.day.toString().padLeft(2, '0')}';
          }),
          if (field == 'amount') ...[
            currencyPicker(amount: true),
            if (_tokens.any((t) => t.field == 'currency' && t.value.toUpperCase() != currency)) CheckboxListTile(value: replaceCurrency,
              title: const Text('Replace conflicting currency filters explicitly'), onChanged: (v) => update(() => replaceCurrency = v!)),
          ],
          if (error != null) Text(error!, style: TextStyle(color: Theme.of(context).colorScheme.error)),
      ];
      return AlertDialog(title: Text(token == null ? 'Add filter' : 'Edit filter'),
        content: SizedBox(width: 460, child: SingleChildScrollView(child: Column(spacing: 12, mainAxisSize: MainAxisSize.min, crossAxisAlignment: CrossAxisAlignment.stretch, children: controls))),
        actions: [TextButton(onPressed: () => Navigator.pop(context, false), child: const Text('Cancel')),
          FilledButton(onPressed: () {
            if (value.text.trim().isEmpty || (numeric && comparison == 'range' && upper.text.trim().isEmpty)) { update(() => error = 'A value and both range bounds are required.'); return; }
            if (field == 'amount' && (currency == null || currencies?.any((c) => c.code == currency) != true)) { update(() => error = 'Choose an amount currency.'); return; }
            if (field == 'amount' && _tokens.any((t) => t.field == 'currency' && t.value.toUpperCase() != currency) && !replaceCurrency) { update(() => error = 'Explicitly replace conflicting currency filters.'); return; }
            Navigator.pop(context, true);
          }, child: const Text('Apply to draft'))]);
    }));
    final result = await Navigator.of(context).push(route);
    if (result == true && mounted) {
      var newValue = value.text;
      if (['date', 'created', 'amount'].contains(field)) newValue = comparison == 'range' ? '${value.text}..${upper.text}' : '$comparison${value.text}';
      _replace(token, serializeRecordQueryToken(field == 'text' ? null : field, newValue, quoted: (field == 'text' && (token?.quoted ?? false)) || (['payer', 'payee', 'member'].contains(field) && quoteIdentity)));
      if (field == 'amount') {
        // Work from the rewritten offsets; remove only explicitly replaced currencies.
        final conflicting = _tokens.where((t) => t.field == 'currency' && t.value.toUpperCase() != currency).toList().reversed;
        for (final t in conflicting) { _replace(t, ''); }
        if (!_tokens.any((t) => t.field == 'currency')) _replace(null, serializeRecordQueryToken('currency', currency!));
      }
    }
    await route.completed;
    value.dispose();
    upper.dispose();
  }

  @override
  Widget build(BuildContext context) {
    final error = _localError ?? widget.queryError;
    final descending = widget.sort.startsWith('-');
    final key = descending ? widget.sort.substring(1) : widget.sort;
    const sorts = {'title': 'Title', 'date': 'Transaction date', 'createdAt': 'Created time', 'type': 'Type', 'payer': 'Payer', 'payee': 'Payee', 'currency': 'Currency', 'amount': 'Amount (grouped by currency)'};
    for (final user in widget.group.members) { _labels[user.userId] = '${user.displayName} (@${user.username})'; }
    final textScale = (MediaQuery.textScalerOf(context).scale(16) / 16).clamp(1.0, double.infinity);
    final tokens = _tokens;
    return Padding(padding: const EdgeInsets.symmetric(vertical: 12), child: LayoutBuilder(builder: (context, constraints) {
      final search = TextField(controller: _text, decoration: InputDecoration(labelText: 'Search records', hintText: 'title:"team lunch" member:@alice', errorText: error, errorMaxLines: 4), onSubmitted: (_) => _search());
      final actions = [
        FilledButton(onPressed: _search, child: const Text('Search')),
        TextButton(onPressed: _clear, child: const Text('Clear')),
        OutlinedButton.icon(onPressed: () => _edit(), icon: const Icon(Icons.add), label: const Text('Add filter')),
      ];
      final sort = DropdownButtonFormField<String>(key: ValueKey(key), initialValue: key, isExpanded: true,
        decoration: const InputDecoration(labelText: 'Sort records'), items: [for (final entry in sorts.entries) DropdownMenuItem(value: entry.key, child: Text(entry.value, overflow: TextOverflow.ellipsis))],
        onChanged: (value) { if (value != null) widget.onSort('${descending ? '-' : ''}$value'); });
      final direction = DropdownButtonFormField<bool>(key: ValueKey('record-direction:$descending'), initialValue: descending, isExpanded: true,
        decoration: const InputDecoration(labelText: 'Direction'),
        items: const [DropdownMenuItem(value: false, child: Text('Ascending')), DropdownMenuItem(value: true, child: Text('Descending'))],
        onChanged: (value) { if (value != null) widget.onSort('${value ? '-' : ''}$key'); });
      return Column(crossAxisAlignment: CrossAxisAlignment.stretch, children: [
        if (constraints.maxWidth >= 720 * textScale)
          Row(crossAxisAlignment: CrossAxisAlignment.start, children: [Expanded(child: search), const SizedBox(width: 12),
            Row(spacing: 8, children: actions)])
        else ...[
          search,
          const SizedBox(height: 12),
          Wrap(spacing: 8, runSpacing: 8, children: actions),
        ],
        if (tokens.isNotEmpty) ...[
          const SizedBox(height: 12),
          Wrap(spacing: 8, runSpacing: 8, children: [for (final token in tokens) InputChip(
            label: Text('${token.field == null ? '' : '${token.field}: '}${['payer', 'payee', 'member'].contains(token.field) ? _labels[token.value] ?? token.value : token.value}'),
            deleteButtonTooltipMessage: 'Remove ${token.field ?? 'text'} filter: ${token.value}',
            tooltip: 'Edit filter', onPressed: () => _edit(token), onDeleted: () => _replace(token, ''))]),
        ],
        if (_localError != null) ...[
          const SizedBox(height: 8),
          const Text('Previous results are unchanged until a valid search is applied.'),
        ],
        const SizedBox(height: 16),
        if (constraints.maxWidth >= 300 * textScale + 12 + 180 * textScale)
          Row(crossAxisAlignment: CrossAxisAlignment.start, children: [SizedBox(width: 300 * textScale, child: sort),
            const SizedBox(width: 12), SizedBox(width: 180 * textScale, child: direction)])
        else ...[
          sort,
          const SizedBox(height: 12),
          direction,
        ],
      ]);
    }));
  }
}
