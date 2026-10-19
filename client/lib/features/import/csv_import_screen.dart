import 'package:file_selector/file_selector.dart';
import 'package:flutter/material.dart';
import 'package:go_router/go_router.dart';

import '../../core/api_client.dart';
import '../../core/exchange_rate_controller.dart';
import '../../core/models.dart';
import '../../shared/currency_amount_fields.dart';
import '../groups/group_screen.dart';
import '../../core/jizhang_repository.dart';
import '../../core/session_controller.dart';
import '../../shared/form_controller.dart';
import '../../shared/widgets.dart';

const _csvTypeGroup = XTypeGroup(
  label: 'CSV', extensions: ['csv'], mimeTypes: ['text/csv'],
  uniformTypeIdentifiers: ['public.comma-separated-values-text'],
);
const _sample = 'title,amount,paidBy,split,date\nDinner,90,alice,"alice,bob,bob",2026-10-14';

class CsvImportScreen extends StatefulWidget {
  const CsvImportScreen({super.key, required this.repository, required this.session, required this.groupId});
  final JizhangRepository repository;
  final SessionController session;
  final String groupId;
  @override
  State<CsvImportScreen> createState() => _CsvImportScreenState();
}

class _CsvImportScreenState extends State<CsvImportScreen> {
  late final FormController _form = FormController(widget.session);
  final TextEditingController _text = TextEditingController();
  bool _reading = false;
  int? _imported;
  String? _fileError;
  int _fileGeneration = 0;
  late int _sessionGeneration;
  ExchangeRateController? _rate;
  CurrencyInfo? _currency;

  @override
  void didChangeDependencies() {
    super.didChangeDependencies();
    final group = GroupScope.of(context).group;
    _currency ??= group.defaultCurrency;
    if (_rate == null) {
      _rate = ExchangeRateController(repository: widget.repository, session: widget.session,
        group: group, sourceCurrency: _currency!.code);
    } else {
      _rate!.updateGroup(group);
    }
    _rate!.onPolicyConflict = () async {
      if (mounted) await GroupScope.of(context).refreshGroup();
    };
  }

  @override
  void initState() {
    super.initState();
    _sessionGeneration = widget.session.generation;
    widget.session.addListener(_sessionChanged);
  }
  void _sessionChanged() {
    if (_sessionGeneration == widget.session.generation) return;
    _sessionGeneration = widget.session.generation;
    _fileGeneration++;
    _text.clear();
    if (mounted) setState(() { _reading = false; _imported = null; _fileError = null; });
  }
  @override
  void dispose() {
    _fileGeneration++;
    widget.session.removeListener(_sessionChanged);
    _form.dispose(); _text.dispose();
    _rate?.dispose();
    super.dispose();
  }
  Future<void> _select() async {
    if (_reading || _form.pending) return;
    final generation = ++_fileGeneration;
    final sessionGeneration = widget.session.generation;
    setState(() { _reading = true; _fileError = null; });
    try {
      final file = await openFile(acceptedTypeGroups: [_csvTypeGroup]);
      if (file == null) return;
      final content = await file.readAsString();
      if (!mounted || generation != _fileGeneration || sessionGeneration != widget.session.generation) return;
      _text.text = content;
      _form.markDirty();
    } catch (_) {
      if (mounted && generation == _fileGeneration && sessionGeneration == widget.session.generation) {
        setState(() => _fileError = 'Could not read the CSV file. Choose a readable UTF-8 text file, or paste its contents below.');
      }
    } finally {
      if (mounted && generation == _fileGeneration) setState(() => _reading = false);
    }
  }
  Future<void> _submit() async {
    if (_form.pending || _reading) return;
    if (_text.text.trim().isEmpty) {
      setState(() => _form.error = 'Enter CSV text or select a CSV file before importing.');
      return;
    }
    ExchangeRateInput? exchangeRate;
    try {
      exchangeRate = _rate!.toInput();
    } on FormatException catch (error) {
      setState(() => _form.error = error.message);
      return;
    }
    final csv = _text.text;
    final scope = GroupScope.of(context);
    var count = 0;
    final success = await _form.save(() async {
      try {
        final records = await widget.repository.importCsv(widget.groupId, csv,
          currency: _currency!.code, exchangeRate: exchangeRate);
        count = records.length;
      } on ApiException catch (error) {
        if (error.statusCode == 409) await scope.refreshGroup();
        rethrow;
      }
      if (count > 0) await scope.refreshGroup();
    });
    if (success && mounted) setState(() => _imported = count);
  }
  @override
  Widget build(BuildContext context) => FormLeaveGuard(controller: _form,
    child: ListenableBuilder(
        listenable: _form, builder: (context, _) => FormSurface(title: 'Import CSV', child: Column(
          crossAxisAlignment: CrossAxisAlignment.stretch, children: [
            if (_imported != null) ...[
              Semantics(liveRegion: true, child: Text(_imported == 0 ? 'No expense rows found.' : 'Imported $_imported expenses.')),
              const SizedBox(height: 16),
              FilledButton(onPressed: () => context.go('/groups/${widget.groupId}/records'), child: const Text('Done')),
            ] else ...[
              const Text('Choose a UTF-8 CSV file or paste its contents below.'),
              const SizedBox(height: 16),
              CurrencyAmountFields(repository: widget.repository, session: widget.session,
                currency: _currency!, label: 'Currency for every imported row', enabled: !_form.pending && !_reading,
                onCurrencyChanged: (currency) {
                  setState(() => _currency = currency);
                  _rate!.setCurrency(currency.code); _form.markDirty();
                }),
              ExchangeRateFields(controller: _rate!, enabled: !_form.pending && !_reading, onChanged: _form.markDirty),
              const Text('Import each currency separately.'),
              const Text('Use decimal amounts. Quote comma-separated split lists; repeated names count as shares.'),
              const SizedBox(height: 8),
              const SelectableText(_sample),
              const SizedBox(height: 16),
              Align(alignment: Alignment.centerLeft, child: OutlinedButton.icon(
                onPressed: _form.pending || _reading ? null : _select,
                icon: const Icon(Icons.upload_file), label: Text(_reading ? 'Reading file…' : 'Select CSV file'))),
              if (_fileError != null) Padding(padding: const EdgeInsets.symmetric(vertical: 12), child: Text(_fileError!)),
              const SizedBox(height: 16),
              TextField(controller: _text, enabled: !_form.pending && !_reading, minLines: 8, maxLines: 20,
                decoration: const InputDecoration(labelText: 'CSV text', alignLabelWithHint: true),
                onChanged: (_) { _fileError = null; _form.markDirty(); }),
              if (_form.error != null) Padding(padding: const EdgeInsets.symmetric(vertical: 16),
                child: Text(_form.error!, semanticsLabel: 'Error: ${_form.error}')),
              const SizedBox(height: 16),
              Wrap(spacing: 8, runSpacing: 8, children: [
                FilledButton(onPressed: _form.pending || _reading ? null : _submit,
                  child: Text(_form.pending ? 'Importing…' : 'Import expenses')),
                TextButton(onPressed: _form.pending || _reading ? null : () => context.go('/groups/${widget.groupId}/records'),
                  child: const Text('Cancel')),
              ]),
            ],
          ],
        )),
      ),
  );
}
