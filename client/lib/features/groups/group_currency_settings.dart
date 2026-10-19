import 'package:flutter/material.dart';

import '../../core/jizhang_repository.dart';
import '../../core/models.dart';
import '../../core/session_controller.dart';
import '../../shared/entity_picker.dart';
import '../../shared/widgets.dart';

String groupCurrencyLabel(ExpenseGroup group) => group.currencyMode == CurrencyMode.single
    ? '${group.defaultCurrency.code} settlement'
    : 'Multiple currencies · default ${group.defaultCurrency.code}';

class GroupCurrencySettings extends StatefulWidget {
  const GroupCurrencySettings({super.key, required this.repository, required this.session,
    required this.mode, required this.currencyCode, required this.onModeChanged,
    required this.onCurrencyChanged, this.locked = false, this.enabled = true});
  final JizhangRepository repository;
  final SessionController session;
  final CurrencyMode mode;
  final String currencyCode;
  final ValueChanged<CurrencyMode> onModeChanged;
  final ValueChanged<String> onCurrencyChanged;
  final bool locked, enabled;
  @override
  State<GroupCurrencySettings> createState() => _GroupCurrencySettingsState();
}

class _GroupCurrencySettingsState extends State<GroupCurrencySettings> {
  List<CurrencyInfo>? _currencies;
  Object? _error;
  int _request = 0;
  @override
  void initState() { super.initState(); if (!widget.locked) _load(); }
  Future<void> _load() async {
    final request = ++_request;
    final generation = widget.session.generation;
    setState(() { _error = null; });
    try {
      final currencies = await widget.repository.getCurrencies();
      if (!mounted || request != _request || generation != widget.session.generation) return;
      setState(() => _currencies = currencies);
    } catch (error) {
      if (mounted && request == _request && generation == widget.session.generation) setState(() => _error = error);
    }
  }
  Future<void> _choose() async {
    final generation = widget.session.generation;
    final currency = await showEntityPicker<CurrencyInfo>(context, session: widget.session,
      title: 'Choose currency', searchLabel: 'Search currency code or name', items: _currencies!,
      selectedId: widget.currencyCode, idOf: (item) => item.code,
      titleOf: (item) => item.code, subtitleOf: (item) => item.name,
      leading: (_) => const Icon(Icons.currency_exchange));
    if (!mounted || generation != widget.session.generation || !widget.enabled || widget.locked || currency == null || currency.code == widget.currencyCode) return;
    widget.onCurrencyChanged(currency.code);
  }
  @override
  Widget build(BuildContext context) => FormField<String>(
    validator: (_) => widget.locked || (_currencies?.any((item) => item.code == widget.currencyCode) ?? false)
      ? null : 'Load the currency catalogue before saving settings.',
    builder: (field) => Column(crossAxisAlignment: CrossAxisAlignment.stretch, children: [
      RadioGroup<CurrencyMode>(groupValue: widget.mode,
        onChanged: (value) { if (widget.enabled && !widget.locked && value != null) widget.onModeChanged(value); },
        child: Column(children: [for (final mode in CurrencyMode.values) RadioListTile<CurrencyMode>(
          contentPadding: EdgeInsets.zero, value: mode, enabled: widget.enabled && !widget.locked,
          title: Text(mode == CurrencyMode.single
            ? 'Single currency settlement'
            : 'Settle each currency separately'))])),
      const SizedBox(height: 12),
      InputDecorator(decoration: InputDecoration(
        labelText: widget.mode == CurrencyMode.single ? 'Settlement currency' : 'Default entry currency'),
        child: TextButton.icon(onPressed: widget.enabled && !widget.locked && _currencies != null ? _choose : null,
          icon: const Icon(Icons.search), label: Text(widget.currencyCode))),
      if (widget.locked) ...[
        const SizedBox(height: 8),
        const Text('Currency settings are locked after the first record'),
      ],
      if (!widget.locked && _currencies == null && _error == null) ...[
        const SizedBox(height: 8),
        const LinearProgressIndicator(),
      ],
      if (_error != null) ...[
        const SizedBox(height: 8),
        ErrorPanel(error: _error!, onRetry: _load),
      ],
      if (field.hasError) ...[
        const SizedBox(height: 8),
        Text(field.errorText!, style: TextStyle(color: Theme.of(context).colorScheme.error)),
      ],
    ]));
}
