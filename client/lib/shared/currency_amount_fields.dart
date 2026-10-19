import 'package:flutter/material.dart';

import '../core/exchange_rate_controller.dart';
import '../core/jizhang_repository.dart';
import '../core/models.dart';
import '../core/session_controller.dart';
import 'entity_picker.dart';

class CurrencyAmountFields extends StatefulWidget {
  const CurrencyAmountFields({super.key, required this.repository, required this.session, required this.currency,
    required this.onCurrencyChanged, this.amountController, this.onChanged, this.enabled = true, this.locked = false, this.label = 'Currency'});
  final JizhangRepository repository;
  final SessionController session;
  final CurrencyInfo currency;
  final ValueChanged<CurrencyInfo> onCurrencyChanged;
  final TextEditingController? amountController;
  final VoidCallback? onChanged;
  final bool enabled, locked;
  final String label;
  @override
  State<CurrencyAmountFields> createState() => _CurrencyAmountFieldsState();
}
class _CurrencyAmountFieldsState extends State<CurrencyAmountFields> {
  bool _loading = false;
  String? _error;
  Future<void> _choose() async {
    final generation = widget.session.generation;
    setState(() { _loading = true; _error = null; });
    try {
      final currencies = await widget.repository.getCurrencies();
      if (!mounted || generation != widget.session.generation) return;
      final selected = await showEntityPicker<CurrencyInfo>(context, session: widget.session,
        title: widget.label, searchLabel: 'Search currency code or name', items: currencies,
        idOf: (c) => c.code, titleOf: (c) => c.code, subtitleOf: (c) => c.name,
        leading: (c) => const Icon(Icons.currency_exchange), selectedId: widget.currency.code);
      if (!mounted || generation != widget.session.generation || !widget.enabled) return;
      if (selected != null && selected.code != widget.currency.code) { widget.onCurrencyChanged(selected); widget.onChanged?.call(); }
    } catch (error) {
      if (mounted && generation == widget.session.generation) setState(() => _error = error.toString());
    } finally { if (mounted) setState(() => _loading = false); }
  }
  @override
  Widget build(BuildContext context) => Column(crossAxisAlignment: CrossAxisAlignment.stretch, children: [
    InputDecorator(decoration: InputDecoration(labelText: widget.label), child: Row(children: [
      Expanded(child: Text('${widget.currency.code} — ${widget.currency.name}')),
      if (!widget.locked) TextButton(onPressed: widget.enabled && !_loading ? _choose : null, child: Text(_loading ? 'Loading…' : 'Choose')),
    ])),
    if (_error != null) Wrap(crossAxisAlignment: WrapCrossAlignment.center, children: [Text(_error!), TextButton(onPressed: widget.enabled ? _choose : null, child: const Text('Retry'))]),
    if (widget.amountController != null) ...[
      const SizedBox(height: 12),
      TextField(controller: widget.amountController, enabled: widget.enabled,
        keyboardType: const TextInputType.numberWithOptions(decimal: true),
        decoration: InputDecoration(labelText: 'Amount', prefixText: '${widget.currency.code} ', helperText: '${widget.currency.minorUnits} decimal places'),
        onChanged: (_) => widget.onChanged?.call()),
    ],
  ]);
}

class ExchangeRateFields extends StatelessWidget {
  const ExchangeRateFields({super.key, required this.controller, this.onChanged, this.enabled = true});
  final ExchangeRateController controller;
  final VoidCallback? onChanged;
  final bool enabled;
  @override
  Widget build(BuildContext context) => ListenableBuilder(listenable: controller, builder: (context, _) {
    final c = controller;
    if (!c.needsConversion) return const SizedBox.shrink();
    return Padding(padding: const EdgeInsets.symmetric(vertical: 12), child: Column(crossAxisAlignment: CrossAxisAlignment.stretch, children: [
      Text('1 ${c.sourceCurrency} = ${c.manualRate.text.isEmpty ? '…' : c.manualRate.text} ${c.targetCurrency.code}'),
      const SizedBox(height: 12),
      TextField(controller: c.manualRate, enabled: enabled, keyboardType: const TextInputType.numberWithOptions(decimal: true),
        decoration: const InputDecoration(labelText: 'Exchange rate'),
        onChanged: (value) { c.setManualRate(value); onChanged?.call(); }),
      if (c.loading) ...[
        const SizedBox(height: 8),
        const LinearProgressIndicator(),
      ],
      if (c.source == 'reference') ...[
        const SizedBox(height: 8),
        Text('Frankfurter reference · ${c.rateDate} (latest available)'),
      ],
      if (c.source == 'manual') ...[
        const SizedBox(height: 8),
        const Text('Manual rate'),
      ],
      if (c.stale) ...[
        const SizedBox(height: 8),
        const Text('Warning: this reference is more than seven days old.'),
      ],
      if (c.error != null) ...[
        const SizedBox(height: 8),
        Text(c.error!, style: const TextStyle(color: Color(0xFFB42318))),
      ],
      const SizedBox(height: 8),
      Wrap(spacing: 8, children: [
        TextButton(onPressed: enabled && !c.loading ? () { onChanged?.call(); c.refreshReference(); } : null,
          child: Text(c.error == null ? 'Refresh rate' : 'Retry')),
        TextButton(onPressed: enabled ? () { c.setManualRate(c.manualRate.text); onChanged?.call(); } : null, child: const Text('Enter manually')),
      ]),
    ]));
  });
}
