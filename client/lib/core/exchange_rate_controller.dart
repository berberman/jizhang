import 'package:flutter/material.dart';

import 'api_client.dart';
import 'jizhang_repository.dart';
import 'models.dart';
import 'money.dart';
import 'session_controller.dart';

class ExchangeRateController extends ChangeNotifier {
  ExchangeRateController({required this.repository, required this.session, required ExpenseGroup group,
    required this.sourceCurrency, ExchangeRate? savedRate}) : _group = group,
      manualRate = TextEditingController(text: savedRate?.rate ?? ''), _saved = savedRate,
      _sessionGeneration = session.generation {
    session.addListener(_sessionChanged);
  }
  final JizhangRepository repository;
  final SessionController session;
  ExpenseGroup _group;
  String sourceCurrency;
  final TextEditingController manualRate;
  ExchangeQuote? selectedQuote;
  ExchangeRate? _saved;
  bool loading = false;
  String? error;
  Future<void> Function()? onPolicyConflict;
  int _request = 0;
  int _sessionGeneration;
  bool _active = true;
  bool get needsConversion => _group.currencyMode == CurrencyMode.single && sourceCurrency != _group.defaultCurrency.code;
  CurrencyInfo get targetCurrency => _group.defaultCurrency;
  String? get rateDate => selectedQuote?.rateDate ?? _saved?.rateDate;
  String? get source => selectedQuote != null ? 'reference' : _saved?.source ?? (manualRate.text.isEmpty ? null : 'manual');
  bool get stale {
    final date = DateTime.tryParse(rateDate ?? '');
    final now = DateTime.now().toUtc();
    return date != null && DateTime.utc(now.year, now.month, now.day).difference(DateTime.utc(date.year, date.month, date.day)).inDays > 7;
  }
  void _sessionChanged() {
    if (session.generation == _sessionGeneration) return;
    _sessionGeneration = session.generation;
    deactivate();
  }
  void deactivate() { _active = false; _request++; loading = false; }
  void _clear() { _request++; selectedQuote = null; _saved = null; manualRate.clear(); loading = false; error = null; }
  void setCurrency(String value) {
    if (!_active || value == sourceCurrency) return;
    sourceCurrency = value;
    _clear();
    notifyListeners();
    if (needsConversion) refreshReference();
  }
  void updateGroup(ExpenseGroup group) {
    final changed = group.groupId != _group.groupId || group.currencyMode != _group.currencyMode || group.defaultCurrency.code != _group.defaultCurrency.code;
    _group = group;
    if (changed) { _clear(); if (_active) notifyListeners(); }
  }
  void setManualRate(String value) {
    if (!_active) return;
    _request++; loading = false; selectedQuote = null; _saved = null; error = null;
    if (manualRate.text != value) manualRate.text = value;
    notifyListeners();
  }
  Future<void> refreshReference() async {
    if (!_active || !needsConversion) return;
    final request = ++_request;
    final generation = session.generation;
    final source = sourceCurrency;
    final target = targetCurrency.code;
    selectedQuote = null; _saved = null; manualRate.clear(); loading = true; error = null;
    notifyListeners();
    final groupId = _group.groupId;
    bool current() => _active && request == _request && generation == session.generation && groupId == _group.groupId &&
      source == sourceCurrency && target == targetCurrency.code && needsConversion;
    try {
      final quote = await repository.createExchangeQuote(groupId, source);
      if (!current()) return;
      selectedQuote = quote; manualRate.text = quote.rate;
    } catch (failure) {
      if (!current()) return;
      error = failure.toString();
      if (failure is ApiException && failure.statusCode == 409) await onPolicyConflict?.call();
    } finally {
      if (current()) { loading = false; notifyListeners(); }
    }
  }
  ExchangeRateInput? toInput() {
    if (!needsConversion) return null;
    if (!_active || loading) throw const FormatException('Wait for the rate or enter a manual rate');
    if (selectedQuote != null) return ExchangeRateInput.reference(quoteId: selectedQuote!.quoteId);
    if (_saved != null) return _saved!.toInput();
    return ExchangeRateInput.manual(rate: parseDecimalRate(manualRate.text).text);
  }
  String get validatedRate {
    toInput();
    return parseDecimalRate(manualRate.text).text;
  }
  @override
  void dispose() {
    deactivate(); session.removeListener(_sessionChanged); manualRate.dispose(); super.dispose();
  }
}
