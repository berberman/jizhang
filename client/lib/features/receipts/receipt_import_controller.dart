import 'package:flutter/material.dart';

import '../../core/jizhang_repository.dart';
import '../../core/models.dart';
import '../../core/money.dart';
import '../../core/receipt_ai_models.dart';
import '../../core/receipt_import_math.dart';
import '../../core/session_controller.dart';
import '../../shared/widgets.dart';
import '../records/record_editor.dart';
import 'receipt_media.dart';

enum ReceiptImportStage { manual, selecting, preview, extracting, review, expenses, saving }

class ReceiptImportRow {
  ReceiptImportRow({required this.id, this.kind = '', this.treatment = '', String title = '', String amount = '',
    String quantity = '', String unit = '', String unitPrice = ''})
    : title = TextEditingController(text: title), amount = TextEditingController(text: amount),
      quantity = TextEditingController(text: quantity), unit = TextEditingController(text: unit),
      unitPrice = TextEditingController(text: unitPrice);
  final String id;
  String kind, treatment;
  final TextEditingController title, amount, quantity, unit, unitPrice;
  final Set<String> itemIds = {};
  void dispose() { title.dispose(); amount.dispose(); quantity.dispose(); unit.dispose(); unitPrice.dispose(); }
}

class ReceiptImportController extends ChangeNotifier {
  ReceiptImportController({required this.repository, required this.session, required this.groupId, required this.media}) {
    _sessionGeneration = session.generation;
    session.addListener(_sessionChanged);
  }
  final JizhangRepository repository;
  final SessionController session;
  final String groupId;
  final ReceiptMedia media;
  late int _sessionGeneration;
  int _generation = 0, _nextId = 0;
  bool _disposed = false;
  ReceiptImportStage stage = ReceiptImportStage.manual;
  ReceiptImportStage _extractionReturnStage = ReceiptImportStage.preview;
  ReceiptImageInput? image;
  MemoryImage? imageProvider;
  ReceiptEvidence? evidence;
  String? extractionId, suggestedTitle, error;
  List<ReceiptReviewIssue> issues = [];
  List<CurrencyInfo> catalog = [];
  bool optionsLoaded = false, available = false;
  int maxBytes = 0;
  String? optionsError;
  final merchant = TextEditingController(), totalTitle = TextEditingController(), date = TextEditingController();
  final printedTotal = TextEditingController(), finalPaid = TextEditingController(), total = TextEditingController(), actualTip = TextEditingController();
  String currency = '', mode = 'itemized', totalBasis = '', tipDecision = '';
  bool totalTitleEdited = false;
  final List<ReceiptImportRow> items = [], adjustments = [];
  final Set<String> acknowledgedWarnings = {};
  ReceiptImportReview? acceptedReview;
  bool get busy => stage == ReceiptImportStage.selecting || stage == ReceiptImportStage.extracting || stage == ReceiptImportStage.saving;
  CurrencyInfo? get selectedCurrency => catalog.where((entry) => entry.code == currency).firstOrNull;
  void changed() { acknowledgedWarnings.clear(); acceptedReview = null; error = null; notifyListeners(); }
  List<ReceiptReviewIssue> get requiredWarnings {
    final warnings = <String, ReceiptReviewIssue>{};
    for (final issue in receiptImportEvidenceWarnings(catalog, evidence, mode)) { warnings[issue.warningToken] = issue; }
    try {
      for (final issue in receiptReviewNumericWarnings(catalog, buildReview())) { warnings[issue.warningToken] = issue; }
    } on FormatException { /* Missing values remain editable until Continue. */ }
    return warnings.values.toList();
  }
  void acknowledgeWarnings(bool value) {
    if (value) {
      try { buildReview(); } on FormatException { return; }
      acknowledgedWarnings..clear()..addAll(requiredWarnings.map((issue) => issue.warningToken));
    } else { acknowledgedWarnings.clear(); }
    notifyListeners();
  }
  Future<void> loadOptions() async {
    final generation = _generation;
    try {
      final result = await repository.receiptExtractionOptions(groupId);
      if (!_current(generation)) return;
      available = result.available; maxBytes = result.maxImageBytes;
      optionsLoaded = true; optionsError = null;
    } catch (failure) {
      if (!_current(generation)) return;
      optionsLoaded = true; optionsError = errorMessage(failure); available = false;
    }
    if (_current(generation)) notifyListeners();
  }
  bool _current(int generation) => !_disposed && generation == _generation && _sessionGeneration == session.generation;
  Future<bool> selectImage(BuildContext context, {bool camera = false}) async {
    if (busy || maxBytes <= 0) return false;
    final generation = ++_generation;
    final previous = stage;
    stage = ReceiptImportStage.selecting; error = null; notifyListeners();
    try {
      final selected = camera ? await media.takePhoto(context, maxBytes: maxBytes) : await media.pickImage(maxBytes: maxBytes);
      if (!_current(generation)) return false;
      if (selected == null) { stage = previous; notifyListeners(); return false; }
      _releaseImage(); _clearReview(); image = selected; imageProvider = MemoryImage(selected.bytes);
      stage = ReceiptImportStage.preview; notifyListeners(); return true;
    } catch (failure) {
      if (!_current(generation)) return false;
      stage = previous; error = failure.toString(); notifyListeners(); return false;
    }
  }
  Future<void> extract() async {
    if (busy || !available || image == null) return;
    final generation = ++_generation;
    final selected = image!;
    _extractionReturnStage = stage;
    stage = ReceiptImportStage.extracting; error = null; notifyListeners();
    try {
      final currencies = await repository.getCurrencies();
      if (!_current(generation)) return;
      final result = await repository.extractReceiptImage(groupId, selected);
      if (!_current(generation)) return;
      _clearReview(); catalog = currencies; evidence = result.evidence; extractionId = result.extractionId; suggestedTitle = result.title; issues = result.issues;
      _initialize(result.evidence);
      stage = ReceiptImportStage.review; notifyListeners();
    } catch (failure) {
      if (!_current(generation)) return;
      stage = _extractionReturnStage; error = errorMessage(failure); notifyListeners();
    }
  }
  Future<bool> openSavedSource(String receiptId) async {
    if (busy) return false;
    final generation = ++_generation;
    final previous = stage;
    stage = ReceiptImportStage.selecting; error = null; notifyListeners();
    try {
      final saved = await repository.getReceiptSource(groupId, receiptId);
      if (!_current(generation)) return false;
      final currencies = await repository.getCurrencies();
      if (!_current(generation)) return false;
      _releaseImage(); _clearReview(); catalog = currencies;
      image = saved.image; imageProvider = MemoryImage(saved.image.bytes);
      evidence = saved.evidence; extractionId = saved.extractionId;
      if (saved.review != null) {
        restoreReview(saved.review!, accepted: false);
      } else {
        stage = ReceiptImportStage.preview;
      }
      notifyListeners(); return true;
    } catch (failure) {
      if (!_current(generation)) return false;
      stage = previous; error = errorMessage(failure); notifyListeners(); return false;
    }
  }
  void restoreReview(ReceiptImportReview review, {bool accepted = true}) {
    for (final row in [...items, ...adjustments]) { row.dispose(); }
    items.clear(); adjustments.clear();
    currency = review.currency; mode = review.mode; totalBasis = review.totalBasis; tipDecision = review.tipDecision;
    final info = selectedCurrency;
    if (info == null) throw const FormatException('The historical currency is unavailable in the current catalog. Select and confirm a supported currency.');
    merchant.text = review.merchant; totalTitle.text = review.totalTitle; date.text = review.date;
    total.text = info.formatMajor(review.totalMinor);
    printedTotal.text = review.printedTotalMinor == null ? '' : info.formatMajor(review.printedTotalMinor!);
    finalPaid.text = review.finalPaidMinor == null ? '' : info.formatMajor(review.finalPaidMinor!);
    actualTip.text = review.actualTipMinor == null ? '' : info.formatMajor(review.actualTipMinor!);
    for (final row in review.items) {
      items.add(ReceiptImportRow(id: row.id, title: row.title, amount: info.formatMajor(row.amountMinor), quantity: row.quantity ?? '',
        unit: row.unit ?? '', unitPrice: row.unitPrice ?? ''));
    }
    for (final row in review.adjustments) {
      final charge = ReceiptImportRow(id: row.id, kind: row.kind, treatment: row.treatment,
        amount: info.formatMajor(row.amountMinor));
      charge.itemIds.addAll(row.itemIds); adjustments.add(charge);
    }
    acknowledgedWarnings.clear();
    if (accepted) acknowledgedWarnings.addAll(review.acknowledgedWarnings);
    totalTitleEdited = true;
    acceptedReview = accepted ? review : null; stage = accepted ? ReceiptImportStage.expenses : ReceiptImportStage.review; notifyListeners();
  }
  void cancelExtraction() {
    if (stage != ReceiptImportStage.extracting) return;
    _generation++; stage = _extractionReturnStage;
    error = 'This request may still be charged.';
    notifyListeners();
  }
  String _observation(Object? value) => value is Map ? (value['value'] as String? ?? '') : '';
  void _initialize(ReceiptEvidence original) {
    final json = original.toJson();
    merchant.text = _observation(json['merchant']);
    totalTitle.text = merchant.text.trim().isEmpty ? 'Receipt expense' : merchant.text;
    date.text = _observation(json['transaction_date']);
    final code = (json['currency'] as Map)['code'];
    currency = catalog.any((entry) => entry.code == code) ? code as String : '';
    final totals = json['totals'] as Map;
    printedTotal.text = _observation(totals['printed_total']); finalPaid.text = _observation(totals['final_paid']);
    total.text = finalPaid.text.isNotEmpty ? finalPaid.text : printedTotal.text;
    totalBasis = finalPaid.text.isNotEmpty ? 'final_paid' : printedTotal.text.isNotEmpty ? 'printed_total' : '';
    for (final value in json['items'] as List) {
      final row = value as Map;
      items.add(ReceiptImportRow(id: _newId('item'), title: row['description'] as String? ?? '', amount: _observation(row['line_total']),
        quantity: _observation(row['quantity']), unit: _observation(row['unit']), unitPrice: _observation(row['unit_price'])));
    }
    for (final value in json['adjustments'] as List) {
      final row = value as Map;
      adjustments.add(ReceiptImportRow(id: _newId('charge'), kind: row['kind'] as String,
        treatment: row['treatment'] as String, amount: _observation(row['amount'])));
    }
  }
  String _newId(String prefix) {
    String id;
    do { id = '${prefix}_${_nextId++}'; } while (items.any((row) => row.id == id) || adjustments.any((row) => row.id == id));
    return id;
  }
  void addItem() { items.add(ReceiptImportRow(id: _newId('item'))); changed(); }
  void addAdjustment() { adjustments.add(ReceiptImportRow(id: _newId('charge'))); changed(); }
  void removeItem(ReceiptImportRow row) { items.remove(row); for (final charge in adjustments) { charge.itemIds.remove(row.id); } row.dispose(); changed(); }
  void removeAdjustment(ReceiptImportRow row) { adjustments.remove(row); row.dispose(); changed(); }
  String? _optional(TextEditingController field) => field.text.trim().isEmpty ? null : field.text.trim();
  ReceiptImportReview buildReview() {
    final selected = selectedCurrency;
    if (selected == null) throw const FormatException('Select a supported receipt currency.');
    validateCalendarDate(date.text.trim());
    if (totalBasis.isEmpty) throw const FormatException('Choose the spending total basis.');
    if (tipDecision.isEmpty) throw const FormatException('Choose an actual-tip decision.');
    String money(TextEditingController value, String field) {
      try { return parseMajorAmount(value.text, minorDigits: selected.minorUnits, allowZero: true).toString(); }
      on FormatException catch (failure) { throw FormatException('$field: ${failure.message}'); }
    }
    String? optionalMoney(TextEditingController value, String field) => value.text.trim().isEmpty ? null : money(value, field);
    return ReceiptImportReview.fromJson({
      'mode': mode, 'merchant': merchant.text, 'totalTitle': totalTitle.text, 'currency': currency, 'date': date.text.trim(),
      'totalBasis': totalBasis, 'printedTotalMinor': optionalMoney(printedTotal, 'printedTotal'), 'finalPaidMinor': tipDecision == 'bill_only' ? null : optionalMoney(finalPaid, 'finalPaid'),
      'totalMinor': money(total, 'total'), 'tipDecision': tipDecision,
      'actualTipMinor': tipDecision == 'bill_only' ? null : tipDecision == 'none' ? '0' : optionalMoney(actualTip, 'actualTip'),
      'items': mode == 'total' ? <Object>[] : [for (final row in items) {
        'id': row.id, 'title': row.title.text, 'amountMinor': money(row.amount, 'items.${row.id}.amount'), 'quantity': _optional(row.quantity),
        'unit': _optional(row.unit), 'unitPrice': _optional(row.unitPrice),
      }],
      'adjustments': mode == 'total' ? <Object>[] : [for (final row in adjustments) {
        'id': row.id, 'kind': row.kind, 'amountMinor': money(row.amount, 'adjustments.${row.id}.amount'), 'treatment': row.treatment, 'itemIds': row.itemIds.toList(),
      }],
      'acknowledgedWarnings': {...acknowledgedWarnings, if (tipDecision == 'bill_only') 'bill_only:tipDecision'}.toList(), 'confirmed': true,
    });
  }
  List<ReceiptImportAmount> acceptReview() {
    final review = buildReview();
    final result = validateReceiptImportReview(catalog, evidence, review);
    acceptedReview = review; stage = ReceiptImportStage.expenses; error = null; notifyListeners(); return result;
  }
  void returnToReview() { acknowledgedWarnings.clear(); acceptedReview = null; stage = ReceiptImportStage.review; notifyListeners(); }
  ReceiptSourceInput? get source {
    if (image == null) return null;
    return ReceiptSourceInput(image: image!, extractionId: acceptedReview == null ? null : extractionId,
      evidence: acceptedReview == null ? null : evidence, review: acceptedReview);
  }
  void useManualAttachment() { _clearReview(); stage = ReceiptImportStage.expenses; notifyListeners(); }
  void _releaseImage() { imageProvider?.evict(); imageProvider = null; image = null; }
  void _clearReview() {
    for (final row in [...items, ...adjustments]) { row.dispose(); }
    items.clear(); adjustments.clear(); evidence = null; extractionId = null; suggestedTitle = null; acceptedReview = null; issues = [];
    for (final field in [merchant, totalTitle, date, printedTotal, finalPaid, total, actualTip]) { field.clear(); }
    acknowledgedWarnings.clear();
    totalTitleEdited = false;
    currency = ''; mode = 'itemized'; totalBasis = ''; tipDecision = '';
  }
  void clear() { _generation++; _releaseImage(); _clearReview(); stage = ReceiptImportStage.manual; error = null; notifyListeners(); }
  void _sessionChanged() {
    if (_sessionGeneration == session.generation) return;
    _sessionGeneration = session.generation; media.dispose(); clear();
  }
  @override
  void dispose() {
    _disposed = true; _generation++; session.removeListener(_sessionChanged); media.dispose(); _releaseImage(); _clearReview();
    for (final field in [merchant, totalTitle, date, printedTotal, finalPaid, total, actualTip]) { field.dispose(); }
    super.dispose();
  }
}
