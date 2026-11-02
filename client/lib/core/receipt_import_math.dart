import 'models.dart' show CurrencyInfo;
import 'money.dart';
import 'receipt_ai_models.dart';

class ReceiptImportAmount {
  const ReceiptImportAmount({required this.itemId, required this.title, required this.amountMinor});
  final String itemId;
  final String title;
  final BigInt amountMinor;
}

bool validReceiptDate(String value) {
  if (value.length != 10 || !RegExp(r'^[0-9]{4}-[0-9]{2}-[0-9]{2}$').hasMatch(value)) return false;
  final year = int.parse(value.substring(0, 4));
  final month = int.parse(value.substring(5, 7));
  final day = int.parse(value.substring(8));
  final date = DateTime.utc(year, month, day);
  return date.year == year && date.month == month && date.day == day;
}

class ReceiptDecimal {
  const ReceiptDecimal(this.numerator, this.denominator);
  final BigInt numerator;
  final BigInt denominator;
}
ReceiptDecimal parseReceiptDecimal(String text) {
  final match = RegExp(r'^([0-9]{1,18})(?:\.([0-9]{1,18}))?$').firstMatch(text);
  if (match == null || match.end != text.length) throw const FormatException('Use an unsigned decimal with at most 18 integer and 18 fractional digits');
  final fraction = match.group(2) ?? '';
  return ReceiptDecimal(BigInt.parse('${match.group(1)}$fraction'), BigInt.from(10).pow(fraction.length));
}
CurrencyInfo _currency(List<CurrencyInfo> catalog, String? code) {
  for (final info in catalog) {
    if (info.code == code) return info;
  }
  throw const FormatException('Confirm a supported catalog currency');
}
bool _valid(void Function() action) {
  try { action(); return true; } on FormatException { return false; }
}
BigInt _money(CurrencyInfo info, String text) {
  if (text != text.trim()) throw const FormatException('Money must not contain surrounding whitespace');
  return parseMajorAmount(text, minorDigits: info.minorUnits, allowZero: true);
}
bool _invalidTreatment(String kind, String treatment) => (kind == 'discount' && treatment == 'add') || (kind == 'actual_tip' && treatment == 'subtract');
BigInt _roundedProduct(CurrencyInfo info, String quantity, String price) {
  final q = parseReceiptDecimal(quantity);
  final p = parseReceiptDecimal(price);
  final numerator = q.numerator * p.numerator * BigInt.from(10).pow(info.minorUnits);
  final denominator = q.denominator * p.denominator;
  return (numerator * BigInt.two + denominator) ~/ (denominator * BigInt.two);
}
ReceiptReviewIssue _issue(String severity, String code, String field, String message) => ReceiptReviewIssue(code: code, field: field, message: message, severity: severity);
ReceiptReviewIssue _mismatch(String field) => _issue('warning', 'quantity_price_mismatch', field, 'Quantity times unit price differs from the printed extended amount; use the confirmed extended amount');
String receiptReviewWarningToken(ReceiptReviewIssue issue) => issue.warningToken;

List<ReceiptReviewIssue> _evidenceIssues(List<CurrencyInfo> catalog, ReceiptEvidence evidence) {
  final result = <ReceiptReviewIssue>[];
  CurrencyInfo? info;
  try { info = _currency(catalog, evidence.currency.code); } on FormatException {
    result.add(_issue('error', 'currency_required', 'currency', 'Confirm a supported catalog currency'));
  }
  if (evidence.currency.basis != 'explicit') result.add(_issue('warning', 'currency_unconfirmed', 'currency', 'Currency was inferred or is unknown; confirm it explicitly'));
  if (!validReceiptDate(evidence.transactionDate.value ?? '')) result.add(_issue('error', 'date_required', 'transaction_date', 'Confirm an explicit full YYYY-MM-DD date; no year is inferred'));
  final ids = evidence.items.map((v) => v.id).toList();
  final adjustmentIds = evidence.adjustments.map((v) => v.id).toList();
  if (ids.toSet().length != ids.length || ids.any((v) => !validReceiptItemId(v))) result.add(_issue('error', 'item_ids', 'items', 'Item IDs must be unique lowercase ASCII identifiers'));
  if (adjustmentIds.toSet().length != adjustmentIds.length || adjustmentIds.any((v) => !validReceiptItemId(v))) result.add(_issue('error', 'adjustment_ids', 'adjustments', 'Adjustment IDs must be unique lowercase ASCII identifiers'));
  List<ReceiptReviewIssue> decimal(String field, String value, String severity) => _valid(() { parseReceiptDecimal(value); }) ? [] : [_issue(severity, 'invalid_decimal', field, 'Enter an ordinary unsigned decimal with at most 18 integer and 18 fractional digits')];
  List<ReceiptReviewIssue> optionalDecimal(String field, ReceiptObservation observation) => observation.value == null ? [] : decimal(field, observation.value!, 'warning');
  List<ReceiptReviewIssue> money(bool required, String field, ReceiptObservation observation) {
    final value = observation.value;
    if (value == null) return required ? [_issue('error', 'amount_required', field, 'Enter the printed amount or correct this field')] : [];
    final currency = info;
    if (currency == null) return decimal(field, value, 'error');
    return _valid(() { _money(currency, value); }) ? [] : [_issue('error', 'invalid_amount', field, 'Amount is not a valid nonnegative amount at the selected currency precision')];
  }
  for (final item in evidence.items) {
    final field = 'items.${item.id}';
    result.addAll(money(true, '$field.line_total', item.lineTotal));
    result.addAll(optionalDecimal('$field.quantity', item.quantity));
    result.addAll(optionalDecimal('$field.unit_price', item.unitPrice));
    if (info != null && item.quantity.value != null && item.unitPrice.value != null && item.lineTotal.value != null) {
      try {
        if (_roundedProduct(info, item.quantity.value!, item.unitPrice.value!) != _money(info, item.lineTotal.value!)) result.add(_mismatch(field));
      } on FormatException { /* Invalid observations already have field issues. */ }
    }
  }
  for (final adjustment in evidence.adjustments) {
    final field = 'adjustments.${adjustment.id}';
    result.addAll(money(true, '$field.amount', adjustment.amount));
    if (adjustment.treatment == 'unknown') result.add(_issue('error', 'treatment_required', '$field.treatment', 'Confirm how this adjustment affects the extracted item amounts'));
    if (adjustment.itemId != null && !ids.contains(adjustment.itemId)) result.add(_issue('error', 'item_reference', '$field.item_id', 'Adjustment references an unknown item'));
    if (adjustment.scope == 'item' && adjustment.itemId == null) result.add(_issue('error', 'item_reference', '$field.item_id', 'An item-scoped adjustment must identify an item'));
    if (_invalidTreatment(adjustment.kind, adjustment.treatment)) result.add(_issue('error', 'invalid_treatment', '$field.treatment', 'Discounts cannot add and actual tips cannot subtract'));
  }
  final totals = evidence.totals;
  for (final entry in {'subtotal': totals.subtotal, 'tax_total': totals.taxTotal, 'printed_total': totals.printedTotal, 'final_paid': totals.finalPaid}.entries) {
    result.addAll(money(false, 'totals.${entry.key}', entry.value));
  }
  for (var index = 0; index < evidence.suggestedTips.length; index++) {
    final tip = evidence.suggestedTips[index];
    for (final warning in [...optionalDecimal('suggested_tips.$index.rate', tip.rate), ...money(false, 'suggested_tips.$index.amount', tip.amount)]) {
      result.add(_issue('warning', warning.code, warning.field, warning.message));
    }
  }
  for (var index = 0; index < evidence.issues.length; index++) {
    final issue = evidence.issues[index];
    result.add(_issue('warning', 'model_issue_$index', issue.field, issue.reason));
  }
  return result;
}
List<ReceiptReviewIssue> validateReceiptEvidence(List<CurrencyInfo> catalog, ReceiptEvidence evidence) => List.unmodifiable(_evidenceIssues(catalog, evidence).take(400));
List<ReceiptReviewIssue> receiptReviewNumericWarnings(List<CurrencyInfo> catalog, ReceiptImportReview review) {
  final info = _currency(catalog, review.currency);
  return [for (final item in review.items) if (item.quantity != null && item.unitPrice != null && _valid(() { parseReceiptDecimal(item.quantity!); parseReceiptDecimal(item.unitPrice!); }) && _roundedProduct(info, item.quantity!, item.unitPrice!) != item.amountMinor) _mismatch('items.${item.id}')];
}
void _require(bool condition, String message) {
  if (!condition) throw FormatException(message);
}
void _acknowledge(ReceiptImportReview review, Iterable<ReceiptReviewIssue> warnings) {
  for (final warning in warnings) {
    _require(review.acknowledgedWarnings.contains(warning.warningToken), 'Acknowledge warning ${warning.warningToken}');
  }
}
void _validateTip(ReceiptImportReview review) {
  final tips = review.adjustments.where((a) => a.kind == 'actual_tip');
  switch (review.tipDecision) {
    case 'none':
      _require(review.actualTipMinor == BigInt.zero, 'No tip requires actualTipMinor=0');
      _require(!tips.any((a) => a.amountMinor != BigInt.zero), 'No tip forbids nonzero actual-tip adjustments');
    case 'confirmed':
      final tip = review.actualTipMinor;
      _require(tip != null, 'Confirm the actual tip amount, already included in the total');
      _require(tip! <= review.totalMinor, 'Actual tip exceeds the confirmed total');
      if (review.mode == 'itemized') _require(tip == tips.fold(BigInt.zero, (sum, a) => sum + a.amountMinor), 'Actual tip must equal the canonical actual-tip adjustments represented once');
    case 'bill_only':
      _require(review.actualTipMinor == null && review.finalPaidMinor == null, 'Bill-only review must leave actual tip and final payment unknown');
      _require(review.totalBasis != 'final_paid', 'Bill-only review cannot use final paid as its total basis');
      _require(!tips.any((a) => a.treatment == 'add'), 'Bill-only review cannot add an actual tip');
      _require(review.acknowledgedWarnings.contains('bill_only:tipDecision'), 'Confirm: Record bill amount only; actual tip is unknown');
  }
}
void _recordAmount(BigInt value) => _require(value > BigInt.zero && value <= maxRecordMinor, 'Amount must be positive and within the supported record range');
List<ReceiptImportAmount> computeReceiptImportAmounts(List<CurrencyInfo> catalog, ReceiptImportReview review) {
  ReceiptImportReview.fromJson(review.toJson());
  _currency(catalog, review.currency);
  _require(review.confirmed, 'Confirm the receipt review');
  _require(validReceiptDate(review.date), 'Confirm an explicit full YYYY-MM-DD date');
  _require(review.totalMinor > BigInt.zero, 'Confirmed total must be positive');
  if (review.totalBasis == 'printed_total') _require(review.printedTotalMinor == review.totalMinor, 'Printed total must equal the confirmed total');
  if (review.totalBasis == 'final_paid') _require(review.finalPaidMinor == review.totalMinor, 'Final paid amount must equal the confirmed total');
  _validateTip(review);
  if (review.mode == 'total') {
    _require(review.items.isEmpty && review.adjustments.isEmpty, 'Total mode must retain empty item and adjustment lists');
    _require(review.totalTitle.trim().isNotEmpty, 'Enter a total expense title');
    _recordAmount(review.totalMinor);
    return List.unmodifiable([ReceiptImportAmount(itemId: 'total', title: review.totalTitle, amountMinor: review.totalMinor)]);
  }
  final keys = review.items.map((a) => a.id).toList();
  _require(keys.isNotEmpty, 'Itemized mode requires at least one item');
  _require(keys.toSet().length == keys.length, 'Item IDs must be unique');
  _require(review.adjustments.map((a) => a.id).toSet().length == review.adjustments.length, 'Adjustment IDs must be unique');
  for (final item in review.items) {
    _require(item.title.trim().isNotEmpty, 'Enter a title for item ${item.id}');
    if (item.quantity != null) parseReceiptDecimal(item.quantity!);
    if (item.unitPrice != null) parseReceiptDecimal(item.unitPrice!);
  }
  for (final adjustment in review.adjustments) {
    _require(!_invalidTreatment(adjustment.kind, adjustment.treatment), 'Discounts cannot add and actual tips cannot subtract');
    _require(adjustment.itemIds.toSet().length == adjustment.itemIds.length && adjustment.itemIds.every(keys.contains), 'Adjustment eligibility must contain unique existing item IDs');
    if (adjustment.treatment == 'add' || adjustment.treatment == 'subtract') _require(adjustment.itemIds.isNotEmpty, 'Confirm a nonempty eligible item set for every external adjustment');
  }
  _acknowledge(review, receiptReviewNumericWarnings(catalog, review));
  final bases = {for (final item in review.items) item.id: item.amountMinor};
  List<MoneyKeyValue> weights(ReceiptReviewAdjustment adjustment, Map<String, BigInt> values) {
    final rows = [for (final key in keys) if (adjustment.itemIds.contains(key)) MoneyKeyValue(key, values[key]!)];
    _require(rows.isNotEmpty && rows.fold(BigInt.zero, (sum, row) => sum + row.value) > BigInt.zero, 'Adjustment requires positive eligible item bases');
    return rows;
  }
  for (final adjustment in review.adjustments.where((a) => a.treatment == 'subtract')) {
    final rows = weights(adjustment, bases);
    _require(adjustment.amountMinor <= rows.fold(BigInt.zero, (sum, row) => sum + row.value), 'Discount exceeds eligible item bases');
    for (final row in apportionByKey(adjustment.amountMinor, rows)) { bases[row.key] = bases[row.key]! - row.value; }
  }
  // Added fees use the discounted bases, never a preceding fee's allocations.
  final finalAmounts = Map<String, BigInt>.of(bases);
  for (final adjustment in review.adjustments.where((a) => a.treatment == 'add')) {
    for (final row in apportionByKey(adjustment.amountMinor, weights(adjustment, bases))) { finalAmounts[row.key] = finalAmounts[row.key]! + row.value; }
  }
  final result = [for (final item in review.items) if (finalAmounts[item.id] != BigInt.zero) ReceiptImportAmount(itemId: item.id, title: item.title, amountMinor: finalAmounts[item.id]!)];
  for (final amount in result) { _recordAmount(amount.amountMinor); }
  _require(result.fold(BigInt.zero, (sum, row) => sum + row.amountMinor) == review.totalMinor, 'Itemized amounts must exactly equal the confirmed total');
  return List.unmodifiable(result);
}
List<ReceiptReviewIssue> receiptImportEvidenceWarnings(List<CurrencyInfo> catalog, ReceiptEvidence? evidence, String mode) {
  if (evidence == null) return const [];
  return _evidenceIssues(catalog, evidence).where((warning) => warning.severity == 'warning' && (warning.code.startsWith('model_issue_') || warning.field.startsWith('suggested_tips.')) && (mode == 'itemized' || !(warning.field.startsWith('items') || warning.field.startsWith('adjustments')))).toList();
}
List<ReceiptImportAmount> validateReceiptImportReview(List<CurrencyInfo> catalog, ReceiptEvidence? evidence, ReceiptImportReview review) {
  final amounts = computeReceiptImportAmounts(catalog, review);
  _acknowledge(review, receiptImportEvidenceWarnings(catalog, evidence, review.mode));
  return amounts;
}
