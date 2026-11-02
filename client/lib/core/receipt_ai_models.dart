import 'dart:convert';
import 'dart:typed_data';

import 'money.dart';
import 'models.dart' show normalizeReceiptTitle;

const maxReceiptImageBytes = 5 * 1024 * 1024;
const maxReceiptEvidenceBytes = 256 * 1024;

Map<String, Object?> _fields(Object? value, String keys) {
  final expected = keys.split(' ').toSet();
  if (value is! Map<String, Object?> || value.length != expected.length || !expected.every(value.containsKey)) {
    throw const FormatException('Unexpected or missing receipt fields');
  }
  return value;
}
String _text(Object? value, [int bound = 2048]) {
  if (value is! String || value.runes.length > bound || value.contains('\u0000')) throw const FormatException('Invalid receipt text');
  return value;
}
String? _nullableText(Object? value, [int bound = 2048]) => value == null ? null : _text(value, bound);
String _enum(Object? value, String options) {
  final text = _text(value);
  if (!options.split(' ').contains(text)) throw const FormatException('Unknown receipt enum');
  return text;
}
bool _bool(Object? value) {
  if (value is! bool) throw const FormatException('Expected a receipt boolean');
  return value;
}
int _int(Object? value) {
  if (value is! int) throw const FormatException('Expected a receipt integer');
  return value;
}
List<T> _list<T>(Object? value, T Function(Object?) parse, [int bound = 200]) {
  if (value is! List || value.length > bound) throw const FormatException('Too many receipt entries or invalid array');
  return List.unmodifiable(value.map(parse));
}
void _boundedJson(Object? value) {
  if (utf8.encode(jsonEncode(value)).length > maxReceiptEvidenceBytes) throw const FormatException('Receipt evidence/review exceeds 256 KiB');
}
bool validReceiptItemId(String value) => value.isNotEmpty && RegExp(r'^[a-z0-9_]{1,64}$').firstMatch(value)?.end == value.length;
String _id(Object? value) {
  final result = _text(value, 64);
  if (!validReceiptItemId(result)) throw const FormatException('Invalid receipt row ID');
  return result;
}
String _uuid(Object? value) {
  final text = _text(value, 36);
  if (!RegExp(r'^[a-fA-F0-9]{8}-[a-fA-F0-9]{4}-[a-fA-F0-9]{4}-[a-fA-F0-9]{4}-[a-fA-F0-9]{12}$').hasMatch(text)) throw const FormatException('Invalid extraction ID');
  return text;
}
BigInt? _nullableMinor(Object? value) => value == null ? null : parseMinorAmount(value);
const _kinds = 'discount tax service_charge actual_tip rounding other';
const _treatments = 'add subtract included_in_line_total informational';

class ReceiptObservation {
  const ReceiptObservation({required this.value, required this.raw, required this.status});
  final String? value;
  final String? raw;
  final String status;
  factory ReceiptObservation.fromJson(Object? value) {
    final j = _fields(value, 'value raw status');
    final result = ReceiptObservation(value: _nullableText(j['value']), raw: _nullableText(j['raw']), status: _enum(j['status'], 'visible absent unreadable ambiguous'));
    if ((result.value != null) != (result.status == 'visible') || (result.status == 'visible' && result.raw == null)) throw const FormatException('Observation status does not match value/raw');
    return result;
  }
  Map<String, Object?> toJson() => {'value': value, 'raw': raw, 'status': status};
}
class ReceiptEvidenceCurrency {
  const ReceiptEvidenceCurrency({required this.code, required this.symbol, required this.basis, required this.evidence});
  final String? code;
  final String? symbol;
  final String basis;
  final String? evidence;
  factory ReceiptEvidenceCurrency.fromJson(Object? value) {
    final j = _fields(value, 'code symbol basis evidence');
    return ReceiptEvidenceCurrency(code: _nullableText(j['code']), symbol: _nullableText(j['symbol']), basis: _enum(j['basis'], 'explicit contextual unknown'), evidence: _nullableText(j['evidence']));
  }
  Map<String, Object?> toJson() => {'code': code, 'symbol': symbol, 'basis': basis, 'evidence': evidence};
}
class ReceiptEvidenceItem {
  ReceiptEvidenceItem({required this.id, required this.description, required this.rawLine, required this.quantity, required this.unit, required this.unitPrice, required this.lineTotal, required this.priceBasis, required List<String> modifiers, required List<String> taxMarkers}) : modifiers = List.unmodifiable(modifiers), taxMarkers = List.unmodifiable(taxMarkers);
  final String id;
  final String description;
  final String rawLine;
  final ReceiptObservation quantity;
  final ReceiptObservation unit;
  final ReceiptObservation unitPrice;
  final ReceiptObservation lineTotal;
  final String priceBasis;
  final List<String> modifiers;
  final List<String> taxMarkers;
  factory ReceiptEvidenceItem.fromJson(Object? value) {
    final j = _fields(value, 'id description raw_line quantity unit unit_price line_total price_basis modifiers tax_markers');
    return ReceiptEvidenceItem(id: _text(j['id'], 64), description: _text(j['description'], 512), rawLine: _text(j['raw_line']), quantity: ReceiptObservation.fromJson(j['quantity']), unit: ReceiptObservation.fromJson(j['unit']), unitPrice: ReceiptObservation.fromJson(j['unit_price']), lineTotal: ReceiptObservation.fromJson(j['line_total']), priceBasis: _enum(j['price_basis'], 'net gross unknown'), modifiers: _list(j['modifiers'], (v) => _text(v, 512)), taxMarkers: _list(j['tax_markers'], (v) => _text(v, 512)));
  }
  Map<String, Object?> toJson() => {'id': id, 'description': description, 'raw_line': rawLine, 'quantity': quantity.toJson(), 'unit': unit.toJson(), 'unit_price': unitPrice.toJson(), 'line_total': lineTotal.toJson(), 'price_basis': priceBasis, 'modifiers': modifiers, 'tax_markers': taxMarkers};
}
class ReceiptEvidenceAdjustment {
  const ReceiptEvidenceAdjustment({required this.id, required this.kind, required this.scope, required this.itemId, required this.amount, required this.treatment, required this.evidence});
  final String id;
  final String kind;
  final String scope;
  final String? itemId;
  final ReceiptObservation amount;
  final String treatment;
  final String evidence;
  factory ReceiptEvidenceAdjustment.fromJson(Object? value) {
    final j = _fields(value, 'id kind scope item_id amount treatment evidence');
    return ReceiptEvidenceAdjustment(id: _text(j['id'], 64), kind: _enum(j['kind'], _kinds), scope: _enum(j['scope'], 'item receipt unknown'), itemId: _nullableText(j['item_id'], 64), amount: ReceiptObservation.fromJson(j['amount']), treatment: _enum(j['treatment'], '$_treatments unknown'), evidence: _text(j['evidence']));
  }
  Map<String, Object?> toJson() => {'id': id, 'kind': kind, 'scope': scope, 'item_id': itemId, 'amount': amount.toJson(), 'treatment': treatment, 'evidence': evidence};
}
class ReceiptEvidenceTotals {
  const ReceiptEvidenceTotals({required this.subtotal, required this.taxTotal, required this.printedTotal, required this.finalPaid});
  final ReceiptObservation subtotal;
  final ReceiptObservation taxTotal;
  final ReceiptObservation printedTotal;
  final ReceiptObservation finalPaid;
  factory ReceiptEvidenceTotals.fromJson(Object? value) {
    final j = _fields(value, 'subtotal tax_total printed_total final_paid');
    return ReceiptEvidenceTotals(subtotal: ReceiptObservation.fromJson(j['subtotal']), taxTotal: ReceiptObservation.fromJson(j['tax_total']), printedTotal: ReceiptObservation.fromJson(j['printed_total']), finalPaid: ReceiptObservation.fromJson(j['final_paid']));
  }
  Map<String, Object?> toJson() => {'subtotal': subtotal.toJson(), 'tax_total': taxTotal.toJson(), 'printed_total': printedTotal.toJson(), 'final_paid': finalPaid.toJson()};
}
class ReceiptSuggestedTip {
  const ReceiptSuggestedTip({required this.rate, required this.amount});
  final ReceiptObservation rate;
  final ReceiptObservation amount;
  factory ReceiptSuggestedTip.fromJson(Object? value) {
    final j = _fields(value, 'rate amount');
    return ReceiptSuggestedTip(rate: ReceiptObservation.fromJson(j['rate']), amount: ReceiptObservation.fromJson(j['amount']));
  }
  Map<String, Object?> toJson() => {'rate': rate.toJson(), 'amount': amount.toJson()};
}
class ReceiptModelIssue {
  const ReceiptModelIssue({required this.field, required this.reason});
  final String field;
  final String reason;
  factory ReceiptModelIssue.fromJson(Object? value) {
    final j = _fields(value, 'field reason');
    return ReceiptModelIssue(field: _text(j['field'], 512), reason: _text(j['reason']));
  }
  Map<String, Object?> toJson() => {'field': field, 'reason': reason};
}
class ReceiptEvidence {
  ReceiptEvidence({required this.documentKind, required this.merchant, required this.transactionDate, required this.currency, required List<ReceiptEvidenceItem> items, required List<ReceiptEvidenceAdjustment> adjustments, required this.totals, required List<ReceiptSuggestedTip> suggestedTips, required List<ReceiptModelIssue> issues}) : items = List.unmodifiable(items), adjustments = List.unmodifiable(adjustments), suggestedTips = List.unmodifiable(suggestedTips), issues = List.unmodifiable(issues);
  final String documentKind;
  final ReceiptObservation merchant;
  final ReceiptObservation transactionDate;
  final ReceiptEvidenceCurrency currency;
  final List<ReceiptEvidenceItem> items;
  final List<ReceiptEvidenceAdjustment> adjustments;
  final ReceiptEvidenceTotals totals;
  final List<ReceiptSuggestedTip> suggestedTips;
  final List<ReceiptModelIssue> issues;
  factory ReceiptEvidence.fromJson(Object? value) {
    _boundedJson(value);
    final j = _fields(value, 'document_kind merchant transaction_date currency items adjustments totals suggested_tips issues');
    final merchant = ReceiptObservation.fromJson(j['merchant']);
    if (merchant.value != null) _text(merchant.value, 512);
    return ReceiptEvidence(documentKind: _enum(j['document_kind'], 'retail_receipt restaurant_receipt restaurant_bill fuel_receipt unknown'), merchant: merchant, transactionDate: ReceiptObservation.fromJson(j['transaction_date']), currency: ReceiptEvidenceCurrency.fromJson(j['currency']), items: _list(j['items'], ReceiptEvidenceItem.fromJson), adjustments: _list(j['adjustments'], ReceiptEvidenceAdjustment.fromJson), totals: ReceiptEvidenceTotals.fromJson(j['totals']), suggestedTips: _list(j['suggested_tips'], ReceiptSuggestedTip.fromJson), issues: _list(j['issues'], ReceiptModelIssue.fromJson, 400));
  }
  Map<String, Object?> toJson() => {'document_kind': documentKind, 'merchant': merchant.toJson(), 'transaction_date': transactionDate.toJson(), 'currency': currency.toJson(), 'items': items.map((v) => v.toJson()).toList(), 'adjustments': adjustments.map((v) => v.toJson()).toList(), 'totals': totals.toJson(), 'suggested_tips': suggestedTips.map((v) => v.toJson()).toList(), 'issues': issues.map((v) => v.toJson()).toList()};
}
class ReceiptReviewItem {
  const ReceiptReviewItem({required this.id, required this.title, required this.amountMinor, required this.quantity, required this.unit, required this.unitPrice});
  final String id;
  final String title;
  final BigInt amountMinor;
  final String? quantity;
  final String? unit;
  final String? unitPrice;
  factory ReceiptReviewItem.fromJson(Object? value) {
    final j = _fields(value, 'id title amountMinor quantity unit unitPrice');
    return ReceiptReviewItem(id: _id(j['id']), title: _text(j['title'], 512), amountMinor: parseMinorAmount(j['amountMinor']), quantity: _nullableText(j['quantity'], 512), unit: _nullableText(j['unit'], 512), unitPrice: _nullableText(j['unitPrice'], 512));
  }
  Map<String, Object?> toJson() => {'id': id, 'title': title, 'amountMinor': amountMinor.toString(), 'quantity': quantity, 'unit': unit, 'unitPrice': unitPrice};
}
class ReceiptReviewAdjustment {
  ReceiptReviewAdjustment({required this.id, required this.kind, required this.amountMinor, required this.treatment, required List<String> itemIds}) : itemIds = List.unmodifiable(itemIds);
  final String id;
  final String kind;
  final BigInt amountMinor;
  final String treatment;
  final List<String> itemIds;
  factory ReceiptReviewAdjustment.fromJson(Object? value) {
    final j = _fields(value, 'id kind amountMinor treatment itemIds');
    return ReceiptReviewAdjustment(id: _id(j['id']), kind: _enum(j['kind'], _kinds), amountMinor: parseMinorAmount(j['amountMinor']), treatment: _enum(j['treatment'], _treatments), itemIds: _list(j['itemIds'], _id));
  }
  Map<String, Object?> toJson() => {'id': id, 'kind': kind, 'amountMinor': amountMinor.toString(), 'treatment': treatment, 'itemIds': itemIds};
}
class ReceiptImportReview {
  ReceiptImportReview({required this.mode, required this.merchant, required this.totalTitle, required this.currency, required this.date, required this.totalBasis, required this.printedTotalMinor, required this.finalPaidMinor, required this.totalMinor, required this.tipDecision, required this.actualTipMinor, required List<ReceiptReviewItem> items, required List<ReceiptReviewAdjustment> adjustments, required List<String> acknowledgedWarnings, required this.confirmed}) : items = List.unmodifiable(items), adjustments = List.unmodifiable(adjustments), acknowledgedWarnings = List.unmodifiable(acknowledgedWarnings);
  final String mode;
  final String merchant;
  final String totalTitle;
  final String currency;
  final String date;
  final String totalBasis;
  final BigInt? printedTotalMinor;
  final BigInt? finalPaidMinor;
  final BigInt totalMinor;
  final String tipDecision;
  final BigInt? actualTipMinor;
  final List<ReceiptReviewItem> items;
  final List<ReceiptReviewAdjustment> adjustments;
  final List<String> acknowledgedWarnings;
  final bool confirmed;
  factory ReceiptImportReview.fromJson(Object? value) {
    _boundedJson(value);
    final j = _fields(value, 'mode merchant totalTitle currency date totalBasis printedTotalMinor finalPaidMinor totalMinor tipDecision actualTipMinor items adjustments acknowledgedWarnings confirmed');
    final currency = _text(j['currency'], 3);
    if (!RegExp(r'^[A-Z]{3}$').hasMatch(currency)) throw const FormatException('Invalid currency code');
    return ReceiptImportReview(mode: _enum(j['mode'], 'itemized total'), merchant: _text(j['merchant'], 512), totalTitle: _text(j['totalTitle'], 512), currency: currency, date: _text(j['date'], 10), totalBasis: _enum(j['totalBasis'], 'printed_total final_paid user_entered'), printedTotalMinor: _nullableMinor(j['printedTotalMinor']), finalPaidMinor: _nullableMinor(j['finalPaidMinor']), totalMinor: parseMinorAmount(j['totalMinor']), tipDecision: _enum(j['tipDecision'], 'confirmed none bill_only'), actualTipMinor: _nullableMinor(j['actualTipMinor']), items: _list(j['items'], ReceiptReviewItem.fromJson), adjustments: _list(j['adjustments'], ReceiptReviewAdjustment.fromJson), acknowledgedWarnings: _list(j['acknowledgedWarnings'], (v) => _text(v, 640), 1200), confirmed: _bool(j['confirmed']));
  }
  Map<String, Object?> toJson() => {'mode': mode, 'merchant': merchant, 'totalTitle': totalTitle, 'currency': currency, 'date': date, 'totalBasis': totalBasis, 'printedTotalMinor': printedTotalMinor?.toString(), 'finalPaidMinor': finalPaidMinor?.toString(), 'totalMinor': totalMinor.toString(), 'tipDecision': tipDecision, 'actualTipMinor': actualTipMinor?.toString(), 'items': items.map((v) => v.toJson()).toList(), 'adjustments': adjustments.map((v) => v.toJson()).toList(), 'acknowledgedWarnings': acknowledgedWarnings, 'confirmed': confirmed};
}
class ReceiptImageInput {
  const ReceiptImageInput({this.mediaType = 'image/jpeg', required this.dataBase64});
  final String mediaType;
  final String dataBase64;
  Uint8List get bytes => base64Decode(dataBase64);
  factory ReceiptImageInput.fromJson(Object? value) {
    final j = _fields(value, 'mediaType dataBase64');
    final result = ReceiptImageInput(mediaType: _text(j['mediaType'], 64), dataBase64: _text(j['dataBase64'], ((maxReceiptImageBytes + 2) ~/ 3) * 4));
    if (result.mediaType != 'image/jpeg') throw const FormatException('Invalid receipt image media type');
    final byteCount = result.bytes.length;
    if (byteCount == 0 || byteCount > maxReceiptImageBytes) throw const FormatException('Invalid receipt image');
    return result;
  }
  Map<String, Object?> toJson() => {'mediaType': mediaType, 'dataBase64': dataBase64};
}
class ReceiptSourceInput {
  const ReceiptSourceInput({required this.image, this.extractionId, this.evidence, this.review});
  final ReceiptImageInput image;
  final String? extractionId;
  final ReceiptEvidence? evidence;
  final ReceiptImportReview? review;
  factory ReceiptSourceInput.fromJson(Object? value) {
    final j = _fields(value, 'image extractionId evidence review');
    final supplied = [j['extractionId'], j['evidence'], j['review']].where((v) => v != null).length;
    if (supplied != 0 && supplied != 3) throw const FormatException('Model source requires extraction, evidence and review together');
    return ReceiptSourceInput(image: ReceiptImageInput.fromJson(j['image']), extractionId: j['extractionId'] == null ? null : _uuid(j['extractionId']), evidence: j['evidence'] == null ? null : ReceiptEvidence.fromJson(j['evidence']), review: j['review'] == null ? null : ReceiptImportReview.fromJson(j['review']));
  }
  Map<String, Object?> toJson() => {'image': image.toJson(), 'extractionId': extractionId, 'evidence': evidence?.toJson(), 'review': review?.toJson()};
}
class ReceiptSourceMeta {
  const ReceiptSourceMeta({required this.mediaType, required this.width, required this.height, required this.byteCount, required this.sha256, required this.hasReview});
  final String mediaType;
  final int width;
  final int height;
  final int byteCount;
  final String sha256;
  final bool hasReview;
  factory ReceiptSourceMeta.fromJson(Object? value) {
    final j = _fields(value, 'mediaType width height byteCount sha256 hasReview');
    final result = ReceiptSourceMeta(mediaType: _text(j['mediaType'], 64), width: _int(j['width']), height: _int(j['height']), byteCount: _int(j['byteCount']), sha256: _text(j['sha256'], 64), hasReview: _bool(j['hasReview']));
    if (result.mediaType != 'image/jpeg' || result.width <= 0 || result.height <= 0 || result.width > 8192 || result.height > 8192 || result.width * result.height > 16000000 || result.byteCount <= 0 || result.byteCount > maxReceiptImageBytes || !RegExp(r'^[0-9a-f]{64}$').hasMatch(result.sha256)) throw const FormatException('Invalid receipt source metadata');
    return result;
  }
  Map<String, Object?> toJson() => {'mediaType': mediaType, 'width': width, 'height': height, 'byteCount': byteCount, 'sha256': sha256, 'hasReview': hasReview};
}
class ReceiptReviewIssue {
  const ReceiptReviewIssue({required this.code, required this.field, required this.message, required this.severity});
  final String code;
  final String field;
  final String message;
  final String severity;
  String get warningToken => code.startsWith('model_issue_') ? code : '$code:$field';
  factory ReceiptReviewIssue.fromJson(Object? value) {
    final j = _fields(value, 'code field message severity');
    return ReceiptReviewIssue(code: _text(j['code'], 64), field: _text(j['field'], 512), message: _text(j['message']), severity: _enum(j['severity'], 'warning error'));
  }
  Map<String, Object?> toJson() => {'code': code, 'field': field, 'message': message, 'severity': severity};
}
class ReceiptExtractionOptions {
  ReceiptExtractionOptions({required this.available, required this.model, required this.reasoningEffort, required this.maxImageBytes, required List<String> acceptedMediaTypes}) : acceptedMediaTypes = List.unmodifiable(acceptedMediaTypes);
  final bool available;
  final String model;
  final String reasoningEffort;
  final int maxImageBytes;
  final List<String> acceptedMediaTypes;
  factory ReceiptExtractionOptions.fromJson(Object? value) {
    final j = _fields(value, 'available model reasoningEffort maxImageBytes acceptedMediaTypes');
    final result = ReceiptExtractionOptions(available: _bool(j['available']), model: _text(j['model'], 128), reasoningEffort: _text(j['reasoningEffort'], 32), maxImageBytes: _int(j['maxImageBytes']), acceptedMediaTypes: _list(j['acceptedMediaTypes'], _text));
    if (result.maxImageBytes < 0 || result.maxImageBytes > maxReceiptImageBytes || result.acceptedMediaTypes.length != 1 || result.acceptedMediaTypes.single != 'image/jpeg') throw const FormatException('Invalid receipt extraction upload limits');
    return result;
  }
  Map<String, Object?> toJson() => {'available': available, 'model': model, 'reasoningEffort': reasoningEffort, 'maxImageBytes': maxImageBytes, 'acceptedMediaTypes': acceptedMediaTypes};
}
class ReceiptExtractionResponse {
  ReceiptExtractionResponse({required this.extractionId, required this.model, required this.promptVersion, required this.title, required this.evidence, required List<ReceiptReviewIssue> issues}) : issues = List.unmodifiable(issues);
  final String extractionId;
  final String model;
  final String promptVersion;
  final String? title;
  final ReceiptEvidence evidence;
  final List<ReceiptReviewIssue> issues;
  factory ReceiptExtractionResponse.fromJson(Object? value) {
    final j = _fields(value, 'extractionId model promptVersion title evidence issues');
    final title = j['title'];
    if (title != null && title is! String) throw const FormatException('Invalid receipt title');
    return ReceiptExtractionResponse(extractionId: _uuid(j['extractionId']), model: _text(j['model'], 128), promptVersion: _text(j['promptVersion'], 64), title: title == null ? null : normalizeReceiptTitle(title as String), evidence: ReceiptEvidence.fromJson(j['evidence']), issues: _list(j['issues'], ReceiptReviewIssue.fromJson, 400));
  }
  Map<String, Object?> toJson() => {'extractionId': extractionId, 'model': model, 'promptVersion': promptVersion, 'title': title, 'evidence': evidence.toJson(), 'issues': issues.map((issue) => issue.toJson()).toList()};
}
sealed class ReceiptSourceChange {
  const ReceiptSourceChange();
  const factory ReceiptSourceChange.keep() = KeepReceiptSource;
  const factory ReceiptSourceChange.remove() = RemoveReceiptSource;
  const factory ReceiptSourceChange.replace(ReceiptSourceInput source) = ReplaceReceiptSource;
}
class KeepReceiptSource extends ReceiptSourceChange { const KeepReceiptSource(); }
class RemoveReceiptSource extends ReceiptSourceChange { const RemoveReceiptSource(); }
class ReplaceReceiptSource extends ReceiptSourceChange {
  const ReplaceReceiptSource(this.source);
  final ReceiptSourceInput source;
}
