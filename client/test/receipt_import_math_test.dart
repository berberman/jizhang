import 'dart:convert';

import 'package:flutter_test/flutter_test.dart';
import 'package:jizhang_client/core/models.dart';
import 'package:jizhang_client/core/money.dart';
import 'package:jizhang_client/core/receipt_ai_models.dart';
import 'package:jizhang_client/core/receipt_import_math.dart';

const catalog = [CurrencyInfo(code: 'USD', name: 'Dollar', minorUnits: 2), CurrencyInfo(code: 'JPY', name: 'Yen', minorUnits: 0), CurrencyInfo(code: 'KWD', name: 'Dinar', minorUnits: 3)];
Map<String, Object?> item(String id, Object amount, {String? quantity, String? unitPrice}) => {'id': id, 'title': id, 'amountMinor': '$amount', 'quantity': quantity, 'unit': null, 'unitPrice': unitPrice};
Map<String, Object?> adjustment(String id, String kind, Object amount, String treatment, List<String> ids) => {'id': id, 'kind': kind, 'amountMinor': '$amount', 'treatment': treatment, 'itemIds': ids};
Map<String, Object?> reviewJson({Object total = 2970, List<Map<String, Object?>>? items, List<Map<String, Object?>>? adjustments, Map<String, Object?> overrides = const {}}) => {
  'mode': 'itemized', 'merchant': 'Synthetic merchant', 'totalTitle': 'Receipt expense', 'currency': 'USD', 'date': '2026-10-21', 'totalBasis': 'printed_total', 'printedTotalMinor': '$total', 'finalPaidMinor': null, 'totalMinor': '$total', 'tipDecision': 'none', 'actualTipMinor': '0', 'items': items ?? [item('a', 1000), item('b', 2000)], 'adjustments': adjustments ?? [adjustment('discount', 'discount', 300, 'subtract', ['a', 'b']), adjustment('tax', 'tax', 270, 'add', ['a', 'b'])], 'acknowledgedWarnings': <String>[], 'confirmed': true, ...overrides,
};
ReceiptImportReview review({Object total = 2970, List<Map<String, Object?>>? items, List<Map<String, Object?>>? adjustments, Map<String, Object?> overrides = const {}}) => ReceiptImportReview.fromJson(reviewJson(total: total, items: items, adjustments: adjustments, overrides: overrides));
List<String> amounts(ReceiptImportReview value) => computeReceiptImportAmounts(catalog, value).map((row) => '${row.itemId}:${row.amountMinor}').toList();
Map<String, Object?> observation(String? value) => {'value': value, 'raw': value, 'status': value == null ? 'absent' : 'visible'};
Map<String, Object?> evidenceJson() => {
  'document_kind': 'retail_receipt', 'merchant': observation('Synthetic 商店'), 'transaction_date': observation('2026-10-21'), 'currency': {'code': 'USD', 'symbol': r'$', 'basis': 'explicit', 'evidence': 'USD printed'},
  'items': [{'id': 'a', 'description': 'Synthetic item', 'raw_line': 'Synthetic item 29.70', 'quantity': observation(null), 'unit': observation(null), 'unit_price': observation(null), 'line_total': observation('29.70'), 'price_basis': 'unknown', 'modifiers': <String>[], 'tax_markers': <String>[]}],
  'adjustments': <Object?>[], 'totals': {'subtotal': observation(null), 'tax_total': observation(null), 'printed_total': observation('29.70'), 'final_paid': observation(null)}, 'suggested_tips': <Object?>[], 'issues': <Object?>[],
};
void main() {
  test('discount then proportional tax conserves 29.70', () { expect(amounts(review()), ['a:990', 'b:1980']); });
  test('already-net lines do not subtract an included discount twice', () {
    expect(amounts(review(items: [item('a', 900), item('b', 1800)], adjustments: [adjustment('discount', 'discount', 300, 'included_in_line_total', []), adjustment('tax', 'tax', 270, 'add', ['a', 'b'])])), ['a:990', 'b:1980']);
  });
  test('remainder ties follow stable IDs while output follows display order', () {
    final charges = [adjustment('tax', 'tax', 1, 'add', ['a', 'b'])];
    expect(amounts(review(total: 3, items: [item('b', 1), item('a', 1)], adjustments: charges)), ['b:1', 'a:2']);
    expect(amounts(review(total: 3, items: [item('a', 1), item('b', 1)], adjustments: charges)), ['a:2', 'b:1']);
  });
  test('selected eligibility and frozen added-fee bases', () {
    expect(amounts(review(adjustments: [adjustment('discount', 'discount', 300, 'subtract', ['a', 'b']), adjustment('tax', 'tax', 270, 'add', ['b'])])), ['a:900', 'b:2070']);
    expect(amounts(review(total: 3300, adjustments: [adjustment('fee', 'other', 150, 'add', ['a']), adjustment('tax', 'tax', 150, 'add', ['a', 'b'])])), ['a:1200', 'b:2100']);
  });
  test('discounts run sequentially against remaining bases', () {
    expect(amounts(review(total: 1500, adjustments: [adjustment('first', 'discount', 1000, 'subtract', ['b']), adjustment('second', 'discount', 500, 'subtract', ['a', 'b'])])), ['a:750', 'b:750']);
  });
  test('included and informational adjustments contribute zero', () {
    expect(amounts(review(total: 3000, adjustments: [adjustment('tax', 'tax', 300, 'included_in_line_total', []), adjustment('summary', 'tax', 300, 'informational', [])])), ['a:1000', 'b:2000']);
  });
  test('zero rows stay in review and do not create expenses', () {
    final value = review(total: 100, items: [item('free', 0), item('paid', 100)], adjustments: []);
    expect(amounts(value), ['paid:100']); expect(value.items.length, 2);
  });
  test('invalid adjustments and mismatched totals cannot be acknowledged away', () {
    for (final value in [
      review(total: 1, items: [item('a', 0)], adjustments: [adjustment('tax', 'tax', 1, 'add', ['a'])]),
      review(adjustments: [adjustment('discount', 'discount', 3001, 'subtract', ['a', 'b'])]),
      review(adjustments: [adjustment('discount', 'discount', 1, 'subtract', [])]),
      review(adjustments: [adjustment('tax', 'tax', 0, 'add', ['a', 'a'])]),
      review(adjustments: [adjustment('tax', 'tax', 0, 'informational', ['missing'])]),
      review(total: 2971, overrides: {'acknowledgedWarnings': ['total_mismatch:total']}),
      review(total: 3300, adjustments: [adjustment('discount', 'discount', 300, 'add', ['a', 'b'])]),
    ]) { expect(() => amounts(value), throwsFormatException); }
  });
  test('record limits apply per generated row, not the aggregate', () {
    expect(() => amounts(review(total: maxRecordMinor + BigInt.one, items: [item('a', maxRecordMinor + BigInt.one)], adjustments: [])), throwsFormatException);
    expect(amounts(review(total: maxRecordMinor * BigInt.two, items: [item('a', maxRecordMinor), item('b', maxRecordMinor)], adjustments: [])), ['a:$maxRecordMinor', 'b:$maxRecordMinor']);
  });
  test('JPY and fuel decimal precision remain exact', () {
    expect(amounts(review(overrides: {'currency': 'JPY'})), ['a:990', 'b:1980']);
    expect(amounts(review(total: 700, items: [item('fuel', 700, quantity: '2', unitPrice: '3.499')], adjustments: [])), ['fuel:700']);
    expect(amounts(review(total: 1000, items: [item('a', 1000, quantity: '2', unitPrice: '5.00')], adjustments: [])), ['a:1000']);
    final value = review(total: 1000, items: [item('a', 1000, quantity: '2', unitPrice: '4.00')], adjustments: []);
    expect(() => amounts(value), throwsFormatException);
    expect(amounts(ReceiptImportReview.fromJson({...value.toJson(), 'acknowledgedWarnings': ['quantity_price_mismatch:items.a']})), ['a:1000']);
    expect(() => amounts(review(total: 1000, items: [item('a', 1000, quantity: 'unreadable')], adjustments: [])), throwsFormatException);
    expect(amounts(review(total: 1000, items: [item('a', 1000)], adjustments: [])), ['a:1000']);
    expect(() => parseReceiptDecimal('1.0000000000000000001'), throwsFormatException);
  });
  test('known total-mode tip is included once and unknown tip requires bill-only confirmation', () {
    final total = review(items: [], adjustments: [], overrides: {'mode': 'total', 'tipDecision': 'confirmed', 'actualTipMinor': '500'});
    expect(amounts(total), ['total:2970']);
    final bill = {...total.toJson(), 'tipDecision': 'bill_only', 'actualTipMinor': null};
    expect(() => amounts(ReceiptImportReview.fromJson(bill)), throwsFormatException);
    expect(amounts(ReceiptImportReview.fromJson({...bill, 'acknowledgedWarnings': ['bill_only:tipDecision']})), ['total:2970']);
    expect(() => amounts(ReceiptImportReview.fromJson({...bill, 'finalPaidMinor': '2970', 'acknowledgedWarnings': ['bill_only:tipDecision']})), throwsFormatException);
    expect(() => amounts(review(items: [], adjustments: [], overrides: {'mode': 'total', 'actualTipMinor': null})), throwsFormatException);
    expect(amounts(review(total: 3300, adjustments: [adjustment('tip', 'actual_tip', 300, 'add', ['a', 'b'])], overrides: {'tipDecision': 'confirmed', 'actualTipMinor': '300'})), ['a:1100', 'b:2200']);
  });
  test('shared required data and total basis remain explicit', () {
    for (final override in [<String, Object?>{'currency': 'ZZZ'}, {'date': ''}, {'date': '2026-02-30'}, {'confirmed': false}, {'printedTotalMinor': null}, {'totalBasis': 'final_paid'}, {'totalTitle': ' '}]) {
      expect(() => amounts(review(items: [], adjustments: [], overrides: {'mode': 'total', ...override})), throwsFormatException);
    }
    expect(amounts(review(items: [], adjustments: [], overrides: {'mode': 'total', 'totalBasis': 'user_entered', 'printedTotalMinor': null})), ['total:2970']);
    expect(() => amounts(review(overrides: {'mode': 'total'})), throwsFormatException);
    expect(validReceiptDate('2024-02-29'), isTrue);
    for (final date in ['', '24-02-29', '2026-1-02', '2026-10-21T00:00:00Z']) { expect(validReceiptDate(date), isFalse); }
  });
  test('evidence preserves invalid observations but corrected total mode is usable', () {
    final json = evidenceJson();
    ((json['items'] as List).single as Map)['line_total'] = observation('1O.00');
    final original = ReceiptEvidence.fromJson(json);
    expect(validateReceiptEvidence(catalog, original).any((v) => v.field == 'items.a.line_total' && v.severity == 'error'), isTrue);
    expect(validateReceiptImportReview(catalog, original, review(items: [], adjustments: [], overrides: {'mode': 'total'})).single.amountMinor, BigInt.from(2970));
    expect(original.items.single.lineTotal.value, '1O.00');
    expect(() => original.items.clear(), throwsUnsupportedError);
    expect(ReceiptEvidence.fromJson(jsonDecode(jsonEncode(original.toJson()))).merchant.value, 'Synthetic 商店');
  });
  test('warning tokens distinguish original model issues and current numeric warnings', () {
    final json = evidenceJson()..['issues'] = [{'field': 'items.a', 'reason': '<b>check</b>'}, {'field': 'merchant', 'reason': 'check'}];
    final original = ReceiptEvidence.fromJson(json);
    final total = review(items: [], adjustments: [], overrides: {'mode': 'total', 'acknowledgedWarnings': ['model_issue_1']});
    expect(validateReceiptImportReview(catalog, original, total).single.itemId, 'total');
    expect(() => validateReceiptImportReview(catalog, original, review()), throwsFormatException);
    expect(validateReceiptEvidence(catalog, original).last.warningToken, 'model_issue_1');
  });
  test('strict models reject missing unknown malformed and oversized fields', () {
    final original = evidenceJson();
    expect(() => ReceiptEvidence.fromJson({...original, 'extra': true}), throwsFormatException);
    expect(() => ReceiptEvidence.fromJson({...original}..remove('merchant')), throwsFormatException);
    expect(() => ReceiptObservation.fromJson({'value': null, 'raw': null, 'status': 'visible'}), throwsFormatException);
    expect(() => ReceiptObservation.fromJson({'value': 'x', 'raw': 'x', 'status': 'ambiguous'}), throwsFormatException);
    expect(() => ReceiptImportReview.fromJson({...review().toJson(), 'totalMinor': '01'}), throwsFormatException);
    expect(() => ReceiptImportReview.fromJson({...review().toJson(), 'totalMinor': 2970}), throwsFormatException);
    expect(() => ReceiptEvidence.fromJson({...original, 'items': List.filled(201, (original['items'] as List).single)}), throwsFormatException);
    expect(() => ReceiptEvidence.fromJson({...original, 'merchant': observation('bad\u0000text')}), throwsFormatException);
    expect(() => ReceiptEvidence.fromJson({...original, 'merchant': observation(List.filled(513, 'x').join())}), throwsFormatException);
  });
  test('source keep remove replace serialize distinctly and manual drafts omit source', () {
    final source = ReceiptSourceInput(image: ReceiptImageInput(dataBase64: base64Encode([255, 216, 255, 217])));
    expect(ReceiptSourceInput.fromJson(source.toJson()).image.bytes, [255, 216, 255, 217]);
    expect(ReceiptDraft(title: 'Synthetic Market groceries', note: '', records: []).toJson().containsKey('source'), isFalse);
    expect(ReceiptDraft(title: 'Synthetic Market groceries', note: '', records: [], sourceChange: const ReceiptSourceChange.remove()).toJson()['source'], isNull);
    final replacement = ReceiptDraft(title: 'Synthetic Market groceries', note: '', records: [], sourceChange: ReceiptSourceChange.replace(source));
    expect(ReceiptDraft.fromJson(replacement.toJson()).sourceChange, isA<ReplaceReceiptSource>());
    expect(ReceiptDraft.fromJson({'title': 'Synthetic Market groceries', 'note': '', 'records': <Object?>[]}).sourceChange, isA<KeepReceiptSource>());
    expect(ReceiptDraft.fromJson({'title': 'Synthetic Market groceries', 'note': '', 'records': <Object?>[], 'source': null}).sourceChange, isA<RemoveReceiptSource>());
    expect(() => ReceiptSourceInput.fromJson({...source.toJson(), 'extractionId': '00000000-0000-0000-0000-000000000000'}), throwsFormatException);
  });
  test('retained source metadata and extraction limits are bounded', () {
    final metadata = {'mediaType': 'image/jpeg', 'width': 100, 'height': 200, 'byteCount': 1000, 'sha256': List.filled(64, 'a').join(), 'hasReview': true};
    expect(ReceiptSourceMeta.fromJson(metadata).width, 100);
    expect(() => ReceiptSourceMeta.fromJson({...metadata, 'width': 8193}), throwsFormatException);
    expect(() => ReceiptSourceMeta.fromJson({...metadata, 'width': 8192, 'height': 8192}), throwsFormatException);
    expect(() => ReceiptSourceMeta.fromJson({...metadata, 'byteCount': maxReceiptImageBytes + 1}), throwsFormatException);
    expect(() => ReceiptSourceMeta.fromJson({...metadata, 'hasReview': 1}), throwsFormatException);
    final options = {'available': false, 'model': 'gpt-5.4-mini-2026-03-17', 'reasoningEffort': 'low', 'maxImageBytes': 0, 'acceptedMediaTypes': ['image/jpeg']};
    expect(ReceiptExtractionOptions.fromJson(options).maxImageBytes, 0);
    expect(() => ReceiptExtractionOptions.fromJson({...options, 'maxImageBytes': -1}), throwsFormatException);
    expect(() => ReceiptExtractionOptions.fromJson({...options, 'acceptedMediaTypes': ['image/png']}), throwsFormatException);
  });
  test('display diagnostics cap does not truncate save warning checks', () {
    final original = ReceiptEvidence.fromJson({...evidenceJson(), 'currency': {'code': 'USD', 'symbol': null, 'basis': 'contextual', 'evidence': null}, 'issues': [for (var i = 0; i < 400; i++) {'field': 'merchant', 'reason': 'check $i'}]});
    expect(validateReceiptEvidence(catalog, original).length, 400);
    final value = review(items: [], adjustments: [], overrides: {'mode': 'total', 'acknowledgedWarnings': [for (var i = 0; i < 399; i++) 'model_issue_$i']});
    expect(() => validateReceiptImportReview(catalog, original, value), throwsFormatException);
    final confirmed = ReceiptImportReview.fromJson({...value.toJson(), 'acknowledgedWarnings': [for (var i = 0; i < 400; i++) 'model_issue_$i']});
    expect(validateReceiptImportReview(catalog, original, confirmed).single.itemId, 'total');
  });
}
