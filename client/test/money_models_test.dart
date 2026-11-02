import 'dart:async';
import 'dart:convert';

import 'package:flutter_test/flutter_test.dart';
import 'package:http/http.dart' as http;
import 'package:http/testing.dart';
import 'package:jizhang_client/core/api_client.dart';
import 'package:jizhang_client/core/jizhang_repository.dart';
import 'package:jizhang_client/core/models.dart';

const _userId = '00000000-0000-0000-0000-000000000001';
const _currency = {'code': 'USD', 'name': 'US Dollar', 'minorUnits': 2};
const _user = {'userId': _userId, 'username': 'alice', 'displayName': 'Alice'};

Map<String, Object?> expenseJson() => {
  'tag': 'ExpenseRecord',
  'recordId': '00000000-0000-0000-0000-000000000002',
  'groupId': '00000000-0000-0000-0000-000000000003',
  'receipt': null,
  'title': 'Large exact expense',
  'note': null,
  'amountMinor': '9007199254740993',
  'currency': _currency,
  'settlementAmountMinor': '9007199254740993',
  'settlementCurrency': _currency,
  'exchangeRate': null,
  'paidBy': _user,
  'date': '2026-10-16',
  'createdAt': '2026-10-16T10:00:00Z',
  'splitMode': 'exact',
  'splits': [
    {'user': _user, 'value': '9007199254740993', 'amountMinor': '9007199254740993', 'settlementAmountMinor': '9007199254740993'},
  ],
};

Map<String, Object?> incomeJson() => expenseJson()
  ..['tag'] = 'IncomeRecord'
  ..['title'] = 'Large exact income'
  ..remove('paidBy')
  ..['receivedBy'] = _user;

JizhangRepository repository(MockClient client) => JizhangRepository(ApiClient(
  baseUri: Uri.parse('https://example.test'), httpClient: client, accessToken: () => null,
));

void main() {
  test('receipt titles are required validated text in responses and drafts', () {
    const title = '会议 <b>Market</b>';
    final receipt = <String, Object?>{'receiptId': 'receipt-id', 'groupId': 'group-id',
      'uploadedBy': _user, 'title': title, 'note': 'Separate note', 'records': <Object?>[],
      'createdAt': '2026-10-21T00:00:00Z', 'source': null};
    final draft = ReceiptDraft(title: title, note: 'Separate note', records: []).toJson();
    expect(Receipt.fromJson(receipt).title, title);
    expect(Receipt.fromJson(receipt).note, 'Separate note');
    expect(ReceiptDraft.fromJson(draft).title, title);
    expect(ReceiptDraft.fromJson(draft).toJson(), draft);
    final emoji = List.filled(512, '😀').join();
    expect(Receipt.fromJson({...receipt, 'title': ' $emoji '}).title, emoji);
    expect(ReceiptDraft.fromJson({...draft, 'title': ' $emoji '}).title, emoji);
    expect(draft['title'], title);
    for (final json in [receipt, draft]) {
      final Object Function(Object?) decode = json.containsKey('receiptId') ? Receipt.fromJson : ReceiptDraft.fromJson;
      expect(() => decode({...json}..remove('title')), throwsFormatException);
      for (final invalid in <Object?>[null, 1, false, '', '   ', 'bad\ntext', List.filled(513, '😀').join()]) {
        expect(() => decode({...json, 'title': invalid}), throwsFormatException);
      }
      expect(() => decode({...json, 'unknown': true}), throwsFormatException);
    }
    expect(() => ReceiptDraft(title: '', note: '', records: []), throwsFormatException);
  });

  test('receipt title normalization rejects controls before trimming and counts code points', () {
    const literal = '会议 <b>Market</b>  groceries';
    expect(normalizeReceiptTitle(' \u00a0\u1680$literal\u2000\u202f\u205f\u3000 '), literal);
    expect(normalizeReceiptTitle('\ufeff$literal\ufeff'), '\ufeff$literal\ufeff');
    final emoji = List.filled(512, '😀').join();
    expect(normalizeReceiptTitle(emoji), emoji);
    expect(ReceiptDraft(title: ' $emoji ', note: '', records: []).title, emoji);
    expect(normalizeReceiptTitle(List.filled(256, 'e\u0301').join()).runes.length, 512);
    expect(() => normalizeReceiptTitle('$emoji😀'), throwsA(isA<FormatException>().having(
      (error) => error.message, 'message', 'Receipt title must be at most 512 characters.')));
    expect(() => normalizeReceiptTitle(' \u00a0 '), throwsA(isA<FormatException>().having(
      (error) => error.message, 'message', 'Receipt title is required.')));
    for (final codePoint in [for (var value = 0; value <= 0x1f; value++) value,
        for (var value = 0x7f; value <= 0x9f; value++) value, 0x2028, 0x2029]) {
      for (final title in [String.fromCharCode(codePoint), '${String.fromCharCode(codePoint)}Market', 'Market${String.fromCharCode(codePoint)}']) {
        expect(() => normalizeReceiptTitle(title), throwsA(isA<FormatException>().having(
          (error) => error.message, 'message', 'Receipt title cannot contain control characters.')));
      }
    }
  });

  test('balance roles and record context are required exact values without record bounds', () {
    const large = '18446744073709551614';
    final line = <String, Object?>{'recordId': 'record-id', 'title': 'Saved amount',
      'receipt': null,
      'date': '2026-10-02', 'recordType': 'expense', 'recordAmountMinor': '9000', 'amountMinor': '-3000'};
    final value = <String, Object?>{'user': _user, 'totalMinor': large,
      'expensesPaidMinor': large, 'expenseShareMinor': '0', 'incomeReceivedMinor': '0',
      'incomeEntitlementMinor': '0', 'paymentsSentMinor': '0', 'paymentsReceivedMinor': '0',
      'breakdown': [line]};
    final parsed = Balance.fromJson(value);
    expect(parsed.expensesPaidMinor, BigInt.parse(large));
    expect(parsed.totalMinor, BigInt.parse(large));
    expect(parsed.breakdown.single.amountMinor, BigInt.from(-3000));
    expect(parsed.breakdown.single.recordAmountMinor, BigInt.from(9000));
    expect(parsed.breakdown.single.recordType, RecordKind.expense);
    for (final field in ['expensesPaidMinor', 'expenseShareMinor', 'incomeReceivedMinor', 'incomeEntitlementMinor', 'paymentsSentMinor', 'paymentsReceivedMinor']) {
      expect(() => Balance.fromJson({...value}..remove(field)), throwsFormatException);
      for (final invalid in [1, '-1', '01', '+1', '1.0', null]) {
        expect(() => Balance.fromJson({...value, field: invalid}), throwsFormatException);
      }
    }
    for (final field in ['receipt', 'date', 'recordType', 'recordAmountMinor']) {
      expect(() => BalanceBreakdown.fromJson({...line}..remove(field)), throwsFormatException);
    }
    for (final date in ['2026-02-29', '2026-13-01', '2026-1-01']) {
      expect(() => BalanceBreakdown.fromJson({...line, 'date': date}), throwsFormatException);
    }
    expect(() => BalanceBreakdown.fromJson({...line, 'recordType': 'payment'}), throwsFormatException);
    expect(() => BalanceBreakdown.fromJson({...line, 'recordAmountMinor': '-1'}), throwsFormatException);
    expect(() => BalanceBreakdown.fromJson({...line, 'recordAmountMinor': 9000}), throwsFormatException);
    expect(() => Balance.fromJson({...value, 'obsolete': '0'}), throwsFormatException);
  });
  test('receipt association is required nullable response-only metadata on every record kind', () {
    const summary = {'receiptId': 'receipt-id', 'title': '会议 <b>Market</b>'};
    final transfer = expenseJson()..['tag'] = 'TransferRecord'..['transferTo'] = _user
      ..remove('splitMode')..remove('splits');
    for (final json in [expenseJson(), incomeJson(), transfer]) {
      expect(LedgerRecord.fromJson(json).receipt, isNull);
      expect(() => LedgerRecord.fromJson({...json}..remove('receipt')), throwsFormatException);
      final record = LedgerRecord.fromJson({...json, 'receipt': summary});
      expect(record.receipt!.receiptId, 'receipt-id');
      expect(record.receipt!.title, summary['title']);
      final draft = record is SplitRecord ? SplitRecordDraft.fromRecord(record).toJson()
        : TransferDraft.fromRecord(record as TransferRecord).toJson();
      expect(draft.containsKey('receipt'), isFalse);
      for (final invalid in <Object>[false, [], {'receiptId': 'receipt-id'},
          {'receiptId': 'receipt-id', 'title': null}, {...summary, 'note': 'private'}]) {
        expect(() => LedgerRecord.fromJson({...json, 'receipt': invalid}), throwsFormatException);
      }
    }
    final line = {'recordId': 'record-id', 'receipt': summary, 'title': 'Apples',
      'date': '2026-10-21', 'recordType': 'expense', 'recordAmountMinor': '1000', 'amountMinor': '0'};
    expect(BalanceBreakdown.fromJson(line).receipt!.title, summary['title']);
    expect(BalanceBreakdown.fromJson({...line, 'receipt': null}).receipt, isNull);
  });

  test('record notes are required nullable text while draft notes may be omitted', () {
    const note = '  Invoice 42\n会议 <b>plain</b>  ';
    final transfer = expenseJson()
      ..['tag'] = 'TransferRecord'
      ..['transferTo'] = _user
      ..remove('splitMode')
      ..remove('splits');
    for (final json in [expenseJson(), incomeJson(), transfer]) {
      expect(LedgerRecord.fromJson(json).note, isNull);
      expect(() => LedgerRecord.fromJson({...json}..remove('note')), throwsFormatException);
      expect(() => LedgerRecord.fromJson({...json, 'note': 42}), throwsFormatException);
      final record = LedgerRecord.fromJson({...json, 'note': note});
      expect(record.note, note);
      if (record is SplitRecord) {
        final saved = SplitRecordDraft.fromRecord(record).toJson();
        final reopened = SplitRecordDraft.fromJson(saved, kind: record.kind);
        expect(reopened.note, note);
        expect(reopened.toJson()['note'], note);
        expect(SplitRecordDraft.fromJson({...saved}..remove('note'), kind: record.kind).note, isNull);
        expect(SplitRecordDraft.fromJson({...saved, 'note': null}, kind: record.kind).note, isNull);
        expect(() => SplitRecordDraft.fromJson({...saved, 'note': false}, kind: record.kind), throwsFormatException);
      } else {
        final saved = TransferDraft.fromRecord(record as TransferRecord).toJson();
        expect(TransferDraft.fromJson(saved).note, note);
        expect(TransferDraft.fromJson(saved).toJson()['note'], note);
        expect(TransferDraft.fromJson({...saved}..remove('note')).note, isNull);
        expect(TransferDraft.fromJson({...saved, 'note': null}).note, isNull);
        expect(() => TransferDraft.fromJson({...saved, 'note': false}), throwsFormatException);
      }
    }
  });

  test('saved expense can be edited without losing integers above JS precision', () {
    final record = ExpenseRecord.fromJson(jsonDecode(jsonEncode(expenseJson())));
    expect(record.currency.format(record.amountMinor), 'USD 90071992547409.93');
    final draft = SplitRecordDraft.fromRecord(record);
    final reopened = SplitRecordDraft.fromJson(jsonDecode(jsonEncode(draft.toJson())), kind: RecordKind.expense);
    expect(reopened.amountMinor, BigInt.parse('9007199254740993'));
    expect(reopened.splits.single.value, reopened.amountMinor);
    expect(reopened.splitMode, SplitMode.exact);
    expect(reopened.currency, 'USD');
    expect(reopened.toJson()['amountMinor'], '9007199254740993');
  });

  test('saved income can be edited without losing integers above JS precision', () {
    final record = LedgerRecord.fromJson(jsonDecode(jsonEncode(incomeJson()))) as IncomeRecord;
    expect(record.kind, RecordKind.income);
    expect(record.actor.userId, _userId);
    expect(record.receivedBy.userId, _userId);
    expect(record.currency.format(record.amountMinor), 'USD 90071992547409.93');
    final draft = SplitRecordDraft.fromRecord(record);
    final reopened = SplitRecordDraft.fromJson(jsonDecode(jsonEncode(draft.toJson())), kind: RecordKind.income);
    expect(reopened.kind, RecordKind.income);
    expect(reopened.actorUserId, _userId);
    expect(reopened.amountMinor, BigInt.parse('9007199254740993'));
    expect(reopened.splits.single.value, reopened.amountMinor);
    expect(reopened.splits.single.userId, _userId);
    expect(reopened.splitMode, SplitMode.exact);
    expect(reopened.currency, 'USD');
    expect(reopened.toJson()['amountMinor'], '9007199254740993');
    expect(reopened.toJson()['receivedByUserId'], _userId);
  });

  test('record tags and actor fields cannot be interchanged', () {
    expect(() => ExpenseRecord.fromJson(incomeJson()), throwsFormatException);
    expect(() => IncomeRecord.fromJson(expenseJson()), throwsFormatException);
    expect(() => LedgerRecord.fromJson({...incomeJson(), 'paidBy': _user}), throwsFormatException);
    expect(() => LedgerRecord.fromJson({...expenseJson(), 'receivedBy': _user}), throwsFormatException);
    final missingActor = incomeJson()..remove('receivedBy');
    expect(() => LedgerRecord.fromJson(missingActor), throwsFormatException);
    expect(() => LedgerRecord.fromJson({...incomeJson(), 'tag': 'UnknownRecord'}), throwsFormatException);
    final expense = LedgerRecord.fromJson(expenseJson());
    expect(expense.kind, RecordKind.expense);
    expect(expense.actor.userId, _userId);
  });

  test('split drafts enforce kind-specific actor keys and reject transfer kind', () {
    final value = SplitRecordDraft.fromRecord(IncomeRecord.fromJson(incomeJson())).toJson();
    final wrongActor = {...value}..remove('receivedByUserId');
    wrongActor['byUserId'] = _userId;
    expect(() => SplitRecordDraft.fromJson(wrongActor, kind: RecordKind.income), throwsFormatException);
    expect(() => SplitRecordDraft.fromJson({...value, 'byUserId': _userId}, kind: RecordKind.income), throwsFormatException);
    expect(() => SplitRecordDraft.fromJson(value, kind: RecordKind.expense), throwsFormatException);
    expect(() => SplitRecordDraft.fromJson(value, kind: RecordKind.transfer), throwsArgumentError);
  });

  test('money readers reject numeric and noncanonical money rather than round', () {
    for (final value in <Object>[9007199254740992, 1.5, '01', '-0', '+1', '1e3']) {
      expect(() => ExpenseRecord.fromJson({...expenseJson(), 'amountMinor': value}), throwsFormatException);
    }
    final obsolete = expenseJson()..['amount'] = 1;
    expect(() => ExpenseRecord.fromJson(obsolete), throwsFormatException);
    final numericSplit = expenseJson()..['splits'] = [
      {'user': _user, 'value': 1, 'amountMinor': '1', 'settlementAmountMinor': '1'},
    ];
    expect(() => ExpenseRecord.fromJson(numericSplit), throwsFormatException);
  });

  test('editing a reference expense preserves provenance without a new quote', () {
    final json = expenseJson()
      ..['currency'] = {'code': 'EUR', 'name': 'Euro', 'minorUnits': 2}
      ..['exchangeRate'] = {
        'source': 'reference', 'rate': '1.2', 'rateDate': '2026-10-15',
        'fetchedAt': '2026-10-16T10:00:00Z', 'quoteId': '00000000-0000-0000-0000-000000000004',
      };
    final draft = SplitRecordDraft.fromRecord(ExpenseRecord.fromJson(json));
    expect(draft.savedExchangeRate!.rateDate, '2026-10-15');
    expect(draft.savedExchangeRate!.rate, '1.2');
    expect(draft.toJson()['exchangeRate'], {
      'source': 'reference', 'quoteId': '00000000-0000-0000-0000-000000000004',
    });
    expect(draft.currency, 'EUR');
  });

  test('catalogue failure is retryable and successful catalogue is immutable and cached', () async {
    var requests = 0;
    final client = MockClient((_) async {
      requests++;
      if (requests == 1) return http.Response('Unavailable', 503);
      return http.Response(jsonEncode([_currency]), 200);
    });
    addTearDown(client.close);
    final repo = repository(client);
    await expectLater(repo.getCurrencies(), throwsA(isA<ApiException>()));
    final currencies = await repo.getCurrencies();
    expect(currencies.single.format(BigInt.from(1234)), 'USD 12.34');
    expect(() => currencies.clear(), throwsUnsupportedError);
    expect(identical(await repo.getCurrencies(), currencies), isTrue);
    expect(requests, 2);
  });

  test('invalid catalogue is not retained and concurrent consumers share loading', () async {
    var requests = 0;
    final response = Completer<http.Response>();
    final client = MockClient((_) async {
      requests++;
      if (requests == 1) return http.Response(jsonEncode([_currency, _currency]), 200);
      return response.future;
    });
    addTearDown(client.close);
    final repo = repository(client);
    await expectLater(repo.getCurrencies(), throwsA(isA<ApiException>()));
    final first = repo.getCurrencies();
    final second = repo.getCurrencies();
    response.complete(http.Response(jsonEncode([_currency]), 200));
    final values = await Future.wait([first, second]);
    expect(identical(values.first, values.last), isTrue);
    expect(values.first.single.code, 'USD');
    expect(requests, 2);
  });
}
