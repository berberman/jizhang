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
  test('balance roles and record context are required exact values without record bounds', () {
    const large = '18446744073709551614';
    final line = <String, Object?>{'recordId': 'record-id', 'title': 'Saved amount',
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
    for (final field in ['date', 'recordType', 'recordAmountMinor']) {
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
