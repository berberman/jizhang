import 'package:flutter_test/flutter_test.dart';
import 'package:jizhang_client/core/models.dart';
import 'package:jizhang_client/shared/spending_charts.dart';

void main() {
  test('statistics range uses inclusive valid calendar days', () {
    expect(const StatisticsQuery(from: '2024-02-29', to: '2024-02-29').error, isNull);
    expect(const StatisticsQuery(from: '2023-02-29', to: '2023-03-01').error, isNotNull);
    expect(const StatisticsQuery(from: '2024-03-01', to: '2024-02-29').error, isNotNull);
    final start = DateTime.utc(2020, 1, 1);
    String day(DateTime d) => d.toIso8601String().substring(0, 10);
    expect(StatisticsQuery(from: day(start), to: day(start.add(const Duration(days: 3659)))).error, isNull);
    expect(StatisticsQuery(from: day(start), to: day(start.add(const Duration(days: 3660)))).error, isNotNull);
    expect(const StatisticsQuery(from: '2026-01-01', to: '2026-01-31', userId: 'not-a-uuid').error, isNotNull);
  });

  test('statistics preserves unsigned aggregates beyond Int64 and rejects numeric money', () {
    final value = <String, Object?>{
      'currency': {'code': 'USD', 'name': 'US Dollar', 'minorUnits': 2},
      'totalMinor': '18446744073709551614', 'expenseCount': 2,
      'incomeMinor': '0', 'incomeCount': 0, 'netMinor': '-18446744073709551614',
      'paidMinor': '9007199254740993', 'shareMinor': '0',
      'otherExpensesMinor': '0', 'buckets': [], 'members': [], 'topExpenses': [],
    };
    final parsed = CurrencySpending.fromJson(value);
    expect(parsed.totalMinor, BigInt.parse('18446744073709551614'));
    expect(parsed.paidMinor, BigInt.parse('9007199254740993'));
    for (final invalid in [1, '-1', '01', '+1', '1.0']) {
      expect(() => CurrencySpending.fromJson({...value, 'totalMinor': invalid}), throwsFormatException);
    }
  });

  test('statistics net stays signed and exact for currencies and buckets', () {
    final bucket = <String, Object?>{
      'start': '2026-10-01', 'end': '2026-10-31',
      'totalMinor': '9007199254740994', 'expenseCount': 1,
      'incomeMinor': '1', 'incomeCount': 1, 'netMinor': '-9007199254740993',
      'paidMinor': '0', 'shareMinor': '0',
    };
    final value = <String, Object?>{
      'currency': {'code': 'USD', 'name': 'US Dollar', 'minorUnits': 2},
      'totalMinor': bucket['totalMinor'], 'expenseCount': 1,
      'incomeMinor': '1', 'incomeCount': 1, 'netMinor': bucket['netMinor'],
      'paidMinor': '0', 'shareMinor': '0', 'otherExpensesMinor': '0',
      'buckets': [bucket], 'members': [], 'topExpenses': [],
    };
    final parsed = CurrencySpending.fromJson(value);
    expect(parsed.netMinor, BigInt.parse('-9007199254740993'));
    expect(parsed.buckets.single.netMinor, parsed.netMinor);
    expect(parsed.incomeMinor, BigInt.one);
    expect(parsed.incomeCount, 1);
    expect(parsed.buckets.single.incomeMinor, BigInt.one);
    expect(parsed.buckets.single.incomeCount, 1);
    expect(parsed.currency.format(parsed.netMinor), 'USD -90071992547409.93');
    for (final invalid in <Object>[1, '-0', '-01', '+1', '1.0']) {
      expect(() => CurrencySpending.fromJson({...value, 'netMinor': invalid}), throwsFormatException);
      expect(() => SpendingBucket.fromJson({...bucket, 'netMinor': invalid}), throwsFormatException);
    }
    for (final field in ['incomeMinor', 'incomeCount', 'netMinor']) {
      final missingCurrency = {...value}..remove(field);
      final missingBucket = {...bucket}..remove(field);
      expect(() => CurrencySpending.fromJson(missingCurrency), throwsFormatException);
      expect(() => SpendingBucket.fromJson(missingBucket), throwsFormatException);
    }
    expect(() => CurrencySpending.fromJson({...value, 'incomeMinor': '-1'}), throwsFormatException);
    expect(() => SpendingBucket.fromJson({...bucket, 'incomeMinor': '-1'}), throwsFormatException);
  });

  test('chart ratios remain finite for zero and arbitrary precision totals', () {
    expect(spendingRatio(BigInt.zero, BigInt.zero), 0);
    final total = BigInt.from(10).pow(400);
    expect(spendingRatio(total ~/ BigInt.two, total), 0.5);
    expect(spendingRatio(total, total), 1);
  });
}
