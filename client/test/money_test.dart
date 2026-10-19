import 'package:flutter_test/flutter_test.dart';
import 'package:jizhang_client/core/money.dart';

const a = '00000000-0000-0000-0000-000000000001';
const b = '00000000-0000-0000-0000-000000000002';
const c = '00000000-0000-0000-0000-000000000003';

List<MoneySplitValue> rows(List<int> values) => [
  for (var i = 0; i < values.length; i++)
    MoneySplitValue([a, b, c][i], BigInt.from(values[i])),
];

Map<String, BigInt> byUser(List<MoneySplitValue> values) => {
  for (final row in values) row.userId: row.value,
};

void main() {
  group('Money parsing and formatting', () {
    test('currency precision is respected without floating point', () {
      expect(parseMajorAmount(' 12.34 ', minorDigits: 2), BigInt.from(1234));
      expect(parseMajorAmount('123', minorDigits: 0), BigInt.from(123));
      expect(parseMajorAmount('1.234', minorDigits: 3), BigInt.from(1234));
      expect(parseMajorAmount('0001.2', minorDigits: 2), BigInt.from(120));
      expect(parseMajorAmount('0', minorDigits: 2, allowZero: true), BigInt.zero);
      expect(formatMoney(code: 'USD', minorDigits: 2, amountMinor: BigInt.from(1234)), 'USD 12.34');
      expect(formatMoney(code: 'JPY', minorDigits: 0, amountMinor: BigInt.from(123)), 'JPY 123');
      expect(formatMoney(code: 'KWD', minorDigits: 3, amountMinor: BigInt.from(1234)), 'KWD 1.234');
      expect(formatMoney(code: 'USD', minorDigits: 2, amountMinor: BigInt.from(-1)), 'USD -0.01');
    });

    test('ordinary nonnegative decimal input rejects lossy or ambiguous forms', () {
      for (final value in ['', '0', '-1', '+1', '.5', '1.', '1e2', 'NaN', 'Infinity', '1,000', '1.001']) {
        expect(() => parseMajorAmount(value, minorDigits: 2), throwsFormatException, reason: value);
      }
      expect(() => parseMajorAmount('1.0', minorDigits: 0), throwsFormatException);
      expect(() => parseMajorAmount('1.0000', minorDigits: 3), throwsFormatException);
      expect(() => parseMajorAmount('92233720368547758.08', minorDigits: 2), throwsFormatException);
      expect(parseMajorAmount('92233720368547758.07', minorDigits: 2), maxRecordMinor);
    });

    test('wire money stays exact beyond JavaScript safe integers and Int64 sums', () {
      final amount = parseMinorAmount('9007199254740993');
      expect(amount.toString(), '9007199254740993');
      expect(formatMajorAmount(amount, minorDigits: 2), '90071992547409.93');
      expect(parseMajorAmount('90071992547409.93', minorDigits: 2), amount);
      final aggregate = maxRecordMinor * BigInt.two;
      expect(parseMinorAmount(aggregate.toString()), aggregate);
      expect(parseMinorAmount('-123', allowNegative: true), BigInt.from(-123));
      for (final value in <Object?>[9007199254740993, null, '01', '+1', '-0', '1.0', ' 1', '1\n', '-1']) {
        expect(() => parseMinorAmount(value), throwsFormatException, reason: '$value');
      }
    });

    test('percentage input is converted exactly to basis points', () {
      expect(parsePercentage('33.33'), BigInt.from(3333));
      expect(parsePercentage('100'), BigInt.from(10000));
      expect(parsePercentage('0'), BigInt.zero);
      for (final value in ['100.01', '33.333', '-1', '1e1']) {
        expect(() => parsePercentage(value), throwsFormatException);
      }
    });
  });

  group('Money allocation', () {
    test('largest remainder ties use UUID, independent of row order', () {
      final input = rows([1, 1, 1]);
      final expected = {a: BigInt.from(334), b: BigInt.from(333), c: BigInt.from(333)};
      expect(byUser(allocateSplits('equal', BigInt.from(1000), input)), expected);
      expect(byUser(allocateSplits('equal', BigInt.from(1000), input.reversed.toList())), expected);
      expect(byUser(allocateSplits('shares', BigInt.from(12000), rows([2, 1, 1]))), {
        a: BigInt.from(6000), b: BigInt.from(3000), c: BigInt.from(3000),
      });
      expect(byUser(allocateSplits('percentage', BigInt.from(10000), rows([3333, 3333, 3334]))), {
        a: BigInt.from(3333), b: BigInt.from(3333), c: BigInt.from(3334),
      });
      expect(byUser(allocateSplits('exact', BigInt.from(10000), rows([1234, 8766]))), {
        a: BigInt.from(1234), b: BigInt.from(8766),
      });
    });

    test('invalid totals, identities, modes and boundaries are rejected', () {
      final invalid = <(String, List<MoneySplitValue>)>[
        ('equal', rows([1, 0])),
        ('shares', rows([0, 0])),
        ('shares', rows([32768, 1])),
        ('shares', rows([-1, 2])),
        ('percentage', rows([5000, 4999])),
        ('percentage', rows([5000, 5001])),
        ('exact', rows([1, 9998])),
        ('exact', rows([1, 10000])),
        ('equal', []),
        ('equal', [MoneySplitValue(a, BigInt.one), MoneySplitValue(a, BigInt.one)]),
        ('equal', [MoneySplitValue('not-a-uuid', BigInt.one)]),
        ('unknown', rows([1])),
      ];
      for (final (mode, input) in invalid) {
        expect(() => allocateSplits(mode, BigInt.from(10000), input), throwsFormatException);
        expect(validateMoneySplits(mode, BigInt.from(10000), input), isNotNull);
      }
      expect(() => allocateSplits('equal', BigInt.zero, rows([1])), throwsFormatException);
      expect(() => allocateSplits('equal', maxRecordMinor + BigInt.one, rows([1])), throwsFormatException);
      expect(validateMoneySplits('exact', BigInt.from(100), rows([0, 100])), isNull);
    });

    test('zero weights never get residue and generic weights are not shares', () {
      expect(byUser(apportion(BigInt.one, rows([0, 1, 1]))), {
        a: BigInt.zero, b: BigInt.one, c: BigInt.zero,
      });
      expect(byUser(apportion(BigInt.from(123451), rows([50000, 50001]))), {
        a: BigInt.from(61725), b: BigInt.from(61726),
      });
      expect(() => apportion(BigInt.one, rows([0, 0])), throwsFormatException);
      expect(() => apportion(-BigInt.one, rows([1, 1])), throwsFormatException);
    });

    test('apportion preserves sums, nonnegativity and zero exclusion', () {
      // Deterministic matrix exercises quotient/remainder boundaries, including
      // aggregate arithmetic beyond Int64 without floating-point intermediates.
      for (final amount in [BigInt.zero, BigInt.one, BigInt.from(101), maxRecordMinor * BigInt.two]) {
        for (final weights in [rows([0, 3, 7]), rows([1, 1, 1]), rows([32768, 99999, 1])]) {
          final allocated = apportion(amount, weights);
          expect(allocated.fold(BigInt.zero, (sum, row) => sum + row.value), amount);
          for (var i = 0; i < weights.length; i++) {
            expect(allocated[i].value >= BigInt.zero, isTrue);
            if (weights[i].value == BigInt.zero) expect(allocated[i].value, BigInt.zero);
          }
        }
      }
    });
  });

  group('Money conversion', () {
    test('rates are canonical exact rationals with bounded decimal precision', () {
      final rate = parseDecimalRate(' 001.2000 ');
      expect(rate.text, '1.2');
      expect(rate.numerator, BigInt.from(12));
      expect(rate.denominator, BigInt.from(10));
      expect(parseDecimalRate('0.000000000000000001').text, '0.000000000000000001');
      expect(parseDecimalRate('999999999999999999.999999999999999999').text,
          '999999999999999999.999999999999999999');
      for (final value in ['0', '-1', '+1', 'NaN', '1e2', '.5', '1.', '1000000000000000000', '0.0000000000000000001']) {
        expect(() => parseDecimalRate(value), throwsFormatException, reason: value);
      }
    });

    test('exact half-up conversion and independent logical allocations', () {
      expect(convertMinorAmount(BigInt.from(10000), sourceMinorDigits: 2,
          targetMinorDigits: 2, rate: parseDecimalRate('1.20')), BigInt.from(12000));
      final exact = allocateExpenseSplits('exact', BigInt.from(10000), rows([2500, 7500]), settlementMinor: BigInt.from(12000));
      expect(exact.map((row) => row.amountMinor), [BigInt.from(2500), BigInt.from(7500)]);
      expect(exact.map((row) => row.settlementAmountMinor), [BigInt.from(3000), BigInt.from(9000)]);
      expect(convertMinorAmount(BigInt.from(101), sourceMinorDigits: 0,
          targetMinorDigits: 2, rate: parseDecimalRate('0.0067')), BigInt.from(68));
      expect(convertMinorAmount(BigInt.one, sourceMinorDigits: 2,
          targetMinorDigits: 2, rate: parseDecimalRate('0.5')), BigInt.one);
      final coarse = allocateExpenseSplits('equal', BigInt.one, rows([1, 1]), settlementMinor: BigInt.from(100));
      expect(coarse.map((row) => row.amountMinor), [BigInt.one, BigInt.zero]);
      expect(coarse.map((row) => row.settlementAmountMinor), [BigInt.from(50), BigInt.from(50)]);
      final largeExact = allocateExpenseSplits('exact', BigInt.from(100001), rows([50000, 50001]), settlementMinor: BigInt.from(123451));
      expect(largeExact.map((row) => row.amountMinor), [BigInt.from(50000), BigInt.from(50001)]);
      expect(largeExact.map((row) => row.settlementAmountMinor), [BigInt.from(61725), BigInt.from(61726)]);
    });

    test('converted zero and overflow fail instead of silently changing value', () {
      expect(() => convertMinorAmount(BigInt.one, sourceMinorDigits: 2,
          targetMinorDigits: 2, rate: parseDecimalRate('0.49')), throwsFormatException);
      expect(() => convertMinorAmount(maxRecordMinor, sourceMinorDigits: 2,
          targetMinorDigits: 2, rate: parseDecimalRate('2')), throwsFormatException);
      expect(convertMinorAmount(maxRecordMinor, sourceMinorDigits: 2,
          targetMinorDigits: 2, rate: parseDecimalRate('1')), maxRecordMinor);
      expect(() => convertMinorAmount(BigInt.zero, sourceMinorDigits: 2,
          targetMinorDigits: 2, rate: parseDecimalRate('1')), throwsFormatException);
    });
  });
}
