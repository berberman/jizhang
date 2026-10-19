/// Exact money helpers, independent of API models. Modes use wire literals:
/// `equal`, `shares`, `percentage`, and `exact`. No monetary arithmetic uses double.
library;

final maxRecordMinor = BigInt.parse('9223372036854775807');
final _uuid = RegExp(
  r'^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$',
);
final _integer = RegExp(r'^(0|[1-9][0-9]*)$');
final _signedInteger = RegExp(r'^(0|-?[1-9][0-9]*)$');
final _decimal = RegExp(r'^([0-9]+)(?:\.([0-9]+))?$');

void _checkDigits(int digits) {
  if (digits < 0 || digits > 4) {
    throw ArgumentError.value(digits, 'minorDigits', 'Must be between 0 and 4');
  }
}

/// Decodes wire integer strings, including aggregate values beyond Int64.
/// JSON numbers, leading zeros, a leading plus, and negative zero are rejected.
BigInt parseMinorAmount(Object? value, {bool allowNegative = false}) {
  if (value is! String ||
      (allowNegative ? _signedInteger : _integer).firstMatch(value)?.end !=
          value.length) {
    throw const FormatException('Amount must be a canonical integer string');
  }
  return BigInt.parse(value);
}

BigInt _parseDecimal(String text, int digits) {
  final match = _decimal.firstMatch(text.trim());
  if (match == null) {
    throw const FormatException('Use an ordinary nonnegative decimal amount');
  }
  final fraction = match.group(2) ?? '';
  if (fraction.length > digits) {
    throw FormatException('Amount allows at most $digits decimal places');
  }
  return BigInt.parse(match.group(1)!) * BigInt.from(10).pow(digits) +
      (fraction.isEmpty ? BigInt.zero : BigInt.parse(fraction.padRight(digits, '0')));
}

/// Parses a major-unit input without rounding. allowZero is for exact split rows.
BigInt parseMajorAmount(
  String text, {
  required int minorDigits,
  bool allowZero = false,
}) {
  _checkDigits(minorDigits);
  final amount = _parseDecimal(text, minorDigits);
  _checkRecordAmount(amount, allowZero: allowZero);
  return amount;
}

void _checkRecordAmount(BigInt amount, {bool allowZero = false}) {
  if (amount < BigInt.zero || (!allowZero && amount == BigInt.zero)) {
    throw FormatException(allowZero
        ? 'Amount must not be negative'
        : 'Amount must be greater than zero');
  }
  if (amount > maxRecordMinor) {
    throw const FormatException('Amount exceeds the maximum record amount');
  }
}

/// Returns major units only, suitable for an editable input. Handles aggregates.
String formatMajorAmount(BigInt amountMinor, {required int minorDigits}) {
  _checkDigits(minorDigits);
  final sign = amountMinor.isNegative ? '-' : '';
  final digits = amountMinor.abs().toString().padLeft(minorDigits + 1, '0');
  if (minorDigits == 0) return '$sign$digits';
  final point = digits.length - minorDigits;
  return '$sign${digits.substring(0, point)}.${digits.substring(point)}';
}

String formatMoney({
  required String code,
  required int minorDigits,
  required BigInt amountMinor,
}) => '$code ${formatMajorAmount(amountMinor, minorDigits: minorDigits)}';

/// UI percentages use two decimal places; allocation uses integer basis points.
BigInt parsePercentage(String text) {
  final value = _parseDecimal(text, 2);
  if (value > BigInt.from(10000)) {
    throw const FormatException('Percentage must be between 0 and 100');
  }
  return value;
}

class MoneySplitValue {
  const MoneySplitValue(this.userId, this.value);
  final String userId;
  final BigInt value;
}

void _checkRows(List<MoneySplitValue> rows) {
  if (rows.isEmpty) {
    throw const FormatException('Expense must include at least one split');
  }
  final users = <String>{};
  for (final row in rows) {
    if (row.userId.length != 36 || !_uuid.hasMatch(row.userId)) {
      throw const FormatException('Split user must be a UUID');
    }
    if (!users.add(row.userId.toLowerCase())) {
      throw const FormatException('Each user may appear only once in splits');
    }
    if (row.value < BigInt.zero) {
      throw const FormatException('Split values must not be negative');
    }
  }
}

/// Returns a displayable validation error, or null. Does not alter raw inputs.
String? validateMoneySplits(
  String mode,
  BigInt amountMinor,
  List<MoneySplitValue> rows,
) {
  try {
    _validateSplits(mode, amountMinor, rows);
    return null;
  } on FormatException catch (error) {
    return error.message;
  }
}

void _validateSplits(String mode, BigInt amount, List<MoneySplitValue> rows) {
  _checkRecordAmount(amount);
  _checkRows(rows);
  final total = rows.fold(BigInt.zero, (sum, row) => sum + row.value);
  switch (mode) {
    case 'equal':
      if (rows.any((row) => row.value != BigInt.one)) {
        throw const FormatException('Equal splits must have value 1');
      }
      break;
    case 'shares':
      if (rows.any((row) => row.value > BigInt.from(32767))) {
        throw const FormatException('Split shares must be between 0 and 32767');
      }
      if (total == BigInt.zero) {
        throw const FormatException('Total split shares must be greater than zero');
      }
      break;
    case 'percentage':
      if (rows.any((row) => row.value > BigInt.from(10000)) ||
          total != BigInt.from(10000)) {
        throw const FormatException('Split percentages must total exactly 100%');
      }
      break;
    case 'exact':
      if (total != amount) {
        throw const FormatException('Exact splits must total the expense amount');
      }
      break;
    default:
      throw const FormatException('Unknown split method');
  }
}

/// Largest remainder allocation with immutable UUID-text tie breaking.
/// Generic weights have no Shares-specific 32767 limit. Result preserves input
/// order; UUID casing is ignored for identity and ordering. Zero weights never
/// receive a remainder. Aggregate totals are not restricted to Int64 here.
List<MoneySplitValue> apportion(BigInt amountMinor, List<MoneySplitValue> rows) {
  if (amountMinor < BigInt.zero) {
    throw const FormatException('Allocation amount must not be negative');
  }
  _checkRows(rows);
  final total = rows.fold(BigInt.zero, (sum, row) => sum + row.value);
  if (total == BigInt.zero) {
    throw const FormatException('Total allocation weight must be positive');
  }
  final allocated = [for (final row in rows) amountMinor * row.value ~/ total];
  final remainders = [for (final row in rows) amountMinor * row.value % total];
  final keys = [for (final row in rows) row.userId.toLowerCase()];
  final remainderOrder = [for (var i = 0; i < rows.length; i++) i]
    ..sort((a, b) {
      final comparison = remainders[b].compareTo(remainders[a]);
      return comparison != 0
          ? comparison
          : keys[a].compareTo(keys[b]);
    });
  var remaining = amountMinor - allocated.fold(BigInt.zero, (a, b) => a + b);
  for (final index in remainderOrder) {
    if (remaining == BigInt.zero) break;
    if (rows[index].value == BigInt.zero) continue;
    allocated[index] += BigInt.one;
    remaining -= BigInt.one;
  }
  return [
    for (var i = 0; i < rows.length; i++)
      MoneySplitValue(rows[i].userId, allocated[i]),
  ];
}

List<MoneySplitValue> allocateSplits(
  String mode,
  BigInt amountMinor,
  List<MoneySplitValue> rows,
) {
  _validateSplits(mode, amountMinor, rows);
  return mode == 'exact' ? List.of(rows) : apportion(amountMinor, rows);
}

/// An exact positive decimal rate; numerator/denominator are a Rational pair.
class ExactDecimalRate {
  const ExactDecimalRate._(this.text, this.numerator, this.denominator);
  final String text;
  final BigInt numerator;
  final BigInt denominator;
}

ExactDecimalRate parseDecimalRate(String text) {
  final match = _decimal.firstMatch(text.trim());
  if (match == null) {
    throw const FormatException('Rate must be an ordinary positive decimal');
  }
  final whole = BigInt.parse(match.group(1)!).toString();
  final fraction = (match.group(2) ?? '').replaceFirst(RegExp(r'0+$'), '');
  if (whole.length > 18 || fraction.length > 18) {
    throw const FormatException('Rate allows at most 18 integer and 18 decimal digits');
  }
  final numerator = BigInt.parse('$whole$fraction');
  if (numerator == BigInt.zero) {
    throw const FormatException('Rate must be greater than zero');
  }
  return ExactDecimalRate._(
    fraction.isEmpty ? whole : '$whole.$fraction',
    numerator,
    BigInt.from(10).pow(fraction.length),
  );
}

/// Direction: 1 source major unit = rate * target major units. Round half-up.
BigInt convertMinorAmount(
  BigInt originalMinor, {
  required int sourceMinorDigits,
  required int targetMinorDigits,
  required ExactDecimalRate rate,
}) {
  _checkDigits(sourceMinorDigits);
  _checkDigits(targetMinorDigits);
  _checkRecordAmount(originalMinor);
  final numerator = originalMinor * rate.numerator *
      BigInt.from(10).pow(targetMinorDigits);
  final denominator = rate.denominator * BigInt.from(10).pow(sourceMinorDigits);
  final converted = (numerator * BigInt.two + denominator) ~/
      (denominator * BigInt.two);
  if (converted == BigInt.zero) {
    throw const FormatException('Converted amount is below one minor unit');
  }
  _checkRecordAmount(converted);
  return converted;
}

class MoneySplitAllocation {
  const MoneySplitAllocation(this.userId, this.value, this.amountMinor,
      this.settlementAmountMinor);
  final String userId;
  final BigInt value;
  final BigInt amountMinor;
  final BigInt settlementAmountMinor;
}

/// Both currencies round independently from the SAME logical input ratios.
/// In particular, never reuse a coarse original allocation as a converted weight.
List<MoneySplitAllocation> allocateExpenseSplits(
  String mode,
  BigInt originalMinor,
  List<MoneySplitValue> rows, {
  required BigInt settlementMinor,
}) {
  final original = allocateSplits(mode, originalMinor, rows);
  _checkRecordAmount(settlementMinor);
  final settlement = apportion(settlementMinor, rows);
  return [
    for (var i = 0; i < rows.length; i++)
      MoneySplitAllocation(rows[i].userId, rows[i].value,
          original[i].value, settlement[i].value),
  ];
}
