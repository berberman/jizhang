import '../core/models.dart';
import '../core/money.dart';

String? validateAmount(String value, CurrencyInfo currency) {
  try { parseMajorAmount(value, minorDigits: currency.minorUnits); return null; }
  on FormatException catch (error) { return error.message; }
}
String? validateSplits(SplitMode mode, BigInt amountMinor, List<SplitDraft> splits) =>
  validateMoneySplits(mode.name, amountMinor, [for (final s in splits) MoneySplitValue(s.userId, s.value)]);

bool canRecordPayment(ExpenseGroup group, Settlement settlement) {
  final active = group.members.map((user) => user.userId).toSet();
  return active.contains(settlement.fromUser.userId) && active.contains(settlement.toUser.userId);
}
