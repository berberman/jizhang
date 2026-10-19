import 'package:flutter_test/flutter_test.dart';
import 'package:http/http.dart' as http;
import 'package:http/testing.dart';
import 'package:jizhang_client/core/api_client.dart';
import 'package:jizhang_client/core/jizhang_repository.dart';
import 'package:jizhang_client/core/models.dart';
import 'package:jizhang_client/core/session_controller.dart';
import 'package:jizhang_client/features/records/record_editor.dart';

const usd = CurrencyInfo(code: 'USD', name: 'US Dollar', minorUnits: 2);
const jpy = CurrencyInfo(code: 'JPY', name: 'Yen', minorUnits: 0);
final users = [for (var n = 1; n <= 3; n++) User(
  userId: '00000000-0000-0000-0000-00000000000$n', username: 'user$n', displayName: 'Person',
)];
void main() {
  late SessionController session;
  late SplitFieldsController fields;
  setUp(() {
    final repository = JizhangRepository(ApiClient(baseUri: Uri.parse('https://example.test'),
      accessToken: () => null, httpClient: MockClient((_) async => http.Response('Unexpected request', 500))));
    session = SessionController(repository);
    fields = SplitFieldsController(repository: repository, session: session, kind: RecordKind.expense,
      group: ExpenseGroup(groupId: 'group', groupName: 'Trip', owner: users.first, members: users,
        currencyMode: CurrencyMode.multi, defaultCurrency: usd, currencyLocked: false));
    fields.title.text = 'Dinner';
    fields.amount.text = '100.00';
  });
  tearDown(() { fields.dispose(); session.dispose(); });

  test('mode changes and deselection retain blank and invalid raw split inputs', () {
    final a = users[0].userId, b = users[1].userId;
    fields.setSplitMode(SplitMode.shares);
    fields.setSplitValue(a, '');
    fields.setSplitValue(b, 'bad');
    fields.select(a, false);
    fields.setSplitMode(SplitMode.percentage);
    fields.setSplitValue(b, '33.');
    fields.setSplitMode(SplitMode.shares);
    fields.select(a, true);
    expect(fields.splitInput(a).text, '');
    expect(fields.splitInput(b).text, 'bad');
    expect(fields.toDraft, throwsFormatException);
    fields.setSplitMode(SplitMode.percentage);
    expect(fields.splitInput(b).text, '33.');
  });

  test('percentage rounding and explicit remainder preserve exact 100 percent', () {
    fields.setSplitMode(SplitMode.percentage);
    expect(fields.splitDrafts.map((s) => s.value), [BigInt.from(3334), BigInt.from(3333), BigInt.from(3333)]);
    fields.setSplitValue(users[0].userId, '10');
    fields.setSplitValue(users[1].userId, '20');
    fields.assignRemainder(users[2].userId);
    expect(fields.toDraft().splits.map((s) => s.value), [BigInt.from(1000), BigInt.from(2000), BigInt.from(7000)]);
    fields.setSplitValue(users[0].userId, '90');
    expect(() => fields.assignRemainder(users[2].userId), throwsFormatException);
    expect(fields.splitInput(users[2].userId).text, '70.00');
  });

  test('total and currency changes do not silently convert exact amounts', () {
    fields.setSplitMode(SplitMode.exact);
    final a = users[0].userId;
    fields.setSplitValue(a, '12.34');
    fields.amount.text = '120.00';
    expect(fields.splitInput(a).text, '12.34');
    fields.setCurrency(jpy);
    expect(fields.amount.text, '120.00');
    expect(fields.splitInput(a).text, '12.34');
    expect(fields.toDraft, throwsFormatException);
    fields.amount.text = '120';
    fields.splitEqually();
    expect(fields.toDraft().amountMinor, BigInt.from(120));
    expect(fields.toDraft().splits.map((s) => s.value), [BigInt.from(40), BigInt.from(40), BigInt.from(40)]);
  });
}
