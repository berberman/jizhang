import 'dart:async';
import 'dart:convert';

import 'package:flutter/material.dart';
import 'package:flutter/services.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:go_router/go_router.dart';
import 'package:http/http.dart' as http;
import 'package:http/testing.dart';
import 'package:package_info_plus/package_info_plus.dart';
import 'package:jizhang_client/app.dart';
import 'package:jizhang_client/core/api_client.dart';
import 'package:jizhang_client/core/auth_browser.dart';
import 'package:jizhang_client/core/jizhang_repository.dart';
import 'package:jizhang_client/core/models.dart';
import 'package:jizhang_client/core/session_controller.dart';
import 'package:jizhang_client/shared/form_controller.dart';
import 'package:jizhang_client/shared/widgets.dart';
import 'package:jizhang_client/shared/record_navigation.dart';
import 'package:jizhang_client/shared/split_validation.dart';
import 'package:jizhang_client/features/records/record_editor.dart';
import 'package:jizhang_client/shared/entity_picker.dart';
import 'package:jizhang_client/features/records/record_search.dart';

const aliceId = '00000000-0000-0000-0000-000000000001';
const bobId = '00000000-0000-0000-0000-000000000002';
const usdJson = {'code': 'USD', 'name': 'US Dollar', 'minorUnits': 2};
const usd = CurrencyInfo(code: 'USD', name: 'US Dollar', minorUnits: 2);
const aliceJson = {'userId': aliceId, 'username': 'alice', 'displayName': 'Alice'};
const bobJson = {'userId': bobId, 'username': 'bob', 'displayName': 'Bob'};

class LogoutTestBrowser implements AuthBrowser {
  final destinations = <Uri>[];
  @override
  bool get supported => true;
  @override
  void navigate(Uri url) => destinations.add(url);
  @override
  void clearPending() {}
  @override
  Future<String> newVerifier() async => 'test-verifier';
  @override
  Future<String> challenge(String verifier) async => 'test-challenge';
  @override
  void savePending(Map<String, String> value) => throw UnimplementedError();
  @override
  Map<String, String>? readPending() => null;
  @override
  void replaceUrl(Uri url) => throw UnimplementedError();
}

SessionController roleSession({
  List<Map<String, Object>> members = const [aliceJson, bobJson],
  Future<http.Response?> Function(http.Request)? records,
}) {
  final group = {'groupId': 'group-id', 'groupName': 'Trip',
    'owner': aliceJson, 'members': members, 'currencyMode': 'single',
    'defaultCurrency': usdJson, 'currencyLocked': false};
  return makeSession((request) async {
    if (request.url.path.endsWith('/auth/login')) return signedIn();
    if (request.url.path.endsWith('/currencies')) return http.Response(jsonEncode([usdJson]), 200);
    if (request.url.path.endsWith('/groups/group-id')) return http.Response(jsonEncode(group), 200);
    if (request.url.path.endsWith('/users/me/groups')) {
      return http.Response(jsonEncode({'items': [group], 'pageInfo': {
        'offset': 0, 'limit': 20, 'total': 1, 'hasNext': false, 'hasPrev': false,
      }}), 200);
    }
    return await records?.call(request) ?? http.Response('Not found', 404);
  });
}

Future<GoRouter> openRoleEditor(WidgetTester tester, SessionController session, String path) async {
  await session.login('alice', 'password');
  await tester.pumpWidget(JizhangApp(repository: session.repository, session: session));
  await tester.pumpAndSettle();
  final router = GoRouter.of(tester.element(find.text('Create group')));
  router.go('/groups/group-id/$path');
  await tester.pumpAndSettle();
  return router;
}

Finder rolePicker(String label) => find.byWidgetPredicate((widget) => widget is UserPickerField && widget.label == label);

Future<void> chooseRole(WidgetTester tester, String label, String handle) async {
  final button = find.descendant(of: rolePicker(label), matching: find.byType(OutlinedButton));
  await tester.ensureVisible(button);
  await tester.tap(button);
  await tester.pumpAndSettle();
  final option = find.descendant(of: find.byType(Dialog),
    matching: find.widgetWithText(ListTile, '@$handle'));
  await tester.ensureVisible(option);
  await tester.tap(option);
  await tester.pumpAndSettle();
}

Map<String, Object?> transferFixture({String payer = aliceId, String recipient = bobId,
  String amount = '1200', String date = '2026-10-01', String? note}) => {
  'tag': 'TransferRecord', 'recordId': 'transfer-id', 'groupId': 'group-id',
  'title': 'Payment', 'amountMinor': amount, 'currency': usdJson,
  'note': note,
  'settlementCurrency': usdJson, 'settlementAmountMinor': amount, 'exchangeRate': null,
  'paidBy': payer == aliceId ? aliceJson : bobJson,
  'transferTo': recipient == aliceId ? aliceJson : bobJson,
  'date': date, 'createdAt': '2026-10-01T00:00:00Z',
};

http.Response signedIn() => http.Response(
  jsonEncode({'accessToken': 'test-token', 'loginUser': aliceJson}),
  200,
);

SessionController makeSession(
  Future<http.Response> Function(http.Request) handler, {
  AuthBrowser? authBrowser,
}) {
  late SessionController session;
  final api = ApiClient(
    baseUri: Uri.parse('https://example.test/api'),
    httpClient: MockClient(handler),
    accessToken: () => session.accessToken,
  );
  session = SessionController(JizhangRepository(api), authBrowser: authBrowser);
  return session;
}

SessionController statisticsSession(
  Future<http.Response> Function(http.Request) statistics,
) {
  const group = {'groupId': 'group-id', 'groupName': 'Statistics trip',
    'owner': aliceJson, 'members': [aliceJson], 'currencyMode': 'single',
    'defaultCurrency': usdJson, 'currencyLocked': true};
  return makeSession((request) async {
    if (request.url.path.endsWith('/auth/login')) return signedIn();
    if (request.url.path.endsWith('/statistics')) return statistics(request);
    if (request.url.path.endsWith('/groups/group-id')) return http.Response(jsonEncode(group), 200);
    if (request.url.path.endsWith('/users/me/groups')) {
      return http.Response(jsonEncode({'items': [group], 'pageInfo': {
        'offset': 0, 'limit': 20, 'total': 1, 'hasNext': false, 'hasPrev': false,
      }}), 200);
    }
    return http.Response('Not found', 404);
  });
}

http.Response spendingResponse(http.Request request, String amount) {
  final from = request.url.queryParameters['from']!;
  final to = request.url.queryParameters['to']!;
  return http.Response(jsonEncode({
    'groupId': 'group-id', 'from': from, 'to': to, 'bucket': 'month',
    'user': aliceJson, 'participants': [aliceJson], 'currencies': [{
      'currency': usdJson, 'totalMinor': amount, 'expenseCount': 1,
      'incomeMinor': '0', 'incomeCount': 0, 'netMinor': (-BigInt.parse(amount)).toString(),
      'paidMinor': amount, 'shareMinor': amount,
      'buckets': [{'start': from, 'end': to, 'totalMinor': amount,
        'expenseCount': 1, 'incomeMinor': '0', 'incomeCount': 0,
        'netMinor': (-BigInt.parse(amount)).toString(), 'paidMinor': amount, 'shareMinor': amount}],
      'members': [{'user': aliceJson, 'paidMinor': amount, 'shareMinor': amount}],
      'topExpenses': [{'recordId': 'expense-id', 'title': 'Private expense',
        'date': from, 'amountMinor': amount}], 'otherExpensesMinor': '0',
    }],
  }), 200);
}

String statisticsLocation(String from, String to, {bool currency = true}) =>
  Uri(path: '/groups/group-id/statistics', queryParameters: {
    'from': from, 'to': to, 'bucket': 'month', 'view': 'group',
    'memberMeasure': 'share', if (currency) 'currency': 'USD',
  }).toString();

Future<GoRouter> openStatistics(
  WidgetTester tester, SessionController session, String location,
) async {
  await session.login('alice', 'password');
  await tester.pumpWidget(JizhangApp(repository: session.repository, session: session));
  await tester.pumpAndSettle();
  final router = GoRouter.of(tester.element(find.text('Create group')));
  router.go(location);
  return router;
}

Map<String, Object?> expenseFixture({String id = 'expense-id', String title = 'Shared meal', String amount = '1200'}) => {
  ...transferFixture(amount: amount), 'tag': 'ExpenseRecord', 'recordId': id,
  'title': title, 'splitMode': 'equal',
  'splits': [{'user': aliceJson, 'value': '1', 'amountMinor': amount, 'settlementAmountMinor': amount}],
}..remove('transferTo');

http.Response recordPage(List<Map<String, Object?>> rows, {int offset = 0, int total = 1}) =>
  http.Response(jsonEncode({'items': rows, 'pageInfo': {
    'offset': offset, 'limit': 20, 'total': total,
    'hasNext': offset + 20 < total, 'hasPrev': offset > 0,
  }}), 200);

void main() {
  setUp(() => PackageInfo.setMockInitialValues(
    appName: 'Jizhang', packageName: 'jizhang_client',
    version: '', buildNumber: '', buildSignature: '',
  ));
  testWidgets('retained LoadView preserves subtree across failure and ignores superseded access responses', (tester) async {
    final key = GlobalKey<LoadViewState<String>>();
    final loads = <Completer<String>>[];
    await tester.pumpWidget(MaterialApp(home: Scaffold(body: LoadView<String>(
      key: key, retainDataOnReload: true,
      load: () { final request = Completer<String>(); loads.add(request); return request.future; },
      builder: (value) => Column(children: [Text(value), const TextField(key: ValueKey('retained-draft'))]),
    ))));
    loads.single.complete('Private result');
    await tester.pumpAndSettle();
    await tester.enterText(find.byType(TextField), 'Unsubmitted draft');
    final original = tester.element(find.byType(TextField));
    final failed = key.currentState!.reload();
    await tester.pump();
    expect(find.text('Private result'), findsOneWidget);
    expect(find.byType(LinearProgressIndicator), findsOneWidget);
    loads.last.completeError(const ApiException(statusCode: 500, message: 'Temporary reload failure'));
    await failed;
    await tester.pumpAndSettle();
    expect(find.text('Showing previously loaded data.'), findsOneWidget);
    expect(find.text('Unsubmitted draft'), findsOneWidget);
    expect(tester.element(find.byType(TextField)), same(original));
    await tester.tap(find.text('Retry'));
    await tester.pump();
    final obsolete = loads.last;
    final newest = key.currentState!.reload();
    loads.last.complete('Newest private result');
    await newest;
    await tester.pumpAndSettle();
    obsolete.completeError(const ApiException(statusCode: 403, message: 'Obsolete access denial'));
    await tester.pumpAndSettle();
    expect(find.text('Newest private result'), findsOneWidget);
    expect(find.text('Unsubmitted draft'), findsOneWidget);
    expect(find.text('Obsolete access denial'), findsNothing);
    for (final status in [401, 403, 404]) {
      final revoked = key.currentState!.reload();
      loads.last.completeError(ApiException(statusCode: status, message: 'Access revoked $status'));
      await revoked;
      await tester.pumpAndSettle();
      expect(find.text('Newest private result'), findsNothing);
      expect(find.byType(TextField), findsNothing);
      expect(find.text('Showing previously loaded data.'), findsNothing);
      expect(find.text('Access revoked $status'), findsOneWidget);
      if (status != 404) {
        final restored = key.currentState!.reload();
        loads.last.complete('Newest private result');
        await restored;
        await tester.pumpAndSettle();
      }
    }
    final disposed = key.currentState!.reload();
    await tester.pumpWidget(const SizedBox.shrink());
    loads.last.complete('Disposed private result');
    await disposed;
    await tester.pump();
    expect(find.text('Disposed private result'), findsNothing);
  });

  testWidgets('Records return preserves page applied query sort draft scroll and keyboard focus without a GET', (tester) async {
    await tester.binding.setSurfaceSize(const Size(1100, 800));
    addTearDown(() => tester.binding.setSurfaceSize(null));
    final requests = <Uri>[];
    final session = roleSession(records: (request) async {
      if (request.url.path.endsWith('/records/row-27')) {
        return http.Response(jsonEncode({...transferFixture(), 'recordId': 'row-27', 'title': 'Payment 27'}), 200);
      }
      if (request.url.path.endsWith('/records')) {
        requests.add(request.url);
        final offset = int.parse(request.url.queryParameters['offset'] ?? '0');
        return recordPage([for (var i = offset; i < offset + 20; i++) {
          ...transferFixture(note: i.isEven ? 'A longer note\nwith another line' : null),
          'recordId': 'row-$i', 'title': 'Payment $i',
        }], offset: offset, total: 40);
      }
      return null;
    });
    addTearDown(session.dispose);
    final router = await openRoleEditor(tester, session, 'records');
    final input = find.widgetWithText(TextField, 'Search records');
    await tester.enterText(input, 'type:transfer');
    await tester.tap(find.widgetWithText(FilledButton, 'Search'));
    await tester.pumpAndSettle();
    final direction = find.widgetWithText(DropdownButtonFormField<bool>, 'Direction');
    await tester.ensureVisible(direction);
    await tester.tap(direction); await tester.pumpAndSettle();
    await tester.tap(find.text('Ascending').last); await tester.pumpAndSettle();
    await tester.ensureVisible(find.widgetWithText(OutlinedButton, 'Next'));
    await tester.tap(find.widgetWithText(OutlinedButton, 'Next'));
    await tester.pumpAndSettle();
    await tester.ensureVisible(input);
    await tester.enterText(input, 'note:"unfinished');
    final card = find.text('Payment 27');
    await tester.ensureVisible(card);
    await tester.pumpAndSettle();
    final sourcePosition = Scrollable.of(tester.element(card)).position;
    final initiatingFocus = Focus.of(tester.element(card));
    initiatingFocus.requestFocus();
    await tester.pump();
    final offset = sourcePosition.pixels;
    final count = requests.length;
    expect(offset, greaterThan(0));
    expect(requests.last.queryParameters, containsPair('offset', '20'));
    expect(requests.last.queryParameters, containsPair('query', 'type:transfer'));
    expect(requests.last.queryParameters, containsPair('sort', 'date'));
    await tester.sendKeyEvent(LogicalKeyboardKey.enter);
    await tester.pumpAndSettle();
    expect(router.routeInformationProvider.value.uri.path, '/groups/group-id/records/row-27');
    await tester.tap(find.text('Back'));
    await tester.pumpAndSettle();
    expect(requests.length, count);
    expect(sourcePosition.pixels, closeTo(offset, 1));
    expect(initiatingFocus.hasFocus, isTrue);
    expect(tester.widget<TextField>(input).controller!.text, 'note:"unfinished');
    await tester.sendKeyEvent(LogicalKeyboardKey.enter);
    await tester.pumpAndSettle();
    router.pop();
    await tester.pumpAndSettle();
    expect(requests.length, count);
    expect(sourcePosition.pixels, closeTo(offset, 1));
    expect(initiatingFocus.hasFocus, isTrue);
  });

  testWidgets('detail deletion clamps a vanished final Records page and retains the search draft', (tester) async {
    var deleted = false;
    final offsets = <int>[];
    final session = roleSession(records: (request) async {
      if (request.method == 'DELETE') { deleted = true; return http.Response('', 204); }
      if (request.url.path.endsWith('/records/transfer-id')) return http.Response(jsonEncode(transferFixture()), 200);
      if (request.url.path.endsWith('/records')) {
        final offset = int.parse(request.url.queryParameters['offset'] ?? '0');
        offsets.add(offset);
        return recordPage(offset == 20 ? (deleted ? [] : [transferFixture()]) :
          [for (var i = 0; i < 20; i++) {...transferFixture(), 'recordId': 'earlier-$i', 'title': 'Earlier payment $i'}],
          offset: offset, total: deleted ? 20 : 21);
      }
      return null;
    });
    addTearDown(session.dispose);
    final router = await openRoleEditor(tester, session, 'records');
    await tester.ensureVisible(find.text('Next'));
    await tester.tap(find.text('Next'));
    await tester.pumpAndSettle();
    final input = find.widgetWithText(TextField, 'Search records');
    await tester.ensureVisible(input);
    await tester.enterText(input, 'Unapplied search');
    await tester.ensureVisible(find.text('Payment'));
    await tester.tap(find.text('Payment'));
    await tester.pumpAndSettle();
    await tester.ensureVisible(find.widgetWithText(OutlinedButton, 'Delete'));
    await tester.tap(find.widgetWithText(OutlinedButton, 'Delete'));
    await tester.pumpAndSettle();
    await tester.tap(find.descendant(of: find.byType(AlertDialog), matching: find.text('Delete')));
    await tester.pumpAndSettle();
    expect(deleted, isTrue);
    expect(router.routeInformationProvider.value.uri.path, '/groups/group-id/records');
    expect(offsets, [0, 20, 20, 0]);
    expect(tester.widget<TextField>(input).controller!.text, 'Unapplied search');
    expect(tester.widget<OutlinedButton>(find.widgetWithText(OutlinedButton, 'Previous')).onPressed, isNull);
  });

  testWidgets('receipt origin refreshes saved detail then receipt totals and retains failed refresh for Retry', (tester) async {
    var record = expenseFixture();
    var deleted = false;
    var receiptGets = 0;
    var detailGets = 0;
    var failReceipt = false;
    final session = roleSession(records: (request) async {
      if (request.method == 'DELETE') { deleted = true; return http.Response('', 204); }
      if (request.method == 'PUT') {
        final body = jsonDecode(request.body) as Map<String, dynamic>;
        record = expenseFixture(title: body['title'] as String, amount: body['amountMinor'].toString());
        return http.Response(jsonEncode(record), 200);
      }
      if (request.url.path.endsWith('/records/expense-id')) {
        detailGets++;
        return http.Response(jsonEncode(record), 200);
      }
      if (request.url.path.endsWith('/receipts/receipt-id')) {
        receiptGets++;
        if (failReceipt) return http.Response('Receipt refresh unavailable', 500);
        return http.Response(jsonEncode({'receiptId': 'receipt-id', 'groupId': 'group-id',
          'uploadedBy': aliceJson, 'note': 'Team dinner receipt', 'createdAt': '2026-10-01T00:00:00Z',
          'records': deleted ? [] : [record]}), 200);
      }
      return null;
    });
    addTearDown(session.dispose);
    final router = await openRoleEditor(tester, session, 'receipts/receipt-id');
    Future<void> openExpense() async {
      await tester.ensureVisible(find.text('Open expense'));
      await tester.tap(find.text('Open expense'));
      await tester.pumpAndSettle();
    }
    await openExpense();
    await tester.tap(find.text('Back'));
    await tester.pumpAndSettle();
    expect(receiptGets, 1);
    await openExpense();
    await tester.ensureVisible(find.text('Edit'));
    await tester.tap(find.text('Edit'));
    await tester.pumpAndSettle();
    final title = find.widgetWithText(TextField, 'Title');
    await tester.ensureVisible(title);
    await tester.enterText(title, 'Updated shared meal');
    final amount = find.widgetWithText(TextField, 'Amount');
    await tester.ensureVisible(amount);
    await tester.enterText(amount, '24.00');
    await tester.ensureVisible(find.widgetWithText(FilledButton, 'Save'));
    await tester.tap(find.widgetWithText(FilledButton, 'Save'));
    await tester.pumpAndSettle();
    expect(router.routeInformationProvider.value.uri.path, '/groups/group-id/records/expense-id');
    expect(find.text('Updated shared meal'), findsOneWidget);
    expect(detailGets, 4);
    failReceipt = true;
    await tester.ensureVisible(find.text('Back'));
    await tester.tap(find.text('Back'));
    await tester.pumpAndSettle();
    expect(router.routeInformationProvider.value.uri.path, '/groups/group-id/receipts/receipt-id');
    expect(find.text('Shared meal'), findsOneWidget);
    expect(find.text('Showing previously loaded data.'), findsOneWidget);
    expect(find.textContaining('Receipt refresh unavailable'), findsOneWidget);
    failReceipt = false;
    await tester.ensureVisible(find.text('Retry'));
    await tester.tap(find.text('Retry'));
    await tester.pumpAndSettle();
    expect(receiptGets, 3);
    expect(find.text('Updated shared meal'), findsOneWidget);
    expect(find.text('Shared meal'), findsNothing);
    expect(find.text('USD 24.00'), findsWidgets);
    await tester.pump(const Duration(seconds: 5));
    await openExpense();
    await tester.ensureVisible(find.widgetWithText(OutlinedButton, 'Delete'));
    await tester.tap(find.widgetWithText(OutlinedButton, 'Delete'));
    await tester.pumpAndSettle();
    await tester.tap(find.descendant(of: find.byType(AlertDialog), matching: find.text('Delete')));
    await tester.pumpAndSettle();
    expect(router.routeInformationProvider.value.uri.path, '/groups/group-id/receipts/receipt-id');
    expect(receiptGets, 4);
    expect(find.text('Updated shared meal'), findsNothing);
    expect(find.text('No expenses'), findsOneWidget);
  });

  testWidgets('late pushed detail response cannot restore protected content after logout', (tester) async {
    final delayed = Completer<http.Response>();
    final session = roleSession(records: (request) async {
      if (request.url.path.endsWith('/records/transfer-id')) return delayed.future;
      if (request.url.path.endsWith('/records')) return recordPage([transferFixture()]);
      return null;
    });
    addTearDown(session.dispose);
    final router = await openRoleEditor(tester, session, 'records');
    await tester.tap(find.text('Payment'));
    await tester.pump();
    expect(router.routeInformationProvider.value.uri.path, '/groups/group-id/records/transfer-id');
    await session.logout();
    await tester.pumpAndSettle();
    delayed.complete(http.Response(jsonEncode(transferFixture(note: 'Previous session secret')), 200));
    await tester.pumpAndSettle();
    expect(find.text('Previous session secret'), findsNothing);
    expect(find.text('Payment'), findsNothing);
    expect(router.routeInformationProvider.value.uri.path, isNot(startsWith('/groups/')));
  });

  testWidgets('route returns acknowledge only the revision refreshed and ignore initial mount', (tester) async {
    final session = roleSession();
    addTearDown(session.dispose);
    await session.login('alice', 'password');
    final changes = RecordChangeTracker();
    final returns = <bool>[];
    Completer<void>? refresh;
    final navigator = GlobalKey<NavigatorState>();
    await tester.pumpWidget(MaterialApp(navigatorKey: navigator, home: Scaffold(
      body: RecordReturnBoundary(session: session, changes: changes,
        onReturn: (changed) async { returns.add(changed); await refresh?.future; },
        child: const Text('Source')))));
    await tester.pumpAndSettle();
    expect(returns, isEmpty);
    Future<void> cover() async {
      unawaited(navigator.currentState!.push<void>(MaterialPageRoute(
        builder: (_) => const Scaffold(body: Text('Detail')))));
      await tester.pumpAndSettle();
    }
    await cover();
    navigator.currentState!.pop();
    await tester.pumpAndSettle();
    expect(returns, [false]);
    await cover();
    changes.markChanged();
    refresh = Completer<void>();
    navigator.currentState!.pop();
    await tester.pumpAndSettle();
    expect(returns, [false, true]);
    changes.markChanged();
    refresh.complete();
    await tester.pumpAndSettle();
    refresh = null;
    await cover();
    navigator.currentState!.pop();
    await tester.pumpAndSettle();
    expect(returns, [false, true, true]);
  });

  testWidgets('record detail and editor return to their real predecessor with dirty guards', (tester) async {
    var lists = 0;
    final session = roleSession(records: (request) async {
      if (request.url.path.endsWith('/records/transfer-id')) {
        return http.Response(jsonEncode(transferFixture()), 200);
      }
      if (request.url.path.endsWith('/records')) {
        lists++;
        return http.Response(jsonEncode({'items': [transferFixture()], 'pageInfo': {
          'offset': 0, 'limit': 20, 'total': 1, 'hasNext': false, 'hasPrev': false,
        }}), 200);
      }
      return null;
    });
    addTearDown(session.dispose);
    final router = await openRoleEditor(tester, session, 'records');
    await tester.tap(find.text('Payment'));
    await tester.pumpAndSettle();
    expect(router.routeInformationProvider.value.uri.path, '/groups/group-id/records/transfer-id');
    await tester.ensureVisible(find.text('Edit'));
    await tester.tap(find.text('Edit'));
    await tester.pumpAndSettle();
    final note = find.widgetWithText(TextField, 'Note (optional)');
    await tester.ensureVisible(note);
    await tester.enterText(note, 'Unsaved draft');
    await tester.ensureVisible(find.text('Cancel'));
    await tester.tap(find.text('Cancel'));
    await tester.pumpAndSettle();
    await tester.tap(find.text('Cancel').last);
    await tester.pumpAndSettle();
    expect(find.text('Unsaved draft'), findsOneWidget);
    await tester.ensureVisible(find.text('Cancel'));
    await tester.tap(find.text('Cancel'));
    await tester.pumpAndSettle();
    await tester.tap(find.text('Discard'));
    await tester.pumpAndSettle();
    expect(router.routeInformationProvider.value.uri.path, '/groups/group-id/records/transfer-id');
    await tester.ensureVisible(find.text('Back'));
    await tester.tap(find.text('Back'));
    await tester.pumpAndSettle();
    expect(router.routeInformationProvider.value.uri.path, '/groups/group-id/records');
    expect(lists, 1);
    await tester.tap(find.text('Payment'));
    await tester.pumpAndSettle();
    router.pop();
    await tester.pumpAndSettle();
    expect(router.routeInformationProvider.value.uri.path, '/groups/group-id/records');
    expect(lists, 1);
    router.go('/groups/group-id/records/transfer-id');
    await tester.pumpAndSettle();
    await tester.tap(find.text('Back to records'));
    await tester.pumpAndSettle();
    expect(router.routeInformationProvider.value.uri.path, '/groups/group-id/records');
  });

  testWidgets('suggested payments open an exact draft for review and former members stay disabled', (tester) async {
    await tester.binding.setSurfaceSize(const Size(1100, 1800));
    addTearDown(() => tester.binding.setSurfaceSize(null));
    const former = {'userId': '00000000-0000-0000-0000-000000000003', 'username': 'carol', 'displayName': 'Carol'};
    var mutations = 0;
    final session = roleSession(records: (request) async {
      if (request.method != 'GET') mutations++;
      if (request.url.path.endsWith('/report')) return http.Response(jsonEncode({
        'groupId': 'group-id', 'currencies': [{'currency': usdJson, 'balances': [], 'settlements': [
          {'fromUser': bobJson, 'toUser': aliceJson, 'amountMinor': '7000'},
          {'fromUser': former, 'toUser': aliceJson, 'amountMinor': '1200'},
        ]}],
      }), 200);
      return null;
    });
    await openRoleEditor(tester, session, 'balances');
    expect(find.text('No record activity'), findsOneWidget);
    expect(find.byWidgetPredicate((widget) => widget is IdentityLabel && widget.user.userId == former['userId'] && widget.former), findsOneWidget);
    expect(find.text('Re-add the former member before recording a payment.'), findsOneWidget);
    final buttons = tester.widgetList<OutlinedButton>(find.widgetWithText(OutlinedButton, 'Record payment')).toList();
    expect(buttons.first.onPressed, isNotNull);
    expect(buttons.last.onPressed, isNull);
    await tester.tap(find.widgetWithText(OutlinedButton, 'Record payment').first);
    await tester.pumpAndSettle();
    expect(find.descendant(of: rolePicker('Paid by'), matching: find.textContaining('Bob')), findsOneWidget);
    expect(find.descendant(of: rolePicker('Paid to'), matching: find.textContaining('Alice')), findsOneWidget);
    final amounts = tester.widgetList<TextField>(find.byType(TextField));
    expect(amounts.any((field) => field.controller?.text == '70.00'), isTrue);
    expect(mutations, 0);
  });
  testWidgets('balances preserve exact roles, local sorting, currency and expanded records', (tester) async {
    await tester.binding.setSurfaceSize(const Size(1440, 2200));
    addTearDown(() => tester.binding.setSurfaceSize(null));
    const eur = {'code': 'EUR', 'name': 'Euro', 'minorUnits': 2};
    var editedTitle = 'Alice expense 0';
    Map<String, Object?> balance(Map<String, Object> user, String net, String title, {int activity = 1}) => {
      'user': user, 'totalMinor': net, 'expensesPaidMinor': '9007199254740993',
      'expenseShareMinor': '3000', 'incomeReceivedMinor': '0', 'incomeEntitlementMinor': '3000',
      'paymentsSentMinor': '0', 'paymentsReceivedMinor': '2000',
      'breakdown': [for (var i = 0; i < activity; i++) {
        'recordId': '$title-$i', 'title': title == 'Alice expense' && i == 0 ? editedTitle : '$title $i', 'date': '2026-10-02',
        'recordType': 'expense', 'recordAmountMinor': '9000', 'amountMinor': net,
      }],
    };
    var reports = 0;
    var allowOpen = false;
    final session = makeSession((request) async {
      final group = {'groupId': 'group-id', 'groupName': 'Balances trip', 'owner': aliceJson,
        'members': [aliceJson, bobJson], 'currencyMode': 'multi', 'defaultCurrency': usdJson, 'currencyLocked': true};
      if (request.url.path.endsWith('/auth/login')) return signedIn();
      if (request.url.path.endsWith('/groups/group-id')) return http.Response(jsonEncode(group), 200);
      if (request.url.path.endsWith('/users/me/groups')) return http.Response(jsonEncode({'items': [group], 'pageInfo': {
        'offset': 0, 'limit': 20, 'total': 1, 'hasNext': false, 'hasPrev': false}}), 200);
      if (request.method == 'PUT') {
        editedTitle = (jsonDecode(request.body) as Map<String, dynamic>)['title'] as String;
        return http.Response(jsonEncode(expenseFixture(id: 'Alice expense-0', title: editedTitle)), 200);
      }
      if (allowOpen && request.url.pathSegments.last == 'Alice expense-0') {
        return http.Response(jsonEncode(expenseFixture(id: 'Alice expense-0', title: editedTitle)), 200);
      }
      if (request.url.path.endsWith('/report')) {
        reports++;
        return http.Response(jsonEncode({'groupId': 'group-id', 'currencies': [
          {'currency': usdJson, 'balances': [balance({...aliceJson, 'displayName': 'Straße'}, '9007199254740992', 'Alice expense'),
            balance({...bobJson, 'displayName': 'STRASSE'}, '-9007199254740993', 'Bob expense', activity: 2)], 'settlements': []},
          {'currency': eur, 'balances': [balance(bobJson, '0', 'Euro Bob'), balance(aliceJson, '0', 'Euro expense')], 'settlements': []},
        ]}), 200, headers: {'content-type': 'application/json; charset=utf-8'});
      }
      return http.Response('Record no longer exists', 404);
    });
    addTearDown(session.dispose);
    final router = await openRoleEditor(tester, session, 'balances');
    Finder member(String id, {String currency = 'USD'}) => find.byKey(ValueKey('member:$currency:$id'));
    expect(tester.getTopLeft(member(bobId)).dy, lessThan(tester.getTopLeft(member(aliceId)).dy));
    final alice = member(aliceId);
    for (final label in ['Expenses paid', 'Expense share', 'Income received', 'Income entitlement', 'Payments sent', 'Payments received']) {
      expect(find.descendant(of: alice, matching: find.text(label)), findsOneWidget);
    }
    expect(find.descendant(of: alice, matching: find.text('USD 90071992547409.93')), findsOneWidget);
    expect(find.descendant(of: alice, matching: find.text('USD 0.00')), findsNWidgets(2));
    await tester.tap(find.descendant(of: alice, matching: find.byType(ExpansionTile)).first);
    await tester.pumpAndSettle();
    expect(find.text('Alice expense 0'), findsOneWidget);
    expect(find.text('USD 90.00'), findsOneWidget);
    expect(find.text('USD 90071992547409.92'), findsWidgets);
    Future<void> choose(String label, String value) async {
      final control = find.widgetWithText(DropdownButtonFormField<String>, label);
      await tester.ensureVisible(control);
      await tester.tap(control); await tester.pumpAndSettle();
      await tester.tap(find.text(value).last); await tester.pumpAndSettle();
    }
    await choose('Sort members', 'Name');
    final direction = find.widgetWithText(DropdownButtonFormField<bool>, 'Direction');
    await tester.tap(direction); await tester.pumpAndSettle();
    await tester.tap(find.text('Ascending').last); await tester.pumpAndSettle();
    expect(tester.getTopLeft(member(aliceId)).dy, lessThan(tester.getTopLeft(member(bobId)).dy));
    expect(find.text('Alice expense 0'), findsOneWidget);
    await choose('Sort members', 'Signed net');
    expect(tester.getTopLeft(member(bobId)).dy, lessThan(tester.getTopLeft(member(aliceId)).dy));
    await tester.tap(direction); await tester.pumpAndSettle();
    await tester.tap(find.text('Descending').last); await tester.pumpAndSettle();
    expect(tester.getTopLeft(member(aliceId)).dy, lessThan(tester.getTopLeft(member(bobId)).dy));
    await choose('Sort members', 'Record activity');
    expect(tester.getTopLeft(member(bobId)).dy, lessThan(tester.getTopLeft(member(aliceId)).dy));
    await tester.tap(direction); await tester.pumpAndSettle();
    await tester.tap(find.text('Ascending').last); await tester.pumpAndSettle();
    expect(tester.getTopLeft(member(aliceId)).dy, lessThan(tester.getTopLeft(member(bobId)).dy));
    await choose('Sort members', 'Signed net');
    await choose('Currency', 'EUR');
    expect(find.text('All settled. Nobody owes a payment.'), findsOneWidget);
    expect(find.text('Alice expense 0'), findsNothing);
    expect(tester.getTopLeft(member(aliceId, currency: 'EUR')).dy, lessThan(tester.getTopLeft(member(bobId, currency: 'EUR')).dy));
    await tester.tap(direction); await tester.pumpAndSettle();
    await tester.tap(find.text('Descending').last); await tester.pumpAndSettle();
    expect(tester.getTopLeft(member(aliceId, currency: 'EUR')).dy, lessThan(tester.getTopLeft(member(bobId, currency: 'EUR')).dy));
    await choose('Currency', 'USD');
    expect(find.text('Alice expense 0'), findsOneWidget);
    expect(tester.getTopLeft(member(aliceId)).dy, lessThan(tester.getTopLeft(member(bobId)).dy));
    expect(reports, 1);
    final open = find.widgetWithText(TextButton, 'Open record');
    await tester.ensureVisible(open); await tester.tap(open); await tester.pumpAndSettle();
    expect(find.textContaining('Could not open Alice expense 0'), findsOneWidget);
    expect(find.text('Balances'), findsWidgets);
    expect(find.text('Alice expense 0'), findsOneWidget);
    expect(reports, 1);
    allowOpen = true;
    await tester.binding.setSurfaceSize(const Size(800, 800));
    await tester.pumpAndSettle();
    final position = Scrollable.of(tester.element(member(aliceId))).position;
    await tester.ensureVisible(open);
    final horizontal = Scrollable.of(tester.element(open)).position;
    final horizontalOffset = horizontal.pixels;
    final offset = position.pixels;
    final sourceConfiguration = router.routerDelegate.currentConfiguration;
    await tester.tap(open);
    await tester.pumpAndSettle();
    expect(router.routeInformationProvider.value.uri.pathSegments.last, 'Alice expense-0');
    router.restore(sourceConfiguration);
    await tester.pumpAndSettle();
    expect(reports, 1);
    expect(position.pixels, closeTo(offset.clamp(position.minScrollExtent, position.maxScrollExtent), 1));
    expect(horizontal.pixels, closeTo(horizontalOffset, 1));
    expect(find.text('Alice expense 0'), findsOneWidget);
    expect(tester.widget<TextButton>(open).onPressed, isNotNull);
    expect(tester.getTopLeft(member(aliceId)).dy, lessThan(tester.getTopLeft(member(bobId)).dy));
    await tester.ensureVisible(open);
    await tester.tap(open);
    await tester.pumpAndSettle();
    await tester.ensureVisible(find.text('Edit'));
    await tester.tap(find.text('Edit'));
    await tester.pumpAndSettle();
    final title = find.widgetWithText(TextField, 'Title');
    await tester.ensureVisible(title);
    await tester.enterText(title, 'Updated balance expense');
    await tester.ensureVisible(find.widgetWithText(FilledButton, 'Save'));
    await tester.tap(find.widgetWithText(FilledButton, 'Save'));
    await tester.pumpAndSettle();
    expect(find.text('Updated balance expense'), findsOneWidget);
    await tester.ensureVisible(find.text('Back'));
    await tester.tap(find.text('Back'));
    await tester.pumpAndSettle();
    expect(reports, 2);
    expect(find.text('Updated balance expense'), findsOneWidget);
    expect(position.pixels, closeTo(offset.clamp(position.minScrollExtent, position.maxScrollExtent), 1));
    expect(horizontal.pixels, closeTo(horizontalOffset, 1));
    expect(tester.getTopLeft(member(aliceId)).dy, lessThan(tester.getTopLeft(member(bobId)).dy));
  });
  for (final size in [const Size(360, 640), const Size(1440, 1000)]) {
    testWidgets('filter controls remain separated at $size', (tester) async {
      await tester.binding.setSurfaceSize(size);
      addTearDown(() => tester.binding.setSurfaceSize(null));
      final session = roleSession();
      addTearDown(session.dispose);
      final group = ExpenseGroup(groupId: 'group-id', groupName: 'Trip',
        owner: const User(userId: aliceId, username: 'alice', displayName: 'Alice'),
        members: const [User(userId: aliceId, username: 'alice', displayName: 'Alice')],
        currencyMode: CurrencyMode.single, defaultCurrency: usd, currencyLocked: false);
      await tester.pumpWidget(MaterialApp(home: Scaffold(body: SingleChildScrollView(child: RecordSearchToolbar(
        repository: session.repository, session: session, group: group,
        query: '', sort: '-date', onSearch: (_) {}, onSort: (_) {})))));
      await tester.tap(find.widgetWithText(OutlinedButton, 'Add filter'));
      await tester.pumpAndSettle();
      Finder dropdown(String label) => find.widgetWithText(DropdownButtonFormField<String>, label);
      Finder text(String label) => find.widgetWithText(TextField, label);
      void separated(Finder first, Finder second) {
        expect(tester.getTopLeft(second).dy - tester.getBottomLeft(first).dy, greaterThanOrEqualTo(12));
      }
      Future<void> choose(String label, String option) async {
        await tester.ensureVisible(dropdown(label));
        await tester.tap(dropdown(label));
        await tester.pumpAndSettle();
        final choice = label == 'Field'
          ? find.descendant(of: find.byWidgetPredicate((widget) => widget is DropdownMenuItem<String> && widget.value == option), matching: find.byType(Text))
          : find.text(option);
        if (choice.evaluate().isEmpty) {
          await tester.scrollUntilVisible(choice, 120, scrollable: find.byType(Scrollable).last);
        }
        await tester.ensureVisible(choice.last);
        await tester.tap(choice.last);
        await tester.pumpAndSettle();
      }
      separated(dropdown('Field'), text('Contains text'));
      await tester.tap(find.widgetWithText(FilledButton, 'Apply to draft'));
      await tester.pumpAndSettle();
      expect(find.text('A value and both range bounds are required.'), findsOneWidget);
      separated(dropdown('Field'), text('Contains text'));
      for (final field in ['amount', 'date', 'created']) {
        await choose('Field', field);
        await choose('Comparison', 'Inclusive range');
        separated(dropdown('Field'), dropdown('Comparison'));
        separated(dropdown('Comparison'), text('Lower bound'));
        expect(tester.getTopLeft(text('Upper bound')).dy - tester.getBottomLeft(text('Lower bound')).dy, greaterThanOrEqualTo(12));
        await tester.ensureVisible(text('Upper bound'));
        await tester.enterText(text('Upper bound'), field == 'amount' ? '10' : '2026-10-01');
        if (field == 'amount') separated(text('Upper bound'), dropdown('Required amount currency'));
      }
      await choose('Field', 'member');
      separated(dropdown('Field'), find.widgetWithText(OutlinedButton, 'Choose member'));
      separated(find.widgetWithText(OutlinedButton, 'Choose member'), text('UUID, handle, or display name'));
      await choose('Field', 'type');
      separated(dropdown('Field'), dropdown('Record type'));
      await choose('Field', 'currency');
      separated(dropdown('Field'), dropdown('Currency'));
      await tester.ensureVisible(find.widgetWithText(TextButton, 'Cancel'));
      await tester.tap(find.widgetWithText(TextButton, 'Cancel'));
      await tester.pumpAndSettle();
      expect(find.byType(AlertDialog), findsNothing);
      expect(tester.takeException(), isNull);
    });
  }
  testWidgets('identity filters discover former public identities only when opened and store UUID labels', (tester) async {
    await tester.binding.setSurfaceSize(const Size(1100, 1800));
    addTearDown(() => tester.binding.setSurfaceSize(null));
    var reports = 0;
    final session = makeSession((request) async {
      if (request.url.path.endsWith('/report')) {
        reports++;
        return http.Response(jsonEncode({'groupId': 'identity-group', 'currencies': [
          {'currency': usdJson, 'settlements': [], 'balances': [
            {'user': bobJson, 'totalMinor': '0', 'breakdown': [],
              'expensesPaidMinor': '0', 'expenseShareMinor': '0', 'incomeReceivedMinor': '0',
              'incomeEntitlementMinor': '0', 'paymentsSentMinor': '0', 'paymentsReceivedMinor': '0'},
          ]},
        ]}), 200);
      }
      return http.Response('Not found', 404);
    });
    addTearDown(session.dispose);
    final group = ExpenseGroup(groupId: 'identity-group', groupName: 'Trip',
      owner: const User(userId: aliceId, username: 'alice', displayName: 'Alice'),
      members: const [User(userId: aliceId, username: 'alice', displayName: 'Alice')],
      currencyMode: CurrencyMode.single, defaultCurrency: usd, currencyLocked: false);
    final searches = <String>[];
    await tester.pumpWidget(MaterialApp(home: Scaffold(body: RecordSearchToolbar(
      repository: session.repository, session: session, group: group,
      query: 'note:unchanged', sort: '-date', onSearch: searches.add, onSort: (_) {}))));
    expect(reports, 0);
    await tester.tap(find.widgetWithText(OutlinedButton, 'Add filter'));
    await tester.pumpAndSettle();
    await tester.tap(find.widgetWithText(DropdownButtonFormField<String>, 'Field'));
    await tester.pumpAndSettle();
    await tester.tap(find.text('member').last);
    await tester.pumpAndSettle();
    expect(reports, 0);
    await tester.tap(find.widgetWithText(OutlinedButton, 'Choose member'));
    await tester.pumpAndSettle();
    expect(reports, 1);
    expect(find.text('Bob (former member)'), findsOneWidget);
    await tester.tap(find.widgetWithText(ListTile, '@bob'));
    await tester.pumpAndSettle();
    await tester.tap(find.widgetWithText(FilledButton, 'Apply to draft'));
    await tester.pumpAndSettle();
    expect(find.text('member: Bob (@bob)'), findsOneWidget);
    expect(tester.widget<TextField>(find.widgetWithText(TextField, 'Search records')).controller!.text,
      'note:unchanged member:$bobId');
    expect(searches, isEmpty);
  });
  testWidgets('amount filter accepts zero and requires explicit currency replacement without committing', (tester) async {
    await tester.binding.setSurfaceSize(const Size(1100, 1800));
    addTearDown(() => tester.binding.setSurfaceSize(null));
    var reportRequests = 0;
    final session = makeSession((request) async {
      if (request.url.path.endsWith('/report')) reportRequests++;
      if (request.url.path.endsWith('/currencies')) return http.Response(jsonEncode([
        usdJson, {'code': 'EUR', 'name': 'Euro', 'minorUnits': 2},
      ]), 200);
      return http.Response('Not found', 404);
    });
    addTearDown(session.dispose);
    final group = ExpenseGroup(groupId: 'amount-group', groupName: 'Trip',
      owner: const User(userId: aliceId, username: 'alice', displayName: 'Alice'),
      members: const [User(userId: aliceId, username: 'alice', displayName: 'Alice')],
      currencyMode: CurrencyMode.single, defaultCurrency: usd, currencyLocked: false);
    final searches = <String>[];
    await tester.pumpWidget(MaterialApp(home: Scaffold(body: RecordSearchToolbar(
      repository: session.repository, session: session, group: group,
      query: r'"literal:colon" currency:USD member:00000000-0000-0000-0000-000000000001',
      sort: '-date', onSearch: searches.add, onSort: (_) {}))));
    expect(find.text('member: Alice (@alice)'), findsOneWidget);
    expect(reportRequests, 0);
    await tester.tap(find.widgetWithText(OutlinedButton, 'Add filter'));
    await tester.pumpAndSettle();
    await tester.tap(find.widgetWithText(DropdownButtonFormField<String>, 'Field'));
    await tester.pumpAndSettle();
    await tester.tap(find.descendant(of: find.byWidgetPredicate((widget) => widget is DropdownMenuItem<String> && widget.value == 'amount'), matching: find.byType(Text)).last);
    await tester.pumpAndSettle();
    await tester.enterText(find.widgetWithText(TextField, 'Value'), '0');
    await tester.tap(find.widgetWithText(DropdownButtonFormField<String>, 'Required amount currency'));
    await tester.pumpAndSettle();
    await tester.tap(find.text('EUR · Euro').last);
    await tester.pumpAndSettle();
    await tester.tap(find.widgetWithText(FilledButton, 'Apply to draft'));
    await tester.pumpAndSettle();
    expect(find.text('Explicitly replace conflicting currency filters.'), findsOneWidget);
    await tester.tap(find.widgetWithText(CheckboxListTile, 'Replace conflicting currency filters explicitly'));
    await tester.tap(find.widgetWithText(FilledButton, 'Apply to draft'));
    await tester.pumpAndSettle();
    expect(searches, isEmpty);
    final draft = tester.widget<TextField>(find.widgetWithText(TextField, 'Search records')).controller!.text;
    expect(draft, contains(r'"literal:colon"'));
    expect(draft, contains('member:$aliceId'));
    expect(draft, contains('amount:=0'));
    expect(draft, contains('currency:EUR'));
    expect(draft, isNot(contains('currency:USD')));
    expect(reportRequests, 0);
  });
  test('record token spans preserve literal colons, escapes and repeated AND terms', () {
    const query = r'"literal:colon" title:"team \"lunch\"" note:"a\\b" title:second amount:0';
    final tokens = tokenizeRecordQuery(query);
    expect(tokens.map((t) => t.field), [null, 'title', 'note', 'title', 'amount']);
    expect(tokens.map((t) => t.value), ['literal:colon', 'team "lunch"', r'a\b', 'second', '0']);
    final changed = query.replaceRange(tokens[3].start, tokens[3].end,
      serializeRecordQueryToken('title', 'new phrase'));
    expect(changed, r'"literal:colon" title:"team \"lunch\"" note:"a\\b" title:"new phrase" amount:0');
    for (final invalid in ['title:"unfinished', 'unknown:value', 'note:""', 'title:', r'note:"bad\q"']) {
      expect(() => tokenizeRecordQuery(invalid), throwsFormatException);
    }
  });

  testWidgets('record draft survives sort, rejects malformed search, and chips change only draft', (tester) async {
    await tester.binding.setSurfaceSize(const Size(1100, 1800));
    addTearDown(() => tester.binding.setSurfaceSize(null));
    final session = roleSession();
    addTearDown(session.dispose);
    final group = ExpenseGroup(groupId: 'query-group', groupName: 'Trip',
      owner: const User(userId: aliceId, username: 'alice', displayName: 'Alice'),
      members: const [User(userId: aliceId, username: 'alice', displayName: 'Alice')],
      currencyMode: CurrencyMode.single, defaultCurrency: usd, currencyLocked: false);
    var applied = 'title:committed';
    var sort = '-date';
    final searches = <String>[];
    await tester.pumpWidget(MaterialApp(home: Scaffold(body: StatefulBuilder(builder: (context, update) =>
      RecordSearchToolbar(repository: session.repository, session: session, group: group,
        query: applied, sort: sort,
        onSearch: (value) => update(() { applied = value; searches.add(value); }),
        onSort: (value) => update(() => sort = value))))));
    final input = find.widgetWithText(TextField, 'Search records');
    await tester.enterText(input, 'note:"unfinished');
    final direction = find.widgetWithText(DropdownButtonFormField<bool>, 'Direction');
    await tester.ensureVisible(direction);
    await tester.tap(direction); await tester.pumpAndSettle();
    await tester.tap(find.text('Ascending').last); await tester.pumpAndSettle();
    expect(sort, 'date');
    expect(applied, 'title:committed');
    expect(tester.widget<TextField>(input).controller!.text, 'note:"unfinished');
    await tester.tap(find.widgetWithText(FilledButton, 'Search'));
    await tester.pumpAndSettle();
    expect(searches, isEmpty);
    expect(find.text('Unterminated quoted value.'), findsOneWidget);
    await tester.enterText(input, r'"literal:colon" title:"team \"lunch\"" note:invoice');
    await tester.pumpAndSettle();
    await tester.tap(find.byTooltip('Remove note filter: invoice'));
    await tester.pumpAndSettle();
    expect(tester.widget<TextField>(input).controller!.text, r'"literal:colon" title:"team \"lunch\"" ');
    expect(searches, isEmpty);
    await tester.tap(find.widgetWithText(FilledButton, 'Search'));
    await tester.pumpAndSettle();
    expect(searches.single, r'"literal:colon" title:"team \"lunch\"" ');
    await tester.tap(find.widgetWithText(TextButton, 'Clear'));
    await tester.pumpAndSettle();
    expect(applied, '');
    expect(tester.widget<TextField>(input).controller!.text, '');
  });

  testWidgets('record 400 retains last successful rows and editable query; sorting does not submit draft', (tester) async {
    await tester.binding.setSurfaceSize(const Size(1100, 1800));
    addTearDown(() => tester.binding.setSurfaceSize(null));
    final requests = <Map<String, String>>[];
    final session = roleSession(records: (request) async {
      if (!request.url.path.endsWith('/records')) return null;
      requests.add(request.url.queryParameters);
      if (request.url.queryParameters['query'] == 'amount:abc') return http.Response('Invalid record query: amount requires currency', 400);
      return http.Response(jsonEncode({'items': [transferFixture()], 'pageInfo': {
        'offset': 0, 'limit': 20, 'total': 1, 'hasNext': false, 'hasPrev': false,
      }}), 200);
    });
    addTearDown(session.dispose);
    await openRoleEditor(tester, session, 'records');
    expect(requests.last['sort'], '-date');
    final input = find.widgetWithText(TextField, 'Search records');
    await tester.enterText(input, 'note:"unfinished');
    final direction = find.widgetWithText(DropdownButtonFormField<bool>, 'Direction');
    await tester.ensureVisible(direction);
    await tester.tap(direction); await tester.pumpAndSettle();
    await tester.tap(find.text('Ascending').last); await tester.pumpAndSettle();
    expect(requests.last['query'] ?? '', '');
    expect(tester.widget<TextField>(input).controller!.text, 'note:"unfinished');
    await tester.enterText(input, 'amount:abc');
    await tester.tap(find.widgetWithText(FilledButton, 'Search'));
    await tester.pumpAndSettle();
    expect(find.textContaining('Previous results are shown below'), findsOneWidget);
    expect(find.text('Payment'), findsOneWidget);
    expect(tester.widget<TextField>(input).controller!.text, 'amount:abc');
    await tester.enterText(input, 'type:transfer');
    await tester.tap(find.widgetWithText(FilledButton, 'Search'));
    await tester.pumpAndSettle();
    expect(requests.last['query'], 'type:transfer');
    expect(find.textContaining('Previous results are shown below'), findsNothing);
  });
  for (final kind in ['expenses', 'incomes', 'transfers']) {
    testWidgets('$kind note-only edit guards leaving and survives failed save and later edits', (tester) async {
      await tester.binding.setSurfaceSize(const Size(1100, 1800));
      addTearDown(() => tester.binding.setSurfaceSize(null));
      const note = '  Invoice 42\n会议 <b>plain</b>  ';
      final transfer = kind == 'transfers';
      final income = kind == 'incomes';
      var saved = transferFixture();
      if (!transfer) {
        saved = {...saved, 'tag': income ? 'IncomeRecord' : 'ExpenseRecord',
          'title': 'Saved record', 'splitMode': 'equal',
          'splits': [{'user': aliceJson, 'value': '1', 'amountMinor': '1200', 'settlementAmountMinor': '1200'}]}
          ..remove('transferTo');
        if (income) saved = {...saved, 'receivedBy': aliceJson}..remove('paidBy');
      }
      var attempts = 0;
      final session = roleSession(records: (request) async {
        if (request.method == 'PUT') {
          attempts++;
          if (attempts == 1) return http.Response('Temporary save failure', 500);
          final body = jsonDecode(request.body) as Map<String, dynamic>;
          saved = {...saved, 'note': (body['note'] as String?)?.trim().isEmpty == true ? null : body['note'],
            'date': body['date']};
          return http.Response(jsonEncode(saved), 200, headers: {'content-type': 'application/json; charset=utf-8'});
        }
        if (request.url.path.endsWith('/records/transfer-id')) return http.Response(jsonEncode(saved), 200, headers: {'content-type': 'application/json; charset=utf-8'});
        return null;
      });
      addTearDown(session.dispose);
      final router = await openRoleEditor(tester, session, '$kind/transfer-id/edit');
      final noteField = find.widgetWithText(TextField, 'Note (optional)');
      final displayedNote = find.byWidgetPredicate((widget) => widget is SelectableText && widget.data == note);
      await tester.ensureVisible(noteField);
      await tester.enterText(noteField, note);
      await tester.ensureVisible(find.widgetWithText(TextButton, 'Cancel'));
      await tester.tap(find.widgetWithText(TextButton, 'Cancel'));
      await tester.pumpAndSettle();
      expect(find.text('Discard unsaved changes?'), findsOneWidget);
      await tester.tap(find.descendant(of: find.byType(AlertDialog), matching: find.text('Cancel')));
      await tester.pumpAndSettle();
      final save = find.widgetWithText(FilledButton, 'Save');
      await tester.ensureVisible(save);
      await tester.tap(save);
      await tester.pumpAndSettle();
      expect(attempts, 1);
      expect(find.text('Temporary save failure'), findsOneWidget);
      expect(tester.widget<TextField>(noteField).controller!.text, note);
      await tester.tap(save);
      await tester.pumpAndSettle();
      expect(displayedNote, findsOneWidget);
      expect(saved['amountMinor'], '1200');
      expect(saved['date'], '2026-10-01');
      router.go('/groups/group-id/$kind/transfer-id/edit');
      await tester.pumpAndSettle();
      expect(tester.widget<TextField>(noteField).controller!.text, note);
      final dateField = find.widgetWithText(TextField, 'Date (YYYY-MM-DD)');
      await tester.ensureVisible(dateField);
      await tester.enterText(dateField, '2026-10-12');
      await tester.ensureVisible(save);
      await tester.tap(save);
      await tester.pumpAndSettle();
      expect(displayedNote, findsOneWidget);
      expect(find.text('2026-10-12'), findsOneWidget);
      router.go('/groups/group-id/$kind/transfer-id/edit');
      await tester.pumpAndSettle();
      await tester.ensureVisible(noteField);
      await tester.enterText(noteField, '');
      await tester.ensureVisible(save);
      await tester.tap(save);
      await tester.pumpAndSettle();
      expect(saved['note'], isNull);
      expect(displayedNote, findsNothing);
      await tester.pumpWidget(const SizedBox.shrink());
    });
  }

  for (final editing in [false, true]) {
    for (final payerFirst in [true, false]) {
      testWidgets('${editing ? 'edit' : 'create'} payment swaps ${payerFirst ? 'payer' : 'recipient'} first and retains failed draft', (tester) async {
        await tester.binding.setSurfaceSize(const Size(1100, 1800));
        addTearDown(() => tester.binding.setSurfaceSize(null));
        var saved = transferFixture();
        var attempts = 0;
        final session = roleSession(records: (request) async {
          if (request.method == 'POST' || request.method == 'PUT') {
            attempts++;
            expect(request.method, editing ? 'PUT' : 'POST');
            expect(request.url.path, editing ? '/api/groups/group-id/records/transfer/transfer-id' : '/api/groups/group-id/records/transfer');
            if (attempts == 1) return http.Response('Temporary save failure', 500);
            final body = jsonDecode(request.body) as Map<String, dynamic>;
            saved = transferFixture(payer: body['byUserId'] as String, recipient: body['toUserId'] as String,
              amount: body['amountMinor'] as String, date: body['date'] as String);
            return http.Response(jsonEncode(saved), 200);
          }
          if (request.url.path.endsWith('/records/transfer-id')) return http.Response(jsonEncode(saved), 200);
          return null;
        });
        addTearDown(session.dispose);
        await openRoleEditor(tester, session, editing ? 'transfers/transfer-id/edit' : 'transfers/new');
        await tester.enterText(find.widgetWithText(TextField, 'Amount'), '23.45');
        await tester.enterText(find.widgetWithText(TextField, 'Date (YYYY-MM-DD)'), '2026-10-12');
        String selected(String label) => tester.widget<UserPickerField>(rolePicker(label)).selectedUserId!;
        expect(selected('Paid by'), aliceId);
        expect(selected('Paid to'), bobId);
        await chooseRole(tester, payerFirst ? 'Paid by' : 'Paid to', payerFirst ? 'bob' : 'alice');
        expect(selected('Paid by'), payerFirst ? bobId : aliceId);
        expect(selected('Paid to'), payerFirst ? bobId : aliceId);
        expect(find.text('Payer and recipient must be different'), findsOneWidget);
        final save = find.widgetWithText(FilledButton, 'Save');
        expect(tester.widget<FilledButton>(save).onPressed, isNull);
        await tester.ensureVisible(save);
        await tester.tap(save);
        await tester.pumpAndSettle();
        expect(attempts, 0);
        await chooseRole(tester, payerFirst ? 'Paid to' : 'Paid by', payerFirst ? 'alice' : 'bob');
        expect(selected('Paid by'), bobId);
        expect(selected('Paid to'), aliceId);
        expect(find.text('Payer and recipient must be different'), findsNothing);
        await tester.ensureVisible(save);
        await tester.tap(save);
        await tester.pumpAndSettle();
        expect(attempts, 1);
        expect(selected('Paid by'), bobId);
        expect(selected('Paid to'), aliceId);
        expect(tester.widget<TextField>(find.widgetWithText(TextField, 'Amount')).controller!.text, '23.45');
        expect(tester.widget<TextField>(find.widgetWithText(TextField, 'Date (YYYY-MM-DD)')).controller!.text, '2026-10-12');
        expect(find.text('Temporary save failure'), findsOneWidget);
        await tester.tap(find.widgetWithText(TextButton, 'Cancel'));
        await tester.pumpAndSettle();
        expect(find.text('Discard unsaved changes?'), findsOneWidget);
        await tester.tap(find.descendant(of: find.byType(AlertDialog), matching: find.text('Cancel')));
        await tester.pumpAndSettle();
        expect(selected('Paid by'), bobId);
        expect(selected('Paid to'), aliceId);
        await tester.tap(save);
        await tester.pumpAndSettle();
        expect(attempts, 2);
        expect(find.text('Payment / Transfer'), findsOneWidget);
        final identities = tester.widgetList<IdentityLabel>(find.byType(IdentityLabel)).toList();
        expect(identities.map((identity) => identity.user.userId), [bobId, aliceId]);
        expect(find.text('2026-10-12'), findsOneWidget);
        expect(saved['amountMinor'], '2345');
        await tester.pumpWidget(const SizedBox.shrink());
      });
    }
  }

  for (final members in <List<Map<String, Object>>>[[], [aliceJson]]) {
    testWidgets('payment needs two active members with roster size ${members.length}', (tester) async {
      final session = roleSession(members: members);
      addTearDown(session.dispose);
      await openRoleEditor(tester, session, 'transfers/new');
      expect(find.text('Payments require at least two active members'), findsOneWidget);
      expect(tester.widget<FilledButton>(find.widgetWithText(FilledButton, 'Save')).onPressed, isNull);
      await tester.pumpWidget(const SizedBox.shrink());
    });
  }

  testWidgets('historical payment endpoint stays labeled and cannot save until active', (tester) async {
    final session = roleSession(members: [aliceJson, {
      'userId': '00000000-0000-0000-0000-000000000003', 'username': 'charlie', 'displayName': 'Charlie',
    }], records: (request) async =>
      request.url.path.endsWith('/records/transfer-id') ? http.Response(jsonEncode(transferFixture()), 200) : null);
    addTearDown(session.dispose);
    await openRoleEditor(tester, session, 'transfers/transfer-id/edit');
    expect(find.descendant(of: rolePicker('Paid to'), matching: find.text('@bob · Bob')), findsOneWidget);
    expect(find.text('Former member'), findsOneWidget);
    expect(find.text('Re-add this member before saving changes'), findsOneWidget);
    expect(tester.widget<FilledButton>(find.widgetWithText(FilledButton, 'Save')).onPressed, isNull);
    await tester.pumpWidget(const SizedBox.shrink());
  });

  for (final kind in ['expenses', 'incomes', 'receipts']) {
    testWidgets('$kind actor and weighted participants remain independent', (tester) async {
      await tester.binding.setSurfaceSize(const Size(1100, 1800));
      addTearDown(() => tester.binding.setSurfaceSize(null));
      final session = roleSession();
      addTearDown(session.dispose);
      await openRoleEditor(tester, session, '$kind/new');
      if (kind == 'receipts') {
        await tester.tap(find.text('Add expense'));
        await tester.pumpAndSettle();
      }
      final fields = tester.widget<SplitFields>(find.byType(SplitFields)).controller;
      await tester.enterText(find.widgetWithText(TextField, 'Title'), 'Independent roles');
      await tester.enterText(find.widgetWithText(TextField, 'Amount'), '12');
      await tester.tap(find.text('Shares'));
      await tester.pumpAndSettle();
      final bob = find.byKey(const ValueKey('candidate:$bobId'));
      final aliceWeight = find.descendant(of: find.byKey(const ValueKey('selected:$aliceId')), matching: find.byType(TextField));
      await tester.ensureVisible(aliceWeight);
      await tester.enterText(aliceWeight, '3');
      final actorLabel = kind == 'incomes' ? 'Received by' : 'Paid by';
      await chooseRole(tester, actorLabel, 'bob');
      expect(fields.actorUserId, bobId);
      expect(fields.toDraft().splits.map((split) => (split.userId, split.value)), [(aliceId, BigInt.from(3)), (bobId, BigInt.one)]);
      await tester.ensureVisible(bob);
      await tester.tap(bob);
      await tester.pumpAndSettle();
      expect(fields.actorUserId, bobId);
      expect(fields.toDraft().splits.single.userId, aliceId);
      expect(fields.toDraft().splits.single.value, BigInt.from(3));
      await chooseRole(tester, actorLabel, 'alice');
      expect(fields.actorUserId, aliceId);
      expect(fields.toDraft().splits.single.userId, aliceId);
      expect(fields.toDraft().splits.single.value, BigInt.from(3));
      await tester.pumpWidget(const SizedBox.shrink());
    });
  }

  for (final removeSecond in [true, false]) {
    testWidgets(removeSecond ? 'receipt removing second preserves first draft'
        : 'receipt removing first preserves remaining draft', (tester) async {
      await tester.binding.setSurfaceSize(const Size(1100, 1800));
      addTearDown(() => tester.binding.setSurfaceSize(null));
      final group = {'groupId': 'group-id', 'groupName': 'Trip',
        'owner': aliceJson, 'members': [aliceJson],
        'currencyMode': 'single', 'defaultCurrency': usdJson, 'currencyLocked': false};
      Map<String, dynamic>? submitted;
      Map<String, Object?>? saved;
      final session = makeSession((request) async {
        if (request.url.path.endsWith('/auth/login')) return signedIn();
        if (request.url.path.endsWith('/currencies')) return http.Response(jsonEncode([usdJson]), 200);
        if (request.url.path.endsWith('/users/me/groups')) {
          return http.Response(jsonEncode({'items': [group], 'pageInfo': {
            'offset': 0, 'limit': 20, 'total': 1, 'hasNext': false, 'hasPrev': false,
          }}), 200);
        }
        if (request.url.path.endsWith('/groups/group-id')) return http.Response(jsonEncode(group), 200);
        if ((request.method == 'POST' && request.url.path.endsWith('/receipts')) ||
            (request.method == 'PUT' && request.url.path.endsWith('/receipts/saved-id'))) {
          submitted = jsonDecode(request.body) as Map<String, dynamic>;
          saved = {'receiptId': 'saved-id', 'groupId': 'group-id',
            'uploadedBy': aliceJson, 'note': submitted!['note'],
            'createdAt': '2026-01-01T00:00:00Z',
            'records': [for (final row in submitted!['records'] as List) {
              'tag': 'ExpenseRecord', 'recordId': 'record-id', 'groupId': 'group-id',
              'title': row['title'], 'amountMinor': row['amountMinor'], 'paidBy': aliceJson,
              'note': row['note'],
              'currency': usdJson, 'settlementCurrency': usdJson,
              'settlementAmountMinor': row['amountMinor'], 'exchangeRate': null,
              'splitMode': row['splitMode'],
              'date': row['date'], 'createdAt': '2026-01-01T00:00:00Z',
              'splits': [{'user': aliceJson, 'value': '1', 'amountMinor': row['amountMinor'], 'settlementAmountMinor': row['amountMinor']}],
            }]};
          return http.Response(jsonEncode(saved), 200, headers: {'content-type': 'application/json; charset=utf-8'});
        }
        if (request.url.path.endsWith('/receipts/saved-id')) return http.Response(jsonEncode(saved), 200, headers: {'content-type': 'application/json; charset=utf-8'});
        return http.Response('Not found', 404);
      });
      addTearDown(session.dispose);
      await session.login('alice', 'password');
      await tester.pumpWidget(JizhangApp(repository: session.repository, session: session));
      await tester.pumpAndSettle();
      GoRouter.of(tester.element(find.text('Create group'))).go('/groups/group-id/receipts/new');
      await tester.pumpAndSettle();
      await tester.enterText(find.widgetWithText(TextField, 'Note (optional)'), 'Keep this note');
      Future<void> add(String title, String amount) async {
        await tester.ensureVisible(find.text('Add expense'));
        await tester.tap(find.text('Add expense'));
        await tester.pumpAndSettle();
        final fields = find.byType(SplitFields).last;
        final titleField = find.descendant(of: fields, matching: find.widgetWithText(TextField, 'Title'));
        await tester.ensureVisible(titleField);
        await tester.enterText(titleField, title);
        final amountField = find.descendant(of: fields, matching: find.widgetWithText(TextField, 'Amount'));
        await tester.ensureVisible(amountField);
        await tester.enterText(amountField, amount);
        final noteField = find.descendant(of: fields, matching: find.widgetWithText(TextField, 'Note (optional)'));
        await tester.ensureVisible(noteField);
        await tester.enterText(noteField, '$title child\n会议 <b>plain</b>');
        await tester.pumpAndSettle();
      }
      await add('First', '25');
      await add('Second', '35');
      final remove = find.text('Remove expense ${removeSecond ? 2 : 1}');
      await tester.ensureVisible(remove);
      await tester.tap(remove);
      await tester.pumpAndSettle();
      expect(tester.takeException(), isNull);
      final survivor = tester.widget<SplitFields>(find.byType(SplitFields)).controller;
      expect(survivor.title.text, removeSecond ? 'First' : 'Second');
      expect(survivor.amount.text, removeSecond ? '25' : '35');
      expect(survivor.note.text, '${removeSecond ? 'First' : 'Second'} child\n会议 <b>plain</b>');
      expect(survivor.actorUserId, aliceId);
      expect(survivor.toDraft().splits.single.userId, aliceId);
      if (!removeSecond) {
        await add('Third', '45');
        await tester.ensureVisible(find.text('Remove expense 2'));
        await tester.tap(find.text('Remove expense 2'));
        await tester.pumpAndSettle();
      }
      final titleField = find.widgetWithText(TextField, 'Title');
      await tester.ensureVisible(titleField);
      await tester.enterText(titleField, 'Survivor edited');
      await tester.ensureVisible(find.text('Save receipt'));
      await tester.tap(find.text('Save receipt'));
      await tester.pumpAndSettle();
      expect(submitted!['note'], 'Keep this note');
      final rows = submitted!['records'] as List;
      expect(rows, hasLength(1));
      expect(rows.single['title'], 'Survivor edited');
      expect(rows.single['note'], '${removeSecond ? 'First' : 'Second'} child\n会议 <b>plain</b>');
      expect(rows.single['amountMinor'], removeSecond ? '2500' : '3500');
      expect(rows.single['currency'], 'USD');
      expect(rows.single['byUserId'], aliceId);
      expect(rows.single['splits'], [{'userId': aliceId, 'value': '1'}]);
      expect(find.text('Keep this note'), findsOneWidget);
      expect(find.byWidgetPredicate((widget) => widget is Text && widget.data == 'Survivor edited'), findsOneWidget);
      GoRouter.of(tester.element(find.text('Keep this note'))).go('/groups/group-id/receipts/saved-id/edit');
      await tester.pumpAndSettle();
      expect(tester.widget<SplitFields>(find.byType(SplitFields)).controller.note.text,
        '${removeSecond ? 'First' : 'Second'} child\n会议 <b>plain</b>');
      final summary = find.byWidgetPredicate((widget) => widget is Text && widget.data == 'Survivor edited');
      await tester.ensureVisible(summary);
      await tester.tap(summary);
      await tester.pumpAndSettle();
      await tester.tap(summary);
      await tester.pumpAndSettle();
      await tester.ensureVisible(find.text('Remove expense 1'));
      await tester.tap(find.text('Remove expense 1'));
      await tester.pumpAndSettle();
      expect(find.byType(SplitFields), findsNothing);
      expect(find.text('Keep this note'), findsOneWidget);
      await tester.ensureVisible(find.text('Save receipt'));
      await tester.tap(find.text('Save receipt'));
      await tester.pumpAndSettle();
      expect(submitted!['records'], isEmpty);
      expect(submitted!['note'], 'Keep this note');
      expect(tester.takeException(), isNull);
      await tester.pumpWidget(const SizedBox.shrink());
    });
  }
  testWidgets('statistics tables include every member and exact large JPY amounts', (tester) async {
    await tester.binding.setSurfaceSize(const Size(1100, 1800));
    addTearDown(() => tester.binding.setSurfaceSize(null));
    const amount = '9007199254740993';
    final session = statisticsSession((request) async {
      final response = jsonDecode(spendingResponse(request, amount).body) as Map<String, dynamic>;
      final currency = (response['currencies'] as List).single as Map<String, dynamic>;
      currency['currency'] = {'code': 'JPY', 'name': 'Japanese Yen', 'minorUnits': 0};
      currency['members'] = [
        {'user': aliceJson, 'paidMinor': amount, 'shareMinor': amount},
        for (var i = 2; i <= 11; i++) {'user': {
          'userId': '00000000-0000-0000-0000-${i.toString().padLeft(12, '0')}',
          'username': 'member$i', 'displayName': 'Member $i',
        }, 'paidMinor': '0', 'shareMinor': '0'},
      ];
      return http.Response(jsonEncode(response), 200);
    });
    addTearDown(() async { await tester.pumpWidget(const SizedBox.shrink()); session.dispose(); });
    final location = Uri.parse(statisticsLocation('2026-01-01', '2026-01-31'));
    final router = await openStatistics(tester, session,
      location.replace(queryParameters: {...location.queryParameters, 'currency': 'JPY'}).toString());
    await tester.pumpAndSettle();
    expect(find.text('Show all'), findsOneWidget);
    final disclosure = find.widgetWithText(ExpansionTile, 'View data').last;
    await tester.ensureVisible(disclosure);
    await tester.tap(find.descendant(of: disclosure, matching: find.text('View data')));
    await tester.pumpAndSettle();
    final data = tester.widget<DataTable>(find.byType(DataTable));
    expect(data.columns.map((column) => (column.label as Text).data),
      ['Member', 'Allocated share', 'Paid']);
    expect(data.rows, hasLength(11));
    final member = (data.rows.last.cells.first.child as ConstrainedBox).child as SelectableText;
    expect(member.data, contains('Member 11'));
    expect(member.data, contains('(former)'));
    expect(((data.rows.first.cells[1].child as ConstrainedBox).child as SelectableText).data,
      'JPY 9007199254740993');
    final uri = router.routeInformationProvider.value.uri;
    router.go(uri.replace(queryParameters: {...uri.queryParameters, 'view': 'personal'}).toString());
    await tester.pumpAndSettle();
    expect(find.text('Total allocated share: JPY 9007199254740993'), findsOneWidget);
    final personalData = find.widgetWithText(ExpansionTile, 'View data').last;
    await tester.ensureVisible(personalData);
    await tester.tap(find.descendant(of: personalData, matching: find.text('View data')));
    await tester.pumpAndSettle();
    final personal = tester.widget<DataTable>(find.byType(DataTable));
    expect(((personal.rows.first.cells[3].child as ConstrainedBox).child as SelectableText).data,
      'JPY 9007199254740993');
    expect(tester.takeException(), isNull);
  });

  testWidgets('late Statistics canonicalization waits for detail to return', (tester) async {
    final delayed = Completer<http.Response>();
    http.Request? statisticsRequest;
    var reports = 0;
    final session = roleSession(records: (request) async {
      if (request.url.path.endsWith('/statistics')) {
        reports++;
        statisticsRequest = request;
        return delayed.future;
      }
      if (request.url.path.endsWith('/records/expense-id')) {
        return http.Response(jsonEncode(expenseFixture()), 200);
      }
      return null;
    });
    addTearDown(session.dispose);
    final router = await openStatistics(tester, session,
      statisticsLocation('2026-01-01', '2026-01-31', currency: false));
    for (var frame = 0; frame < 10 && statisticsRequest == null; frame++) {
      await tester.pump(const Duration(milliseconds: 20));
    }
    expect(statisticsRequest, isNotNull);
    unawaited(router.push<void>('/groups/group-id/records/expense-id'));
    await tester.pumpAndSettle();
    delayed.complete(spendingResponse(statisticsRequest!, '1200'));
    await tester.pumpAndSettle();
    expect(router.routeInformationProvider.value.uri.path, '/groups/group-id/records/expense-id');
    expect(find.text('Shared meal'), findsOneWidget);
    router.pop();
    await tester.pumpAndSettle();
    expect(router.routeInformationProvider.value.uri.path, '/groups/group-id/statistics');
    expect(router.routeInformationProvider.value.uri.queryParameters['currency'], 'USD');
    expect(reports, 1);
  });

  testWidgets('statistics ranks allocated shares and tables open the actual expense', (tester) async {
    await tester.binding.setSurfaceSize(const Size(1100, 1800));
    addTearDown(() => tester.binding.setSurfaceSize(null));
    final opened = <String>[];
    var reports = 0;
    var savedTitle = 'Expense 1';
    final session = roleSession(records: (request) async {
      if (request.method == 'PUT') {
        savedTitle = (jsonDecode(request.body) as Map<String, dynamic>)['title'] as String;
        return http.Response(jsonEncode(expenseFixture(id: 'ranked-1', title: savedTitle, amount: '1000')), 200);
      }
      if (request.url.path.endsWith('/statistics')) {
        reports++;
        final response = jsonDecode(spendingResponse(request, '4500').body) as Map<String, dynamic>;
        final currency = (response['currencies'] as List).single as Map<String, dynamic>;
        currency['expenseCount'] = 6;
        currency['topExpenses'] = [for (var i = 0; i < 5; i++) {
          'recordId': 'ranked-${i + 1}', 'title': i == 0 ? savedTitle : 'Expense ${i + 1}',
          'date': '2026-01-0${i + 1}', 'amountMinor': '${1000 - i * 100}',
        }];
        currency['otherExpensesMinor'] = '500';
        return http.Response(jsonEncode(response), 200);
      }
      if (request.url.path.endsWith('/records/ranked-1')) {
        opened.add(request.url.path);
        return http.Response(jsonEncode({
          'tag': 'ExpenseRecord', 'recordId': 'ranked-1', 'groupId': 'group-id',
          'title': savedTitle, 'note': 'Actual first ranked record', 'amountMinor': '1000',
          'currency': usdJson, 'settlementCurrency': usdJson,
          'settlementAmountMinor': '1000', 'exchangeRate': null, 'paidBy': aliceJson,
          'splitMode': 'equal', 'splits': [{'user': aliceJson, 'value': '1',
            'amountMinor': '1000', 'settlementAmountMinor': '1000'}],
          'date': '2026-01-01', 'createdAt': '2026-01-01T00:00:00Z',
        }), 200);
      }
      return null;
    });
    addTearDown(() async { await tester.pumpWidget(const SizedBox.shrink()); session.dispose(); });
    final location = Uri.parse(statisticsLocation('2026-01-01', '2026-01-31'));
    final router = await openStatistics(tester, session,
      location.replace(queryParameters: {...location.queryParameters, 'view': 'personal'}).toString());
    await tester.pumpAndSettle();
    final sourceUri = router.routeInformationProvider.value.uri;
    final fromDraft = find.widgetWithText(TextField, 'From (inclusive)');
    final toDraft = find.widgetWithText(TextField, 'To (inclusive)');
    await tester.enterText(fromDraft, '2025-12-15');
    await tester.enterText(toDraft, '2026-02-14');
    for (var i = 1; i <= 5; i++) {
      expect(find.text('$i. Expense $i'), findsOneWidget);
    }
    expect(find.text('2026-01-01 · USD 10.00 · 22.22%'), findsOneWidget);
    expect(find.text('Other · USD 5.00 · 11.11%'), findsOneWidget);
    expect(find.text('Total allocated share: USD 45.00'), findsOneWidget);
    final disclosure = find.widgetWithText(ExpansionTile, 'View data').last;
    await tester.ensureVisible(disclosure);
    await tester.tap(find.descendant(of: disclosure, matching: find.text('View data')));
    await tester.pumpAndSettle();
    final dataFinder = find.byType(DataTable);
    final data = tester.widget<DataTable>(dataFinder);
    expect(data.columns.map((column) => (column.label as Text).data),
      ['Rank', 'Expense', 'Date', 'Allocated share', 'Percent', 'Action']);
    String cell(int row, int column) =>
      ((data.rows[row].cells[column].child as ConstrainedBox).child as SelectableText).data!;
    expect([for (var i = 0; i < 7; i++) cell(i, 3)],
      ['USD 10.00', 'USD 9.00', 'USD 8.00', 'USD 7.00', 'USD 6.00', 'USD 5.00', 'USD 45.00']);
    expect(cell(0, 4), '22.22%');
    expect(cell(5, 1), 'Other');
    expect(cell(6, 1), 'Total allocated share');
    expect(cell(5, 5), isEmpty);
    expect(cell(6, 5), isEmpty);
    final action = find.descendant(of: dataFinder, matching: find.widgetWithText(TextButton, 'Open expense')).first;
    await tester.binding.setSurfaceSize(const Size(800, 800));
    await tester.pumpAndSettle();
    await tester.ensureVisible(action);
    final vertical = tester.stateList<ScrollableState>(find.byType(Scrollable))
      .firstWhere((state) => state.position.axis == Axis.vertical).position;
    final horizontal = Scrollable.of(tester.element(action)).position;
    final verticalOffset = vertical.pixels;
    final horizontalOffset = horizontal.pixels;
    final requestsBefore = reports;
    await tester.tap(action);
    await tester.pumpAndSettle();
    expect(router.routeInformationProvider.value.uri.path, '/groups/group-id/records/ranked-1');
    expect(opened, ['/api/groups/group-id/records/ranked-1']);
    expect(find.text('Actual first ranked record'), findsOneWidget);
    await tester.tap(find.text('Back'));
    await tester.pumpAndSettle();
    expect(router.routeInformationProvider.value.uri, sourceUri);
    expect(reports, requestsBefore);
    expect(tester.widget<TextField>(fromDraft).controller!.text, '2025-12-15');
    expect(tester.widget<TextField>(toDraft).controller!.text, '2026-02-14');
    expect(find.byType(DataTable), findsOneWidget);
    expect(vertical.pixels, closeTo(verticalOffset, 1));
    expect(horizontal.pixels, closeTo(horizontalOffset, 1));
    final chartAction = find.widgetWithText(TextButton, 'Open expense').first;
    await tester.ensureVisible(chartAction);
    await tester.tap(chartAction);
    await tester.pumpAndSettle();
    expect(router.routeInformationProvider.value.uri.path, '/groups/group-id/records/ranked-1');
    router.pop();
    await tester.pumpAndSettle();
    expect(router.routeInformationProvider.value.uri, sourceUri);
    expect(reports, requestsBefore);
    expect(find.byType(DataTable), findsOneWidget);
    expect(tester.widget<TextField>(fromDraft).controller!.text, '2025-12-15');
    expect(tester.widget<TextField>(toDraft).controller!.text, '2026-02-14');
    await tester.ensureVisible(chartAction);
    await tester.tap(chartAction);
    await tester.pumpAndSettle();
    await tester.ensureVisible(find.text('Edit'));
    await tester.tap(find.text('Edit'));
    await tester.pumpAndSettle();
    final editTitle = find.widgetWithText(TextField, 'Title');
    await tester.ensureVisible(editTitle);
    await tester.enterText(editTitle, 'Edited ranked meal');
    await tester.ensureVisible(find.widgetWithText(FilledButton, 'Save'));
    await tester.tap(find.widgetWithText(FilledButton, 'Save'));
    await tester.pumpAndSettle();
    expect(router.routeInformationProvider.value.uri.path, '/groups/group-id/records/ranked-1');
    expect(find.text('Edited ranked meal'), findsOneWidget);
    await tester.ensureVisible(find.text('Back'));
    await tester.tap(find.text('Back'));
    await tester.pumpAndSettle();
    expect(router.routeInformationProvider.value.uri, sourceUri);
    expect(reports, requestsBefore + 1);
    expect(find.text('1. Edited ranked meal'), findsOneWidget);
    expect(find.byType(DataTable), findsOneWidget);
    expect(horizontal.pixels, closeTo(horizontalOffset, 1));
    expect(tester.widget<TextField>(fromDraft).controller!.text, '2025-12-15');
    expect(tester.widget<TextField>(toDraft).controller!.text, '2026-02-14');
    expect(tester.takeException(), isNull);
  });

  testWidgets('statistics newest committed range survives a late response', (tester) async {
    final delayed = Completer<http.Response>();
    http.Request? february;
    final session = statisticsSession((request) async {
      if (request.url.queryParameters['from'] == '2026-02-01') {
        february = request;
        return delayed.future;
      }
      return spendingResponse(request, request.url.queryParameters['from'] == '2026-01-01' ? '11100' : '33300');
    });
    addTearDown(() async { await tester.pumpWidget(const SizedBox.shrink()); session.dispose(); });
    final router = await openStatistics(tester, session,
      statisticsLocation('2026-01-01', '2026-01-31'));
    await tester.pumpAndSettle();
    router.go(statisticsLocation('2026-02-01', '2026-02-28'));
    await tester.pump();
    await tester.pump();
    expect(february, isNotNull);
    expect(tester.widgetList<SelectableText>(find.byType(SelectableText)).map((w) => w.data), isNot(contains('USD 111.00')));
    router.go(statisticsLocation('2026-03-01', '2026-03-31'));
    await tester.pumpAndSettle();
    delayed.complete(spendingResponse(february!, '22200'));
    await tester.pumpAndSettle();
    final values = tester.widgetList<SelectableText>(find.byType(SelectableText)).map((w) => w.data);
    expect(values, contains('USD 333.00'));
    expect(values, isNot(contains('USD 222.00')));
    expect(router.routeInformationProvider.value.uri.queryParameters['from'], '2026-03-01');
  });

  testWidgets('statistics retry preserves committed range and unsaved draft', (tester) async {
    final requests = <Uri>[];
    final session = statisticsSession((request) async {
      requests.add(request.url);
      return requests.length == 1 ? http.Response('Unavailable', 503) : spendingResponse(request, '12300');
    });
    addTearDown(() async { await tester.pumpWidget(const SizedBox.shrink()); session.dispose(); });
    await openStatistics(tester, session,
      statisticsLocation('2026-01-01', '2026-01-31', currency: false));
    await tester.pumpAndSettle();
    final to = find.widgetWithText(TextField, 'To (inclusive)');
    await tester.enterText(to, '2026-02-28');
    await tester.ensureVisible(find.text('Retry'));
    await tester.tap(find.text('Retry'));
    await tester.pumpAndSettle();
    expect(tester.widget<TextField>(to).controller!.text, '2026-02-28');
    expect(requests.every((uri) => uri.queryParameters['to'] == '2026-01-31'), isTrue);
    expect(tester.widgetList<SelectableText>(find.byType(SelectableText)).map((w) => w.data), contains('USD 123.00'));
  });

  testWidgets('statistics 401 clears private values and rejects a late response', (tester) async {
    final delayed = Completer<http.Response>();
    http.Request? february;
    final session = statisticsSession((request) async {
      final from = request.url.queryParameters['from'];
      if (from == '2026-02-01') { february = request; return delayed.future; }
      if (from == '2026-03-01') return http.Response('Not authorized', 401);
      return spendingResponse(request, '11100');
    });
    addTearDown(() async { await tester.pumpWidget(const SizedBox.shrink()); session.dispose(); });
    final router = await openStatistics(tester, session,
      statisticsLocation('2026-01-01', '2026-01-31'));
    await tester.pumpAndSettle();
    expect(tester.widgetList<SelectableText>(find.byType(SelectableText)).map((w) => w.data), contains('USD 111.00'));
    router.go(statisticsLocation('2026-02-01', '2026-02-28'));
    await tester.pump();
    await tester.pump();
    expect(february, isNotNull);
    router.go(statisticsLocation('2026-03-01', '2026-03-31'));
    await tester.pumpAndSettle();
    delayed.complete(spendingResponse(february!, '22200'));
    await tester.pumpAndSettle();
    expect(session.isAuthenticated, isFalse);
    expect(session.accessToken, isNull);
    expect(router.routeInformationProvider.value.uri.path, '/login');
    expect(find.byType(SelectableText), findsNothing);
    expect(find.text('Private expense'), findsNothing);
  });

  testWidgets('statistics summary and full bucket table preserve financial meanings', (tester) async {
    final semantics = tester.ensureSemantics();
    try {
      final session = statisticsSession((request) async => spendingResponse(request, '12300'));
      addTearDown(() async { await tester.pumpWidget(const SizedBox.shrink()); session.dispose(); });
      await openStatistics(tester, session,
        statisticsLocation('2026-01-01', '2026-01-31'));
      await tester.pumpAndSettle();
      expect(tester.getSemantics(find.text('Group spending')).getSemanticsData().label, contains('USD 123.00'));
      final table = find.widgetWithText(ExpansionTile, 'View data').first;
      await tester.ensureVisible(table);
      await tester.tap(find.descendant(of: table, matching: find.text('View data')));
      await tester.pumpAndSettle();
      final data = tester.widget<DataTable>(find.byType(DataTable));
      expect(data.columns.map((column) => (column.label as Text).data), [
        'Start', 'End', 'Group expenses', 'Expense count', 'Group income',
        'Income count', 'Group net', 'Selected member share', 'Selected member paid',
      ]);
      final values = data.rows.single.cells.map((cell) =>
        ((cell.child as ConstrainedBox).child as SelectableText).data).toList();
      expect(values, ['2026-01-01', '2026-01-31', 'USD 123.00', '1',
        'USD 0.00', '0', 'USD -123.00', 'USD 123.00', 'USD 123.00']);
    } finally {
      semantics.dispose();
    }
  });

  testWidgets('income-only activity keeps exact group totals in Personal view', (tester) async {
    await tester.binding.setSurfaceSize(const Size(1100, 1800));
    addTearDown(() => tester.binding.setSurfaceSize(null));
    final semantics = tester.ensureSemantics();
    try {
      final session = statisticsSession((request) async {
        final from = request.url.queryParameters['from']!;
        final to = request.url.queryParameters['to']!;
        return http.Response(jsonEncode({
          'groupId': 'group-id', 'from': from, 'to': to, 'bucket': 'month',
          'user': aliceJson, 'participants': [aliceJson], 'currencies': [{
            'currency': usdJson, 'totalMinor': '0', 'expenseCount': 0,
            'incomeMinor': '9007199254740993', 'incomeCount': 1, 'netMinor': '9007199254740993',
            'paidMinor': '0', 'shareMinor': '0', 'otherExpensesMinor': '0',
            'buckets': [{'start': from, 'end': to, 'totalMinor': '0', 'expenseCount': 0,
              'incomeMinor': '9007199254740993', 'incomeCount': 1, 'netMinor': '9007199254740993',
              'paidMinor': '0', 'shareMinor': '0'}],
            'members': [], 'topExpenses': [],
          }],
        }), 200);
      });
      addTearDown(() async { await tester.pumpWidget(const SizedBox.shrink()); session.dispose(); });
      final router = await openStatistics(tester, session,
        statisticsLocation('2026-01-01', '2026-01-31'));
      await tester.pumpAndSettle();
      void expectIncomeSummary() {
        expect(tester.getSemantics(find.text('Group income')).getSemanticsData().label,
          contains('USD 90071992547409.93'));
        expect(tester.getSemantics(find.text('Net (income − expenses)')).getSemanticsData().label,
          contains('USD 90071992547409.93'));
        expect(tester.getSemantics(find.text('Income count')).getSemanticsData().label, contains('1'));
        expect(tester.getSemantics(find.text('Group spending')).getSemanticsData().label, contains('USD 0.00'));
      }
      expectIncomeSummary();
      final uri = router.routeInformationProvider.value.uri;
      router.go(uri.replace(queryParameters: {...uri.queryParameters, 'view': 'personal'}).toString());
      await tester.pumpAndSettle();
      expectIncomeSummary();
      expect(tester.getSemantics(find.text('Your expense share')).getSemanticsData().label, contains('USD 0.00'));
      expect(find.text('No allocated spending in this date range'), findsOneWidget);
      expect(find.text('Open expense'), findsNothing);
      final personalData = find.widgetWithText(ExpansionTile, 'View data').last;
      await tester.ensureVisible(personalData);
      await tester.tap(find.descendant(of: personalData, matching: find.text('View data')));
      await tester.pumpAndSettle();
      final data = tester.widget<DataTable>(find.byType(DataTable));
      expect(data.rows, hasLength(1));
      expect(find.text('Total allocated share'), findsOneWidget);
      expect(find.text('0.00%'), findsOneWidget);
      expect(tester.takeException(), isNull);
    } finally {
      semantics.dispose();
    }
  });

  testWidgets('profile discard cancels safely then replaces the editor', (tester) async {
    final session = makeSession((request) async {
      if (request.url.path.endsWith('/auth/login')) return signedIn();
      if (request.url.path.endsWith('/users/me')) {
        return http.Response(jsonEncode({
          'user': aliceJson, 'email': 'alice@example.test', 'emailVerified': false,
        }), 200);
      }
      if (request.url.path.endsWith('/users/me/authentication')) {
        return http.Response(jsonEncode({
          'hasPassword': true, 'identity': null, 'sessionMethod': 'password',
          'authenticatedAt': '2026-10-17T12:00:00Z', 'oidcReauthenticatedAt': null,
          'providerEnabled': false, 'canSignOutProvider': false,
        }), 200);
      }
      if (request.url.path.endsWith('/users/me/groups')) {
        return http.Response(jsonEncode({
          'items': [],
          'pageInfo': {'offset': 0, 'limit': 20, 'total': 0, 'hasNext': false, 'hasPrev': false},
        }), 200);
      }
      return http.Response('Not found', 404);
    });
    addTearDown(session.dispose);
    await session.login('alice', 'password');
    await tester.pumpWidget(JizhangApp(repository: session.repository, session: session));
    await tester.pumpAndSettle();
    await tester.tap(find.byIcon(Icons.account_circle_outlined));
    await tester.pumpAndSettle();
    await tester.enterText(find.byType(TextFormField).at(1), 'Alice draft');
    await tester.tap(find.text('Back to groups'));
    await tester.pumpAndSettle();
    expect(find.byType(AlertDialog), findsOneWidget);
    await tester.tap(find.descendant(of: find.byType(AlertDialog), matching: find.text('Cancel')));
    await tester.pumpAndSettle();
    expect(find.text('Alice draft'), findsOneWidget);
    await tester.tap(find.text('Back to groups'));
    await tester.pumpAndSettle();
    await tester.tap(find.text('Discard'));
    await tester.pumpAndSettle();
    expect(find.text('Create group'), findsOneWidget);
    expect(FormNavigation.active, isNull);
    await tester.pumpWidget(const SizedBox.shrink());
  });

  test('protected 401 clears session and pending dirty form irreversibly', () async {
    final session = makeSession((request) async =>
        request.url.path.endsWith('/auth/login')
            ? signedIn()
            : http.Response('Not authorized', 401));
    addTearDown(session.dispose);
    await session.login('alice', 'password');
    final form = FormController(session);
    addTearDown(form.dispose);
    final completion = Completer<void>();
    form.markDirty();
    final save = form.save(() => completion.future);
    final before = session.generation;
    await session.refreshAccount();
    expect(session.isAuthenticated, isFalse);
    expect(session.accessToken, isNull);
    expect(session.user, isNull);
    expect(session.generation, greaterThan(before));
    expect(session.message, 'Your session expired. Sign in again.');
    expect(form.dirty, isFalse);
    expect(form.pending, isFalse);
    expect(form.error, isNull);
    completion.completeError(StateError('late save failure'));
    expect(await save, isFalse);
    expect(form.error, isNull);
  });

  test('failed save keeps dirty state and canceling exit preserves it', () async {
    final session = makeSession((_) async => signedIn());
    addTearDown(session.dispose);
    final form = FormController(session);
    addTearDown(form.dispose);
    form.markDirty();
    final saved = await form.save(() async {
      throw StateError('save failed');
    });
    expect(saved, isFalse);
    expect(form.dirty, isTrue);
    expect(form.pending, isFalse);
    expect(form.error, isNotNull);
    expect(await form.canLeave(() async => false), isFalse);
    expect(form.dirty, isTrue);
    expect(await form.canLeave(() async => true), isTrue);
    expect(form.dirty, isFalse);
  });

  test('pristine pending saves block navigation without prompting', () async {
    final session = makeSession((_) async => signedIn());
    addTearDown(session.dispose);
    final form = FormController(session);
    addTearDown(form.dispose);
    final completion = Completer<void>();
    addTearDown(() { if (!completion.isCompleted) completion.complete(); });
    final save = form.save(() => completion.future);
    var prompts = 0;
    Future<bool> confirm() async { prompts++; return true; }
    expect(await form.canLeave(confirm), isFalse);
    expect(prompts, 0);
    completion.complete();
    expect(await save, isTrue);
    expect(await form.canLeave(confirm), isTrue);
    expect(prompts, 0);
  });

  test('logout retires local state before server revocation completes', () async {
    final response = Completer<http.Response>();
    final started = Completer<http.Request>();
    final session = makeSession((request) {
      if (request.url.path.endsWith('/login')) return Future.value(signedIn());
      started.complete(request);
      return response.future;
    });
    addTearDown(session.dispose);
    await session.login('alice', 'password');
    final form = FormController(session)..markDirty();
    addTearDown(form.dispose);
    final logout = session.logout();
    expect(session.isAuthenticated, isFalse);
    expect(form.dirty, isFalse);
    final request = await started.future;
    expect(request.url, Uri.parse('https://example.test/api/users/me/logout'));
    expect(request.method, 'POST');
    expect(request.headers['Authorization'], 'Bearer test-token');
    expect(request.followRedirects, isFalse);
    response.completeError(http.ClientException('offline'));
    await logout;
    expect(session.status, SessionStatus.unauthenticated);
    expect(session.message, 'Signed out on this device; server session revocation could not be confirmed.');
  });

  for (final provider in [false, true]) {
    for (final failure in [false, true]) {
      test('late ${provider ? 'provider' : 'local'} logout ${failure ? 'failure' : 'success'} leaves a newer login untouched', () async {
        final response = Completer<http.Response>();
        final browser = LogoutTestBrowser();
        final session = makeSession((request) async {
          if (request.url.path.endsWith('/logout')) return response.future;
          final username = (jsonDecode(request.body) as Map)['loginIdentifier'];
          return http.Response(jsonEncode({
            'accessToken': 'token-$username',
            'loginUser': username == 'bob' ? bobJson : aliceJson,
          }), 200);
        }, authBrowser: browser);
        addTearDown(session.dispose);
        await session.login('alice', 'password');
        final logout = session.logout(provider: provider);
        expect(session.isAuthenticated, isFalse);
        await session.login('bob', 'password');
        response.complete(failure ? http.Response('Not authorized', 401) :
          provider ? http.Response(jsonEncode({'logoutUrl': 'https://example.test/api/auth/oidc/logout?token=handoff'}), 200) :
          http.Response('', 204));
        await logout;
        expect(session.user!.userId, bobId);
        expect(session.accessToken, 'token-bob');
        expect(session.message, isNull);
        expect(browser.destinations, isEmpty);
      });
    }
  }

  test('provider logout navigates only to the configured backend handoff', () async {
    final browser = LogoutTestBrowser();
    var external = false;
    final session = makeSession((request) async {
      if (request.url.path.endsWith('/login')) return signedIn();
      return http.Response(jsonEncode({'logoutUrl': external
        ? 'https://provider.test/end-session?token=handoff'
        : 'https://example.test/api/auth/oidc/logout?token=handoff'}), 200);
    }, authBrowser: browser);
    addTearDown(session.dispose);
    await session.login('alice', 'password');
    await session.logout(provider: true);
    expect(session.isAuthenticated, isFalse);
    expect(browser.destinations, [Uri.parse('https://example.test/api/auth/oidc/logout?token=handoff')]);
    browser.destinations.clear();
    external = true;
    await session.login('alice', 'password');
    await session.logout(provider: true);
    expect(session.isAuthenticated, isFalse);
    expect(browser.destinations, isEmpty);
    expect(session.message, isNotNull);
  });

  test('unavailable provider logout remains signed out with an explicit error', () async {
    final browser = LogoutTestBrowser();
    final session = makeSession((request) async => request.url.path.endsWith('/login')
      ? signedIn() : http.Response('Provider sign-out is unavailable', 503), authBrowser: browser);
    addTearDown(session.dispose);
    await session.login('alice', 'password');
    await session.logout(provider: true);
    expect(session.isAuthenticated, isFalse);
    expect(session.status, SessionStatus.unauthenticated);
    expect(session.message, 'Signed out on this device. Provider sign-out is unavailable');
    expect(browser.destinations, isEmpty);
  });

  test('successful account deletion clears state without a logout request', () async {
    var logoutRequests = 0;
    final session = makeSession((request) async {
      if (request.url.path.endsWith('/login')) return signedIn();
      if (request.url.path.endsWith('/logout')) logoutRequests++;
      return http.Response('', 204);
    });
    addTearDown(session.dispose);
    await session.login('alice', 'password');
    await session.deleteAccount();
    expect(session.isAuthenticated, isFalse);
    expect(session.message, isNull);
    expect(logoutRequests, 0);
  });

  test('logout suppresses a late successful sign in', () async {
    final response = Completer<http.Response>();
    final session = makeSession((_) => response.future);
    addTearDown(session.dispose);
    final login = session.login('alice', 'password');
    await session.logout();
    response.complete(signedIn());
    await login;
    expect(session.status, SessionStatus.unauthenticated);
    expect(session.accessToken, isNull);
  });

  test('registration success and login failure return to sign in identity', () async {
    final session = makeSession((request) async {
      if (request.url.path.endsWith('/register')) {
        return http.Response(jsonEncode(aliceJson), 200);
      }
      return http.Response('Invalid credentials', 401);
    });
    addTearDown(session.dispose);
    await session.register('alice', 'Alice', 'alice@example.test', 'password');
    expect(session.isAuthenticated, isFalse);
    expect(session.status, SessionStatus.error);
    expect(session.registeredUsername, 'alice');
    expect(session.message, 'Invalid credentials');
  });
  test('late account refresh cannot replace a newly signed-in identity', () async {
    final response = Completer<http.Response>();
    final session = makeSession((request) async {
      if (request.url.path.endsWith('/me')) return response.future;
      final username = (jsonDecode(request.body) as Map)['loginIdentifier'];
      return http.Response(jsonEncode({
        'accessToken': 'token-$username',
        'loginUser': {'userId': '$username-id', 'username': username, 'displayName': username},
      }), 200);
    });
    addTearDown(session.dispose);
    await session.login('alice', 'password');
    final refresh = session.refreshAccount();
    await session.login('bob', 'password');
    response.complete(http.Response(jsonEncode({'user': aliceJson, 'email': 'alice@example.test', 'emailVerified': false}), 200));
    await refresh;
    expect(session.user!.username, 'bob');
    expect(session.accessToken, 'token-bob');
    expect(session.status, SessionStatus.authenticated);
  });

  test('profile save updates identity without replacing the session', () async {
    final session = makeSession((request) async {
      if (request.url.path.endsWith('/login')) return signedIn();
      return http.Response(jsonEncode({
        'user': request.method == 'PUT'
            ? {'userId': aliceId, 'username': 'renamed', 'displayName': 'New name'}
            : aliceJson,
        'email': 'alice@example.test', 'emailVerified': false,
      }), 200);
    });
    addTearDown(session.dispose);
    await session.login('alice', 'password');
    await session.refreshAccount();
    final generation = session.generation;
    await session.updateProfile('renamed', 'New name');
    expect(session.user!.userId, aliceId);
    expect(session.user!.username, 'renamed');
    expect(session.user!.displayName, 'New name');
    expect(session.profile!.email, 'alice@example.test');
    expect(session.profile!.emailVerified, isFalse);
    expect(session.accessToken, 'test-token');
    expect(session.generation, generation);
  });

  test('profile conflict retains loaded identity and unsaved form', () async {
    final session = makeSession((request) async {
      if (request.url.path.endsWith('/login')) return signedIn();
      if (request.method == 'PUT') return http.Response('Username is already in use', 409);
      return http.Response(jsonEncode({'user': aliceJson, 'email': 'alice@example.test', 'emailVerified': false}), 200);
    });
    addTearDown(session.dispose);
    await session.login('alice', 'password');
    await session.refreshAccount();
    final profile = session.profile;
    final form = FormController(session);
    addTearDown(form.dispose);
    form.markDirty();
    expect(await form.save(() => session.updateProfile('taken', 'Changed')), isFalse);
    expect(form.dirty, isTrue);
    expect(form.error, 'Username is already in use');
    expect(session.profile, same(profile));
    expect(session.user!.username, 'alice');
  });

  test('late profile save cannot restore a signed-out identity', () async {
    final response = Completer<http.Response>();
    final session = makeSession((request) async {
      if (request.method == 'PUT') return response.future;
      if (request.url.path.endsWith('/logout')) return http.Response('', 204);
      return signedIn();
    });
    addTearDown(session.dispose);
    await session.login('alice', 'password');
    final save = session.updateProfile('renamed', 'Changed');
    await session.logout();
    response.complete(http.Response(jsonEncode({'user': aliceJson, 'email': 'alice@example.test', 'emailVerified': false}), 200));
    await save;
    expect(session.user, isNull);
    expect(session.profile, isNull);
    expect(session.accessToken, isNull);
  });

  test('uncertain mutation preserves draft and does not replay submission', () async {
    var submissions = 0;
    final session = makeSession((request) async {
      if (request.url.path.endsWith('/login')) return signedIn();
      submissions++;
      throw http.ClientException('connection lost');
    });
    addTearDown(session.dispose);
    await session.login('alice', 'password');
    final form = FormController(session);
    addTearDown(form.dispose);
    form.markDirty();
    expect(await form.save(() async {
      await session.repository.createGroup('Unsaved trip');
    }), isFalse);
    expect(form.dirty, isTrue);
    expect(form.error, contains('Check refreshed data before submitting again'));
    expect(submissions, 1);
  });


  test('network account refresh retains session but account 404 expires it', () async {
    var missing = false;
    final session = makeSession((request) async {
      if (request.url.path.endsWith('/login')) return signedIn();
      if (missing) return http.Response('User not found', 404);
      throw http.ClientException('offline');
    });
    addTearDown(session.dispose);
    await session.login('alice', 'password');
    await session.refreshAccount();
    expect(session.isAuthenticated, isTrue);
    expect(session.status, SessionStatus.error);
    expect(session.message, contains('Could not reach the server'));
    missing = true;
    await session.refreshAccount();
    expect(session.isAuthenticated, isFalse);
    expect(session.user, isNull);
  });

  test('shares enforce UUID uniqueness, positive totals and Int16 boundaries', () {
    final total = BigInt.from(100);
    SplitDraft split(String id, int value) => SplitDraft(userId: id, value: BigInt.from(value));
    String? validate(List<SplitDraft> values) => validateSplits(SplitMode.shares, total, values);
    expect(validate([]), isNotNull);
    expect(validate([split(aliceId, 1), split(aliceId, 1)]), isNotNull);
    expect(validate([split(aliceId, 0), split(bobId, 0)]), isNotNull);
    expect(validate([split(aliceId, 32767), split(bobId, 32767)]), isNull);
    expect(validate([split(aliceId, 32768)]), isNotNull);
    expect(validate([split(aliceId, -1), split(bobId, 2)]), isNotNull);
    expect(validate([split(aliceId, 0), split(bobId, 1)]), isNull);
  });

  test('amount validation rejects nonpositive and excess-precision inputs', () {
    for (final value in ['NaN', 'Infinity', '1e309', '0', '-1', 'no', '90.123456']) {
      expect(validateAmount(value, usd), isNotNull, reason: value);
    }
    expect(validateAmount('90.12', usd), isNull);
  });

  test('settlement payment requires both participant IDs to be active', () {
    final alice = User(userId: aliceId, username: 'alice', displayName: 'Alice');
    final bob = User(userId: bobId, username: 'bob', displayName: 'Bob');
    final payment = Settlement(fromUser: bob, toUser: alice, amountMinor: BigInt.from(6000));
    ExpenseGroup group(List<User> members) => ExpenseGroup(
      groupId: 'group', groupName: 'Trip', owner: alice, members: members,
      currencyMode: CurrencyMode.single, defaultCurrency: usd, currencyLocked: false,
    );
    expect(canRecordPayment(group([alice, bob]), payment), isTrue);
    expect(canRecordPayment(group([alice]), payment), isFalse);
    expect(canRecordPayment(group([bob]), payment), isFalse);
    expect(canRecordPayment(group([
      alice, User(userId: 'other-id', username: 'bob', displayName: 'Bob'),
    ]), payment), isFalse);
  });
}
