import 'dart:async';
import 'dart:convert';

import 'package:flutter/material.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:flutter/services.dart';
import 'package:go_router/go_router.dart';
import 'package:http/http.dart' as http;
import 'package:http/testing.dart';
import 'package:package_info_plus/package_info_plus.dart';
import 'package:jizhang_client/app.dart';
import 'package:jizhang_client/core/api_client.dart';
import 'package:jizhang_client/core/auth_browser.dart';
import 'package:jizhang_client/core/auth_flow_controller.dart';
import 'package:jizhang_client/core/jizhang_repository.dart';
import 'package:jizhang_client/core/models.dart';
import 'package:jizhang_client/core/session_controller.dart';
import 'package:jizhang_client/features/auth/auth_screen.dart';
import 'package:jizhang_client/features/auth/invitation_screen.dart';
import 'package:jizhang_client/features/auth/oidc_callback_screen.dart';
import 'package:jizhang_client/features/account/account_screen.dart';
import 'package:jizhang_client/features/account/account_security_section.dart';

const _alice = {'userId': '00000000-0000-0000-0000-000000000001', 'username': 'alice', 'displayName': 'Alice'};
const _bob = {'userId': '00000000-0000-0000-0000-000000000002', 'username': 'bob', 'displayName': 'Bob'};
const _flowId = '00000000-0000-0000-0000-000000000010';
final _now = DateTime.utc(2026, 10, 17, 12);
final _verifier = List.filled(43, 'v').join();
final _handoff = List.filled(43, 'h').join();
final _continuation = List.filled(43, 'c').join();
Uri get _callback => Uri(path: '/auth/oidc/callback', queryParameters: {'flow': _flowId, 'code': _handoff});
Map<String, Object?> _login([Map<String, String> user = _alice]) => {'accessToken': 'application-${user['username']}', 'loginUser': user};
Map<String, Object?> _registration({bool canRegister = true}) => {
  'status': 'registration_required', 'continuationToken': _continuation, 'canRegister': canRegister,
  'profile': {'username': 'alice', 'displayName': 'Alice', 'email': 'alice@example.test', 'emailVerified': true},
};
Map<String, Object?> _confirmation() => {
  'status': 'link_confirmation_required', 'continuationToken': _continuation, 'target': _alice,
  'identity': {'issuer': 'https://provider.example.test/', 'subject': 'synthetic-alice',
    'providerName': 'Example provider', 'claimedEmail': 'alice@example.test', 'claimedEmailVerified': true},
};
http.Response _json(Object? value) => http.Response(jsonEncode(value), 200);

class _Browser implements AuthBrowser {
  bool enabled = true;
  bool storageDenied = false;
  bool navigationDenied = false;
  Map<String, String>? stored;
  final replacements = <Uri>[];
  final navigations = <Uri>[];
  @override
  bool get supported => enabled;
  @override
  Future<String> newVerifier() async => _verifier;
  @override
  Future<String> challenge(String verifier) async => List.filled(43, 's').join();
  @override
  void savePending(Map<String, String> value) {
    if (storageDenied) throw StateError('Storage unavailable');
    stored = Map.of(value);
  }
  @override
  Map<String, String>? readPending() => stored == null ? null : Map.of(stored!);
  @override
  void clearPending() => stored = null;
  @override
  void replaceUrl(Uri url) => replacements.add(url);
  @override
  void navigate(Uri url) {
    if (navigationDenied) throw StateError('Navigation unavailable');
    navigations.add(url);
  }
  void seed({String flowId = _flowId, DateTime? createdAt, String returnTo = '/groups', String purpose = 'login'}) {
    stored = {'flowId': flowId, 'browserVerifier': _verifier, 'returnTo': returnTo,
      'purpose': purpose, 'createdAt': (createdAt ?? _now).toIso8601String()};
  }
}

class _Harness {
  _Harness(Future<http.Response> Function(http.Request) handler) {
    final api = ApiClient(baseUri: Uri.parse('https://app.example.test/api/'),
      httpClient: MockClient((request) { requests.add(request); return handler(request); }),
      accessToken: () => session.accessToken);
    repository = JizhangRepository(api);
    session = SessionController(repository, authBrowser: browser);
    flow = AuthFlowController(session: session, clock: () => _now);
  }
  final browser = _Browser();
  final requests = <http.Request>[];
  late final JizhangRepository repository;
  late final SessionController session;
  late final AuthFlowController flow;
  void dispose() { flow.dispose(); session.dispose(); }
  Iterable<http.Request> get exchanges => requests.where((r) => r.url.path.endsWith('/exchange'));
}

Map<String, Object?> _authentication({bool hasPassword = true, bool linked = false,
  bool providerEnabled = true, DateTime? verifiedAt, String method = 'password'}) => {
  'hasPassword': hasPassword,
  'identity': linked ? {...(_confirmation()['identity'] as Map<String, Object?>),
    'linkedAt': _now.toIso8601String(), 'lastLoginAt': _now.toIso8601String()} : null,
  'sessionMethod': method, 'authenticatedAt': DateTime.now().toUtc().toIso8601String(),
  'oidcReauthenticatedAt': verifiedAt?.toIso8601String(),
  'providerEnabled': providerEnabled, 'canSignOutProvider': linked && providerEnabled,
};

http.Response _accountResponse(http.Request request, Map<String, Object?> authentication) {
  if (request.url.path.endsWith('/login')) return _json(_login());
  if (request.url.path.endsWith('/authentication')) return _json(authentication);
  if (request.method == 'GET' && request.url.path.endsWith('/users/me')) return _json({
    'user': _alice, 'email': 'alice@example.test', 'emailVerified': true,
  });
  if (request.url.path.endsWith('/users/me/groups')) return _json({
    'items': [], 'pageInfo': {'offset': 0, 'limit': 20, 'total': 0, 'hasNext': false, 'hasPrev': false},
  });
  if (request.url.path.endsWith('/options')) return _json({
    'localRegistrationEnabled': true, 'ssoRegistrationEnabled': true,
    'ssoEnabled': true, 'providerName': 'Example provider',
  });
  return http.Response('Unexpected account request', 404);
}

Future<GoRouter> _openAccount(WidgetTester tester, _Harness h) async {
  addTearDown(h.dispose);
  addTearDown(() => tester.pumpWidget(const SizedBox.shrink()));
  await h.session.login('alice', 'password');
  await tester.pumpWidget(JizhangApp(repository: h.repository, session: h.session));
  await tester.pumpAndSettle();
  await tester.tap(find.byIcon(Icons.account_circle_outlined));
  await tester.pumpAndSettle();
  return GoRouter.of(tester.element(find.byType(AccountScreen)));
}

Finder _profileField(String label) => find.widgetWithText(TextFormField, label);

Finder _accountDisclosure(String title) => find.byWidgetPredicate((widget) => widget is ExpansionTile &&
  widget.title is Text && (widget.title as Text).data == title);

Future<void> _toggleDisclosure(WidgetTester tester, String title) async {
  final tile = _accountDisclosure(title);
  final label = find.descendant(of: tile, matching: find.text(title)).first;
  await tester.ensureVisible(label);
  await tester.tap(label);
  await tester.pumpAndSettle();
}

Future<void> _tapAccountAction(WidgetTester tester, String label) async {
  final action = find.ancestor(of: find.text(label), matching:
    find.byWidgetPredicate((widget) => widget is ButtonStyleButton));
  await tester.ensureVisible(action);
  await tester.tap(action);
  await tester.pumpAndSettle();
}

Future<GoRouter> _openAuth(WidgetTester tester, _Harness h, {bool register = false}) async {
  addTearDown(h.dispose);
  addTearDown(() => tester.pumpWidget(const SizedBox.shrink()));
  await tester.pumpWidget(JizhangApp(repository: h.repository, session: h.session));
  await tester.pumpAndSettle();
  final router = GoRouter.of(tester.element(find.byType(AuthScreen)));
  if (register) { router.go('/register'); await tester.pumpAndSettle(); }
  await tester.enterText(_profileField(register ? 'Username' : 'Username or email'), 'alice-local');
  if (register) {
    await tester.enterText(_profileField('Display name'), 'Alice local draft');
    await tester.enterText(_profileField('Email'), 'alice-local@example.test');
  }
  await tester.enterText(_profileField('Password'), 'alice-local-password');
  return router;
}

void _expectAuthLocked(WidgetTester tester, bool register) {
  expect(find.text('Please wait…'), findsOneWidget);
  expect(tester.widget<FilledButton>(find.widgetWithText(FilledButton, 'Please wait…')).enabled, isFalse);
  expect(tester.widget<TextButton>(find.widgetWithText(TextButton,
    register ? 'Already have an account? Sign in' : 'Create an account')).enabled, isFalse);
  expect(tester.widget<OutlinedButton>(find.widgetWithText(OutlinedButton, 'Continue with Example provider')).enabled, isFalse);
}

void main() {
  setUp(() => PackageInfo.setMockInitialValues(
    appName: 'Jizhang', packageName: 'jizhang_client',
    version: '', buildNumber: '', buildSignature: '',
  ));
  for (final purpose in AuthFlowPurpose.values) {
    test('SSO redirect handoff retains the ${purpose.name} operation until canceled', () async {
      final response = Completer<http.Response>();
      final arrived = Completer<void>();
      var starts = 0;
      final h = _Harness((request) async {
        if (request.url.path.endsWith('/login')) return _json(_login());
        starts++;
        if (starts == 1) { arrived.complete(); return response.future; }
        return _json({'flowId': _flowId, 'authorizationUrl': 'https://provider.example.test/authorize'});
      });
      addTearDown(h.dispose);
      if (purpose == AuthFlowPurpose.link || purpose == AuthFlowPurpose.reauth) {
        await h.session.login('alice', 'password');
      }
      Future<void> start() => h.flow.start(purpose: purpose,
        invitationToken: purpose == AuthFlowPurpose.invitation ? _handoff : null);
      final starting = start();
      expect(h.flow.pending, isTrue);
      await arrived.future;
      await start();
      expect(starts, 1);
      response.complete(_json({'flowId': _flowId, 'authorizationUrl': 'https://provider.example.test/authorize'}));
      await starting;
      expect(h.flow.pending, isTrue);
      expect(h.browser.navigations, [Uri.parse('https://provider.example.test/authorize')]);
      expect(h.browser.stored?['purpose'], purpose.name);
      await start();
      expect(starts, 1);
      await h.flow.cancel();
      expect(h.flow.pending, isFalse);
      expect(h.browser.stored, isNull);
      await start();
      expect(starts, 2);
      expect(h.flow.pending, isTrue);
    });
  }

  for (final failure in ['HTTP', 'storage', 'navigation']) {
    test('SSO redirect handoff recovers from $failure failure', () async {
      var available = false;
      final h = _Harness((_) async => !available && failure == 'HTTP'
        ? http.Response('Provider unavailable', 503)
        : _json({'flowId': _flowId, 'authorizationUrl': 'https://provider.example.test/authorize'}));
      addTearDown(h.dispose);
      h.browser.storageDenied = failure == 'storage';
      h.browser.navigationDenied = failure == 'navigation';
      await h.flow.start();
      expect(h.flow.pending, isFalse);
      expect(h.flow.error, isNotNull);
      expect(h.browser.stored, isNull);
      expect(h.browser.navigations, isEmpty);
      available = true;
      h.browser.storageDenied = false;
      h.browser.navigationDenied = false;
      await h.flow.start();
      expect(h.flow.pending, isTrue);
      expect(h.flow.error, isNull);
      expect(h.browser.navigations, [Uri.parse('https://provider.example.test/authorize')]);
      expect(h.browser.stored?['flowId'], _flowId);
    });
  }

  for (final newerLogin in [false, true]) {
    test('SSO redirect handoff rejects a start superseded by ${newerLogin ? 'local login' : 'logout'}', () async {
      final response = Completer<http.Response>();
      final arrived = Completer<void>();
      final h = _Harness((request) async {
        if (request.url.path.endsWith('/oidc/start')) { arrived.complete(); return response.future; }
        if (request.url.path.endsWith('/login')) return _json(_login(_bob));
        return http.Response('', 204);
      });
      addTearDown(h.dispose);
      final starting = h.flow.start();
      await arrived.future;
      if (newerLogin) { await h.session.login('bob', 'password'); } else { await h.session.logout(); }
      response.complete(_json({'flowId': _flowId, 'authorizationUrl': 'https://provider.example.test/authorize'}));
      await starting;
      expect(h.browser.navigations, isEmpty);
      expect(h.browser.stored, isNull);
      expect(h.flow.pending, isFalse);
      expect(h.flow.error, isNull);
      expect(h.session.user?.userId, newerLogin ? _bob['userId'] : null);
      expect(h.session.accessToken, newerLogin ? 'application-bob' : null);
    });
  }

  test('SSO redirect handoff allows its matching callback to finish', () async {
    final h = _Harness((request) async => request.url.path.endsWith('/exchange')
      ? _json({'status': 'authenticated', 'session': _login()})
      : _json({'flowId': _flowId, 'authorizationUrl': 'https://provider.example.test/authorize'}));
    addTearDown(h.dispose);
    await h.flow.start();
    expect(h.flow.pending, isTrue);
    expect(await h.flow.handleCallback(_callback), '/groups');
    expect(h.flow.pending, isFalse);
    expect(h.exchanges, hasLength(1));
    expect(h.browser.stored, isNull);
    expect(h.session.user?.userId, _alice['userId']);
  });

  for (final register in [false, true]) {
    final mode = register ? 'registration' : 'login';
    testWidgets('SSO redirect handoff blocks competing authentication on $mode', (tester) async {
      final response = Completer<http.Response>();
      final arrived = Completer<void>();
      final h = _Harness((request) async {
        if (request.url.path.endsWith('/oidc/start')) { arrived.complete(); return response.future; }
        return _accountResponse(request, _authentication());
      });
      final router = await _openAuth(tester, h, register: register);
      final flow = tester.widget<AuthScreen>(find.byType(AuthScreen)).authFlow!;
      final start = tester.widget<OutlinedButton>(find.widgetWithText(OutlinedButton, 'Continue with Example provider')).onPressed!;
      start();
      start();
      await tester.pump();
      expect(arrived.isCompleted, isTrue);
      for (final handedOff in [false, true]) {
        if (handedOff) {
          response.complete(_json({'flowId': _flowId, 'authorizationUrl': 'https://provider.example.test/authorize'}));
          await tester.pump();
          await tester.pump();
          expect(h.browser.navigations, [Uri.parse('https://provider.example.test/authorize')]);
          expect(flow.pending, isTrue);
        }
        _expectAuthLocked(tester, register);
        await tester.ensureVisible(find.byType(FilledButton));
        await tester.tap(find.byType(FilledButton));
        await tester.showKeyboard(_profileField('Password'));
        await tester.testTextInput.receiveAction(TextInputAction.done);
        await tester.tap(find.widgetWithText(TextButton,
          register ? 'Already have an account? Sign in' : 'Create an account'));
        await tester.pump();
        expect(router.routeInformationProvider.value.uri.path, register ? '/register' : '/login');
        expect(h.requests.where((r) => r.url.path.endsWith('/login') || r.url.path.endsWith('/register')), isEmpty);
        expect(h.requests.where((r) => r.url.path.endsWith('/oidc/start')), hasLength(1));
      }
      router.go(register ? '/login' : '/register');
      await tester.pumpAndSettle();
      _expectAuthLocked(tester, !register);
      expect(tester.widget<AuthScreen>(find.byType(AuthScreen)).authFlow, same(flow));
      await tester.showKeyboard(_profileField('Password'));
      await tester.testTextInput.receiveAction(TextInputAction.done);
      await tester.pump();
      expect(h.requests.where((r) => r.url.path.endsWith('/login') || r.url.path.endsWith('/register')), isEmpty);
    });

    for (final switchFirst in [false, true]) {
      testWidgets('SSO redirect handoff beats queued $mode switch with switchFirst=$switchFirst', (tester) async {
        final h = _Harness((request) async => request.url.path.endsWith('/oidc/start')
          ? _json({'flowId': _flowId, 'authorizationUrl': 'https://provider.example.test/authorize'})
          : _accountResponse(request, _authentication()));
        final router = await _openAuth(tester, h, register: register);
        final start = tester.widget<OutlinedButton>(find.widgetWithText(OutlinedButton, 'Continue with Example provider')).onPressed!;
        final switchMode = tester.widget<TextButton>(find.widgetWithText(TextButton,
          register ? 'Already have an account? Sign in' : 'Create an account')).onPressed!;
        if (switchFirst) { switchMode(); start(); } else { start(); switchMode(); }
        await tester.pumpAndSettle();
        expect(router.routeInformationProvider.value.uri.path, register ? '/register' : '/login');
        expect(h.requests.where((r) => r.url.path.endsWith('/oidc/start')), hasLength(1));
        expect(h.browser.navigations, [Uri.parse('https://provider.example.test/authorize')]);
        _expectAuthLocked(tester, register);
      });
    }

    for (final failure in ['HTTP', 'storage', 'navigation']) {
      testWidgets('SSO redirect handoff restores $mode drafts and actions after $failure failure', (tester) async {
        var available = false;
        final h = _Harness((request) async {
          if (request.url.path.endsWith('/oidc/start')) return !available && failure == 'HTTP'
            ? http.Response('Provider unavailable', 503)
            : _json({'flowId': _flowId, 'authorizationUrl': 'https://provider.example.test/authorize'});
          if (request.url.path.endsWith('/login') || request.url.path.endsWith('/register')) {
            return http.Response('Local service unavailable', 503);
          }
          return _accountResponse(request, _authentication());
        });
        final router = await _openAuth(tester, h, register: register);
        h.browser.storageDenied = failure == 'storage';
        h.browser.navigationDenied = failure == 'navigation';
        await tester.tap(find.widgetWithText(OutlinedButton, 'Continue with Example provider'));
        await tester.pumpAndSettle();
        final flow = tester.widget<AuthScreen>(find.byType(AuthScreen)).authFlow!;
        expect(flow.error, isNotNull);
        expect(find.textContaining(flow.error!), findsWidgets);
        expect(flow.pending, isFalse);
        expect(h.browser.stored, isNull);
        expect(tester.widget<TextFormField>(_profileField(register ? 'Username' : 'Username or email')).controller!.text, 'alice-local');
        expect(tester.widget<TextFormField>(_profileField('Password')).controller!.text, 'alice-local-password');
        if (register) {
          expect(tester.widget<TextFormField>(_profileField('Display name')).controller!.text, 'Alice local draft');
          expect(tester.widget<TextFormField>(_profileField('Email')).controller!.text, 'alice-local@example.test');
        }
        expect(tester.widget<TextButton>(find.widgetWithText(TextButton,
          register ? 'Already have an account? Sign in' : 'Create an account')).enabled, isTrue);
        await tester.ensureVisible(find.widgetWithText(FilledButton, register ? 'Create account' : 'Sign in'));
        await tester.tap(find.widgetWithText(FilledButton, register ? 'Create account' : 'Sign in'));
        await tester.pumpAndSettle();
        expect(h.requests.where((r) => r.url.path.endsWith(register ? '/register' : '/login')), hasLength(1));
        available = true;
        h.browser.storageDenied = false;
        h.browser.navigationDenied = false;
        await tester.tap(find.widgetWithText(OutlinedButton, 'Continue with Example provider'));
        await tester.pumpAndSettle();
        expect(h.requests.where((r) => r.url.path.endsWith('/oidc/start')), hasLength(2));
        expect(h.browser.navigations, [Uri.parse('https://provider.example.test/authorize')]);
        expect(router.routeInformationProvider.value.uri.path, register ? '/register' : '/login');
        _expectAuthLocked(tester, register);
      });
    }
  }

  for (final fail in [false, true]) {
    testWidgets('SSO redirect handoff protects invitation actions${fail ? ' and recovers from failure' : ''}', (tester) async {
      final response = Completer<http.Response>();
      final arrived = Completer<void>();
      final h = _Harness((request) async {
        if (request.url.path.endsWith('/inspect')) return _json({
          'purpose': 'oidc_link', 'target': _alice,
          'expiresAt': _now.add(const Duration(minutes: 10)).toIso8601String(),
        });
        arrived.complete();
        return response.future;
      });
      addTearDown(h.dispose);
      addTearDown(() => tester.pumpWidget(const SizedBox.shrink()));
      await tester.pumpWidget(MaterialApp(home: InvitationScreen(flow: h.flow,
        location: Uri(path: '/auth/invitation', queryParameters: {'token': _handoff}))));
      await tester.pumpAndSettle();
      final start = tester.widget<FilledButton>(find.widgetWithText(FilledButton, 'Continue with provider')).onPressed!;
      start(); start();
      await tester.pump();
      expect(arrived.isCompleted, isTrue);
      for (final completed in [false, true]) {
        if (completed) {
          response.complete(fail ? http.Response('Provider unavailable', 503)
            : _json({'flowId': _flowId, 'authorizationUrl': 'https://provider.example.test/authorize'}));
          await tester.pump(); await tester.pump();
        }
        final locked = !completed || !fail;
        expect(tester.widget<FilledButton>(find.widgetWithText(FilledButton, 'Continue with provider')).enabled, !locked);
        expect(tester.widget<TextButton>(find.widgetWithText(TextButton, 'Cancel and return to sign-in')).enabled, !locked);
        expect(find.byType(LinearProgressIndicator), locked ? findsOneWidget : findsNothing);
        expect(find.textContaining('Account: Alice'), findsOneWidget);
        expect(h.requests.where((r) => r.url.path.endsWith('/oidc/start')), hasLength(1));
      }
      if (fail) {
        expect(find.text('Provider unavailable'), findsOneWidget);
        expect(h.browser.stored, isNull);
      } else {
        expect(h.browser.stored?['purpose'], 'invitation');
        expect(jsonDecode(h.requests.last.body)['invitationToken'], _handoff);
      }
    });
  }

  testWidgets('SSO redirect handoff protects reset drafts until the shared flow clears', (tester) async {
    final response = Completer<http.Response>();
    final arrived = Completer<void>();
    final h = _Harness((request) async {
      if (request.url.path.endsWith('/login')) return _json(_login());
      if (request.url.path.endsWith('/inspect')) return _json({
        'purpose': 'password_reset', 'target': _alice,
        'expiresAt': _now.add(const Duration(minutes: 10)).toIso8601String(),
      });
      if (request.url.path.endsWith('/oidc/start')) { arrived.complete(); return response.future; }
      return http.Response('Reset service unavailable', 503);
    });
    addTearDown(h.dispose);
    addTearDown(() => tester.pumpWidget(const SizedBox.shrink()));
    await h.session.login('alice', 'password');
    await tester.pumpWidget(MaterialApp(home: InvitationScreen(flow: h.flow,
      location: Uri(path: '/auth/invitation', queryParameters: {'token': _handoff}))));
    await tester.pumpAndSettle();
    await tester.enterText(_profileField('New password'), 'replacement-password');
    await tester.enterText(_profileField('Confirm new password'), 'replacement-password');
    final starting = h.flow.start(purpose: AuthFlowPurpose.link);
    await tester.pump();
    expect(arrived.isCompleted, isTrue);
    for (final handedOff in [false, true]) {
      if (handedOff) {
        response.complete(_json({'flowId': _flowId, 'authorizationUrl': 'https://provider.example.test/authorize'}));
        await starting;
        await tester.pump();
      }
      expect(tester.widget<TextFormField>(_profileField('New password')).enabled, isFalse);
      expect(tester.widget<TextFormField>(_profileField('Confirm new password')).enabled, isFalse);
      expect(tester.widget<FilledButton>(find.widgetWithText(FilledButton, 'Reset password')).enabled, isFalse);
      await tester.tap(find.widgetWithText(FilledButton, 'Reset password'));
      await tester.pump();
      expect(h.requests.where((r) => r.url.path.endsWith('/password-reset')), isEmpty);
    }
    h.flow.clear();
    await tester.pumpAndSettle();
    expect(tester.widget<TextFormField>(_profileField('New password')).controller!.text, 'replacement-password');
    expect(tester.widget<TextFormField>(_profileField('Confirm new password')).controller!.text, 'replacement-password');
    await tester.tap(find.widgetWithText(FilledButton, 'Reset password'));
    await tester.pumpAndSettle();
    expect(h.requests.where((r) => r.url.path.endsWith('/password-reset')), hasLength(1));
    expect(find.text('Reset service unavailable'), findsOneWidget);
  });

  for (final linked in [false, true]) {
    for (final fail in [false, true]) {
      testWidgets('SSO redirect handoff propagates Account ${linked ? 'reauth' : 'link'} pending${fail ? ' failure' : ' and clear'}', (tester) async {
        final response = Completer<http.Response>();
        final arrived = Completer<void>();
        final h = _Harness((request) async {
          if (request.url.path.endsWith('/oidc/start')) { arrived.complete(); return response.future; }
          return _accountResponse(request, _authentication(linked: linked));
        });
        final router = await _openAccount(tester, h);
        final flow = tester.widget<AccountScreen>(find.byType(AccountScreen)).authFlow!;
        await tester.enterText(_profileField('Display name'), 'Alice handoff draft');
        await _toggleDisclosure(tester, 'Password');
        await _toggleDisclosure(tester, linked ? 'Example provider' : 'SSO provider');
        await _toggleDisclosure(tester, 'Delete account');
        await _tapAccountAction(tester, linked ? 'Verify with provider' : 'Link SSO identity');
        expect(find.text('Discard unsaved changes?'), findsOneWidget);
        await tester.tap(find.text('Discard'));
        await tester.pumpAndSettle();
        expect(arrived.isCompleted, isTrue);
        for (final completed in [false, true]) {
          if (completed) {
            response.complete(fail ? http.Response('Provider unavailable', 503)
              : _json({'flowId': _flowId, 'authorizationUrl': 'https://provider.example.test/authorize'}));
            await tester.pumpAndSettle();
          }
          final locked = !completed || !fail;
          for (final label in ['Save profile', 'Change password', 'Delete account', linked ? 'Verify with provider' : 'Link SSO identity']) {
            final button = find.ancestor(of: find.text(label), matching: find.byWidgetPredicate((w) => w is ButtonStyleButton));
            expect(tester.widget<ButtonStyleButton>(button).enabled, !locked, reason: label);
            if (locked) { await tester.ensureVisible(button); await tester.tap(button); await tester.pump(); }
          }
          final refresh = find.byWidgetPredicate((w) => w is IconButton && w.tooltip == 'Refresh account');
          expect(tester.widget<IconButton>(refresh).onPressed, locked ? isNull : isNotNull);
          if (locked) { await tester.ensureVisible(refresh); await tester.tap(refresh); await tester.pump(); }
          expect(tester.widget<TextFormField>(_profileField('Display name')).controller!.text, 'Alice handoff draft');
          if (locked) {
            await tester.tap(find.text('Jizhang'));
            await tester.pumpAndSettle();
            expect(router.routeInformationProvider.value.uri.path, '/account');
            expect(find.byType(AlertDialog), findsNothing);
          }
          expect(h.requests.where((r) => r.method == 'PUT' || r.method == 'DELETE' || r.url.path.endsWith('/reauthenticate')), isEmpty);
          expect(h.requests.where((r) => r.method == 'GET' && r.url.path.endsWith('/users/me')), hasLength(1));
        }
        expect(jsonDecode(h.requests.singleWhere((r) => r.url.path.endsWith('/oidc/start')).body)['purpose'], linked ? 'reauth' : 'link');
        if (fail) {
          expect(find.text('Provider unavailable'), findsOneWidget);
        } else {
          flow.clear();
          await tester.pumpAndSettle();
          expect(tester.widget<FilledButton>(find.widgetWithText(FilledButton, 'Save profile')).enabled, isTrue);
          expect(tester.widget<IconButton>(find.byWidgetPredicate((w) => w is IconButton && w.tooltip == 'Refresh account')).onPressed, isNotNull);
          await tester.tap(find.text('Jizhang'));
          await tester.pumpAndSettle();
          expect(find.text('Discard unsaved changes?'), findsOneWidget);
          await tester.tap(find.descendant(of: find.byType(AlertDialog), matching: find.text('Cancel')));
          await tester.pumpAndSettle();
          expect(router.routeInformationProvider.value.uri.path, '/account');
        }
      });
    }
  }

  testWidgets('failed provider verification retains its category and returns to Account', (tester) async {
    final h = _Harness((request) async => _accountResponse(request, _authentication(linked: true)));
    addTearDown(h.dispose);
    addTearDown(() => tester.pumpWidget(const SizedBox.shrink()));
    await tester.pumpWidget(JizhangApp(repository: h.repository, session: h.session));
    await tester.pumpAndSettle();
    final router = GoRouter.of(tester.element(find.byType(AuthScreen)));
    h.browser.seed(returnTo: '/account', purpose: 'reauth', createdAt: DateTime.now().toUtc());
    router.go(Uri(path: '/auth/oidc/callback', queryParameters: {
      'flow': _flowId, 'error': 'invalid_response',
    }).toString());
    await tester.pumpAndSettle();
    expect(find.textContaining('invalid_response'), findsOneWidget);
    expect(h.exchanges, isEmpty);
    expect(h.browser.stored, isNull);
    expect(router.routeInformationProvider.value.uri.path, '/auth/oidc/continue');
    await tester.tap(find.widgetWithText(FilledButton, 'Back to Account'));
    await tester.pumpAndSettle();
    expect(find.byType(AuthScreen), findsOneWidget);
    await h.session.login('alice', 'password');
    await tester.pumpAndSettle();
    expect(find.byType(AccountScreen), findsOneWidget);
  });

  for (final category in ['access_denied', 'expired', 'configuration_changed', 'provider_unavailable', 'invalid_response']) {
    test('provider callback retains safe failure category $category without exchange', () async {
      final h = _Harness((_) async => throw StateError('Unexpected exchange'));
      addTearDown(h.dispose);
      h.browser.seed(returnTo: '/account', purpose: 'reauth');
      await h.flow.handleCallback(Uri(path: '/auth/oidc/callback', queryParameters: {
        'flow': _flowId, 'error': category,
      }));
      expect(h.flow.error, contains(category));
      expect(h.flow.returnTo, '/account');
      expect(h.exchanges, isEmpty);
      expect(h.browser.stored, isNull);
      expect(h.session.isAuthenticated, isFalse);
    });
  }

  testWidgets('credential dialogs accept Escape only while idle', (tester) async {
    final saving = Completer<http.Response>();
    final h = _Harness((request) async {
      if (request.url.path.endsWith('/reauthenticate')) return http.Response('', 204);
      if (request.method == 'PUT' && request.url.path.endsWith('/password')) return saving.future;
      return _accountResponse(request, _authentication());
    });
    await _openAccount(tester, h);
    await _toggleDisclosure(tester, 'Password');
    await _tapAccountAction(tester, 'Change password');
    await tester.sendKeyEvent(LogicalKeyboardKey.escape);
    await tester.pumpAndSettle();
    expect(find.byType(AlertDialog), findsNothing);
    await _tapAccountAction(tester, 'Change password');
    await tester.enterText(find.widgetWithText(TextField, 'Current password'), 'correct-password');
    await tester.tap(find.widgetWithText(FilledButton, 'Verify'));
    await tester.pumpAndSettle();
    await tester.sendKeyEvent(LogicalKeyboardKey.escape);
    await tester.pumpAndSettle();
    expect(find.byType(AlertDialog), findsNothing);
    await _tapAccountAction(tester, 'Change password');
    await tester.enterText(find.widgetWithText(TextField, 'Current password'), 'correct-password');
    await tester.tap(find.widgetWithText(FilledButton, 'Verify'));
    await tester.pumpAndSettle();
    await tester.enterText(find.widgetWithText(TextFormField, 'New password'), 'replacement-password');
    await tester.enterText(find.widgetWithText(TextFormField, 'Confirm new password'), 'replacement-password');
    await tester.tap(find.widgetWithText(FilledButton, 'Save password'));
    await tester.pump();
    await tester.sendKeyEvent(LogicalKeyboardKey.escape);
    await tester.pump();
    expect(find.byType(AlertDialog), findsOneWidget);
    saving.complete(http.Response('Password service unavailable', 503));
    await tester.pumpAndSettle();
    expect(find.text('Password service unavailable'), findsOneWidget);
    await tester.sendKeyEvent(LogicalKeyboardKey.escape);
    await tester.pumpAndSettle();
    expect(find.byType(AlertDialog), findsNothing);
    expect(h.session.isAuthenticated, isTrue);
  });

  for (final web in [false, true]) {
    testWidgets('SSO-only ${web ? 'disabled provider' : 'unsupported browser'} cannot collect an unverified password', (tester) async {
      final authentication = _authentication(hasPassword: false, linked: true, method: 'oidc',
        providerEnabled: !web);
      final h = _Harness((request) async => _accountResponse(request, authentication));
      h.browser.enabled = web;
      await _openAccount(tester, h);
      await _toggleDisclosure(tester, 'Password');
      await _tapAccountAction(tester, 'Set password');
      expect(find.text('Reauthentication required'), findsOneWidget);
      expect(find.widgetWithText(TextFormField, 'New password'), findsNothing);
      expect(find.descendant(of: find.byType(AlertDialog), matching: find.text('Verify with provider')), findsNothing);
      expect(h.requests.where((request) => request.method == 'PUT'), isEmpty);
      await tester.tap(find.descendant(of: find.byType(AlertDialog), matching: find.text('Cancel')));
      await tester.pumpAndSettle();
      expect(h.session.isAuthenticated, isTrue);
    });
  }

  testWidgets('account disclosures do not leak into the next signed-in identity', (tester) async {
    var user = _alice;
    final authentication = _authentication(linked: true);
    final h = _Harness((request) async {
      if (request.url.path.endsWith('/login')) return _json(_login(user));
      if (request.method == 'GET' && request.url.path.endsWith('/users/me')) return _json({
        'user': user, 'email': '${user['username']}@example.test', 'emailVerified': true,
      });
      if (request.url.path.endsWith('/users/me/logout')) return http.Response('', 204);
      return _accountResponse(request, authentication);
    });
    final router = await _openAccount(tester, h);
    for (final title in ['Account details', 'Password', 'Example provider', 'Identity details', 'Delete account']) {
      await _toggleDisclosure(tester, title);
    }
    await _tapAccountAction(tester, 'Sign out');
    user = _bob;
    await h.session.login('bob', 'password');
    router.go('/account');
    await tester.pumpAndSettle();
    expect(tester.widget<TextFormField>(_profileField('Display name')).controller!.text, 'Bob');
    expect(find.text('Change password'), findsNothing);
    expect(find.text('Unlink SSO identity'), findsNothing);
    expect(find.textContaining(_alice['userId']!), findsNothing);
    expect(find.widgetWithText(OutlinedButton, 'Delete account'), findsNothing);
    await _toggleDisclosure(tester, 'Account details');
    expect(find.textContaining(_bob['userId']!), findsOneWidget);
  });

  testWidgets('linking cancellation and failed provider start preserve the profile draft and leave guard', (tester) async {
    final authentication = _authentication();
    final h = _Harness((request) async {
      if (request.url.path.endsWith('/oidc/start')) return http.Response('Provider unavailable', 503);
      return _accountResponse(request, authentication);
    });
    await _openAccount(tester, h);
    await tester.enterText(_profileField('Display name'), 'Alice linking draft');
    await _toggleDisclosure(tester, 'SSO provider');
    await _tapAccountAction(tester, 'Link SSO identity');
    expect(find.text('Discard unsaved changes?'), findsOneWidget);
    await tester.tap(find.descendant(of: find.byType(AlertDialog), matching: find.text('Cancel')));
    await tester.pumpAndSettle();
    expect(h.requests.where((request) => request.url.path.endsWith('/oidc/start')), isEmpty);
    await _tapAccountAction(tester, 'Link SSO identity');
    await tester.tap(find.text('Discard'));
    await tester.pumpAndSettle();
    expect(find.text('Provider unavailable'), findsOneWidget);
    expect(h.browser.navigations, isEmpty);
    expect(h.session.isAuthenticated, isTrue);
    expect(tester.widget<TextFormField>(_profileField('Display name')).controller!.text, 'Alice linking draft');
    expect(jsonDecode(h.requests.singleWhere((request) => request.url.path.endsWith('/oidc/start')).body)['purpose'], 'link');
    await _toggleDisclosure(tester, 'SSO provider');
    expect(find.text('Provider unavailable'), findsOneWidget);
    await _tapAccountAction(tester, 'Back to groups');
    expect(find.text('Discard unsaved changes?'), findsOneWidget);
    await tester.tap(find.descendant(of: find.byType(AlertDialog), matching: find.text('Cancel')));
    await tester.pumpAndSettle();
    expect(find.byType(AccountScreen), findsOneWidget);
  });

  for (final identity in [false, true]) {
    testWidgets('successful ${identity ? 'unlink' : 'password removal'} retires the application session', (tester) async {
      final authentication = _authentication(linked: true, method: 'oidc',
        verifiedAt: DateTime.now().toUtc().subtract(const Duration(minutes: 1)));
      final h = _Harness((request) async {
        if (request.method == 'DELETE' && request.url.path.endsWith('/password') ||
          request.url.path.endsWith('/unlink')) return http.Response('', 204);
        return _accountResponse(request, authentication);
      });
      await _openAccount(tester, h);
      await _toggleDisclosure(tester, identity ? 'Example provider' : 'Password');
      await _tapAccountAction(tester, identity ? 'Unlink SSO identity' : 'Remove password');
      await tester.tap(find.descendant(of: find.byType(AlertDialog),
        matching: find.text(identity ? 'Unlink identity' : 'Remove password')));
      await tester.pumpAndSettle();
      if (identity) {
        await tester.enterText(find.byType(TextField).last, 'correct-password');
        await _tapAccountAction(tester, 'Verify');
        expect(jsonDecode(h.requests.singleWhere((request) => request.url.path.endsWith('/unlink')).body),
          {'currentPassword': 'correct-password'});
      }
      expect(h.session.isAuthenticated, isFalse);
      expect(find.byType(AuthScreen), findsOneWidget);
      expect(h.requests.where((request) => request.url.path.endsWith('/logout')), isEmpty);
    });
  }

  testWidgets('failed password change retains password inputs and the profile draft', (tester) async {
    final authentication = _authentication();
    final h = _Harness((request) async {
      if (request.url.path.endsWith('/reauthenticate')) return http.Response('', 204);
      if (request.method == 'PUT' && request.url.path.endsWith('/password')) {
        return http.Response('Password update unavailable', 503);
      }
      return _accountResponse(request, authentication);
    });
    await _openAccount(tester, h);
    await tester.enterText(_profileField('Display name'), 'Alice password draft');
    await _toggleDisclosure(tester, 'Password');
    await _tapAccountAction(tester, 'Change password');
    expect(find.widgetWithText(TextFormField, 'New password'), findsNothing);
    await tester.enterText(find.byType(TextField).last, 'correct-password');
    await _tapAccountAction(tester, 'Verify');
    await tester.enterText(find.widgetWithText(TextFormField, 'New password'), 'replacement-password');
    await tester.enterText(find.widgetWithText(TextFormField, 'Confirm new password'), 'replacement-password');
    await _tapAccountAction(tester, 'Save password');
    expect(find.text('Discard unsaved changes?'), findsOneWidget);
    await tester.tap(find.descendant(of: find.byType(AlertDialog).last, matching: find.text('Cancel')));
    await tester.pumpAndSettle();
    expect(h.requests.where((request) => request.method == 'PUT'), isEmpty);
    expect(find.widgetWithText(TextFormField, 'New password'), findsOneWidget);
    await _tapAccountAction(tester, 'Save password');
    await tester.tap(find.text('Continue'));
    await tester.pumpAndSettle();
    expect(find.text('Password update unavailable'), findsOneWidget);
    expect(tester.widget<TextFormField>(find.widgetWithText(TextFormField, 'New password')).controller!.text,
      'replacement-password');
    expect(tester.widget<TextFormField>(find.widgetWithText(TextFormField, 'Confirm new password')).controller!.text,
      'replacement-password');
    expect(h.session.isAuthenticated, isTrue);
    await tester.tap(find.descendant(of: find.byType(AlertDialog), matching: find.text('Cancel')));
    await tester.pumpAndSettle();
    expect(tester.widget<TextFormField>(_profileField('Display name')).controller!.text, 'Alice password draft');
    expect(find.text('Change password'), findsOneWidget);
    expect(jsonDecode(h.requests.singleWhere((request) => request.method == 'PUT').body),
      {'currentPassword': 'correct-password', 'newPassword': 'replacement-password'});
  });

  testWidgets('unlink cancellation and a failed pending proof preserve the dirty profile', (tester) async {
    final mutation = Completer<http.Response>();
    final authentication = _authentication(linked: true);
    final h = _Harness((request) async {
      if (request.url.path.endsWith('/unlink')) return mutation.future;
      return _accountResponse(request, authentication);
    });
    final router = await _openAccount(tester, h);
    await tester.enterText(_profileField('Display name'), 'Alice security draft');
    await _toggleDisclosure(tester, 'Example provider');
    await _tapAccountAction(tester, 'Unlink SSO identity');
    await tester.tap(find.descendant(of: find.byType(AlertDialog), matching: find.text('Cancel')));
    await tester.pumpAndSettle();
    expect(find.byType(AlertDialog), findsNothing);
    expect(h.requests.where((request) => request.url.path.endsWith('/unlink')), isEmpty);
    await _tapAccountAction(tester, 'Unlink SSO identity');
    await tester.tap(find.text('Unlink identity'));
    await tester.pumpAndSettle();
    expect(find.text('Discard unsaved changes?'), findsOneWidget);
    await tester.tap(find.text('Continue'));
    await tester.pumpAndSettle();
    await tester.enterText(find.byType(TextField).last, 'correct-password');
    await tester.tap(find.widgetWithText(FilledButton, 'Verify'));
    await tester.pump();
    expect(tester.widget<TextField>(find.byType(TextField).last).enabled, isFalse);
    expect(tester.widget<TextFormField>(_profileField('Display name')).enabled, isFalse);
    expect(tester.widget<TextButton>(find.widgetWithText(TextButton, 'Cancel').last).onPressed, isNull);
    await tester.sendKeyEvent(LogicalKeyboardKey.escape);
    await tester.pump();
    expect(find.text('Verify your password'), findsOneWidget);
    router.go('/groups');
    await tester.pump();
    expect(find.byType(AccountScreen), findsOneWidget);
    mutation.complete(http.Response('Identity unlink failed', 503));
    await tester.pumpAndSettle();
    expect(find.text('Identity unlink failed'), findsOneWidget);
    expect(tester.widget<TextField>(find.byType(TextField).last).controller!.text, 'correct-password');
    await tester.tap(find.descendant(of: find.byType(AlertDialog), matching: find.text('Cancel')));
    await tester.pumpAndSettle();
    expect(tester.widget<TextFormField>(_profileField('Display name')).controller!.text, 'Alice security draft');
    expect(find.text('Unlink SSO identity'), findsOneWidget);
    expect(h.session.isAuthenticated, isTrue);
    expect(jsonDecode(h.requests.singleWhere((request) => request.url.path.endsWith('/unlink')).body),
      {'currentPassword': 'correct-password'});
  });

  for (final proof in ['missing', 'expired', 'future', 'password session']) {
    testWidgets('$proof OIDC proof cannot authorize password removal', (tester) async {
      final now = DateTime.now().toUtc();
      final authentication = _authentication(linked: true,
        method: proof == 'password session' ? 'password' : 'oidc',
        verifiedAt: switch (proof) {
          'missing' => null,
          'expired' => now.subtract(const Duration(minutes: 11)),
          'future' => now.add(const Duration(minutes: 1)),
          _ => now.subtract(const Duration(minutes: 1)),
        });
      final h = _Harness((request) async => _accountResponse(request, authentication));
      await _openAccount(tester, h);
      await _toggleDisclosure(tester, 'Password');
      await _tapAccountAction(tester, 'Remove password');
      expect(find.text('Reauthentication required'), findsOneWidget);
      expect(h.requests.where((request) => request.method == 'DELETE'), isEmpty);
      expect(h.session.isAuthenticated, isTrue);
      await tester.tap(find.descendant(of: find.byType(AlertDialog), matching: find.text('Cancel')));
      await tester.pumpAndSettle();
    });
  }

  testWidgets('fresh explicit OIDC proof permits password setup and retires the session', (tester) async {
    final authentication = _authentication(hasPassword: false, linked: true, method: 'oidc',
      verifiedAt: DateTime.now().toUtc().subtract(const Duration(minutes: 1)));
    final h = _Harness((request) async {
      if (request.method == 'PUT' && request.url.path.endsWith('/password')) return http.Response('', 204);
      return _accountResponse(request, authentication);
    });
    await _openAccount(tester, h);
    await _toggleDisclosure(tester, 'Password');
    await _tapAccountAction(tester, 'Set password');
    expect(find.text('Reauthentication required'), findsNothing);
    await tester.enterText(find.widgetWithText(TextFormField, 'New password'), 'new-password');
    await tester.enterText(find.widgetWithText(TextFormField, 'Confirm new password'), 'new-password');
    await _tapAccountAction(tester, 'Save password');
    expect(h.session.isAuthenticated, isFalse);
    expect(find.byType(AuthScreen), findsOneWidget);
    expect(jsonDecode(h.requests.singleWhere((request) => request.method == 'PUT').body),
      {'currentPassword': null, 'newPassword': 'new-password'});
    expect(h.requests.where((request) => request.url.path.endsWith('/reauthenticate')), isEmpty);
  });

  testWidgets('server rejection of fresh proof offers explicit provider reauthentication', (tester) async {
    final authentication = _authentication(linked: true, method: 'oidc',
      verifiedAt: DateTime.now().toUtc().subtract(const Duration(minutes: 1)));
    final h = _Harness((request) async {
      if (request.method == 'DELETE') return http.Response('Reauthentication required', 403);
      if (request.url.path.endsWith('/oidc/start')) return _json({
        'flowId': _flowId, 'authorizationUrl': 'https://provider.example.test/authorize',
      });
      return _accountResponse(request, authentication);
    });
    await _openAccount(tester, h);
    await _toggleDisclosure(tester, 'Password');
    await _tapAccountAction(tester, 'Remove password');
    await tester.tap(find.descendant(of: find.byType(AlertDialog), matching: find.text('Cancel')));
    await tester.pumpAndSettle();
    expect(h.requests.where((request) => request.method == 'DELETE'), isEmpty);
    await _tapAccountAction(tester, 'Remove password');
    await tester.tap(find.descendant(of: find.byType(AlertDialog), matching: find.text('Remove password')));
    await tester.pumpAndSettle();
    expect(h.session.isAuthenticated, isTrue);
    expect(find.text('Reauthentication required'), findsWidgets);
    await tester.tap(find.descendant(of: find.byType(AlertDialog), matching: find.text('Verify with provider')));
    await tester.pumpAndSettle();
    final start = h.requests.singleWhere((request) => request.url.path.endsWith('/oidc/start'));
    expect(jsonDecode(start.body)['purpose'], 'reauth');
    expect(h.browser.navigations, [Uri.parse('https://provider.example.test/authorize')]);
    expect(h.session.isAuthenticated, isTrue);
  });

  testWidgets('provider sign-out cancellation does not become local sign-out', (tester) async {
    final authentication = _authentication(linked: true);
    final h = _Harness((request) async {
      if (request.url.path.endsWith('/users/me/logout')) return http.Response('', 204);
      return _accountResponse(request, authentication);
    });
    await _openAccount(tester, h);
    await _tapAccountAction(tester, 'Sign out of Example provider too');
    await tester.tap(find.descendant(of: find.byType(AlertDialog), matching: find.text('Cancel')));
    await tester.pumpAndSettle();
    expect(h.session.isAuthenticated, isTrue);
    expect(h.requests.where((request) => request.url.path.endsWith('/logout')), isEmpty);
    await _tapAccountAction(tester, 'Sign out');
    expect(h.session.isAuthenticated, isFalse);
    expect(h.requests.where((request) => request.url.path.endsWith('/users/me/logout')), hasLength(1));
    expect(h.requests.where((request) => request.url.path.endsWith('/oidc/logout')), isEmpty);
  });

  testWidgets('deletion disclosure retains discard and irreversible confirmation cancellation', (tester) async {
    final authentication = _authentication();
    final h = _Harness((request) async => _accountResponse(request, authentication));
    await _openAccount(tester, h);
    await tester.enterText(_profileField('Display name'), 'Alice delete draft');
    await _toggleDisclosure(tester, 'Delete account');
    await _tapAccountAction(tester, 'Delete account');
    expect(find.text('Discard unsaved changes?'), findsOneWidget);
    await tester.tap(find.descendant(of: find.byType(AlertDialog), matching: find.text('Cancel')));
    await tester.pumpAndSettle();
    expect(tester.widget<TextFormField>(_profileField('Display name')).controller!.text, 'Alice delete draft');
    await _tapAccountAction(tester, 'Delete account');
    await tester.tap(find.text('Discard'));
    await tester.pumpAndSettle();
    expect(find.text('Delete account?'), findsOneWidget);
    await tester.tap(find.descendant(of: find.byType(AlertDialog), matching: find.text('Cancel')));
    await tester.pumpAndSettle();
    expect(h.requests.where((request) => request.method == 'DELETE'), isEmpty);
    expect(h.session.isAuthenticated, isTrue);
    expect(tester.widget<TextFormField>(_profileField('Display name')).controller!.text, 'Alice');
  });

  for (final fixture in [
    (name: '320px', size: const Size(320, 900), scale: 1.0),
    (name: '768px', size: const Size(768, 1000), scale: 1.0),
    (name: '1440px', size: const Size(1440, 1000), scale: 1.0),
    (name: '320px with 200% text', size: const Size(320, 900), scale: 2.0),
  ]) {
    testWidgets('Account disclosure insets separate headers and bodies at ${fixture.name}', (tester) async {
      await tester.binding.setSurfaceSize(fixture.size);
      addTearDown(() => tester.binding.setSurfaceSize(null));
      final authentication = _authentication(linked: true);
      final h = _Harness((request) async => _accountResponse(request, authentication));
      await _openAccount(tester, h);
      tester.platformDispatcher.textScaleFactorTestValue = fixture.scale;
      addTearDown(tester.platformDispatcher.clearTextScaleFactorTestValue);
      await tester.pumpAndSettle();
      for (final title in ['Account details', 'Password', 'Example provider', 'Identity details', 'Delete account']) {
        await _toggleDisclosure(tester, title);
      }

      Finder selectable(String prefix) => find.byWidgetPredicate((widget) =>
        widget is SelectableText && (widget.data?.startsWith(prefix) ?? false));
      final accountId = selectable('Account ID:');
      for (final section in [
        (title: 'Account details', inset: 12.0, first: accountId, last: accountId),
        (title: 'Password', inset: 12.0,
          first: find.widgetWithText(OutlinedButton, 'Change password'),
          last: find.widgetWithText(TextButton, 'Remove password')),
        (title: 'Example provider', inset: 12.0,
          first: find.widgetWithText(OutlinedButton, 'Verify with provider'),
          last: find.widgetWithText(TextButton, 'Unlink SSO identity')),
        (title: 'Identity details', inset: 12.0,
          first: selectable('Issuer:'), last: selectable('Last SSO sign-in:')),
        (title: 'Delete account', inset: 16.0,
          first: find.text('Referenced group or expense history may prevent deletion. Ledger history is preserved.'),
          last: find.widgetWithText(OutlinedButton, 'Delete account')),
      ]) {
        final tile = _accountDisclosure(section.title);
        final header = find.descendant(of: tile, matching: find.byType(ListTile)).first;
        final label = find.descendant(of: header, matching: find.text(section.title)).first;
        final arrow = find.descendant(of: header, matching: find.byType(Icon)).last;
        final headerRect = tester.getRect(header);
        final minimum = section.inset - 0.01;
        expect(tester.getRect(label).left - headerRect.left, greaterThanOrEqualTo(minimum),
          reason: '${section.title} title inset');
        expect(headerRect.right - tester.getRect(arrow).right, greaterThanOrEqualTo(minimum),
          reason: '${section.title} arrow inset');
        expect(headerRect.height, greaterThanOrEqualTo(48 - 0.01));
        expect(section.first, findsOneWidget);
        expect(section.last, findsOneWidget);
        expect(tester.getRect(section.first).top - headerRect.bottom, greaterThanOrEqualTo(minimum),
          reason: '${section.title} header/body gap');
        expect(tester.getRect(tile).bottom - tester.getRect(section.last).bottom, greaterThanOrEqualTo(minimum),
          reason: '${section.title} bottom inset');
        for (final content in [section.first, section.last]) {
          final rect = tester.getRect(content);
          expect(rect.left - headerRect.left, greaterThanOrEqualTo(minimum),
            reason: '${section.title} body left inset');
          expect(headerRect.right - rect.right, greaterThanOrEqualTo(minimum),
            reason: '${section.title} body right inset');
        }
      }
      final detailsHeader = find.descendant(of: _accountDisclosure('Account details'),
        matching: find.byType(ListTile)).first;
      final email = find.byWidgetPredicate((widget) => widget is InputDecorator &&
        widget.decoration.labelText == 'Email');
      expect(tester.getRect(detailsHeader).top - tester.getRect(email).bottom,
        greaterThanOrEqualTo(12 - 0.01));
      final accountText = tester.widget<SelectableText>(accountId).data;
      for (final expanded in [false, true]) {
        await tester.ensureVisible(detailsHeader);
        await tester.pumpAndSettle();
        final rect = tester.getRect(detailsHeader);
        await tester.tapAt(Offset(rect.left + 2, rect.center.dy));
        await tester.pumpAndSettle();
        expect(accountId, expanded ? findsOneWidget : findsNothing);
      }
      expect(tester.widget<SelectableText>(accountId).data, accountText);
    });
  }

  for (final fixture in [
    (name: 'password only', password: true, linked: false, enabled: true, web: true),
    (name: 'password and SSO', password: true, linked: true, enabled: true, web: true),
    (name: 'SSO only', password: false, linked: true, enabled: true, web: true),
    (name: 'disabled provider', password: true, linked: true, enabled: false, web: true),
    (name: 'unsupported browser', password: true, linked: true, enabled: true, web: false),
  ]) {
    testWidgets('Account discloses applicable controls for ${fixture.name}', (tester) async {
      final authentication = _authentication(hasPassword: fixture.password, linked: fixture.linked,
        providerEnabled: fixture.enabled, method: fixture.password ? 'password' : 'oidc');
      final h = _Harness((request) async => _accountResponse(request, authentication));
      h.browser.enabled = fixture.web;
      await _openAccount(tester, h);
      expect(find.text('Change password'), findsNothing);
      expect(find.text('Set password'), findsNothing);
      expect(find.text('Link SSO identity'), findsNothing);
      expect(find.text('Unlink SSO identity'), findsNothing);
      expect(find.text('Save profile'), findsNothing);
      await _toggleDisclosure(tester, 'Password');
      expect(find.text(fixture.password ? 'Change password' : 'Set password'), findsOneWidget);
      expect(find.text('Verify current password'), fixture.password ? findsOneWidget : findsNothing);
      expect(find.text('Remove password'), fixture.password && fixture.linked && fixture.enabled
        ? findsOneWidget : findsNothing);
      await _toggleDisclosure(tester, fixture.linked ? 'Example provider' : 'SSO provider');
      expect(find.text('Link SSO identity'), !fixture.linked && fixture.enabled && fixture.web
        ? findsOneWidget : findsNothing);
      expect(find.text('Verify with provider'), fixture.linked && fixture.enabled && fixture.web
        ? findsOneWidget : findsNothing);
      expect(find.text('Unlink SSO identity'), fixture.linked ? findsOneWidget : findsNothing);
      expect(find.text('Sign out of Example provider too'), fixture.linked && fixture.enabled && fixture.web
        ? findsOneWidget : findsNothing);
      if (fixture.linked) {
        expect(find.textContaining('synthetic-alice'), findsNothing);
        await _toggleDisclosure(tester, 'Identity details');
        expect(find.textContaining('synthetic-alice'), findsOneWidget);
        expect(find.textContaining('https://provider.example.test/'), findsOneWidget);
        expect(find.textContaining('Provider email: alice@example.test'), findsOneWidget);
      }
      expect(h.requests.where((request) => request.method != 'GET' &&
        !request.url.path.endsWith('/login')), isEmpty);
    });
  }

  testWidgets('dirty profile survives disclosures, a failed save, and canceled exits', (tester) async {
    final authentication = _authentication(linked: true);
    final h = _Harness((request) async {
      if (request.method == 'PUT' && request.url.path.endsWith('/users/me')) {
        return http.Response('Username is already in use', 409);
      }
      return _accountResponse(request, authentication);
    });
    final router = await _openAccount(tester, h);
    await tester.enterText(_profileField('Username'), 'alice_draft');
    await tester.enterText(_profileField('Display name'), 'Alice draft');
    for (final title in ['Account details', 'Password', 'Example provider', 'Identity details', 'Delete account']) {
      await _toggleDisclosure(tester, title);
    }
    await _tapAccountAction(tester, 'Save profile');
    expect(find.text('Username is already in use'), findsOneWidget);
    for (final title in ['Identity details', 'Example provider', 'Password', 'Account details', 'Delete account']) {
      await _toggleDisclosure(tester, title);
    }
    expect(tester.widget<TextFormField>(_profileField('Username')).controller!.text, 'alice_draft');
    expect(tester.widget<TextFormField>(_profileField('Display name')).controller!.text, 'Alice draft');
    expect(find.text('Username is already in use'), findsOneWidget);
    await tester.ensureVisible(find.byTooltip('Refresh account'));
    await tester.tap(find.byTooltip('Refresh account'));
    await tester.pumpAndSettle();
    expect(find.text('Discard unsaved changes?'), findsOneWidget);
    await tester.tap(find.descendant(of: find.byType(AlertDialog), matching: find.text('Cancel')));
    await tester.pumpAndSettle();
    await _tapAccountAction(tester, 'Back to groups');
    expect(find.text('Discard unsaved changes?'), findsOneWidget);
    await tester.tap(find.descendant(of: find.byType(AlertDialog), matching: find.text('Cancel')));
    await tester.pumpAndSettle();
    expect(router.routeInformationProvider.value.uri.path, '/account');
    expect(tester.widget<TextFormField>(_profileField('Display name')).controller!.text, 'Alice draft');
    expect(h.requests.where((request) => request.method == 'GET' &&
      request.url.path.endsWith('/users/me')), hasLength(1));
    final save = h.requests.singleWhere((request) => request.method == 'PUT');
    expect(jsonDecode(save.body), {'username': 'alice_draft', 'displayName': 'Alice draft'});
  });

  testWidgets('security loading failure is retryable without opening methods or losing profile input', (tester) async {
    var available = false;
    final authentication = _authentication();
    final h = _Harness((request) async {
      if (request.url.path.endsWith('/authentication') && !available) {
        return http.Response('Sign-in methods unavailable', 503);
      }
      return _accountResponse(request, authentication);
    });
    await _openAccount(tester, h);
    expect(find.text('Sign-in methods unavailable'), findsOneWidget);
    await tester.enterText(_profileField('Display name'), 'Alice retry draft');
    available = true;
    await _tapAccountAction(tester, 'Retry sign-in methods');
    expect(find.text('Sign-in methods unavailable'), findsNothing);
    expect(find.text('Change password'), findsNothing);
    expect(tester.widget<TextFormField>(_profileField('Display name')).controller!.text, 'Alice retry draft');
    await _toggleDisclosure(tester, 'Password');
    expect(find.text('Change password'), findsOneWidget);
    expect(h.requests.where((request) => request.url.path.endsWith('/authentication')), hasLength(2));
  });

  testWidgets('SSO-only account cannot unlink its last method', (tester) async {
    final authentication = _authentication(hasPassword: false, linked: true, method: 'oidc');
    final h = _Harness((request) async => _accountResponse(request, authentication));
    await _openAccount(tester, h);
    await _toggleDisclosure(tester, 'Example provider');
    await _tapAccountAction(tester, 'Unlink SSO identity');
    expect(find.textContaining('Cannot remove the last sign-in method'), findsOneWidget);
    expect(find.byType(AlertDialog), findsNothing);
    expect(h.session.isAuthenticated, isTrue);
    expect(h.requests.where((request) => request.url.path.endsWith('/unlink')), isEmpty);
    await _toggleDisclosure(tester, 'Example provider');
    expect(find.textContaining('Cannot remove the last sign-in method'), findsOneWidget);
  });

  for (final proof in ['missing', 'different flow', 'expired', 'future']) {
    test('$proof tab proof never exchanges or establishes a session', () async {
      final h = _Harness((_) async => _json({'status': 'authenticated', 'session': _login()}));
      addTearDown(h.dispose);
      if (proof != 'missing') h.browser.seed(
        flowId: proof == 'different flow' ? '00000000-0000-0000-0000-000000000099' : _flowId,
        createdAt: proof == 'expired' ? _now.subtract(const Duration(hours: 1)) :
          proof == 'future' ? _now.add(const Duration(hours: 1)) : _now);
      await h.flow.handleCallback(_callback);
      expect(h.exchanges, isEmpty);
      expect(h.session.isAuthenticated, isFalse);
      expect(h.flow.error, isNotNull);
      expect(h.browser.stored, isNull);
      expect(h.browser.replacements.every((uri) => !uri.toString().contains(_handoff)), isTrue);
    });
  }

  test('valid callback strips proof before exactly one exchange and accepts only its response', () async {
    late _Harness h;
    h = _Harness((request) async {
      expect(h.browser.stored, isNull);
      expect(h.browser.replacements.last.queryParameters, isEmpty);
      expect(h.browser.replacements.last.toString(), isNot(contains(_handoff)));
      expect(jsonDecode(request.body), {'flowId': _flowId, 'code': _handoff, 'browserVerifier': _verifier});
      expect(request.headers.containsKey('Authorization'), isFalse);
      return _json({'status': 'authenticated', 'session': _login()});
    });
    addTearDown(h.dispose);
    h.browser.seed();
    final destination = await h.flow.handleCallback(_callback);
    await h.flow.handleCallback(_callback);
    expect(h.exchanges, hasLength(1));
    expect(destination, '/groups');
    expect(h.session.user!.userId, _alice['userId']);
    expect(h.browser.stored, isNull);
  });

  test('duplicate callback parameters are rejected without exchange', () async {
    final h = _Harness((_) async => throw StateError('Unexpected exchange'));
    addTearDown(h.dispose);
    h.browser.seed();
    await h.flow.handleCallback(Uri.parse('$_callback&code=$_handoff'));
    expect(h.exchanges, isEmpty);
    expect(h.session.isAuthenticated, isFalse);
  });

  test('unsolicited callback cannot replace an authenticated identity', () async {
    final h = _Harness((_) async => _json(_login(_bob)));
    addTearDown(h.dispose);
    await h.session.login('bob', 'password');
    h.browser.seed();
    await h.flow.handleCallback(_callback);
    expect(h.session.user!.userId, _bob['userId']);
    expect(h.session.accessToken, 'application-bob');
    expect(h.flow.error, 'A new sign-in started; restart SSO');
    expect(h.exchanges, isEmpty);
  });

  for (final newerLogin in [false, true]) {
    test('late callback after ${newerLogin ? 'new login' : 'logout'} cannot establish an obsolete session', () async {
      final response = Completer<http.Response>();
      final started = Completer<void>();
      final h = _Harness((request) async {
        if (request.url.path.endsWith('/exchange')) { started.complete(); return response.future; }
        if (request.url.path.endsWith('/login')) return _json(_login(_bob));
        return http.Response('', 204);
      });
      addTearDown(h.dispose);
      h.browser.seed();
      final callback = h.flow.handleCallback(_callback);
      await started.future;
      if (newerLogin) { await h.session.login('bob', 'password'); } else { await h.session.logout(); }
      response.complete(_json({'status': 'authenticated', 'session': _login()}));
      await callback;
      expect(h.session.user?.userId, newerLogin ? _bob['userId'] : null);
      expect(h.flow.hasContinuation, isFalse);
    });
  }

  test('late onboarding result after logout cannot restore continuation', () async {
    final response = Completer<http.Response>();
    final started = Completer<void>();
    final h = _Harness((_) { started.complete(); return response.future; });
    addTearDown(h.dispose);
    h.browser.seed();
    final callback = h.flow.handleCallback(_callback);
    await started.future;
    await h.session.logout();
    response.complete(_json(_registration()));
    await callback;
    expect(h.flow.hasContinuation, isFalse);
    expect(h.session.isAuthenticated, isFalse);
  });

  test('email hint and failed explicit linking grant no session; conflict retains continuation', () async {
    final h = _Harness((request) async {
      if (request.url.path.endsWith('/exchange')) return _json(_registration());
      return http.Response('Username is already in use', 409);
    });
    addTearDown(h.dispose);
    h.browser.seed();
    await h.flow.handleCallback(_callback);
    expect(h.session.isAuthenticated, isFalse);
    expect(h.flow.result, isA<OidcRegistrationRequired>());
    await h.flow.register(username: 'alice', displayName: 'Alice', email: 'alice@example.test');
    expect(h.flow.hasContinuation, isTrue);
    await h.flow.linkExisting(identifier: 'alice', password: 'wrong-password');
    expect(h.session.isAuthenticated, isFalse);
    expect(h.flow.hasContinuation, isTrue);
    expect(h.browser.stored, isNull);
  });

  test('link confirmation is never automatic and cancel retires a delayed confirmation', () async {
    final confirmed = Completer<http.Response>();
    final started = Completer<void>();
    final h = _Harness((request) async {
      if (request.url.path.endsWith('/exchange')) return _json(_confirmation());
      if (request.url.path.endsWith('/confirm-link')) { started.complete(); return confirmed.future; }
      return http.Response('', 204);
    });
    addTearDown(h.dispose);
    h.browser.seed();
    await h.flow.handleCallback(_callback);
    expect(h.requests.where((r) => r.url.path.endsWith('/confirm-link')), isEmpty);
    expect(h.session.isAuthenticated, isFalse);
    final confirmation = h.flow.confirmLink();
    await started.future;
    await h.flow.cancel();
    confirmed.complete(_json(_login()));
    await confirmation;
    expect(h.session.isAuthenticated, isFalse);
    expect(h.flow.hasContinuation, isFalse);
  });

  test('redirect storage denial prevents navigation and retains authenticated account', () async {
    final h = _Harness((request) async => request.url.path.endsWith('/login') ? _json(_login()) :
      _json({'flowId': _flowId, 'authorizationUrl': 'https://provider.example.test/authorize'}));
    addTearDown(h.dispose);
    await h.session.login('alice', 'password');
    h.browser.storageDenied = true;
    await h.flow.start(purpose: AuthFlowPurpose.link, returnTo: '/account');
    expect(h.browser.navigations, isEmpty);
    expect(h.flow.error, isNotNull);
    expect(h.session.isAuthenticated, isTrue);
    final request = h.requests.last;
    expect(request.url.path, '/api/users/me/oidc/start');
    expect(request.headers['Authorization'], 'Bearer application-alice');
    expect(jsonDecode(request.body), isNot(contains('browserVerifier')));
  });

  for (final destination in ['https://evil.example.test/', '//evil.example.test/', '/auth/oidc/callback', '/auth/invitation', '/unknown']) {
    test('untrusted return target $destination is replaced by groups', () async {
      final h = _Harness((_) async => _json({'flowId': _flowId, 'authorizationUrl': 'https://provider.example.test/authorize'}));
      addTearDown(h.dispose);
      await h.flow.start(returnTo: destination);
      expect(h.browser.stored!['returnTo'], '/groups');
      expect(h.browser.stored!.keys.toSet(), {'flowId', 'browserVerifier', 'returnTo', 'purpose', 'createdAt'});
      expect(h.browser.stored!.values, isNot(contains(_continuation)));
      expect(h.browser.stored!.values, isNot(contains('application-alice')));
    });
  }

  test('startup clears obsolete proof but preserves the matching callback until pinned', () async {
    final h = _Harness((_) async => http.Response('', 204));
    addTearDown(h.dispose);
    h.browser.seed();
    h.flow.prepareStartup(Uri.parse('/login'));
    expect(h.browser.stored, isNull);
    h.browser.seed();
    h.flow.prepareStartup(_callback);
    expect(h.browser.stored?['flowId'], _flowId);
    await h.session.login('alice', 'password');
    expect(h.browser.stored, isNull);
  });

  test('mixed exchange states and missing nullable fields fail closed', () async {
    final malformed = <Map<String, Object?>>[
      {..._registration(), 'session': _login()},
      {'status': 'unknown', 'session': _login()},
      {..._registration(), 'profile': {'username': 'alice', 'displayName': 'Alice', 'emailVerified': true}},
    ];
    for (final response in malformed) {
      final h = _Harness((_) async => _json(response));
      try {
        h.browser.seed();
        await h.flow.handleCallback(_callback);
        expect(h.session.isAuthenticated, isFalse);
        expect(h.flow.hasContinuation, isFalse);
        expect(h.flow.error, isNotNull);
      } finally {
        h.dispose();
      }
    }
  });

  testWidgets('unauthenticated callback route requires explicit identity confirmation', (tester) async {
    final h = _Harness((request) async {
      if (request.url.path.endsWith('/options')) return _json({
        'localRegistrationEnabled': true, 'ssoRegistrationEnabled': true,
        'ssoEnabled': true, 'providerName': 'Example provider',
      });
      if (request.url.path.endsWith('/exchange')) return _json(_confirmation());
      if (request.url.path.endsWith('/confirm-link')) return _json(_login());
      if (request.url.path.endsWith('/groups')) return _json({
        'items': [], 'pageInfo': {'offset': 0, 'limit': 20, 'total': 0, 'hasNext': false, 'hasPrev': false},
      });
      return http.Response('Not found', 404);
    });
    addTearDown(h.dispose);
    addTearDown(() => tester.pumpWidget(const SizedBox.shrink()));
    await tester.pumpWidget(JizhangApp(repository: h.repository, session: h.session));
    await tester.pumpAndSettle();
    final router = GoRouter.of(tester.element(find.byType(AuthScreen)));
    h.browser.seed(createdAt: DateTime.now().toUtc());
    router.go(_callback.toString());
    await tester.pumpAndSettle();
    expect(find.byType(OidcCallbackScreen), findsOneWidget);
    expect(h.session.isAuthenticated, isFalse);
    expect(h.requests.where((r) => r.url.path.endsWith('/confirm-link')), isEmpty);
    expect(find.textContaining('synthetic-alice'), findsOneWidget);
    final confirm = find.widgetWithText(FilledButton, 'Link identity');
    await tester.ensureVisible(confirm);
    await tester.tap(confirm);
    await tester.pumpAndSettle();
    expect(h.session.user!.userId, _alice['userId']);
    expect(router.routeInformationProvider.value.uri.path, '/groups');
    expect(h.requests.where((r) => r.url.path.endsWith('/confirm-link')), hasLength(1));
  });

  testWidgets('opening a reset invitation strips its token and only inspects it', (tester) async {
    late _Harness h;
    h = _Harness((request) async {
      expect(h.browser.replacements, isNotEmpty);
      expect(h.browser.replacements.last.toString(), isNot(contains(_handoff)));
      if (request.url.path.endsWith('/inspect')) return _json({
        'purpose': 'password_reset', 'target': _alice,
        'expiresAt': _now.add(const Duration(minutes: 10)).toIso8601String(),
      });
      throw StateError('Invitation load must not mutate credentials');
    });
    addTearDown(h.dispose);
    addTearDown(() => tester.pumpWidget(const SizedBox.shrink()));
    await tester.pumpWidget(MaterialApp(home: InvitationScreen(flow: h.flow,
      location: Uri(path: '/auth/invitation', queryParameters: {'token': _handoff}))));
    await tester.pumpAndSettle();
    expect(h.requests.map((r) => r.url.path), ['/api/auth/invitations/inspect']);
    expect(h.session.isAuthenticated, isFalse);
    expect(find.byType(TextFormField), findsNWidgets(2));
    expect(h.browser.stored, isNull);
  });

  for (final local in [false, true]) {
    for (final sso in [false, true]) {
      testWidgets('signup switches local=$local SSO=$sso preserve local sign-in', (tester) async {
        final h = _Harness((request) async => _json({
          'localRegistrationEnabled': local, 'ssoRegistrationEnabled': sso,
          'ssoEnabled': true, 'providerName': 'Example provider',
        }));
        addTearDown(h.dispose);
        await tester.pumpWidget(MaterialApp(home: AuthScreen(session: h.session, authFlow: h.flow)));
        await tester.pumpAndSettle();
        expect(tester.widget<FilledButton>(find.widgetWithText(FilledButton, 'Sign in')).onPressed, isNotNull);
        expect(find.text('Create an account'), local ? findsOneWidget : findsNothing);
        expect(find.text('Continue with Example provider'), findsOneWidget);
        await tester.pumpWidget(MaterialApp(home: AuthScreen(key: const ValueKey('registration'),
          session: h.session, authFlow: h.flow, register: true)));
        await tester.pumpAndSettle();
        expect(tester.widget<FilledButton>(find.widgetWithText(FilledButton, 'Create account')).onPressed,
          local ? isNotNull : isNull);
        expect(find.text('Continue with Example provider'), sso ? findsOneWidget : findsNothing);
        await tester.pumpWidget(const SizedBox.shrink());
      });
    }
  }

  for (final register in [false, true]) {
    testWidgets('licenses modal preserves ${register ? 'signup' : 'login'} drafts', (tester) async {
      final h = _Harness((request) async => _accountResponse(request, _authentication()));
      await _openAuth(tester, h, register: register);
      await tester.ensureVisible(find.widgetWithText(TextButton, 'Open-source licenses'));
      await tester.tap(find.widgetWithText(TextButton, 'Open-source licenses'));
      await tester.pump();
      await tester.pump(const Duration(milliseconds: 300));
      expect(find.byType(LicensePage), findsOneWidget);
      await tester.tap(find.widgetWithText(TextButton, 'Close licenses'));
      await tester.pumpAndSettle();
      expect(tester.widget<TextFormField>(_profileField(register ? 'Username' : 'Username or email')).controller!.text, 'alice-local');
      expect(tester.widget<TextFormField>(_profileField('Password')).controller!.text, 'alice-local-password');
      if (register) {
        expect(tester.widget<TextFormField>(_profileField('Display name')).controller!.text, 'Alice local draft');
        expect(tester.widget<TextFormField>(_profileField('Email')).controller!.text, 'alice-local@example.test');
      }
      expect(h.requests.where((request) => request.url.path.endsWith('/register') || request.url.path.endsWith('/login')), isEmpty);
    });
  }

  testWidgets('failed options offers Retry without enabling signup or blocking local login', (tester) async {
    var available = false;
    final h = _Harness((_) async => available ? _json({
      'localRegistrationEnabled': true, 'ssoRegistrationEnabled': true,
      'ssoEnabled': true, 'providerName': 'Example provider',
    }) : http.Response('Options unavailable', 503));
    addTearDown(h.dispose);
    await tester.pumpWidget(MaterialApp(home: AuthScreen(session: h.session, authFlow: h.flow)));
    await tester.pumpAndSettle();
    expect(find.text('Create an account'), findsNothing);
    expect(find.text('Continue with Example provider'), findsNothing);
    expect(tester.widget<FilledButton>(find.widgetWithText(FilledButton, 'Sign in')).onPressed, isNotNull);
    available = true;
    await tester.tap(find.text('Retry'));
    await tester.pumpAndSettle();
    expect(find.text('Create an account'), findsOneWidget);
    await tester.pumpWidget(const SizedBox.shrink());
  });

  testWidgets('failed local login retains editable credentials across router generations', (tester) async {
    final h = _Harness((request) async {
      if (request.url.path.endsWith('/options')) return _json({
        'localRegistrationEnabled': false, 'ssoRegistrationEnabled': false,
        'ssoEnabled': false, 'providerName': 'Example provider',
      });
      return http.Response('Incorrect credentials', 401);
    });
    addTearDown(h.dispose);
    await tester.pumpWidget(JizhangApp(repository: h.repository, session: h.session));
    await tester.pumpAndSettle();
    await tester.enterText(find.byType(TextFormField).at(0), 'alice');
    await tester.enterText(find.byType(TextFormField).at(1), 'wrong-password');
    await tester.tap(find.widgetWithText(FilledButton, 'Sign in'));
    await tester.pumpAndSettle();
    expect(find.text('Incorrect credentials'), findsOneWidget);
    expect(tester.widget<TextFormField>(find.byType(TextFormField).at(0)).controller!.text, 'alice');
    expect(tester.widget<TextFormField>(find.byType(TextFormField).at(1)).controller!.text, 'wrong-password');
    expect(h.session.isAuthenticated, isFalse);
    await tester.pumpWidget(const SizedBox.shrink());
  });

  testWidgets('current password is verified before new-password collection and success retires session', (tester) async {
    final h = _Harness((request) async {
      if (request.url.path.endsWith('/login')) return _json(_login());
      if (request.url.path.endsWith('/authentication')) return _json({
        'hasPassword': true, 'identity': null, 'sessionMethod': 'password',
        'authenticatedAt': _now.toIso8601String(), 'oidcReauthenticatedAt': null,
        'providerEnabled': false, 'canSignOutProvider': false,
      });
      if (request.url.path.endsWith('/reauthenticate')) {
        return jsonDecode(request.body)['password'] == 'correct-password'
          ? http.Response('', 204) : http.Response('Incorrect current password', 403);
      }
      if (request.method == 'PUT' && request.url.path.endsWith('/password')) return http.Response('', 204);
      return http.Response('Not found', 404);
    });
    addTearDown(h.dispose);
    await h.session.login('alice', 'correct-password');
    await tester.pumpWidget(MaterialApp(home: Scaffold(body: AccountSecuritySection(
      session: h.session, authFlow: h.flow, canLeave: () async => true,
      beforeExternalNavigation: () async => true, onPendingChanged: (_) {}))));
    await tester.pumpAndSettle();
    await _toggleDisclosure(tester, 'Password');
    await tester.tap(find.text('Change password'));
    await tester.pumpAndSettle();
    expect(find.byType(TextFormField), findsNothing);
    await tester.enterText(find.byType(TextField), 'wrong-password');
    await tester.tap(find.widgetWithText(FilledButton, 'Verify'));
    await tester.pumpAndSettle();
    expect(find.text('Incorrect current password'), findsOneWidget);
    expect(find.byType(TextFormField), findsNothing);
    expect(tester.widget<TextField>(find.byType(TextField)).controller!.text, 'wrong-password');
    await tester.enterText(find.byType(TextField), 'correct-password');
    await tester.tap(find.widgetWithText(FilledButton, 'Verify'));
    await tester.pumpAndSettle();
    expect(find.byType(TextFormField), findsNWidgets(2));
    await tester.enterText(find.byType(TextFormField).at(0), 'new-password');
    await tester.enterText(find.byType(TextFormField).at(1), 'new-password');
    await tester.tap(find.widgetWithText(FilledButton, 'Save password'));
    await tester.pumpAndSettle();
    expect(h.session.isAuthenticated, isFalse);
    final mutation = h.requests.singleWhere((request) => request.method == 'PUT');
    expect(jsonDecode(mutation.body), {'currentPassword': 'correct-password', 'newPassword': 'new-password'});
    expect(h.requests.where((request) => request.url.path.endsWith('/logout')), isEmpty);
    await tester.pumpWidget(const SizedBox.shrink());
  });

  testWidgets('ordinary SSO login cannot collect a password without explicit provider verification', (tester) async {
    final h = _Harness((request) async {
      if (request.url.path.endsWith('/login')) return _json(_login());
      return _json({
        'hasPassword': false,
        'identity': {...(_confirmation()['identity'] as Map<String, Object?>),
          'linkedAt': _now.toIso8601String(), 'lastLoginAt': null},
        'sessionMethod': 'oidc', 'authenticatedAt': _now.toIso8601String(),
        'oidcReauthenticatedAt': null, 'providerEnabled': true, 'canSignOutProvider': true,
      });
    });
    addTearDown(h.dispose);
    await h.session.login('alice', 'password');
    await tester.pumpWidget(MaterialApp(home: Scaffold(body: SingleChildScrollView(child: AccountSecuritySection(
      session: h.session, authFlow: h.flow, canLeave: () async => true,
      beforeExternalNavigation: () async => true, onPendingChanged: (_) {})))));
    await tester.pumpAndSettle();
    await _toggleDisclosure(tester, 'Password');
    await tester.tap(find.text('Set password'));
    await tester.pumpAndSettle();
    expect(find.text('Reauthentication required'), findsOneWidget);
    expect(find.byType(TextFormField), findsNothing);
    expect(h.requests.where((request) => request.method == 'PUT'), isEmpty);
    expect(find.widgetWithText(FilledButton, 'Verify with provider'), findsOneWidget);
    await tester.tap(find.widgetWithText(TextButton, 'Cancel'));
    await tester.pumpAndSettle();
    expect(h.session.isAuthenticated, isTrue);
    await tester.pumpWidget(const SizedBox.shrink());
  });

  testWidgets('registration conflict retains editable hints and expired continuation retires the form', (tester) async {
    var expired = false;
    final h = _Harness((request) async {
      if (request.url.path.endsWith('/exchange')) return _json(_registration());
      if (request.url.path.endsWith('/options')) return _json({
        'localRegistrationEnabled': true, 'ssoRegistrationEnabled': true,
        'ssoEnabled': true, 'providerName': 'Example provider',
      });
      return http.Response(expired ? 'Invalid or expired continuation' : 'Username is already in use', expired ? 400 : 409);
    });
    addTearDown(h.dispose);
    h.browser.seed();
    await h.flow.handleCallback(_callback);
    await tester.pumpWidget(MaterialApp(home: OidcCallbackScreen(flow: h.flow,
      location: Uri(path: '/auth/oidc/continue'))));
    await tester.pumpAndSettle();
    await tester.enterText(find.byType(TextFormField).at(0), 'chosen_alice');
    await tester.enterText(find.byType(TextFormField).at(1), 'Chosen Alice');
    await tester.enterText(find.byType(TextFormField).at(2), 'chosen@example.test');
    await tester.ensureVisible(find.widgetWithText(TextButton, 'Open-source licenses'));
    await tester.tap(find.widgetWithText(TextButton, 'Open-source licenses'));
    await tester.pump();
    await tester.pump(const Duration(milliseconds: 300));
    await tester.tap(find.widgetWithText(TextButton, 'Close licenses'));
    await tester.pumpAndSettle();
    expect(tester.widget<TextFormField>(find.byType(TextFormField).at(1)).controller!.text, 'Chosen Alice');
    expect(h.requests.where((request) => request.url.path.endsWith('/exchange')), hasLength(1));
    await tester.ensureVisible(find.widgetWithText(FilledButton, 'Create account with Example provider'));
    await tester.tap(find.widgetWithText(FilledButton, 'Create account with Example provider'));
    await tester.pumpAndSettle();
    expect(find.text('Username is already in use'), findsOneWidget);
    expect(tester.widget<TextFormField>(find.byType(TextFormField).at(0)).controller!.text, 'chosen_alice');
    expect(tester.widget<TextFormField>(find.byType(TextFormField).at(2)).controller!.text, 'chosen@example.test');
    expect(h.session.isAuthenticated, isFalse);
    expired = true;
    await tester.tap(find.widgetWithText(FilledButton, 'Create account with Example provider'));
    await tester.pumpAndSettle();
    expect(find.byType(TextFormField), findsNothing);
    expect(find.text('Restart sign-in'), findsOneWidget);
    expect(h.flow.hasContinuation, isFalse);
    expect(h.session.isAuthenticated, isFalse);
    await tester.pumpWidget(const SizedBox.shrink());
  });

  testWidgets('expired invitation offers no credential mutation', (tester) async {
    final h = _Harness((_) async => _json({
      'purpose': 'password_reset', 'target': _alice,
      'expiresAt': _now.toIso8601String(),
    }));
    addTearDown(h.dispose);
    await tester.pumpWidget(MaterialApp(home: InvitationScreen(flow: h.flow,
      location: Uri(path: '/auth/invitation', queryParameters: {'token': _handoff}))));
    await tester.pumpAndSettle();
    expect(find.byType(TextFormField), findsNothing);
    expect(find.textContaining('Request a new invitation'), findsOneWidget);
    expect(h.session.isAuthenticated, isFalse);
    expect(h.requests.map((request) => request.url.path), ['/api/auth/invitations/inspect']);
    await tester.pumpWidget(const SizedBox.shrink());
  });

  testWidgets('explicit invitation reset clears an existing session without automatic sign-in', (tester) async {
    final h = _Harness((request) async {
      if (request.url.path.endsWith('/login')) return _json(_login());
      if (request.url.path.endsWith('/inspect')) return _json({
        'purpose': 'password_reset', 'target': _alice,
        'expiresAt': _now.add(const Duration(minutes: 10)).toIso8601String(),
      });
      return http.Response('', 204);
    });
    addTearDown(h.dispose);
    await h.session.login('alice', 'password');
    await tester.pumpWidget(MaterialApp(home: InvitationScreen(flow: h.flow,
      location: Uri(path: '/auth/invitation', queryParameters: {'token': _handoff}))));
    await tester.pumpAndSettle();
    expect(h.session.isAuthenticated, isTrue);
    await tester.enterText(find.byType(TextFormField).at(0), 'replacement-password');
    await tester.enterText(find.byType(TextFormField).at(1), 'replacement-password');
    await tester.tap(find.widgetWithText(FilledButton, 'Reset password'));
    await tester.pumpAndSettle();
    expect(h.session.isAuthenticated, isFalse);
    expect(find.textContaining('Password updated.'), findsOneWidget);
    expect(find.byType(TextFormField), findsNothing);
    expect(h.requests.where((request) => request.url.path.endsWith('/password-reset')), hasLength(1));
    expect(h.requests.where((request) => request.url.path.endsWith('/login')), hasLength(1));
    expect(h.browser.stored, isNull);
    await tester.pumpWidget(const SizedBox.shrink());
  });

  testWidgets('router retires invitation query without losing the inspected form or inspecting twice', (tester) async {
    final h = _Harness((request) async {
      if (request.url.path.endsWith('/options')) return _json({
        'localRegistrationEnabled': true, 'ssoRegistrationEnabled': true,
        'ssoEnabled': true, 'providerName': 'Example provider',
      });
      return _json({'purpose': 'password_reset', 'target': _alice,
        'expiresAt': DateTime.now().toUtc().add(const Duration(minutes: 10)).toIso8601String()});
    });
    addTearDown(h.dispose);
    addTearDown(() => tester.pumpWidget(const SizedBox.shrink()));
    await tester.pumpWidget(JizhangApp(repository: h.repository, session: h.session));
    await tester.pumpAndSettle();
    final router = GoRouter.of(tester.element(find.byType(AuthScreen)));
    router.go(Uri(path: '/auth/invitation', queryParameters: {'token': _handoff}).toString());
    await tester.pumpAndSettle();
    expect(router.routeInformationProvider.value.uri.toString(), '/auth/invitation');
    expect(find.byType(TextFormField), findsNWidgets(2));
    expect(h.requests.where((request) => request.url.path.endsWith('/inspect')), hasLength(1));
    expect(h.requests.where((request) => request.url.path.endsWith('/password-reset')), isEmpty);
    expect(h.session.isAuthenticated, isFalse);
  });
}
