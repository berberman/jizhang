import 'package:flutter/foundation.dart';
import 'api_client.dart';

import 'auth_browser.dart';
import 'jizhang_repository.dart';
import 'models.dart';
import 'session_controller.dart';

enum AuthFlowPurpose { login, invitation, link, reauth }

class AuthFlowController extends ChangeNotifier {
  AuthFlowController({required this.session, AuthBrowser? browser, DateTime Function()? clock})
      : browser = browser ?? session.authBrowser, _clock = clock ?? DateTime.now {
    _generation = session.generation;
    session.addListener(_sessionChanged);
  }

  final SessionController session;
  final AuthBrowser browser;
  final DateTime Function() _clock;
  JizhangRepository get repository => session.repository;
  bool _pending = false;
  String? _error;
  DateTime get now => _clock().toUtc();
  OidcExchangeResult? _result;
  String _returnTo = '/groups';
  AuthFlowPurpose _purpose = AuthFlowPurpose.login;
  int _epoch = 0;
  int _generation = 0;
  int? _request;
  bool _ownSessionChange = false;
  bool _disposed = false;
  final Set<String> _handledCallbacks = {};
  bool get pending => _pending;
  String? get error => _error;
  OidcExchangeResult? get result => _result;
  String get returnTo => _returnTo;
  AuthFlowPurpose get purpose => _purpose;
  bool get hasContinuation => _result is OidcRegistrationRequired || _result is OidcLinkConfirmationRequired;

  void _sessionChanged() {
    if (_ownSessionChange || _generation == session.generation) return;
    _generation = session.generation;
    clear();
  }

  bool _current(int epoch, int generation) => !_disposed && epoch == _epoch && generation == session.generation;
  void _notify() { if (!_disposed) notifyListeners(); }
  void _clearProof() { if (browser.supported) browser.clearPending(); }
  void _fail(String message) {
    _error = message;
    final request = _request;
    if (request != null) session.failExternalAuthentication(message, request: request);
  }
  String _describe(Object failure) => failure is ApiException ? failure.message : failure is StateError ? failure.message.toString() : 'Sign-in could not be completed. Restart SSO.';
  static String safeReturnTo(String value, {AuthFlowPurpose purpose = AuthFlowPurpose.login}) {
    if (purpose == AuthFlowPurpose.link || purpose == AuthFlowPurpose.reauth) return '/account';
    final uri = Uri.tryParse(value);
    if (uri == null || uri.hasScheme || uri.hasAuthority || !value.startsWith('/') || value.startsWith('//') || uri.hasFragment || uri.hasQuery || value.contains('\\')) return '/groups';
    final path = uri.path;
    if (path == '/groups' || path == '/groups/new' || path == '/account') return path;
    final segments = uri.pathSegments;
    if (segments.length < 3 || segments.first != 'groups' || !segments.skip(1).every((part) => RegExp(r'^[A-Za-z0-9_-]+$').hasMatch(part))) return '/groups';
    final section = segments[2];
    if (segments.length == 3 && {'records', 'receipts', 'members', 'balances', 'statistics', 'import'}.contains(section)) return path;
    if (segments.length == 4 && ({'records', 'receipts'}.contains(section) || ({'expenses', 'incomes', 'transfers'}.contains(section) && segments[3] == 'new'))) return path;
    if (segments.length == 5 && {'expenses', 'incomes', 'transfers', 'receipts'}.contains(section) && segments[4] == 'edit') return path;
    return '/groups';
  }

  void prepareStartup(Uri route) {
    if (!browser.supported) return;
    try {
      final proof = browser.readPending();
      if (route.path != '/auth/oidc/callback' || proof == null || !_validProof(proof) || route.queryParametersAll['flow']?.length != 1 || route.queryParameters['flow'] != proof['flowId']) _clearProof();
    } catch (_) { try { _clearProof(); } catch (_) {} }
  }

  bool _validProof(Map<String, String> proof) {
    final created = DateTime.tryParse(proof['createdAt'] ?? '');
    final age = created == null ? null : _clock().toUtc().difference(created.toUtc());
    return proof.length == 5 && proof.keys.every({'flowId', 'browserVerifier', 'returnTo', 'purpose', 'createdAt'}.contains) && (proof['flowId'] ?? '').isNotEmpty && RegExp(r'^[A-Za-z0-9_-]{43}$').hasMatch(proof['browserVerifier'] ?? '') && AuthFlowPurpose.values.any((p) => p.name == proof['purpose']) && age != null && !age.isNegative && age <= const Duration(minutes: 11);
  }

  Future<void> start({AuthFlowPurpose purpose = AuthFlowPurpose.login, String returnTo = '/groups', String? invitationToken}) async {
    if (_pending) return;
    clear();
    final epoch = ++_epoch;
    final generation = session.generation;
    _pending = true;
    _returnTo = safeReturnTo(returnTo, purpose: purpose);
    _purpose = purpose;
    _notify();
    var handedOff = false;
    try {
      if (!browser.supported) throw StateError('SSO is available in the web browser only.');
      _clearProof();
      if (purpose == AuthFlowPurpose.login || purpose == AuthFlowPurpose.invitation) {
        _begin();
      }
      final activeGeneration = session.generation;
      final verifier = await browser.newVerifier();
      if (!_current(epoch, activeGeneration)) return;
      final challenge = await browser.challenge(verifier);
      if (!_current(epoch, activeGeneration)) return;
      final response = purpose == AuthFlowPurpose.link || purpose == AuthFlowPurpose.reauth
          ? await repository.startAccountOidc(purpose: purpose == AuthFlowPurpose.link ? AccountOidcPurpose.link : AccountOidcPurpose.reauth, browserChallenge: challenge)
          : await repository.startOidc(challenge, invitationToken: invitationToken);
      if (!_current(epoch, activeGeneration)) return;
      browser.savePending({'flowId': response.flowId, 'browserVerifier': verifier, 'returnTo': _returnTo, 'purpose': purpose.name, 'createdAt': _clock().toUtc().toIso8601String()});
      browser.navigate(response.authorizationUrl);
      handedOff = true;
    } catch (failure) {
      if (!_disposed && epoch == _epoch && (generation == session.generation || _generation == session.generation)) {
        _fail(_describe(failure));
        try { _clearProof(); } catch (_) {}
      }
    } finally {
      if (!_disposed && epoch == _epoch) { _pending = handedOff; _notify(); }
    }
  }

  void _begin() {
    _ownSessionChange = true;
    try { _request = session.beginExternalAuthentication(); _generation = session.generation; }
    finally { _ownSessionChange = false; }
  }
  void _cleanUrl(String route) {
    if (!browser.supported) return;
    browser.replaceUrl(Uri.base.replace(query: '', fragment: route));
  }

  Future<String?> handleCallback(Uri location) async {
    if (location.path != '/auth/oidc/callback') return null;
    final fingerprint = location.toString();
    if (!_handledCallbacks.add(fingerprint)) return null;
    final epoch = ++_epoch;
    _request = null;
    _pending = true; _error = null; _result = null;
    try {
      Map<String, String>? proof;
      try {
        if (browser.supported) proof = browser.readPending();
      } finally {
        _cleanUrl('/auth/oidc/continue');
        _clearProof();
      }
      if (session.isAuthenticated) throw StateError('A new sign-in started; restart SSO');
      final query = location.queryParametersAll;
      if (proof == null || !_validProof(proof) || query.keys.any((key) => !{'flow', 'code', 'error'}.contains(key)) || query.values.any((values) => values.length != 1 || values.single.isEmpty) || query['flow']?.single != proof['flowId']) throw StateError('This sign-in is invalid or expired. Restart SSO.');
      final purposeName = proof['purpose'];
      final purpose = AuthFlowPurpose.values.firstWhere((p) => p.name == purposeName);
      _purpose = purpose;
      _returnTo = safeReturnTo(proof['returnTo'] ?? '/groups', purpose: purpose);
      if (query.containsKey('error')) {
        throw StateError(switch (query['error']!.single) {
          'access_denied' => 'Provider authorization was denied (access_denied).',
          'expired' => 'This provider authentication attempt expired (expired).',
          'configuration_changed' => 'The sign-in configuration or starting session changed (configuration_changed).',
          'provider_unavailable' => 'The authentication provider could not be reached (provider_unavailable).',
          _ => 'The provider authentication response could not be verified (invalid_response).',
        });
      }
      final code = query['code']?.single;
      if (code == null) throw StateError('This sign-in is invalid or expired. Restart SSO.');
      _begin();
      final generation = session.generation;
      final response = await repository.exchangeOidc(flowId: proof['flowId']!, code: code, browserVerifier: proof['browserVerifier']!);
      if (!_current(epoch, generation)) return null;
      if (response is OidcAuthenticated) return _accept(response.session);
      _result = response;
    } catch (failure) {
      if (!_disposed && epoch == _epoch) _fail(_describe(failure));
    } finally {
      if (!_disposed && epoch == _epoch) { _pending = false; _notify(); }
    }
    return null;
  }

  String? get _continuation {
    final value = _result;
    if (value is OidcRegistrationRequired) return value.continuationToken;
    if (value is OidcLinkConfirmationRequired) return value.continuationToken;
    return null;
  }
  String? _accept(LoginSession value) {
    _ownSessionChange = true;
    try { session.acceptExternalSession(value, request: _request!); _generation = session.generation; }
    finally { _ownSessionChange = false; }
    if (session.accessToken != value.accessToken) return null;
    _result = null;
    return _returnTo;
  }
  Future<String?> _finish(Future<LoginSession> Function(String token) action) async {
    final token = _continuation;
    if (_pending || token == null || _request == null) return null;
    final epoch = ++_epoch;
    final generation = session.generation;
    _pending = true; _error = null; _notify();
    try {
      final response = await action(token);
      if (!_current(epoch, generation)) return null;
      return _accept(response);
    } catch (failure) {
      if (_current(epoch, generation)) {
        _fail(_describe(failure));
        if (failure is ApiException && failure.statusCode == 400 && (failure.message.toLowerCase().contains('expired') || failure.message.toLowerCase().contains('invalid continuation'))) _result = null;
      }
    } finally { if (_current(epoch, generation)) { _pending = false; _notify(); } }
    return null;
  }
  Future<String?> register({required String username, required String displayName, required String email}) => _result is OidcRegistrationRequired && (_result as OidcRegistrationRequired).canRegister ? _finish((token) => repository.registerOidc(continuationToken: token, username: username, displayName: displayName, email: email)) : Future.value(null);
  Future<String?> linkExisting({required String identifier, required String password}) => _result is OidcRegistrationRequired ? _finish((token) => repository.linkExistingOidc(continuationToken: token, identifier: identifier, password: password)) : Future.value(null);
  Future<String?> confirmLink() => _result is OidcLinkConfirmationRequired ? _finish(repository.confirmOidcLink) : Future.value(null);
  Future<void> cancel() async {
    final token = _continuation;
    clear();
    try { _clearProof(); } catch (_) {}
    if (token != null) { try { await repository.cancelOidc(token); } catch (_) {} }
  }
  void clear() { ++_epoch; _pending = false; _error = null; _result = null; _request = null; _notify(); }
  @override
  void dispose() { _disposed = true; ++_epoch; session.removeListener(_sessionChanged); super.dispose(); }
}
