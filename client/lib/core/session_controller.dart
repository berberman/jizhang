import 'package:flutter/foundation.dart';

import 'auth_browser.dart';
import 'api_client.dart';
import 'jizhang_repository.dart';
import 'models.dart';

enum SessionStatus { unauthenticated, loading, authenticated, error }

class SessionController extends ChangeNotifier {
  SessionController(this.repository, {AuthBrowser? authBrowser})
      : authBrowser = authBrowser ?? createAuthBrowser() {
    repository.api.onUnauthorized = expire;
  }

  final JizhangRepository repository;
  final AuthBrowser authBrowser;
  SessionStatus status = SessionStatus.unauthenticated;
  String? accessToken;
  User? user;
  UserProfile? profile;
  String? message;
  String? registeredUsername;
  int generation = 0;
  PageQuery groupListQuery = const PageQuery();
  int _request = 0;
  bool _disposed = false;

  bool get isAuthenticated => accessToken != null && user != null;

  bool _current(int request) => !_disposed && request == _request;

  void _clearPendingAuthentication() {
    if (!authBrowser.supported) return;
    try {
      authBrowser.clearPending();
    } catch (_) {
      // Denied browser storage must not prevent local sign-in or sign-out.
    }
  }

  int _beginAuthentication() {
    _clearPendingAuthentication();
    generation++;
    final request = ++_request;
    accessToken = null;
    user = null;
    profile = null;
    message = null;
    registeredUsername = null;
    status = SessionStatus.loading;
    notifyListeners();
    return request;
  }

  Future<void> register(String username, String displayName, String email, String password) async {
    final request = _beginAuthentication();
    try {
      final registered = await repository.register(username, displayName, email, password);
      if (!_current(request)) return;
      registeredUsername = registered.username;
      final result = await repository.login(registered.username, password);
      if (!_current(request)) return;
      _accept(result);
    } catch (error) {
      if (!_current(request)) return;
      status = SessionStatus.error;
      message = _message(error);
      notifyListeners();
    }
  }

  Future<void> login(String username, String password) async {
    final request = _beginAuthentication();
    try {
      final result = await repository.login(username, password);
      if (!_current(request)) return;
      _accept(result);
    } catch (error) {
      if (!_current(request)) return;
      status = SessionStatus.error;
      message = _message(error);
      notifyListeners();
    }
  }

  int beginExternalAuthentication() => _beginAuthentication();

  void acceptExternalSession(LoginSession result, {required int request}) {
    if (_current(request)) _accept(result);
  }

  void failExternalAuthentication(String message, {required int request}) {
    if (!_current(request)) return;
    status = SessionStatus.error;
    this.message = message;
    notifyListeners();
  }

  void clearAfterCredentialChange() => _clear('Your sign-in methods changed. Sign in again.');

  void _accept(LoginSession result) {
    accessToken = result.accessToken;
    user = result.loginUser;
    profile = null;
    registeredUsername = null;
    status = SessionStatus.authenticated;
    message = null;
    notifyListeners();
  }

  Future<void> logout({bool provider = false}) async {
    if (_disposed) return;
    // ApiClient captures the old bearer before its first await. Retire local
    // state immediately, without waiting for a potentially unavailable server.
    final pending = isAuthenticated
        ? (provider ? repository.logoutProvider() : repository.logout().then<ProviderLogout?>((_) => null))
        : null;
    final request = _request + 1;
    _clear(null);
    if (pending == null) return;
    try {
      final result = await pending;
      if (!_current(request)) return;
      if (result is ProviderLogout) {
        if (!authBrowser.supported) {
          message = 'Signed out on this device; provider sign-out requires a web browser.';
          notifyListeners();
          return;
        }
        authBrowser.navigate(result.logoutUrl);
      }
    } catch (error) {
      if (!_current(request)) return;
      message = provider && error is ApiException && error.statusCode != null
          ? 'Signed out on this device. ${error.message}'
          : 'Signed out on this device; server session revocation could not be confirmed.';
      notifyListeners();
    }
  }

  void expire() => _clear('Your session expired. Sign in again.');

  void _clear(String? reason) {
    if (_disposed) return;
    _clearPendingAuthentication();
    _request++;
    generation++;
    accessToken = null;
    groupListQuery = const PageQuery();
    user = null;
    profile = null;
    registeredUsername = null;
    message = reason;
    status = SessionStatus.unauthenticated;
    notifyListeners();
  }

  Future<void> refreshAccount() async {
    if (!isAuthenticated || _disposed) return;
    final request = ++_request;
    // Account owns loading; synchronous notification can refresh its parent router during build.
    try {
      final result = await repository.getMe();
      if (!_current(request)) return;
      profile = result;
      user = result.user;
      status = SessionStatus.authenticated;
      message = null;
      notifyListeners();
    } catch (error) {
      if (!_current(request)) return;
      if (error is ApiException &&
          (error.statusCode == 401 || error.statusCode == 404)) {
        expire();
        return;
      }
      status = SessionStatus.error;
      message = _message(error);
      notifyListeners();
    }
  }

  Future<void> updateProfile(String username, String displayName) async {
    if (!isAuthenticated || _disposed) throw StateError('Not signed in');
    final request = ++_request;
    try {
      final result = await repository.updateProfile(username, displayName);
      if (!_current(request)) return;
      profile = result;
      user = result.user;
      status = SessionStatus.authenticated;
      message = null;
      notifyListeners();
    } catch (error) {
      if (_current(request) && error is ApiException && error.statusCode == 404) expire();
      rethrow;
    }
  }

  Future<void> deleteAccount() async {
    if (!isAuthenticated || _disposed) return;
    final request = ++_request;
    status = SessionStatus.loading;
    message = null;
    notifyListeners();
    try {
      await repository.deleteAccount();
      if (!_current(request)) return;
      _clear(null);
    } catch (error) {
      if (!_current(request)) return;
      status = SessionStatus.error;
      message = _message(error);
      notifyListeners();
    }
  }

  String _message(Object error) => error is ApiException
      ? error.message
      : 'Something went wrong. Please try again.';

  @override
  void dispose() {
    _disposed = true;
    _request++;
    repository.api.onUnauthorized = null;
    super.dispose();
  }
}
