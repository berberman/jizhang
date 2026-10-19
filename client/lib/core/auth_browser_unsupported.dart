import 'auth_browser.dart';

AuthBrowser createAuthBrowser() => const UnsupportedAuthBrowser();

class UnsupportedAuthBrowser implements AuthBrowser {
  const UnsupportedAuthBrowser();
  @override
  bool get supported => false;
  Never _unsupported() => throw UnsupportedError('External authentication requires a web browser');
  @override
  Future<String> newVerifier() async => _unsupported();
  @override
  Future<String> challenge(String verifier) async => _unsupported();
  @override
  void savePending(Map<String, String> value) => _unsupported();
  @override
  Map<String, String>? readPending() => _unsupported();
  @override
  void clearPending() => _unsupported();
  @override
  void replaceUrl(Uri url) => _unsupported();
  @override
  void navigate(Uri url) => _unsupported();
}
