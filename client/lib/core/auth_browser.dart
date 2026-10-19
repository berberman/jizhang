import 'auth_browser_unsupported.dart'
    if (dart.library.js_interop) 'auth_browser_web.dart' as platform;

abstract interface class AuthBrowser {
  bool get supported;
  Future<String> newVerifier();
  Future<String> challenge(String verifier);
  void savePending(Map<String, String> value);
  Map<String, String>? readPending();
  void clearPending();
  void replaceUrl(Uri url);
  void navigate(Uri url);
}

AuthBrowser createAuthBrowser() => platform.createAuthBrowser();
