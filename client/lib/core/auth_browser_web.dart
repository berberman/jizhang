import 'dart:convert';
import 'dart:js_interop';
import 'dart:typed_data';

import 'package:web/web.dart' as web;

import 'auth_browser.dart';

const _pendingKey = 'jizhang.oidc.pending';
const _pendingFields = {'flowId', 'browserVerifier', 'returnTo', 'purpose', 'createdAt'};

AuthBrowser createAuthBrowser() => const WebAuthBrowser();

class WebAuthBrowser implements AuthBrowser {
  const WebAuthBrowser();
  @override
  bool get supported => true;

  String _encode(List<int> bytes) => base64Url.encode(bytes).replaceAll('=', '');

  @override
  Future<String> newVerifier() async {
    final bytes = web.window.crypto.getRandomValues(Uint8List(32).toJS) as JSUint8Array;
    return _encode(bytes.toDart);
  }

  @override
  Future<String> challenge(String verifier) async {
    final bytes = Uint8List.fromList(ascii.encode(verifier));
    final digest = await web.window.crypto.subtle.digest('SHA-256'.toJS, bytes.toJS).toDart;
    return _encode((digest as JSArrayBuffer).toDart.asUint8List());
  }

  bool _valid(Map<String, String> value) => value.length == _pendingFields.length &&
      _pendingFields.every((key) => value[key]?.isNotEmpty == true);

  @override
  void savePending(Map<String, String> value) {
    if (!_valid(value)) throw const FormatException('Invalid pending authentication record');
    web.window.sessionStorage.setItem(_pendingKey, jsonEncode(value));
  }

  @override
  Map<String, String>? readPending() {
    final stored = web.window.sessionStorage.getItem(_pendingKey);
    if (stored == null) return null;
    try {
      final decoded = jsonDecode(stored);
      if (decoded is! Map<String, dynamic> || decoded.values.any((value) => value is! String)) {
        throw const FormatException('Invalid pending authentication record');
      }
      final value = decoded.cast<String, String>();
      if (!_valid(value)) throw const FormatException('Invalid pending authentication record');
      return value;
    } on FormatException {
      clearPending();
      return null;
    }
  }

  @override
  void clearPending() => web.window.sessionStorage.removeItem(_pendingKey);
  @override
  void replaceUrl(Uri url) => web.window.history.replaceState(null, '', url.toString());
  @override
  void navigate(Uri url) => web.window.location.assign(url.toString());
}
