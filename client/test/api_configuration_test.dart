import 'dart:async';
import 'dart:convert';

import 'package:flutter_test/flutter_test.dart';
import 'package:http/http.dart' as http;
import 'package:http/testing.dart';
import 'package:jizhang_client/core/api_configuration.dart';

void main() {
  final browserUri = Uri.parse('https://web.example.test/app/#/login');

  Future<Uri> resolve(http.Client client, {String compiled = '', bool web = true}) => resolveApiBase(
    buildTimeValue: compiled,
    browserUri: browserUri,
    isWeb: web,
    client: client,
  );

  test('compiled override takes precedence without requesting runtime config', () async {
    final client = MockClient((_) async => fail('Runtime config must not be fetched'));
    addTearDown(client.close);
    expect(await resolve(client, compiled: 'https://api.example.test/ledger'),
      Uri.parse('https://api.example.test/ledger'));
    await expectLater(resolve(client, compiled: '//invalid.example.test'), throwsFormatException);
  });

  test('non-web skips runtime config for both empty and compiled values', () async {
    final client = MockClient((_) async => fail('Non-web must not fetch runtime config'));
    addTearDown(client.close);
    expect(await resolve(client, web: false), Uri.parse('https://web.example.test'));
    expect(await resolve(client, web: false, compiled: 'http://127.0.0.1:8080/api'),
      Uri.parse('http://127.0.0.1:8080/api'));
  });

  test('runtime config preserves subdirectory hosting and API path prefix', () async {
    final client = MockClient((request) async {
      expect(request.method, 'GET');
      expect(request.url.origin, browserUri.origin);
      expect(request.url.path, '/app/runtime/api-base-url.txt');
      expect(request.url.hasFragment, isFalse);
      expect(request.url.queryParameters.keys, unorderedEquals(['_runtime']));
      expect(int.tryParse(request.url.queryParameters['_runtime']!), isNotNull);
      expect(request.followRedirects, isFalse);
      expect(request.headers['Accept'], 'text/plain');
      expect(request.headers['Cache-Control'], 'no-cache');
      return http.Response.bytes(utf8.encode('  https://api.example.test/ledger/v1/\n'), 200);
    });
    addTearDown(client.close);
    expect(await resolve(client), Uri.parse('https://api.example.test/ledger/v1/'));
  });

  for (final body in ['', ' \t\r\n']) {
    test('empty or whitespace runtime config explicitly selects browser origin (${body.length})', () async {
      final client = MockClient((_) async => http.Response(body, 200));
      addTearDown(client.close);
      expect(await resolve(client), Uri.parse('https://web.example.test'));
    });
  }

  for (final status in [404, 500, 204, 301, 302, 307, 308]) {
    test('HTTP $status fails closed rather than using browser origin', () async {
      final client = MockClient((_) async => http.Response('', status,
        headers: {'location': 'https://other.example.test/config'}));
      addTearDown(client.close);
      await expectLater(resolve(client), throwsA(isA<ApiConfigurationException>()));
    });
  }

  test('invalid UTF-8 fails with the fixed load error', () async {
    final client = MockClient((_) async => http.Response.bytes([0xc3, 0x28], 200));
    addTearDown(client.close);
    await expectLater(resolve(client), throwsA(isA<ApiConfigurationException>().having(
      (error) => error.toString(), 'safe message', apiConfigurationLoadMessage)));
  });

  test('transport failure does not expose diagnostics or select fallback', () async {
    final client = MockClient((_) async => throw http.ClientException('private transport details'));
    addTearDown(client.close);
    await expectLater(resolve(client), throwsA(isA<ApiConfigurationException>().having(
      (error) => error.toString(), 'safe message', apiConfigurationLoadMessage)));
  });

  for (final value in [
    'https://name:password@api.example.test',
    'https://@api.example.test',
    'https://api.example.test?query=private',
    'https://api.example.test#fragment',
    '//api.example.test',
    'https://api.example.test:invalid',
    'ftp://api.example.test',
    'not a URL',
  ]) {
    test('malformed runtime URL fails validation: $value', () async {
      final client = MockClient((_) async => http.Response(value, 200));
      addTearDown(client.close);
      await expectLater(resolve(client), throwsA(isA<FormatException>().having(
        (error) => error.message, 'safe message', 'API_BASE_URL is not a valid HTTP URL')));
    });
  }

  testWidgets('runtime response headers time out after five seconds', (tester) async {
    final pending = Completer<http.Response>();
    final client = MockClient((_) => pending.future);
    addTearDown(client.close);
    var finished = false;
    final result = resolve(client).whenComplete(() { finished = true; });
    final expectation = expectLater(result, throwsA(isA<ApiConfigurationException>()));
    await tester.pump();
    await tester.pump(const Duration(seconds: 4));
    expect(finished, isFalse);
    await tester.pump(const Duration(seconds: 1));
    await expectation;
    expect(finished, isTrue);
    pending.complete(http.Response('', 200));
    await tester.pump();
  });

  testWidgets('timeout also bounds a stalled response body', (tester) async {
    final body = StreamController<List<int>>();
    final client = MockClient.streaming((_, _) async => http.StreamedResponse(body.stream, 200));
    addTearDown(client.close);
    final result = resolve(client);
    final expectation = expectLater(result, throwsA(isA<ApiConfigurationException>()));
    await tester.pump();
    await tester.pump(const Duration(seconds: 5));
    await expectation;
    unawaited(body.close());
    await tester.pump();
  });
}
