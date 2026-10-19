import 'dart:convert';

import 'package:http/http.dart' as http;

import 'api_client.dart';

const apiConfigurationLoadMessage = 'Could not load API configuration. Reload to retry.';

class ApiConfigurationException implements Exception {
  const ApiConfigurationException();

  @override
  String toString() => apiConfigurationLoadMessage;
}

/// Resolve the one public runtime value before creating any API/session objects.
Future<Uri> resolveApiBase({
  required String buildTimeValue,
  required Uri browserUri,
  required bool isWeb,
  required http.Client client,
}) async {
  if (buildTimeValue.isNotEmpty || !isWeb) {
    return parseApiBase(buildTimeValue, browserUri);
  }

  final configUri = browserUri.resolve('runtime/api-base-url.txt').replace(
    queryParameters: {'_runtime': DateTime.now().microsecondsSinceEpoch.toString()},
  );
  final request = http.Request('GET', configUri)
    ..followRedirects = false
    ..headers['Accept'] = 'text/plain'
    ..headers['Cache-Control'] = 'no-cache';
  final String value;
  try {
    // Bound both response headers and the complete body, not just send().
    final response = await (() async {
      return http.Response.fromStream(await client.send(request));
    })().timeout(const Duration(seconds: 5));
    if (response.statusCode != 200) throw const ApiConfigurationException();
    value = utf8.decode(response.bodyBytes, allowMalformed: false).trim();
  } on Exception {
    // No URL, response body, or underlying transport diagnostics reach the UI.
    throw const ApiConfigurationException();
  }
  // Uri.parse normalizes away an empty user-info marker before validation.
  if (value.contains('@')) {
    throw const FormatException('API_BASE_URL is not a valid HTTP URL');
  }
  return parseApiBase(value, browserUri);
}
