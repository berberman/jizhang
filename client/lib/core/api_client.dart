import 'dart:convert';

import 'package:http/http.dart' as http;

const _invalidBase = 'API_BASE_URL is not a valid HTTP URL';
const responseFormatMessage = 'The server returned an invalid response format';

/// Resolve configuration without accepting credentials or request-specific parts.
Uri parseApiBase(String value, Uri browserUri) {
  try {
    final uri = value.isEmpty ? Uri.parse(browserUri.origin) : Uri.parse(value);
    if (!uri.isAbsolute ||
        (uri.scheme != 'http' && uri.scheme != 'https') ||
        !uri.hasAuthority ||
        uri.host.isEmpty ||
        uri.userInfo.isNotEmpty ||
        uri.hasQuery ||
        uri.hasFragment ||
        uri.authority.contains('@')) {
      throw const FormatException(_invalidBase);
    }
    // Force URI port validation here rather than during a later HTTP request.
    uri.port;
    return uri;
  } on FormatException {
    throw const FormatException(_invalidBase);
  } on StateError {
    throw const FormatException(_invalidBase);
  }
}

class ApiException implements Exception {
  const ApiException({required this.statusCode, required this.message});
  final int? statusCode;
  final String message;
  @override
  String toString() => message;
}

class ApiClient {
  ApiClient({required Uri baseUri, required this.httpClient, required this.accessToken})
      : baseUri = parseApiBase(baseUri.toString(), baseUri);

  final Uri baseUri;
  final http.Client httpClient;
  final String? Function() accessToken;
  void Function()? onUnauthorized;

  Future<Object?> request(
    String method,
    List<String> pathSegments, {
    Map<String, String>? query,
    Object? jsonBody,
    String? csvBody,
  }) async {
    if (jsonBody != null && csvBody != null) {
      throw ArgumentError('A request cannot contain both JSON and CSV');
    }
    final verb = method.toUpperCase();
    final isRead = verb == 'GET' || verb == 'HEAD' || verb == 'OPTIONS';
    final isAuth = pathSegments.isNotEmpty && pathSegments.first == 'auth';
    final prefix = baseUri.pathSegments.toList();
    while (prefix.isNotEmpty && prefix.last.isEmpty) {
      prefix.removeLast();
    }
    final uri = baseUri.replace(
      pathSegments: [...prefix, ...pathSegments],
      queryParameters: query,
    );
    final request = http.Request(verb, uri)
      // A redirect must never forward this client's bearer token to another API.
      ..followRedirects = false
      ..headers['Accept'] = 'application/json';
    final token = accessToken();
    if (!isAuth && token != null) {
      request.headers['Authorization'] = 'Bearer $token';
    }
    if (csvBody != null) {
      request.headers['Content-Type'] = 'text/csv; charset=utf-8';
      request.bodyBytes = utf8.encode(csvBody);
    } else if (jsonBody != null) {
      request.headers['Content-Type'] = 'application/json; charset=utf-8';
      request.bodyBytes = utf8.encode(jsonEncode(jsonBody));
    }

    final http.Response response;
    try {
      response = await http.Response.fromStream(await httpClient.send(request));
    } on Exception {
      throw ApiException(
        statusCode: null,
        message: isRead
            ? 'Could not reach the server'
            : 'Could not reach the server. Check refreshed data before submitting again',
      );
    }
    if (response.statusCode < 200 || response.statusCode >= 300) {
      if (response.statusCode == 401 && !isAuth && accessToken() == token) {
        onUnauthorized?.call();
      }
      final text = utf8.decode(response.bodyBytes, allowMalformed: true);
      throw ApiException(
        statusCode: response.statusCode,
        message: text.isNotEmpty ? text : switch (response.statusCode) {
          401 => isAuth ? 'Invalid username/email or password' : 'Your session expired. Sign in again.',
          403 => 'Access denied',
          404 => 'Resource not found',
          _ => 'The server returned HTTP ${response.statusCode}',
        },
      );
    }
    try {
      final text = utf8.decode(response.bodyBytes);
      if (text.trim().isEmpty) return null;
      return jsonDecode(text);
    } on FormatException {
      throw ApiException(statusCode: response.statusCode, message: responseFormatMessage);
    }
  }
}
