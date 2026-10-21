import 'package:flutter/foundation.dart';
import 'package:flutter/material.dart';
import 'package:http/http.dart' as http;

import 'app.dart';
import 'core/api_client.dart';
import 'core/api_configuration.dart';
import 'core/auth_browser.dart';
import 'core/jizhang_repository.dart';
import 'core/session_controller.dart';

Future<void> main() async {
  WidgetsFlutterBinding.ensureInitialized();
  final client = http.Client();
  final Uri baseUri;
  try {
    baseUri = await resolveApiBase(
      buildTimeValue: const String.fromEnvironment('API_BASE_URL'),
      browserUri: Uri.base,
      isWeb: kIsWeb,
      client: client,
    );
  } on FormatException {
    client.close();
    _showConfigurationError(apiConfigurationFormatMessage);
    return;
  } on ApiConfigurationException {
    client.close();
    _showConfigurationError(apiConfigurationLoadMessage);
    return;
  }
  late SessionController session;
  final api = ApiClient(baseUri: baseUri, httpClient: client, accessToken: () => session.accessToken);
  final repository = JizhangRepository(api);
  session = SessionController(repository, authBrowser: createAuthBrowser());
  runApp(JizhangApp(repository: repository, session: session));
}

void _showConfigurationError(String message) {
  runApp(MaterialApp(home: Scaffold(body: SafeArea(child: Padding(
    padding: const EdgeInsets.all(24), child: Text(message),
  )))));
}
