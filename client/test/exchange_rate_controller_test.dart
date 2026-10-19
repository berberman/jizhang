import 'dart:async';
import 'dart:convert';

import 'package:flutter_test/flutter_test.dart';
import 'package:http/http.dart' as http;
import 'package:http/testing.dart';
import 'package:jizhang_client/core/api_client.dart';
import 'package:jizhang_client/core/exchange_rate_controller.dart';
import 'package:jizhang_client/core/jizhang_repository.dart';
import 'package:jizhang_client/core/models.dart';
import 'package:jizhang_client/core/session_controller.dart';

const usd = CurrencyInfo(code: 'USD', name: 'US Dollar', minorUnits: 2);
const eur = CurrencyInfo(code: 'EUR', name: 'Euro', minorUnits: 2);
const gbp = CurrencyInfo(code: 'GBP', name: 'Pound Sterling', minorUnits: 2);
final owner = User(userId: '00000000-0000-0000-0000-000000000001', username: 'owner', displayName: 'Owner');
ExpenseGroup group({CurrencyMode mode = CurrencyMode.single, CurrencyInfo currency = usd}) => ExpenseGroup(
  groupId: '00000000-0000-0000-0000-000000000010', groupName: 'Trip', owner: owner,
  members: [owner], currencyMode: mode, defaultCurrency: currency, currencyLocked: false,
);
ExchangeQuote quote(CurrencyInfo source, String id) => ExchangeQuote(
  quoteId: id, sourceCurrency: source, targetCurrency: usd, rate: '1.2', provider: 'frankfurter',
  rateDate: '2026-10-16', fetchedAt: DateTime.utc(2026, 10, 16),
);

class PendingQuoteRepository extends JizhangRepository {
  PendingQuoteRepository() : super(ApiClient(
    baseUri: Uri.parse('https://example.test/api'),
    accessToken: () => null,
    httpClient: MockClient((request) async => request.url.path.endsWith('/logout') ? http.Response('', 204) : http.Response(jsonEncode({
      'accessToken': 'test-token',
      'loginUser': {'userId': owner.userId, 'username': owner.username, 'displayName': owner.displayName},
    }), 200)),
  ));
  final pending = <String, Completer<ExchangeQuote>>{};
  @override
  Future<ExchangeQuote> createExchangeQuote(String groupId, String currency) {
    final response = Completer<ExchangeQuote>();
    pending[currency] = response;
    return response.future;
  }
}

void main() {
  late PendingQuoteRepository repository;
  late SessionController session;
  late ExchangeRateController controller;
  setUp(() async {
    repository = PendingQuoteRepository();
    session = SessionController(repository);
    await session.login('owner', 'password');
  });
  tearDown(() { controller.dispose(); session.dispose(); });

  test('manual override cannot be replaced by a pending reference response', () async {
    controller = ExchangeRateController(repository: repository, session: session, group: group(), sourceCurrency: 'EUR');
    final refresh = controller.refreshReference();
    controller.setManualRate('1.35');
    repository.pending['EUR']!.complete(quote(eur, 'eur-quote'));
    await refresh;
    expect(controller.loading, isFalse);
    expect(controller.selectedQuote, isNull);
    expect(controller.toInput()!.toJson(), {'source': 'manual', 'rate': '1.35'});
  });

  test('out-of-order responses cannot reuse a quote from an earlier currency pair', () async {
    controller = ExchangeRateController(repository: repository, session: session, group: group(), sourceCurrency: 'EUR');
    final oldRefresh = controller.refreshReference();
    controller.setCurrency('GBP');
    repository.pending['GBP']!.complete(quote(gbp, 'gbp-quote'));
    await Future<void>.delayed(Duration.zero);
    repository.pending['EUR']!.complete(quote(eur, 'eur-quote'));
    await oldRefresh;
    expect(controller.sourceCurrency, 'GBP');
    expect(controller.selectedQuote!.sourceCurrency.code, 'GBP');
    expect(controller.toInput()!.toJson(), {'source': 'reference', 'quoteId': 'gbp-quote'});
  });

  for (final retire in ['session', 'removed child']) {
    test('$retire invalidates a quote before deferred controller disposal', () async {
      controller = ExchangeRateController(repository: repository, session: session, group: group(), sourceCurrency: 'EUR');
      final refresh = controller.refreshReference();
      if (retire == 'session') { await session.logout(); } else { controller.deactivate(); }
      repository.pending['EUR']!.complete(quote(eur, 'late-quote'));
      await refresh;
      expect(controller.selectedQuote, isNull);
      expect(controller.loading, isFalse);
      expect(controller.toInput, throwsFormatException);
    });
  }

  test('saved references load without requests and policy changes invalidate their pair', () {
    controller = ExchangeRateController(repository: repository, session: session, group: group(), sourceCurrency: 'EUR',
      savedRate: ExchangeRate(source: 'reference', rate: '1.2', rateDate: '2026-10-16',
        fetchedAt: DateTime.utc(2026, 10, 16), quoteId: 'saved-quote'));
    controller.updateGroup(group());
    expect(repository.pending, isEmpty);
    expect(controller.toInput()!.toJson(), {'source': 'reference', 'quoteId': 'saved-quote'});
    controller.updateGroup(group(currency: gbp));
    expect(repository.pending, isEmpty);
    expect(controller.selectedQuote, isNull);
    expect(controller.manualRate.text, isEmpty);
    expect(controller.toInput, throwsFormatException);
  });
}
