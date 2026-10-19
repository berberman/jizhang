import 'api_client.dart';
import 'models.dart';

class JizhangRepository {
  JizhangRepository(this.api);
  final ApiClient api;
  List<CurrencyInfo>? _currencies;
  Future<List<CurrencyInfo>>? _currencyRequest;

  Future<List<CurrencyInfo>> getCurrencies() {
    final cached = _currencies;
    if (cached != null) return Future.value(cached);
    return _currencyRequest ??= _loadCurrencies();
  }

  Future<List<CurrencyInfo>> _loadCurrencies() async {
    try {
      final currencies = await _read(api.request('GET', ['currencies']), (value) {
        if (value is! List) throw const FormatException('Expected an array');
        final result = List<CurrencyInfo>.unmodifiable(value.map(CurrencyInfo.fromJson));
        final codes = <String>{};
        if (result.isEmpty || result.any((currency) => !codes.add(currency.code))) {
          throw const FormatException('Invalid currency catalogue');
        }
        return result;
      });
      _currencies = currencies;
      return currencies;
    } finally {
      _currencyRequest = null;
    }
  }

  Future<ExchangeQuote> createExchangeQuote(String groupId, String currency) => _read(
    api.request('POST', ['groups', groupId, 'exchange-quotes'], jsonBody: {'currency': currency}),
    ExchangeQuote.fromJson,
  );

  Future<T> _read<T>(Future<Object?> response, T Function(Object?) parse) async {
    final value = await response;
    try {
      return parse(value);
    } on FormatException {
      throw const ApiException(statusCode: null, message: responseFormatMessage);
    }
  }

  Future<User> register(String username, String displayName, String email, String password) => _read(
    api.request('POST', ['auth', 'register'], jsonBody: {'registerUsername': username, 'registerDisplayName': displayName, 'registerEmail': email, 'registerPassword': password}),
    User.fromJson,
  );

  Future<LoginSession> login(String identifier, String password) => _read(
    api.request('POST', ['auth', 'login'], jsonBody: {'loginIdentifier': identifier, 'loginPassword': password}),
    LoginSession.fromJson,
  );

  Future<AuthOptions> getAuthOptions() => _read(
    api.request('GET', ['auth', 'options']), AuthOptions.fromJson);

  Future<OidcStart> startOidc(String browserChallenge, {String? invitationToken}) => _read(
    api.request('POST', ['auth', 'oidc', 'start'], jsonBody: {
      'browserChallenge': browserChallenge, 'invitationToken': invitationToken,
    }), OidcStart.fromJson);

  Future<OidcStart> startAccountOidc({required AccountOidcPurpose purpose, required String browserChallenge}) => _read(
    api.request('POST', ['users', 'me', 'oidc', 'start'], jsonBody: {
      'purpose': purpose.name, 'browserChallenge': browserChallenge,
    }), OidcStart.fromJson);

  Future<OidcExchangeResult> exchangeOidc({required String flowId, required String code, required String browserVerifier}) => _read(
    api.request('POST', ['auth', 'oidc', 'exchange'], jsonBody: {
      'flowId': flowId, 'code': code, 'browserVerifier': browserVerifier,
    }), OidcExchangeResult.fromJson);

  Future<LoginSession> registerOidc({required String continuationToken, required String username, required String displayName, required String email}) => _read(
    api.request('POST', ['auth', 'oidc', 'register'], jsonBody: {
      'continuationToken': continuationToken, 'username': username,
      'displayName': displayName, 'email': email,
    }), LoginSession.fromJson);

  Future<LoginSession> linkExistingOidc({required String continuationToken, required String identifier, required String password}) => _read(
    api.request('POST', ['auth', 'oidc', 'link-existing'], jsonBody: {
      'continuationToken': continuationToken, 'loginIdentifier': identifier,
      'loginPassword': password,
    }), LoginSession.fromJson);

  Future<LoginSession> confirmOidcLink(String continuationToken) => _read(
    api.request('POST', ['auth', 'oidc', 'confirm-link'], jsonBody: {
      'continuationToken': continuationToken,
    }), LoginSession.fromJson);

  Future<void> cancelOidc(String continuationToken) async {
    await api.request('POST', ['auth', 'oidc', 'cancel'], jsonBody: {'continuationToken': continuationToken});
  }

  Future<InvitationInspection> inspectInvitation(String token) => _read(
    api.request('POST', ['auth', 'invitations', 'inspect'], jsonBody: {'token': token}),
    InvitationInspection.fromJson,
  );

  Future<void> resetPassword({required String token, required String newPassword}) async {
    await api.request('POST', ['auth', 'password-reset'], jsonBody: {'token': token, 'newPassword': newPassword});
  }

  Future<AccountAuthentication> getAuthentication() => _read(
    api.request('GET', ['users', 'me', 'authentication']), AccountAuthentication.fromJson);

  Future<void> reauthenticatePassword(String password) async {
    await api.request('POST', ['users', 'me', 'reauthenticate'], jsonBody: {'password': password});
  }

  Future<void> changePassword({String? currentPassword, required String newPassword}) async {
    await api.request('PUT', ['users', 'me', 'password'], jsonBody: {
      'currentPassword': currentPassword, 'newPassword': newPassword,
    });
  }

  Future<void> removePassword() async {
    await api.request('DELETE', ['users', 'me', 'password']);
  }

  Future<void> unlinkIdentity(String currentPassword) async {
    await api.request('POST', ['users', 'me', 'identity', 'unlink'], jsonBody: {'currentPassword': currentPassword});
  }

  Future<UserProfile> getMe() => _read(api.request('GET', ['users', 'me']), UserProfile.fromJson);
  Future<UserProfile> updateProfile(String username, String displayName) => _read(
    api.request('PUT', ['users', 'me'], jsonBody: {'username': username, 'displayName': displayName}), UserProfile.fromJson);
  Future<void> deleteAccount() async { await api.request('DELETE', ['users', 'me']); }

  Future<void> logout() async {
    await api.request('POST', ['users', 'me', 'logout']);
  }

  Future<ProviderLogout> logoutProvider() => _read(
    api.request('POST', ['users', 'me', 'oidc', 'logout']),
    (value) {
      final result = ProviderLogout.fromJson(value);
      final prefix = api.baseUri.pathSegments.toList();
      while (prefix.isNotEmpty && prefix.last.isEmpty) {
        prefix.removeLast();
      }
      final expected = api.baseUri.replace(pathSegments: [...prefix, 'auth', 'oidc', 'logout']);
      final url = result.logoutUrl;
      final tokens = url.queryParametersAll['token'];
      if (url.scheme != expected.scheme || url.authority != expected.authority ||
          url.path != expected.path || url.hasFragment ||
          url.queryParametersAll.length != 1 || tokens == null ||
          tokens.length != 1 || tokens.single.isEmpty) {
        throw const FormatException('Invalid logout handoff URL');
      }
      return result;
    },
  );

  Future<Page<ExpenseGroup>> listGroups(PageQuery q) => _read(
    api.request('GET', ['users', 'me', 'groups'], query: q.toQuery()),
    (value) => Page.fromJson(value, ExpenseGroup.fromJson),
  );
  Future<ExpenseGroup> createGroup(String name, {CurrencyMode currencyMode = CurrencyMode.single, String defaultCurrency = 'USD'}) => _read(
    api.request('POST', ['groups'], jsonBody: {'groupName': name, 'currencyMode': currencyMode.name, 'defaultCurrency': defaultCurrency}), ExpenseGroup.fromJson);
  Future<ExpenseGroup> getGroup(String id) => _read(api.request('GET', ['groups', id]), ExpenseGroup.fromJson);
  Future<Page<User>> listMemberCandidates(String groupId, PageQuery query) => _read(
    api.request('GET', ['groups', groupId, 'member-candidates'], query: query.toQuery()),
    (value) => Page.fromJson(value, User.fromJson),
  );
  Future<ExpenseGroup> updateGroupSettings(String id, {required String name, required CurrencyMode currencyMode, required String defaultCurrency}) => _read(
    api.request('PUT', ['groups', id], jsonBody: {'groupName': name, 'currencyMode': currencyMode.name, 'defaultCurrency': defaultCurrency}), ExpenseGroup.fromJson);
  Future<void> deleteGroup(String id) async { await api.request('DELETE', ['groups', id]); }
  Future<ExpenseGroup> addMember(String id, String userId) => _read(api.request('POST', ['groups', id, 'members'], jsonBody: userId), ExpenseGroup.fromJson);
  Future<void> removeMember(String id, String userId) async { await api.request('DELETE', ['groups', id, 'members', userId]); }
  Future<ExpenseGroup> transferOwnership(String id, String userId) => _read(api.request('PUT', ['groups', id, 'owner'], jsonBody: userId), ExpenseGroup.fromJson);

  Future<Page<LedgerRecord>> listRecords(String gid, PageQuery q) => _read(
    api.request('GET', ['groups', gid, 'records'], query: q.toQuery()),
    (value) => Page.fromJson(value, LedgerRecord.fromJson),
  );
  Future<LedgerRecord> getRecord(String gid, String rid) => _read(api.request('GET', ['groups', gid, 'records', rid]), LedgerRecord.fromJson);
  Map<String, Object?> _splitPayload(SplitRecordDraft draft, RecordKind kind) {
    if (draft.kind != kind) throw ArgumentError('Draft kind does not match the record endpoint');
    return draft.toJson();
  }
  Future<LedgerRecord> createExpense(String gid, SplitRecordDraft draft) => _read(api.request('POST', ['groups', gid, 'records', 'expense'], jsonBody: _splitPayload(draft, RecordKind.expense)), LedgerRecord.fromJson);
  Future<LedgerRecord> updateExpense(String gid, String rid, SplitRecordDraft draft) => _read(api.request('PUT', ['groups', gid, 'records', 'expense', rid], jsonBody: _splitPayload(draft, RecordKind.expense)), LedgerRecord.fromJson);
  Future<LedgerRecord> createIncome(String gid, SplitRecordDraft draft) => _read(api.request('POST', ['groups', gid, 'records', 'income'], jsonBody: _splitPayload(draft, RecordKind.income)), LedgerRecord.fromJson);
  Future<LedgerRecord> updateIncome(String gid, String rid, SplitRecordDraft draft) => _read(api.request('PUT', ['groups', gid, 'records', 'income', rid], jsonBody: _splitPayload(draft, RecordKind.income)), LedgerRecord.fromJson);
  Future<LedgerRecord> createTransfer(String gid, TransferDraft draft) => _read(api.request('POST', ['groups', gid, 'records', 'transfer'], jsonBody: draft.toJson()), LedgerRecord.fromJson);
  Future<LedgerRecord> updateTransfer(String gid, String rid, TransferDraft draft) => _read(api.request('PUT', ['groups', gid, 'records', 'transfer', rid], jsonBody: draft.toJson()), LedgerRecord.fromJson);
  Future<void> deleteRecord(String gid, String rid) async { await api.request('DELETE', ['groups', gid, 'records', rid]); }

  Future<Page<Receipt>> listReceipts(String gid, PageQuery q) => _read(
    api.request('GET', ['groups', gid, 'receipts'], query: q.toQuery()),
    (value) => Page.fromJson(value, Receipt.fromJson),
  );
  Future<Receipt> getReceipt(String gid, String id) => _read(api.request('GET', ['groups', gid, 'receipts', id]), Receipt.fromJson);
  Future<Receipt> createReceipt(String gid, ReceiptDraft draft) => _read(api.request('POST', ['groups', gid, 'receipts'], jsonBody: draft.toJson()), Receipt.fromJson);
  Future<Receipt> updateReceipt(String gid, String id, ReceiptDraft draft) => _read(api.request('PUT', ['groups', gid, 'receipts', id], jsonBody: draft.toJson()), Receipt.fromJson);
  Future<void> deleteReceipt(String gid, String id) async { await api.request('DELETE', ['groups', gid, 'receipts', id]); }

  Future<GroupReport> getReport(String gid) => _read(api.request('GET', ['groups', gid, 'report']), GroupReport.fromJson);
  Future<SpendingStatistics> getStatistics(String groupId, StatisticsQuery query) => _read(
    api.request('GET', ['groups', groupId, 'statistics'], query: query.toQuery()), SpendingStatistics.fromJson);
  Future<List<LedgerRecord>> importCsv(String gid, String csv, {required String currency, ExchangeRateInput? exchangeRate}) => _read(
    api.request('POST', ['groups', gid, 'import'], csvBody: csv, query: {
      'currency': currency,
      if (exchangeRate is ReferenceRateInput) 'exchangeQuoteId': exchangeRate.quoteId,
      if (exchangeRate is ManualRateInput) 'manualRate': exchangeRate.rate,
    }),
    (value) {
      if (value is! List) throw const FormatException('Expected an array');
      return List<LedgerRecord>.unmodifiable(value.map(LedgerRecord.fromJson));
    },
  );
}
