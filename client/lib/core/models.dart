import 'money.dart';

// Immutable representations of the user API wire format.
Map<String, Object?> _object(Object? value) {
  if (value is! Map<String, Object?>) {
    throw const FormatException('Expected a JSON object');
  }
  return value;
}

String _string(Object? value) {
  if (value is! String) throw const FormatException('Expected a string');
  return value;
}

int _integer(Object? value) {
  if (value is! int) throw const FormatException('Expected an integer');
  return value;
}

Map<String, Object?> _fields(Object? value, Set<String> required, [Set<String> optional = const {}]) {
  final json = _object(value);
  if (!required.every(json.containsKey) || json.keys.any((key) => !required.contains(key) && !optional.contains(key))) {
    throw const FormatException('Unexpected or missing JSON fields');
  }
  return json;
}

T _enum<T extends Enum>(Object? value, List<T> values) {
  final text = _string(value);
  for (final item in values) {
    if (item.name == text) return item;
  }
  throw FormatException('Unknown value: $text');
}

String _currencyCode(Object? value) {
  final code = _string(value);
  if (!RegExp(r'^[A-Z]{3}$').hasMatch(code)) throw const FormatException('Invalid currency code');
  return code;
}

String _rate(Object? value) {
  final text = _string(value);
  if (parseDecimalRate(text).text != text) throw const FormatException('Expected a canonical decimal rate');
  return text;
}

BigInt _positiveMinor(Object? value) {
  final amount = parseMinorAmount(value);
  if (amount <= BigInt.zero || amount > maxRecordMinor) throw const FormatException('Invalid record amount');
  return amount;
}

bool _boolean(Object? value) {
  if (value is! bool) throw const FormatException('Expected a boolean');
  return value;
}

List<T> _list<T>(Object? value, T Function(Object?) parse) {
  if (value is! List) throw const FormatException('Expected an array');
  return List<T>.unmodifiable(value.map(parse));
}

String _date(Object? value) {
  final text = _string(value);
  if (!RegExp(r'^\d{4}-\d{2}-\d{2}$').hasMatch(text)) {
    throw const FormatException('Expected a calendar date');
  }
  final year = int.parse(text.substring(0, 4));
  final month = int.parse(text.substring(5, 7));
  final day = int.parse(text.substring(8, 10));
  final date = DateTime.utc(year, month, day);
  if (date.year != year || date.month != month || date.day != day) {
    throw const FormatException('Invalid calendar date');
  }
  return text;
}

DateTime _instant(Object? value) {
  final text = _string(value);
  final result = DateTime.tryParse(text);
  if (result == null ||
      !RegExp(r'(Z|[+-]\d{2}:?\d{2})$').hasMatch(text)) {
    throw const FormatException('Expected a timestamp with a time zone');
  }
  return result;
}

class User {
  const User({required this.userId, required this.username, required this.displayName});
  final String userId;
  final String username;
  final String displayName;
  factory User.fromJson(Object? value) {
    final json = _object(value);
    return User(userId: _string(json['userId']), username: _string(json['username']), displayName: _string(json['displayName']));
  }
}

class UserProfile {
  const UserProfile({required this.user, required this.email, required this.emailVerified});
  final User user;
  final String email;
  final bool emailVerified;
  factory UserProfile.fromJson(Object? value) {
    final json = _object(value);
    return UserProfile(user: User.fromJson(json['user']), email: _string(json['email']), emailVerified: _boolean(json['emailVerified']));
  }
}

class ProviderLogout {
  const ProviderLogout({required this.logoutUrl});
  final Uri logoutUrl;

  factory ProviderLogout.fromJson(Object? value) {
    final json = _fields(value, {'logoutUrl'});
    final url = Uri.tryParse(_string(json['logoutUrl']));
    if (url == null) throw const FormatException('Invalid logout URL');
    return ProviderLogout(logoutUrl: url);
  }
}

class LoginSession {
  const LoginSession({required this.accessToken, required this.loginUser});
  final String accessToken;
  final User loginUser;
  factory LoginSession.fromJson(Object? value) {
    final json = _fields(value, {'accessToken', 'loginUser'});
    return LoginSession(accessToken: _string(json['accessToken']), loginUser: User.fromJson(json['loginUser']));
  }
}

class AuthOptions {
  const AuthOptions({required this.localRegistrationEnabled, required this.ssoRegistrationEnabled, required this.ssoEnabled, required this.providerName});
  final bool localRegistrationEnabled;
  final bool ssoRegistrationEnabled;
  final bool ssoEnabled;
  final String providerName;

  factory AuthOptions.fromJson(Object? value) {
    final json = _fields(value, {'localRegistrationEnabled', 'ssoRegistrationEnabled', 'ssoEnabled', 'providerName'});
    return AuthOptions(
      localRegistrationEnabled: _boolean(json['localRegistrationEnabled']),
      ssoRegistrationEnabled: _boolean(json['ssoRegistrationEnabled']),
      ssoEnabled: _boolean(json['ssoEnabled']),
      providerName: _string(json['providerName']),
    );
  }
}

class OidcStart {
  const OidcStart({required this.flowId, required this.authorizationUrl});
  final String flowId;
  final Uri authorizationUrl;

  factory OidcStart.fromJson(Object? value) {
    final json = _fields(value, {'flowId', 'authorizationUrl'});
    final text = _string(json['authorizationUrl']);
    final url = Uri.tryParse(text);
    if (url == null || !url.isAbsolute || !url.hasAuthority || url.host.isEmpty ||
        url.authority.contains('@') || url.hasFragment ||
        RegExp(r'[\x00-\x20\x7f\\]').hasMatch(text) ||
        (url.scheme != 'https' &&
            !(url.scheme == 'http' &&
                const {'localhost', '127.0.0.1', '::1', '[::1]'}.contains(url.host)))) {
      throw const FormatException('Invalid authorization URL');
    }
    return OidcStart(flowId: _string(json['flowId']), authorizationUrl: url);
  }
}

class OidcProfileHints {
  const OidcProfileHints({required this.username, required this.displayName, required this.email, required this.emailVerified});
  final String? username;
  final String? displayName;
  final String? email;
  final bool emailVerified;

  factory OidcProfileHints.fromJson(Object? value) {
    final json = _fields(value, {'username', 'displayName', 'email', 'emailVerified'});
    return OidcProfileHints(
      username: json['username'] == null ? null : _string(json['username']),
      displayName: json['displayName'] == null ? null : _string(json['displayName']),
      email: json['email'] == null ? null : _string(json['email']),
      emailVerified: _boolean(json['emailVerified']),
    );
  }
}

class IdentityCandidate {
  const IdentityCandidate({required this.issuer, required this.subject, required this.providerName, required this.claimedEmail, required this.claimedEmailVerified});
  final String issuer;
  final String subject;
  final String providerName;
  final String? claimedEmail;
  final bool claimedEmailVerified;

  factory IdentityCandidate.fromJson(Object? value) {
    final json = _fields(value, {'issuer', 'subject', 'providerName', 'claimedEmail', 'claimedEmailVerified'});
    return IdentityCandidate(
      issuer: _string(json['issuer']),
      subject: _string(json['subject']),
      providerName: _string(json['providerName']),
      claimedEmail: json['claimedEmail'] == null ? null : _string(json['claimedEmail']),
      claimedEmailVerified: _boolean(json['claimedEmailVerified']),
    );
  }
}

class IdentitySummary extends IdentityCandidate {
  const IdentitySummary({required super.issuer, required super.subject, required super.providerName, required super.claimedEmail, required super.claimedEmailVerified, required this.linkedAt, required this.lastLoginAt});
  final DateTime linkedAt;
  final DateTime? lastLoginAt;

  factory IdentitySummary.fromJson(Object? value) {
    final json = _fields(value, {'issuer', 'subject', 'providerName', 'claimedEmail', 'claimedEmailVerified', 'linkedAt', 'lastLoginAt'});
    return IdentitySummary(
      issuer: _string(json['issuer']),
      subject: _string(json['subject']),
      providerName: _string(json['providerName']),
      claimedEmail: json['claimedEmail'] == null ? null : _string(json['claimedEmail']),
      claimedEmailVerified: _boolean(json['claimedEmailVerified']),
      linkedAt: _instant(json['linkedAt']),
      lastLoginAt: json['lastLoginAt'] == null ? null : _instant(json['lastLoginAt']),
    );
  }
}

sealed class OidcExchangeResult {
  const OidcExchangeResult();

  factory OidcExchangeResult.fromJson(Object? value) {
    final json = _object(value);
    switch (_string(json['status'])) {
      case 'authenticated':
        _fields(value, {'status', 'session'});
        return OidcAuthenticated(session: LoginSession.fromJson(json['session']));
      case 'registration_required':
        _fields(value, {'status', 'continuationToken', 'profile', 'canRegister'});
        return OidcRegistrationRequired(
          continuationToken: _string(json['continuationToken']),
          profile: OidcProfileHints.fromJson(json['profile']),
          canRegister: _boolean(json['canRegister']),
        );
      case 'link_confirmation_required':
        _fields(value, {'status', 'continuationToken', 'target', 'identity'});
        return OidcLinkConfirmationRequired(
          continuationToken: _string(json['continuationToken']),
          target: User.fromJson(json['target']),
          identity: IdentityCandidate.fromJson(json['identity']),
        );
      default:
        throw const FormatException('Unknown OIDC exchange status');
    }
  }
}

class OidcAuthenticated extends OidcExchangeResult {
  const OidcAuthenticated({required this.session});
  final LoginSession session;
}

class OidcRegistrationRequired extends OidcExchangeResult {
  const OidcRegistrationRequired({required this.continuationToken, required this.profile, required this.canRegister});
  final String continuationToken;
  final OidcProfileHints profile;
  final bool canRegister;
}

class OidcLinkConfirmationRequired extends OidcExchangeResult {
  const OidcLinkConfirmationRequired({required this.continuationToken, required this.target, required this.identity});
  final String continuationToken;
  final User target;
  final IdentityCandidate identity;
}

enum InvitationPurpose { passwordReset, oidcLink }

class InvitationInspection {
  const InvitationInspection({required this.purpose, required this.target, required this.expiresAt});
  final InvitationPurpose purpose;
  final User target;
  final DateTime expiresAt;

  factory InvitationInspection.fromJson(Object? value) {
    final json = _fields(value, {'purpose', 'target', 'expiresAt'});
    final purpose = switch (_string(json['purpose'])) {
      'password_reset' => InvitationPurpose.passwordReset,
      'oidc_link' => InvitationPurpose.oidcLink,
      _ => throw const FormatException('Unknown invitation purpose'),
    };
    return InvitationInspection(purpose: purpose, target: User.fromJson(json['target']), expiresAt: _instant(json['expiresAt']));
  }
}

enum SessionMethod { password, oidc }
enum AccountOidcPurpose { link, reauth }

class AccountAuthentication {
  const AccountAuthentication({required this.hasPassword, required this.identity, required this.sessionMethod, required this.authenticatedAt, required this.oidcReauthenticatedAt, required this.providerEnabled, required this.canSignOutProvider});
  final bool hasPassword;
  final IdentitySummary? identity;
  final SessionMethod sessionMethod;
  final DateTime authenticatedAt;
  final DateTime? oidcReauthenticatedAt;
  final bool providerEnabled;
  final bool canSignOutProvider;

  factory AccountAuthentication.fromJson(Object? value) {
    final json = _fields(value, {'hasPassword', 'identity', 'sessionMethod', 'authenticatedAt', 'oidcReauthenticatedAt', 'providerEnabled', 'canSignOutProvider'});
    final method = switch (_string(json['sessionMethod'])) {
      'password' => SessionMethod.password,
      'oidc' => SessionMethod.oidc,
      _ => throw const FormatException('Unknown session method'),
    };
    return AccountAuthentication(
      hasPassword: _boolean(json['hasPassword']),
      identity: json['identity'] == null ? null : IdentitySummary.fromJson(json['identity']),
      sessionMethod: method,
      authenticatedAt: _instant(json['authenticatedAt']),
      oidcReauthenticatedAt: json['oidcReauthenticatedAt'] == null ? null : _instant(json['oidcReauthenticatedAt']),
      providerEnabled: _boolean(json['providerEnabled']),
      canSignOutProvider: _boolean(json['canSignOutProvider']),
    );
  }
}

enum CurrencyMode { single, multi }
enum SplitMode { equal, shares, percentage, exact }

class CurrencyInfo {
  const CurrencyInfo({required this.code, required this.name, required this.minorUnits});
  final String code;
  final String name;
  final int minorUnits;
  String format(BigInt amountMinor) => formatMoney(code: code, minorDigits: minorUnits, amountMinor: amountMinor);
  String formatMajor(BigInt amountMinor) => formatMajorAmount(amountMinor, minorDigits: minorUnits);
  factory CurrencyInfo.fromJson(Object? value) {
    final json = _fields(value, {'code', 'name', 'minorUnits'});
    final digits = _integer(json['minorUnits']);
    if (digits < 0 || digits > 4) throw const FormatException('Invalid currency precision');
    final name = _string(json['name']);
    if (name.isEmpty) throw const FormatException('Invalid currency name');
    return CurrencyInfo(code: _currencyCode(json['code']), name: name, minorUnits: digits);
  }
}

sealed class ExchangeRateInput {
  const ExchangeRateInput();
  const factory ExchangeRateInput.reference({required String quoteId}) = ReferenceRateInput;
  const factory ExchangeRateInput.manual({required String rate}) = ManualRateInput;
  Map<String, Object?> toJson();
  factory ExchangeRateInput.fromJson(Object? value) {
    final json = _object(value);
    switch (_string(json['source'])) {
      case 'reference':
        _fields(json, {'source', 'quoteId'});
        return ReferenceRateInput(quoteId: _string(json['quoteId']));
      case 'manual':
        _fields(json, {'source', 'rate'});
        return ManualRateInput(rate: _rate(json['rate']));
      default: throw const FormatException('Unknown rate source');
    }
  }
}

class ReferenceRateInput extends ExchangeRateInput {
  const ReferenceRateInput({required this.quoteId});
  final String quoteId;
  @override
  Map<String, Object?> toJson() => {'source': 'reference', 'quoteId': quoteId};
}

class ManualRateInput extends ExchangeRateInput {
  const ManualRateInput({required this.rate});
  final String rate;
  @override
  Map<String, Object?> toJson() => {'source': 'manual', 'rate': _rate(rate)};
}

class ExchangeRate {
  const ExchangeRate({required this.source, required this.rate, required this.rateDate, required this.fetchedAt, required this.quoteId});
  final String source;
  final String rate;
  final String? rateDate;
  final DateTime? fetchedAt;
  final String? quoteId;
  ExchangeRateInput toInput() => source == 'reference' ? ReferenceRateInput(quoteId: quoteId!) : ManualRateInput(rate: rate);
  factory ExchangeRate.fromJson(Object? value) {
    final json = _fields(value, {'source', 'rate', 'rateDate', 'fetchedAt', 'quoteId'});
    final source = _string(json['source']);
    if (source == 'reference') {
      return ExchangeRate(source: source, rate: _rate(json['rate']), rateDate: _date(json['rateDate']), fetchedAt: _instant(json['fetchedAt']), quoteId: _string(json['quoteId']));
    }
    if (source != 'manual' || json['rateDate'] != null || json['fetchedAt'] != null || json['quoteId'] != null) {
      throw const FormatException('Invalid rate provenance');
    }
    return ExchangeRate(source: source, rate: _rate(json['rate']), rateDate: null, fetchedAt: null, quoteId: null);
  }
}

class ExchangeQuote {
  const ExchangeQuote({required this.quoteId, required this.sourceCurrency, required this.targetCurrency, required this.rate, required this.provider, required this.rateDate, required this.fetchedAt});
  final String quoteId;
  final CurrencyInfo sourceCurrency;
  final CurrencyInfo targetCurrency;
  final String rate;
  final String provider;
  final String rateDate;
  final DateTime fetchedAt;
  factory ExchangeQuote.fromJson(Object? value) {
    final json = _fields(value, {'quoteId', 'sourceCurrency', 'targetCurrency', 'rate', 'provider', 'rateDate', 'fetchedAt'});
    if (json['provider'] != 'frankfurter') throw const FormatException('Unknown quote provider');
    return ExchangeQuote(quoteId: _string(json['quoteId']), sourceCurrency: CurrencyInfo.fromJson(json['sourceCurrency']), targetCurrency: CurrencyInfo.fromJson(json['targetCurrency']), rate: _rate(json['rate']), provider: _string(json['provider']), rateDate: _date(json['rateDate']), fetchedAt: _instant(json['fetchedAt']));
  }
}

class ExpenseGroup {
  ExpenseGroup({required this.groupId, required this.groupName, required this.owner, required List<User> members, required this.currencyMode, required this.defaultCurrency, required this.currencyLocked}) : members = List.unmodifiable(members);
  final String groupId;
  final String groupName;
  final User owner;
  final List<User> members;
  final CurrencyMode currencyMode;
  final CurrencyInfo defaultCurrency;
  final bool currencyLocked;
  factory ExpenseGroup.fromJson(Object? value) {
    final json = _fields(value, {'groupId', 'groupName', 'owner', 'members', 'currencyMode', 'defaultCurrency', 'currencyLocked'});
    return ExpenseGroup(groupId: _string(json['groupId']), groupName: _string(json['groupName']), owner: User.fromJson(json['owner']), members: _list(json['members'], User.fromJson), currencyMode: _enum(json['currencyMode'], CurrencyMode.values), defaultCurrency: CurrencyInfo.fromJson(json['defaultCurrency']), currencyLocked: _boolean(json['currencyLocked']));
  }
}

const _recordFields = {'tag', 'recordId', 'groupId', 'title', 'note', 'amountMinor', 'currency', 'settlementAmountMinor', 'settlementCurrency', 'exchangeRate', 'date', 'createdAt'};

enum RecordKind { expense, income, transfer }

sealed class LedgerRecord {
  const LedgerRecord({required this.recordId, required this.groupId, required this.title, required this.note, required this.amountMinor, required this.currency, required this.settlementAmountMinor, required this.settlementCurrency, required this.exchangeRate, required this.date, required this.createdAt});
  final String recordId;
  final String groupId;
  final String title;
  final String? note;
  final BigInt amountMinor;
  final CurrencyInfo currency;
  final BigInt settlementAmountMinor;
  final CurrencyInfo settlementCurrency;
  final ExchangeRate? exchangeRate;
  RecordKind get kind;
  User get actor;
  final String date;
  final DateTime createdAt;
  factory LedgerRecord.fromJson(Object? value) {
    final json = _object(value);
    return switch (_string(json['tag'])) {
      'ExpenseRecord' => ExpenseRecord.fromJson(json),
      'IncomeRecord' => IncomeRecord.fromJson(json),
      'TransferRecord' => TransferRecord.fromJson(json),
      final tag => throw FormatException('Unknown record tag: $tag'),
    };
  }
}

sealed class SplitRecord extends LedgerRecord {
  SplitRecord({required super.recordId, required super.groupId, required super.title, required super.note, required super.amountMinor, required super.currency, required super.settlementAmountMinor, required super.settlementCurrency, required super.exchangeRate, required super.date, required super.createdAt, required this.splitMode, required List<RecordSplit> splits}) : splits = List.unmodifiable(splits);
  final SplitMode splitMode;
  final List<RecordSplit> splits;
}

class ExpenseRecord extends SplitRecord {
  ExpenseRecord({required super.recordId, required super.groupId, required super.title, required super.note, required super.amountMinor, required super.currency, required super.settlementAmountMinor, required super.settlementCurrency, required super.exchangeRate, required this.paidBy, required super.date, required super.createdAt, required super.splitMode, required super.splits});
  final User paidBy;
  @override
  RecordKind get kind => RecordKind.expense;
  @override
  User get actor => paidBy;
  factory ExpenseRecord.fromJson(Object? value) {
    final json = _fields(value, {..._recordFields, 'paidBy', 'splitMode', 'splits'});
    if (json['tag'] != 'ExpenseRecord') throw const FormatException('Expected an expense record');
    return ExpenseRecord(recordId: _string(json['recordId']), groupId: _string(json['groupId']), title: _string(json['title']), note: json['note'] == null ? null : _string(json['note']), amountMinor: _positiveMinor(json['amountMinor']), currency: CurrencyInfo.fromJson(json['currency']), settlementAmountMinor: _positiveMinor(json['settlementAmountMinor']), settlementCurrency: CurrencyInfo.fromJson(json['settlementCurrency']), exchangeRate: json['exchangeRate'] == null ? null : ExchangeRate.fromJson(json['exchangeRate']), paidBy: User.fromJson(json['paidBy']), date: _date(json['date']), createdAt: _instant(json['createdAt']), splitMode: _enum(json['splitMode'], SplitMode.values), splits: _list(json['splits'], RecordSplit.fromJson));
  }
}

class IncomeRecord extends SplitRecord {
  IncomeRecord({required super.recordId, required super.groupId, required super.title, required super.note, required super.amountMinor, required super.currency, required super.settlementAmountMinor, required super.settlementCurrency, required super.exchangeRate, required this.receivedBy, required super.date, required super.createdAt, required super.splitMode, required super.splits});
  final User receivedBy;
  @override
  RecordKind get kind => RecordKind.income;
  @override
  User get actor => receivedBy;
  factory IncomeRecord.fromJson(Object? value) {
    final json = _fields(value, {..._recordFields, 'receivedBy', 'splitMode', 'splits'});
    if (json['tag'] != 'IncomeRecord') throw const FormatException('Expected an income record');
    return IncomeRecord(recordId: _string(json['recordId']), groupId: _string(json['groupId']), title: _string(json['title']), note: json['note'] == null ? null : _string(json['note']), amountMinor: _positiveMinor(json['amountMinor']), currency: CurrencyInfo.fromJson(json['currency']), settlementAmountMinor: _positiveMinor(json['settlementAmountMinor']), settlementCurrency: CurrencyInfo.fromJson(json['settlementCurrency']), exchangeRate: json['exchangeRate'] == null ? null : ExchangeRate.fromJson(json['exchangeRate']), receivedBy: User.fromJson(json['receivedBy']), date: _date(json['date']), createdAt: _instant(json['createdAt']), splitMode: _enum(json['splitMode'], SplitMode.values), splits: _list(json['splits'], RecordSplit.fromJson));
  }
}

class TransferRecord extends LedgerRecord {
  const TransferRecord({required super.recordId, required super.groupId, required super.title, required super.note, required super.amountMinor, required super.currency, required super.settlementAmountMinor, required super.settlementCurrency, required super.exchangeRate, required this.paidBy, required super.date, required super.createdAt, required this.transferTo});
  final User paidBy;
  @override
  RecordKind get kind => RecordKind.transfer;
  @override
  User get actor => paidBy;
  final User transferTo;
  factory TransferRecord.fromJson(Object? value) {
    final json = _fields(value, {..._recordFields, 'paidBy', 'transferTo'});
    if (json['tag'] != 'TransferRecord' || json['exchangeRate'] != null) throw const FormatException('Expected a payment without conversion');
    return TransferRecord(recordId: _string(json['recordId']), groupId: _string(json['groupId']), title: _string(json['title']), note: json['note'] == null ? null : _string(json['note']), amountMinor: _positiveMinor(json['amountMinor']), currency: CurrencyInfo.fromJson(json['currency']), settlementAmountMinor: _positiveMinor(json['settlementAmountMinor']), settlementCurrency: CurrencyInfo.fromJson(json['settlementCurrency']), exchangeRate: null, paidBy: User.fromJson(json['paidBy']), date: _date(json['date']), createdAt: _instant(json['createdAt']), transferTo: User.fromJson(json['transferTo']));
  }
}

class RecordSplit {
  const RecordSplit({required this.user, required this.value, required this.amountMinor, required this.settlementAmountMinor});
  final User user;
  final BigInt value;
  final BigInt amountMinor;
  final BigInt settlementAmountMinor;
  factory RecordSplit.fromJson(Object? value) {
    final json = _fields(value, {'user', 'value', 'amountMinor', 'settlementAmountMinor'});
    return RecordSplit(user: User.fromJson(json['user']), value: parseMinorAmount(json['value']), amountMinor: parseMinorAmount(json['amountMinor']), settlementAmountMinor: parseMinorAmount(json['settlementAmountMinor']));
  }
}

class SplitDraft {
  const SplitDraft({required this.userId, required this.value});
  final String userId;
  final BigInt value;
  Map<String, Object?> toJson() => {'userId': userId, 'value': value.toString()};
  factory SplitDraft.fromJson(Object? value) {
    final json = _fields(value, {'userId', 'value'});
    return SplitDraft(userId: _string(json['userId']), value: parseMinorAmount(json['value']));
  }
}

class SplitRecordDraft {
  SplitRecordDraft({required this.kind, required this.title, this.note, required this.amountMinor, required this.currency, required this.actorUserId, required this.date, required this.splitMode, required List<SplitDraft> splits, this.exchangeRate, this.savedExchangeRate}) : splits = List.unmodifiable(splits) {
    if (kind == RecordKind.transfer) throw ArgumentError.value(kind, 'kind', 'Expected expense or income');
  }
  final RecordKind kind;
  final String title;
  final String? note;
  final BigInt amountMinor;
  final String currency;
  final String actorUserId;
  final String date;
  final SplitMode splitMode;
  final List<SplitDraft> splits;
  final ExchangeRateInput? exchangeRate;
  final ExchangeRate? savedExchangeRate;
  factory SplitRecordDraft.fromRecord(SplitRecord record) => SplitRecordDraft(kind: record.kind, title: record.title, note: record.note, amountMinor: record.amountMinor, currency: record.currency.code, actorUserId: record.actor.userId, date: record.date, splitMode: record.splitMode, splits: record.splits.map((split) => SplitDraft(userId: split.user.userId, value: split.value)).toList(), exchangeRate: record.exchangeRate?.toInput(), savedExchangeRate: record.exchangeRate);
  factory SplitRecordDraft.fromJson(Object? value, {required RecordKind kind}) {
    if (kind == RecordKind.transfer) throw ArgumentError.value(kind, 'kind', 'Expected expense or income');
    final actorField = kind == RecordKind.income ? 'receivedByUserId' : 'byUserId';
    final json = _fields(value, {'title', 'amountMinor', 'currency', actorField, 'date', 'splitMode', 'splits'}, {'exchangeRate', 'note'});
    return SplitRecordDraft(kind: kind, title: _string(json['title']), note: json['note'] == null ? null : _string(json['note']), amountMinor: _positiveMinor(json['amountMinor']), currency: _currencyCode(json['currency']), actorUserId: _string(json[actorField]), date: _date(json['date']), splitMode: _enum(json['splitMode'], SplitMode.values), splits: _list(json['splits'], SplitDraft.fromJson), exchangeRate: json['exchangeRate'] == null ? null : ExchangeRateInput.fromJson(json['exchangeRate']));
  }
  Map<String, Object?> toJson() => {'title': title, 'note': note, 'amountMinor': amountMinor.toString(), 'currency': currency, (kind == RecordKind.income ? 'receivedByUserId' : 'byUserId'): actorUserId, 'date': date, 'splitMode': splitMode.name, 'splits': splits.map((split) => split.toJson()).toList(), 'exchangeRate': exchangeRate?.toJson()};
}

class TransferDraft {
  const TransferDraft({required this.amountMinor, required this.currency, required this.byUserId, required this.toUserId, required this.date, this.note});
  final BigInt amountMinor;
  final String currency;
  final String byUserId;
  final String toUserId;
  final String date;
  final String? note;
  factory TransferDraft.fromRecord(TransferRecord record) => TransferDraft(amountMinor: record.amountMinor, currency: record.currency.code, byUserId: record.paidBy.userId, toUserId: record.transferTo.userId, date: record.date, note: record.note);
  factory TransferDraft.fromJson(Object? value) {
    final json = _fields(value, {'amountMinor', 'currency', 'byUserId', 'toUserId', 'date'}, {'note'});
    return TransferDraft(amountMinor: _positiveMinor(json['amountMinor']), currency: _currencyCode(json['currency']), byUserId: _string(json['byUserId']), toUserId: _string(json['toUserId']), date: _date(json['date']), note: json['note'] == null ? null : _string(json['note']));
  }
  Map<String, Object?> toJson() => {'amountMinor': amountMinor.toString(), 'currency': currency, 'byUserId': byUserId, 'toUserId': toUserId, 'date': date, 'note': note};
}

class Receipt {
  Receipt({required this.receiptId, required this.groupId, required this.uploadedBy, required this.note, required List<LedgerRecord> records, required this.createdAt}) : records = List.unmodifiable(records);
  final String receiptId;
  final String groupId;
  final User uploadedBy;
  final String note;
  final List<LedgerRecord> records;
  final DateTime createdAt;
  factory Receipt.fromJson(Object? value) {
    final json = _object(value);
    return Receipt(receiptId: _string(json['receiptId']), groupId: _string(json['groupId']), uploadedBy: User.fromJson(json['uploadedBy']), note: _string(json['note']), records: _list(json['records'], LedgerRecord.fromJson), createdAt: _instant(json['createdAt']));
  }
}

class ReceiptDraft {
  ReceiptDraft({required this.note, required List<SplitRecordDraft> records}) : records = List.unmodifiable(records) {
    if (records.any((record) => record.kind != RecordKind.expense)) throw ArgumentError('Receipt records must be expenses');
  }
  final String note;
  final List<SplitRecordDraft> records;
  factory ReceiptDraft.fromJson(Object? value) {
    final json = _object(value);
    return ReceiptDraft(note: _string(json['note']), records: _list(json['records'], (value) => SplitRecordDraft.fromJson(value, kind: RecordKind.expense)));
  }
  Map<String, Object?> toJson() => {'note': note, 'records': records.map((record) => record.toJson()).toList()};
}

class GroupReport {
  GroupReport({required this.groupId, required List<CurrencyReport> currencies}) : currencies = List.unmodifiable(currencies);
  final String groupId;
  final List<CurrencyReport> currencies;
  factory GroupReport.fromJson(Object? value) {
    final json = _fields(value, {'groupId', 'currencies'});
    return GroupReport(groupId: _string(json['groupId']), currencies: _list(json['currencies'], CurrencyReport.fromJson));
  }
}

class CurrencyReport {
  CurrencyReport({required this.currency, required List<Balance> balances, required List<Settlement> settlements}) : balances = List.unmodifiable(balances), settlements = List.unmodifiable(settlements);
  final CurrencyInfo currency;
  final List<Balance> balances;
  final List<Settlement> settlements;
  factory CurrencyReport.fromJson(Object? value) {
    final json = _fields(value, {'currency', 'balances', 'settlements'});
    return CurrencyReport(currency: CurrencyInfo.fromJson(json['currency']), balances: _list(json['balances'], Balance.fromJson), settlements: _list(json['settlements'], Settlement.fromJson));
  }
}

class Balance {
  Balance({required this.user, required this.totalMinor, required this.expensesPaidMinor, required this.expenseShareMinor, required this.incomeReceivedMinor, required this.incomeEntitlementMinor, required this.paymentsSentMinor, required this.paymentsReceivedMinor, required List<BalanceBreakdown> breakdown}) : breakdown = List.unmodifiable(breakdown);
  final User user;
  final BigInt totalMinor;
  final BigInt expensesPaidMinor, expenseShareMinor, incomeReceivedMinor, incomeEntitlementMinor, paymentsSentMinor, paymentsReceivedMinor;
  final List<BalanceBreakdown> breakdown;
  factory Balance.fromJson(Object? value) {
    final json = _fields(value, {'user', 'totalMinor', 'breakdown', 'expensesPaidMinor', 'expenseShareMinor', 'incomeReceivedMinor', 'incomeEntitlementMinor', 'paymentsSentMinor', 'paymentsReceivedMinor'});
    return Balance(user: User.fromJson(json['user']), totalMinor: parseMinorAmount(json['totalMinor'], allowNegative: true), expensesPaidMinor: parseMinorAmount(json['expensesPaidMinor']), expenseShareMinor: parseMinorAmount(json['expenseShareMinor']), incomeReceivedMinor: parseMinorAmount(json['incomeReceivedMinor']), incomeEntitlementMinor: parseMinorAmount(json['incomeEntitlementMinor']), paymentsSentMinor: parseMinorAmount(json['paymentsSentMinor']), paymentsReceivedMinor: parseMinorAmount(json['paymentsReceivedMinor']), breakdown: _list(json['breakdown'], BalanceBreakdown.fromJson));
  }
}

class BalanceBreakdown {
  const BalanceBreakdown({required this.recordId, required this.title, required this.amountMinor, required this.date, required this.recordType, required this.recordAmountMinor});
  final String recordId;
  final String title;
  final BigInt amountMinor;
  final String date;
  final RecordKind recordType;
  final BigInt recordAmountMinor;
  factory BalanceBreakdown.fromJson(Object? value) {
    final json = _fields(value, {'recordId', 'title', 'amountMinor', 'date', 'recordType', 'recordAmountMinor'});
    return BalanceBreakdown(recordId: _string(json['recordId']), title: _string(json['title']), amountMinor: parseMinorAmount(json['amountMinor'], allowNegative: true), date: _date(json['date']), recordType: _enum(json['recordType'], RecordKind.values), recordAmountMinor: parseMinorAmount(json['recordAmountMinor']));
  }
}

class Settlement {
  const Settlement({required this.fromUser, required this.toUser, required this.amountMinor});
  final User fromUser;
  final User toUser;
  final BigInt amountMinor;
  factory Settlement.fromJson(Object? value) {
    final json = _fields(value, {'fromUser', 'toUser', 'amountMinor'});
    return Settlement(fromUser: User.fromJson(json['fromUser']), toUser: User.fromJson(json['toUser']), amountMinor: _positiveMinor(json['amountMinor']));
  }
}

class Page<T> {
  Page({required List<T> items, required this.pageInfo, required this.appliedQuery, required this.appliedSort}) : items = List.unmodifiable(items);
  final List<T> items;
  final PageInfo pageInfo;
  final String? appliedQuery;
  final String? appliedSort;
  factory Page.fromJson(Object? value, T Function(Object?) parse) {
    final json = _object(value);
    return Page(items: _list(json['items'], parse), pageInfo: PageInfo.fromJson(json['pageInfo']), appliedQuery: json['appliedQuery'] == null ? null : _string(json['appliedQuery']), appliedSort: json['appliedSort'] == null ? null : _string(json['appliedSort']));
  }
}

class PageInfo {
  const PageInfo({required this.offset, required this.limit, required this.total, required this.hasNext, required this.hasPrev});
  final int offset;
  final int limit;
  final int total;
  final bool hasNext;
  final bool hasPrev;
  factory PageInfo.fromJson(Object? value) {
    final json = _object(value);
    return PageInfo(offset: _integer(json['offset']), limit: _integer(json['limit']), total: _integer(json['total']), hasNext: _boolean(json['hasNext']), hasPrev: _boolean(json['hasPrev']));
  }
}

class PageQuery {
  const PageQuery({this.query = '', this.offset = 0, this.limit = 20, this.sort = 'groupName'});
  final String query;
  final int offset;
  final int limit;
  final String sort;
  PageQuery copyWith({String? query, int? offset, int? limit, String? sort}) => PageQuery(query: query ?? this.query, offset: offset ?? this.offset, limit: limit ?? this.limit, sort: sort ?? this.sort);
  Map<String, String> toQuery() => {'query': query, 'offset': '$offset', 'limit': '$limit', 'sort': sort};
  factory PageQuery.fromJson(Object? value) {
    final json = _object(value);
    return PageQuery(query: _string(json['query']), offset: _integer(json['offset']), limit: _integer(json['limit']), sort: _string(json['sort']));
  }
}

enum StatsBucket { day, week, month }

class StatisticsQuery {
  const StatisticsQuery({required this.from, required this.to, this.bucket = StatsBucket.month, this.userId});
  final String from, to;
  final StatsBucket bucket;
  final String? userId;
  String? get error {
    try { _date(from); _date(to); } on FormatException { return 'Enter valid dates as YYYY-MM-DD'; }
    final days = DateTime.parse('${to}T00:00:00Z').difference(DateTime.parse('${from}T00:00:00Z')).inDays + 1;
    if (days < 1 || days > 3660) return 'Choose a date range of at most 10 years';
    if (userId != null && !RegExp(r'^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$').hasMatch(userId!)) return 'Choose a valid participant';
    return null;
  }
  Map<String, String> toQuery() {
    final message = error;
    if (message != null) throw FormatException(message);
    return {'from': from, 'to': to, 'bucket': bucket.name, if (userId != null) 'userId': userId!};
  }
}

class SpendingStatistics {
  const SpendingStatistics({required this.groupId, required this.from, required this.to, required this.bucket, required this.user, required this.participants, required this.currencies});
  final String groupId, from, to;
  final StatsBucket bucket;
  final User user;
  final List<User> participants;
  final List<CurrencySpending> currencies;
  factory SpendingStatistics.fromJson(Object? value) {
    final j = _fields(value, {'groupId','from','to','bucket','user','participants','currencies'});
    return SpendingStatistics(groupId: _string(j['groupId']), from: _date(j['from']), to: _date(j['to']), bucket: _enum(j['bucket'], StatsBucket.values), user: User.fromJson(j['user']), participants: _list(j['participants'], User.fromJson), currencies: _list(j['currencies'], CurrencySpending.fromJson));
  }
}

class CurrencySpending {
  const CurrencySpending({required this.currency, required this.totalMinor, required this.expenseCount, required this.incomeMinor, required this.incomeCount, required this.netMinor, required this.paidMinor, required this.shareMinor, required this.buckets, required this.members, required this.topExpenses, required this.otherExpensesMinor});
  final CurrencyInfo currency;
  final BigInt incomeMinor, netMinor, totalMinor, paidMinor, shareMinor, otherExpensesMinor;
  final int expenseCount, incomeCount;
  final List<SpendingBucket> buckets;
  final List<MemberSpending> members;
  final List<PersonalExpense> topExpenses;
  factory CurrencySpending.fromJson(Object? value) {
    final j = _fields(value, {'currency','totalMinor','expenseCount','incomeMinor','incomeCount','netMinor','paidMinor','shareMinor','buckets','members','topExpenses','otherExpensesMinor'});
    return CurrencySpending(currency: CurrencyInfo.fromJson(j['currency']), totalMinor: parseMinorAmount(j['totalMinor']), expenseCount: _integer(j['expenseCount']), incomeMinor: parseMinorAmount(j['incomeMinor']), incomeCount: _integer(j['incomeCount']), netMinor: parseMinorAmount(j['netMinor'], allowNegative: true), paidMinor: parseMinorAmount(j['paidMinor']), shareMinor: parseMinorAmount(j['shareMinor']), buckets: _list(j['buckets'], SpendingBucket.fromJson), members: _list(j['members'], MemberSpending.fromJson), topExpenses: _list(j['topExpenses'], PersonalExpense.fromJson), otherExpensesMinor: parseMinorAmount(j['otherExpensesMinor']));
  }
}

class SpendingBucket {
  const SpendingBucket({required this.start, required this.end, required this.totalMinor, required this.expenseCount, required this.incomeMinor, required this.incomeCount, required this.netMinor, required this.paidMinor, required this.shareMinor});
  final String start, end;
  final BigInt incomeMinor, netMinor, totalMinor, paidMinor, shareMinor;
  final int expenseCount, incomeCount;
  factory SpendingBucket.fromJson(Object? value) {
    final j = _fields(value, {'start','end','totalMinor','expenseCount','incomeMinor','incomeCount','netMinor','paidMinor','shareMinor'});
    return SpendingBucket(start: _date(j['start']), end: _date(j['end']), totalMinor: parseMinorAmount(j['totalMinor']), expenseCount: _integer(j['expenseCount']), incomeMinor: parseMinorAmount(j['incomeMinor']), incomeCount: _integer(j['incomeCount']), netMinor: parseMinorAmount(j['netMinor'], allowNegative: true), paidMinor: parseMinorAmount(j['paidMinor']), shareMinor: parseMinorAmount(j['shareMinor']));
  }
}

class MemberSpending {
  const MemberSpending({required this.user, required this.paidMinor, required this.shareMinor});
  final User user;
  final BigInt paidMinor, shareMinor;
  factory MemberSpending.fromJson(Object? value) {
    final j = _fields(value, {'user','paidMinor','shareMinor'});
    return MemberSpending(user: User.fromJson(j['user']), paidMinor: parseMinorAmount(j['paidMinor']), shareMinor: parseMinorAmount(j['shareMinor']));
  }
}

class PersonalExpense {
  const PersonalExpense({required this.recordId, required this.title, required this.date, required this.amountMinor});
  final String recordId, title, date;
  final BigInt amountMinor;
  factory PersonalExpense.fromJson(Object? value) {
    final j = _fields(value, {'recordId','title','date','amountMinor'});
    return PersonalExpense(recordId: _string(j['recordId']), title: _string(j['title']), date: _date(j['date']), amountMinor: parseMinorAmount(j['amountMinor']));
  }
}
