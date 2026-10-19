import 'package:flutter/material.dart';
import 'package:go_router/go_router.dart';

import 'core/auth_flow_controller.dart';
import 'core/jizhang_repository.dart';
import 'core/models.dart';
import 'core/session_controller.dart';
import 'features/auth/auth_screen.dart';
import 'features/auth/oidc_callback_screen.dart';
import 'features/auth/invitation_screen.dart';
import 'features/account/account_screen.dart';
import 'features/groups/group_list_screen.dart';
import 'features/groups/group_screen.dart';
import 'features/groups/members_screen.dart';
import 'features/records/record_list_screen.dart';
import 'features/records/record_detail_screen.dart';
import 'features/records/record_editor.dart';
import 'features/receipts/receipt_list_screen.dart';
import 'features/receipts/receipt_detail_screen.dart';
import 'features/receipts/receipt_editor.dart';
import 'features/reports/report_screen.dart';
import 'features/reports/statistics_screen.dart';
import 'features/import/csv_import_screen.dart';
import 'shared/app_scaffold.dart';
import 'shared/widgets.dart';

class JizhangApp extends StatefulWidget {
  const JizhangApp({super.key, required this.repository, required this.session});
  final JizhangRepository repository;
  final SessionController session;
  @override
  State<JizhangApp> createState() => _JizhangAppState();
}

class _JizhangAppState extends State<JizhangApp> {
  String? _intended;
  late final _authFlow = AuthFlowController(session: widget.session);
  bool _authenticationPrepared = false;
  late final bool _previousImperativeURLs;

  @override
  void initState() {
    super.initState();
    _previousImperativeURLs = GoRouter.optionURLReflectsImperativeAPIs;
    GoRouter.optionURLReflectsImperativeAPIs = true;
  }

  late final GoRouter _router = GoRouter(
    refreshListenable: widget.session,
    redirect: (context, state) {
      final path = state.uri.path;
      if (!_authenticationPrepared) {
        _authenticationPrepared = true;
        _authFlow.prepareStartup(state.uri);
      }
      final transition = path == '/auth/oidc/callback' ||
          path == '/auth/oidc/continue' || path == '/auth/invitation';
      if (transition) return null;
      final auth = path == '/login' || path == '/register';
      if (!widget.session.isAuthenticated) {
        if (!auth) {
          if (path != '/') _intended = state.uri.toString();
          return '/login';
        }
      } else if (auth || path == '/') {
        final destination = _intended ?? '/groups';
        _intended = null;
        return destination;
      }
      return null;
    },
    errorBuilder: (context, state) => Scaffold(body: SafeArea(child: Padding(
      padding: const EdgeInsets.all(24), child: Column(crossAxisAlignment: CrossAxisAlignment.start, children: [
        const Text('Page not found', style: TextStyle(fontSize: 28)),
        TextButton(onPressed: () => context.go(widget.session.isAuthenticated ? '/groups' : '/login'), child: const Text('Back')),
      ]),
    ))),
    routes: [
      GoRoute(path: '/', redirect: (context, state) => widget.session.isAuthenticated ? '/groups' : '/login'),
      GoRoute(path: '/login', pageBuilder: (context, state) => NoTransitionPage<void>(
        key: const ValueKey('login'),
        child: AuthScreen(key: const ValueKey('login'), session: widget.session,
          authFlow: _authFlow, returnTo: _intended ?? '/groups'))),
      GoRoute(path: '/register', pageBuilder: (context, state) => NoTransitionPage<void>(
        key: const ValueKey('register'),
        child: AuthScreen(key: const ValueKey('register'), session: widget.session,
          authFlow: _authFlow, returnTo: _intended ?? '/groups', register: true))),
      GoRoute(path: '/auth/oidc/callback', builder: (context, state) =>
        OidcCallbackScreen(flow: _authFlow, location: state.uri)),
      GoRoute(path: '/auth/oidc/continue', builder: (context, state) =>
        OidcCallbackScreen(flow: _authFlow, location: state.uri)),
      GoRoute(path: '/auth/invitation', builder: (context, state) =>
        InvitationScreen(flow: _authFlow, location: state.uri)),
      ShellRoute(pageBuilder: (context, state, child) => NoTransitionPage<void>(
        key: ValueKey('app:${widget.session.generation}'),
        child: AppScaffold(session: widget.session, child: child)), routes: [
        _route('/groups', (state) => GroupListScreen(repository: widget.repository, session: widget.session)),
        _route('/groups/new', (state) => GroupListScreen(repository: widget.repository, session: widget.session, create: true)),
        _route('/account', (state) => AccountScreen(session: widget.session, authFlow: _authFlow)),
        ShellRoute(pageBuilder: (context, state, child) {
          final groupId = state.pathParameters['groupId']!;
          return NoTransitionPage<void>(
            key: ValueKey('group-shell:${widget.session.generation}'),
            child: GroupWorkspace(
              key: ValueKey('${widget.session.generation}:$groupId'),
              repository: widget.repository,
              session: widget.session,
              groupId: groupId,
              location: state.uri,
              child: child,
            ),
          );
        }, routes: [
        _route('/groups/:groupId/records', (state) => RecordListScreen(repository: widget.repository, session: widget.session, groupId: state.pathParameters['groupId']!)),
        _route('/groups/:groupId/records/:recordId', (state) => RecordDetailScreen(repository: widget.repository, session: widget.session, groupId: state.pathParameters['groupId']!, recordId: state.pathParameters['recordId']!)),
        _route('/groups/:groupId/expenses/new', (state) => RecordEditor(repository: widget.repository, session: widget.session, groupId: state.pathParameters['groupId']!)),
        _route('/groups/:groupId/expenses/:recordId/edit', (state) => RecordEditor(repository: widget.repository, session: widget.session, groupId: state.pathParameters['groupId']!, recordId: state.pathParameters['recordId']!)),
        _route('/groups/:groupId/incomes/new', (state) => RecordEditor(repository: widget.repository, session: widget.session, groupId: state.pathParameters['groupId']!, kind: RecordKind.income)),
        _route('/groups/:groupId/incomes/:recordId/edit', (state) => RecordEditor(repository: widget.repository, session: widget.session, groupId: state.pathParameters['groupId']!, kind: RecordKind.income, recordId: state.pathParameters['recordId']!)),
        _route('/groups/:groupId/transfers/new', (state) => RecordEditor(repository: widget.repository, session: widget.session, groupId: state.pathParameters['groupId']!, kind: RecordKind.transfer, fromUserId: state.uri.queryParameters['fromUserId'], toUserId: state.uri.queryParameters['toUserId'], initialAmountMinor: BigInt.tryParse(state.uri.queryParameters['amountMinor'] ?? ''), initialCurrency: state.uri.queryParameters['currency'])),
        _route('/groups/:groupId/transfers/:recordId/edit', (state) => RecordEditor(repository: widget.repository, session: widget.session, groupId: state.pathParameters['groupId']!, kind: RecordKind.transfer, recordId: state.pathParameters['recordId']!)),
        _route('/groups/:groupId/receipts', (state) => ReceiptListScreen(repository: widget.repository, session: widget.session, groupId: state.pathParameters['groupId']!)),
        _route('/groups/:groupId/receipts/new', (state) => ReceiptEditor(repository: widget.repository, session: widget.session, groupId: state.pathParameters['groupId']!)),
        _route('/groups/:groupId/receipts/:receiptId', (state) => ReceiptDetailScreen(repository: widget.repository, session: widget.session, groupId: state.pathParameters['groupId']!, receiptId: state.pathParameters['receiptId']!)),
        _route('/groups/:groupId/receipts/:receiptId/edit', (state) => ReceiptEditor(repository: widget.repository, session: widget.session, groupId: state.pathParameters['groupId']!, receiptId: state.pathParameters['receiptId']!)),
        _route('/groups/:groupId/balances', (state) => ReportScreen(repository: widget.repository, session: widget.session, groupId: state.pathParameters['groupId']!)),
        _route('/groups/:groupId/statistics', (state) => StatisticsScreen(repository: widget.repository, session: widget.session, groupId: state.pathParameters['groupId']!, parameters: state.uri.queryParameters), retainQueryState: true),
        _route('/groups/:groupId/members', (state) => MembersScreen(repository: widget.repository, session: widget.session, groupId: state.pathParameters['groupId']!)),
        _route('/groups/:groupId/import', (state) => CsvImportScreen(repository: widget.repository, session: widget.session, groupId: state.pathParameters['groupId']!)),
        ]),
      ]),
    ],
  );

  GoRoute _route(String path, Widget Function(GoRouterState) build, {bool retainQueryState = false}) => GoRoute(
    path: path,
    onExit: (context, state) async => !widget.session.isAuthenticated || await FormNavigation.canExit(context),
    pageBuilder: (context, state) {
      final location = retainQueryState ? state.uri.path : state.uri.toString();
      return NoTransitionPage<void>(
        key: ValueKey('${widget.session.generation}:$location'),
        child: PageBody(
          storageKey: '${widget.session.generation}:$location',
          child: build(state),
        ),
      );
    },
  );

  @override
  void dispose() {
    _router.dispose();
    GoRouter.optionURLReflectsImperativeAPIs = _previousImperativeURLs;
    _authFlow.dispose();
    super.dispose();
  }
  @override
  Widget build(BuildContext context) => MaterialApp.router(
    title: 'Jizhang', routerConfig: _router,
    theme: ThemeData(
      useMaterial3: true,
      colorScheme: ColorScheme.fromSeed(seedColor: const Color(0xFF0F766E), brightness: Brightness.light).copyWith(
        primary: const Color(0xFF0F766E), surface: Colors.white, error: const Color(0xFFB42318), onSurface: const Color(0xFF172B36)),
      scaffoldBackgroundColor: const Color(0xFFF5F7FA),
      inputDecorationTheme: const InputDecorationTheme(
        border: OutlineInputBorder(borderRadius: BorderRadius.all(Radius.circular(8))),
        contentPadding: EdgeInsets.all(12), filled: true, fillColor: Colors.white),
      cardTheme: const CardThemeData(elevation: 0,
        margin: EdgeInsets.only(bottom: 8),
        shape: RoundedRectangleBorder(borderRadius: BorderRadius.all(Radius.circular(8)),
          side: BorderSide(color: Color(0xFFDCE3E7)))),
      dialogTheme: const DialogThemeData(
        shape: RoundedRectangleBorder(borderRadius: BorderRadius.all(Radius.circular(8)))),
      searchBarTheme: SearchBarThemeData(elevation: const WidgetStatePropertyAll(0),
        shape: WidgetStatePropertyAll(RoundedRectangleBorder(borderRadius: BorderRadius.circular(8))),
        side: const WidgetStatePropertyAll(BorderSide(color: Color(0xFFDCE3E7)))),
      filledButtonTheme: FilledButtonThemeData(style: FilledButton.styleFrom(
        minimumSize: const Size(48, 48), shape: RoundedRectangleBorder(borderRadius: BorderRadius.circular(8)))),
      outlinedButtonTheme: OutlinedButtonThemeData(style: OutlinedButton.styleFrom(
        minimumSize: const Size(48, 48), shape: RoundedRectangleBorder(borderRadius: BorderRadius.circular(8)))),
      textButtonTheme: TextButtonThemeData(style: TextButton.styleFrom(
        minimumSize: const Size(48, 48), shape: RoundedRectangleBorder(borderRadius: BorderRadius.circular(8)))),
      iconButtonTheme: IconButtonThemeData(style: IconButton.styleFrom(
        minimumSize: const Size(48, 48), shape: RoundedRectangleBorder(borderRadius: BorderRadius.circular(8)))),
      visualDensity: VisualDensity.standard,
    ),
  );
}
