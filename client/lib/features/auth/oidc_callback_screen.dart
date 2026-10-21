import 'package:flutter/material.dart';
import 'package:go_router/go_router.dart';

import '../../core/auth_flow_controller.dart';
import '../../core/models.dart';
import '../../shared/widgets.dart';
import '../../shared/legal.dart';

class OidcCallbackScreen extends StatefulWidget {
  const OidcCallbackScreen({super.key, required this.flow, required this.location});
  final AuthFlowController flow;
  final Uri location;
  @override
  State<OidcCallbackScreen> createState() => _OidcCallbackScreenState();
}

class _OidcCallbackScreenState extends State<OidcCallbackScreen> {
  final _registration = GlobalKey<FormState>();
  final _link = GlobalKey<FormState>();
  final _username = TextEditingController();
  final _name = TextEditingController();
  final _email = TextEditingController();
  final _identifier = TextEditingController();
  final _password = TextEditingController();
  OidcExchangeResult? _seeded;
  bool _linkExisting = false;
  bool _loading = true;
  AuthFlowController get flow => widget.flow;
  String _providerName = 'provider';
  bool get _accountFlow => flow.purpose == AuthFlowPurpose.link || flow.purpose == AuthFlowPurpose.reauth;
  @override
  void initState() {
    super.initState();
    flow.addListener(_changed);
    WidgetsBinding.instance.addPostFrameCallback((_) => _initialize());
  }
  @override
  void didUpdateWidget(covariant OidcCallbackScreen oldWidget) {
    super.didUpdateWidget(oldWidget);
    if (widget.location != oldWidget.location && widget.location.path == '/auth/oidc/callback') {
      _loading = true;
      WidgetsBinding.instance.addPostFrameCallback((_) => _initialize());
    }
  }
  Future<void> _initialize() async {
    if (!mounted) return;
    final destination = await flow.handleCallback(widget.location);
    if (!mounted) return;
    _loadProviderName();
    if (destination != null) { context.go(destination); return; }
    _seed();
    setState(() => _loading = false);
    if (widget.location.path == '/auth/oidc/callback') context.go('/auth/oidc/continue');
  }
  Future<void> _loadProviderName() async {
    try {
      final options = await flow.repository.getAuthOptions();
      if (mounted) setState(() => _providerName = options.providerName);
    } catch (_) { /* Exchange errors and restart remain available independently. */ }
  }
  void _seed() {
    final result = flow.result;
    if (identical(_seeded, result)) return;
    _seeded = result;
    if (result is OidcRegistrationRequired) {
      _username.text = result.profile.username ?? '';
      _name.text = result.profile.displayName ?? '';
      _email.text = result.profile.email ?? '';
    }
  }
  void _changed() { if (mounted) { _seed(); setState(() {}); } }
  @override
  void dispose() {
    flow.removeListener(_changed);
    for (final controller in [_username, _name, _email, _identifier, _password]) { controller.dispose(); }
    super.dispose();
  }
  Future<void> _finish(Future<String?> Function() action) async {
    final destination = await action();
    if (mounted && destination != null) context.go(destination);
  }
  Future<void> _cancel() async {
    if (!await FormNavigation.canExit(context) || !mounted) return;
    // Retire the request before navigation; a delayed response must never sign in.
    final cancellation = flow.cancel();
    context.go(_accountFlow ? '/account' : '/login');
    await cancellation;
  }
  Widget _field(TextEditingController controller, String label, {bool password = false, TextInputType? keyboard}) => TextFormField(
    controller: controller, obscureText: password, keyboardType: keyboard,
    decoration: InputDecoration(labelText: label), enabled: !flow.pending,
    validator: (value) => value == null || value.trim().isEmpty ? 'Enter $label' : null,
  );
  @override
  Widget build(BuildContext context) {
    final result = flow.result;
    return Scaffold(appBar: AppBar(title: Text(flow.purpose == AuthFlowPurpose.reauth ? 'Verify provider identity' : 'Complete sign-in')), body: SafeArea(child: SingleChildScrollView(
      padding: const EdgeInsets.all(16), child: Center(child: ConstrainedBox(constraints: const BoxConstraints(maxWidth: 480),
        child: Column(crossAxisAlignment: CrossAxisAlignment.stretch, spacing: 12, children: [
          if (_loading || flow.pending) const LinearProgressIndicator(),
          if (flow.error != null) Text(flow.error!, style: TextStyle(color: Theme.of(context).colorScheme.error)),
          if (result is OidcRegistrationRequired) ...[
            const Text('This identity is not linked to an account. An email match never links an account automatically.'),
            if (!_linkExisting && result.canRegister) Form(key: _registration, child: Column(spacing: 12, crossAxisAlignment: CrossAxisAlignment.stretch, children: [
              _field(_username, 'Username'), _field(_name, 'Display name'), _field(_email, 'Email', keyboard: TextInputType.emailAddress),
              FilledButton(onPressed: flow.pending ? null : () { if (_registration.currentState!.validate()) _finish(() => flow.register(username: _username.text, displayName: _name.text, email: _email.text)); }, child: Text('Create account with $_providerName')),
            ])),
            if (!result.canRegister) const Text('SSO registration is disabled. Link an existing account or contact an administrator.'),
            TextButton(onPressed: flow.pending ? null : () => setState(() => _linkExisting = !_linkExisting), child: Text(_linkExisting && result.canRegister ? 'Create a new account instead' : 'Link to existing account')),
            if (_linkExisting) Form(key: _link, child: Column(spacing: 12, crossAxisAlignment: CrossAxisAlignment.stretch, children: [
              _field(_identifier, 'Username or email'), _field(_password, 'Password', password: true),
              FilledButton(onPressed: flow.pending ? null : () { if (_link.currentState!.validate()) _finish(() => flow.linkExisting(identifier: _identifier.text, password: _password.text)); }, child: const Text('Link to existing account')),
            ])),
          ] else if (result is OidcLinkConfirmationRequired) ...[
            Text('Link identity to ${result.target.displayName} (@${result.target.username})?'),
            Text('Provider: ${result.identity.providerName}'),
            SelectableText('Issuer: ${result.identity.issuer}'),
            SelectableText('Subject: ${result.identity.subject}'),
            const Text('Only confirm if you control this provider identity and intend to use it for this account.'),
            FilledButton(onPressed: flow.pending ? null : () => _finish(flow.confirmLink), child: const Text('Link identity')),
          ] else if (!_loading && !flow.pending) ...[
            if (flow.error == null) const Text('No pending sign-in is available. Restart SSO to continue.'),
            FilledButton(onPressed: () async {
              if (!await FormNavigation.canExit(context) || !mounted) return;
              final destination = _accountFlow ? '/account' : '/login';
              flow.clear();
              context.go(destination);
            }, child: Text(_accountFlow ? 'Back to Account' : 'Restart sign-in')),
          ],
          TextButton(onPressed: _cancel, child: const Text('Cancel')),
          LegalLinks(disabled: _loading || flow.pending),
        ]),
      )),
    )));
  }
}
