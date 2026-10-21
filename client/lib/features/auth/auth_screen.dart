import 'package:flutter/material.dart';
import 'package:go_router/go_router.dart';

import '../../core/auth_flow_controller.dart';
import '../../core/models.dart';
import '../../core/session_controller.dart';
import '../../shared/branding.dart';
import '../../shared/legal.dart';
import '../../shared/widgets.dart';

class AuthScreen extends StatefulWidget {
  const AuthScreen({super.key, required this.session, this.register = false, this.authFlow, this.returnTo = '/groups'});
  final SessionController session;
  final bool register;
  final AuthFlowController? authFlow;
  final String returnTo;
  @override
  State<AuthScreen> createState() => _AuthScreenState();
}

class _AuthScreenState extends State<AuthScreen> {
  final _form = GlobalKey<FormState>();
  late final _username = TextEditingController(text: widget.session.registeredUsername ?? '');
  final _password = TextEditingController();
  final _displayName = TextEditingController();
  final _email = TextEditingController();
  bool _visible = false;
  bool _pending = false;
  String? _error;
  AuthOptions? _options;
  String? _optionsError;
  bool _loadingOptions = true;
  bool get _busy => _pending || (widget.authFlow?.pending ?? false);
  @override
  void initState() { super.initState(); widget.authFlow?.addListener(_flowChanged); _loadOptions(); }
  void _flowChanged() { if (mounted) setState(() {}); }
  Future<void> _loadOptions() async {
    setState(() { _loadingOptions = true; _optionsError = null; });
    try {
      final options = await widget.session.repository.getAuthOptions();
      if (mounted) setState(() => _options = options);
    } catch (error) { if (mounted) setState(() => _optionsError = errorMessage(error)); }
    finally { if (mounted) setState(() => _loadingOptions = false); }
  }
  Future<void> _sso() async {
    final flow = widget.authFlow;
    if (_busy || flow == null) return;
    setState(() => _pending = true);
    try {
      if (!await FormNavigation.canExit(context) || !mounted || flow.pending) return;
      setState(() => _error = null);
      await flow.start(returnTo: widget.returnTo);
      if (mounted) setState(() => _error = flow.error);
    } finally {
      if (mounted) setState(() => _pending = false);
    }
  }

  Future<void> _switchMode() async {
    if (_busy || !await FormNavigation.canExit(context) || !mounted || _busy) return;
    context.go(widget.register ? '/login' : '/register');
  }
  @override
  void dispose() { widget.authFlow?.removeListener(_flowChanged); _username.dispose(); _password.dispose(); _displayName.dispose(); _email.dispose(); super.dispose(); }

  Future<void> _submit() async {
    if (_busy || (widget.register && (_loadingOptions || _options?.localRegistrationEnabled != true)) || !_form.currentState!.validate()) return;
    setState(() { _pending = true; _error = null; });
    try {
      if (widget.register) {
        await widget.session.register(_username.text, _displayName.text, _email.text, _password.text);
      } else {
        await widget.session.login(_username.text, _password.text);
      }
      if (!mounted) return;
      if (widget.session.isAuthenticated) return;
      if (widget.register && widget.session.registeredUsername != null) {
        context.go('/login');
      } else {
        setState(() => _error = widget.session.message);
      }
    } catch (error) {
      if (!mounted) return;
      if (widget.register && widget.session.registeredUsername != null) {
        context.go('/login');
      } else {
        setState(() => _error = errorMessage(error));
      }
    } finally {
      if (mounted) setState(() => _pending = false);
    }
  }

  @override
  Widget build(BuildContext context) => Scaffold(body: SafeArea(child: Center(
    child: SingleChildScrollView(padding: const EdgeInsets.all(16), child: ConstrainedBox(
      constraints: const BoxConstraints(maxWidth: 440),
      child: Card(child: Padding(padding: const EdgeInsets.all(12), child: Form(
        key: _form,
        child: Column(crossAxisAlignment: CrossAxisAlignment.stretch, children: [
          Row(children: [
            const JizhangMark(size: 48),
            const SizedBox(width: 12),
            Expanded(child: Text('Jizhang', style: Theme.of(context).textTheme.headlineSmall)),
          ]),
          const SizedBox(height: 8),
          Text(widget.register ? 'Create your account' : 'Sign in to your shared expenses', style: Theme.of(context).textTheme.titleMedium),
          const SizedBox(height: 16),
          if (_error != null || widget.session.message != null || widget.authFlow?.error != null) Padding(
            padding: const EdgeInsets.only(bottom: 16),
            child: Text(_error ?? widget.session.message ?? widget.authFlow!.error!, style: const TextStyle(color: Color(0xFFB42318))),
          ),
          TextFormField(controller: _username, decoration: InputDecoration(labelText: widget.register ? 'Username' : 'Username or email', prefixText: widget.register ? '@' : null),
            autofillHints: const [AutofillHints.username], textInputAction: TextInputAction.next,
            validator: (value) => value == null || value.trim().isEmpty ? 'Enter ${widget.register ? 'a username' : 'your username or email'}' : widget.register && value.trim().runes.length > 50 ? 'Use a username of 1–50 characters' : null),
          if (widget.register) ...[
            const SizedBox(height: 12),
            TextFormField(controller: _displayName, decoration: const InputDecoration(labelText: 'Display name'),
              autofillHints: const [AutofillHints.name], textInputAction: TextInputAction.next,
              validator: (value) => value == null || value.trim().isEmpty || value.trim().runes.length > 100 ? 'Use a display name of 1–100 characters' : null),
            const SizedBox(height: 12),
            TextFormField(controller: _email, decoration: const InputDecoration(labelText: 'Email'), keyboardType: TextInputType.emailAddress,
              autofillHints: const [AutofillHints.email], textInputAction: TextInputAction.next,
              validator: (value) => value == null || value.trim().isEmpty || value.trim().length > 254 ? 'Enter a valid email address' : null),
          ],
          const SizedBox(height: 12),
          TextFormField(controller: _password, obscureText: !_visible,
            autofillHints: [widget.register ? AutofillHints.newPassword : AutofillHints.password],
            decoration: InputDecoration(labelText: 'Password', suffixIcon: IconButton(
              tooltip: _visible ? 'Hide password' : 'Show password',
              onPressed: () => setState(() => _visible = !_visible), icon: Icon(_visible ? Icons.visibility_off : Icons.visibility))),
            validator: (value) => value == null || value.length < 6 ? 'Use at least 6 characters' : null,
            onFieldSubmitted: (_) => _submit()),
          const SizedBox(height: 12),
          FilledButton(onPressed: _busy || (widget.register && (_loadingOptions || _options?.localRegistrationEnabled != true)) ? null : _submit,
            child: Text(_busy ? 'Please wait…' : widget.register ? 'Create account' : 'Sign in')),
          const SizedBox(height: 8),
          if (widget.register || _options?.localRegistrationEnabled == true)
            TextButton(onPressed: _busy ? null : _switchMode,
              child: Text(widget.register ? 'Already have an account? Sign in' : 'Create an account')),
          if (_loadingOptions) const LinearProgressIndicator(),
          if (_optionsError != null) ...[
            Text('Sign-in options could not be loaded: $_optionsError'),
            TextButton(onPressed: _loadingOptions ? null : _loadOptions, child: const Text('Retry')),
          ],
          if (widget.register && !_loadingOptions && _options != null && !_options!.localRegistrationEnabled)
            const Text('Local registration is disabled. You can still sign in to an existing account.'),
          if (_options?.ssoEnabled == true && widget.authFlow?.browser.supported == true && (!widget.register || _options!.ssoRegistrationEnabled)) ...[
            const SizedBox(height: 12),
            OutlinedButton(onPressed: _busy ? null : _sso, child: Text('Continue with ${_options!.providerName}')),
          ],
          LegalLinks(disabled: _busy),
          const SizedBox(height: 16),
          const AppVersionLabel(),
        ]),
      ))),
    )),
  )));
}
