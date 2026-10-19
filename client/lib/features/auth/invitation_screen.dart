import 'dart:convert';

import 'package:flutter/material.dart';
import 'package:go_router/go_router.dart';

import '../../core/auth_flow_controller.dart';
import '../../core/models.dart';
import '../../shared/widgets.dart';

class InvitationScreen extends StatefulWidget {
  const InvitationScreen({super.key, required this.flow, required this.location});
  final AuthFlowController flow;
  final Uri location;
  @override
  State<InvitationScreen> createState() => _InvitationScreenState();
}
class _InvitationScreenState extends State<InvitationScreen> {
  final _form = GlobalKey<FormState>();
  final _password = TextEditingController();
  final _confirmation = TextEditingController();
  String? _token;
  InvitationInspection? _inspection;
  String? _error;
  bool _pending = true;
  bool _complete = false;
  int _epoch = 0;
  late int _sessionGeneration;
  bool _startingProvider = false;
  bool _completingReset = false;
  bool get _busy => _pending || widget.flow.pending;
  void _sessionChanged() {
    if (_sessionGeneration == widget.flow.session.generation) return;
    _sessionGeneration = widget.flow.session.generation;
    if (_startingProvider || _completingReset) return;
    ++_epoch;
    _token = null;
    _password.clear(); _confirmation.clear();
    if (mounted) setState(() { _pending = false; _inspection = null; _error = 'A new sign-in started. Reopen the invitation to continue.'; });
  }
  @override
  void initState() { super.initState(); _sessionGeneration = widget.flow.session.generation; widget.flow.session.addListener(_sessionChanged); _inspect(); }
  @override
  void didUpdateWidget(covariant InvitationScreen oldWidget) {
    super.didUpdateWidget(oldWidget);
    if (widget.location == oldWidget.location || !widget.location.queryParametersAll.containsKey('token')) return;
    ++_epoch;
    _token = null; _inspection = null; _error = null; _complete = false; _pending = true;
    _password.clear(); _confirmation.clear();
    _inspect();
  }
  Future<void> _inspect() async {
    final epoch = ++_epoch;
    try {
      final values = widget.location.queryParametersAll;
      if (widget.flow.browser.supported) widget.flow.browser.replaceUrl(Uri.base.replace(query: '', fragment: '/auth/invitation'));
      // Initial route publication can overwrite the synchronous history edit.
      // Replace that route too, preserving this page's in-memory invitation.
      WidgetsBinding.instance.addPostFrameCallback((_) {
        if (!mounted || epoch != _epoch) return;
        final router = GoRouter.maybeOf(context);
        if (router != null) {
          Router.neglect(context, () => router.replace('/auth/invitation'));
        }
      });
      if (values.length != 1 || values['token']?.length != 1 || (values['token']?.single ?? '').isEmpty) throw StateError('This invitation is invalid or expired.');
      _token = values['token']!.single;
      final result = await widget.flow.repository.inspectInvitation(_token!);
      if (!mounted || epoch != _epoch) return;
      if (!result.expiresAt.isAfter(widget.flow.now)) throw StateError('This invitation is invalid or expired.');
      setState(() => _inspection = result);
    } catch (failure) { if (mounted && epoch == _epoch) setState(() => _error = errorMessage(failure)); }
    finally { if (mounted && epoch == _epoch) setState(() => _pending = false); }
  }
  Future<void> _reset() async {
    if (_busy || !_form.currentState!.validate() || _token == null) return;
    final epoch = ++_epoch;
    setState(() { _pending = true; _error = null; });
    try {
      await widget.flow.repository.resetPassword(token: _token!, newPassword: _password.text);
      if (!mounted || epoch != _epoch) return;
      _token = null; _password.clear(); _confirmation.clear();
      _completingReset = true;
      try { widget.flow.session.clearAfterCredentialChange(); }
      finally { _completingReset = false; }
      setState(() => _complete = true);
    } catch (failure) { if (mounted && epoch == _epoch) setState(() => _error = errorMessage(failure)); }
    finally { if (mounted && epoch == _epoch) setState(() => _pending = false); }
  }
  Future<void> _continue() async {
    if (_busy || _token == null) return;
    setState(() => _pending = true);
    try {
      if (!await FormNavigation.canExit(context) || !mounted || widget.flow.pending || _token == null) return;
      setState(() => _error = null);
      _startingProvider = true;
      await widget.flow.start(purpose: AuthFlowPurpose.invitation, invitationToken: _token);
      if (mounted) setState(() => _error = widget.flow.error);
    } finally {
      _startingProvider = false;
      if (mounted) setState(() => _pending = false);
    }
  }
  @override
  void dispose() { widget.flow.session.removeListener(_sessionChanged); ++_epoch; _token = null; _password.dispose(); _confirmation.dispose(); super.dispose(); }
  @override
  Widget build(BuildContext context) {
    final inspection = _inspection;
    return ListenableBuilder(listenable: widget.flow, builder: (context, _) => Scaffold(appBar: AppBar(title: const Text('Account invitation')), body: SafeArea(child: SingleChildScrollView(padding: const EdgeInsets.all(16), child: Center(child: ConstrainedBox(
      constraints: const BoxConstraints(maxWidth: 480), child: Column(crossAxisAlignment: CrossAxisAlignment.stretch, spacing: 12, children: [
        if (_busy) const LinearProgressIndicator(),
        if (_error != null) Text(_error!, style: TextStyle(color: Theme.of(context).colorScheme.error)),
        if (_complete) ...[
          const Text('Password updated. Sign in with your new password.'),
          FilledButton(onPressed: _busy ? null : () => context.go('/login'), child: const Text('Sign in')),
        ] else if (inspection != null) ...[
          Text('Account: ${inspection.target.displayName} (@${inspection.target.username})'),
          Text('Invitation expires: ${inspection.expiresAt.toLocal()}'),
          if (inspection.purpose == InvitationPurpose.passwordReset) Form(key: _form, child: Column(crossAxisAlignment: CrossAxisAlignment.stretch, spacing: 12, children: [
            const Text('Resetting your password signs out all existing sessions.'),
            TextFormField(controller: _password, enabled: !_busy, obscureText: true, autofillHints: const [AutofillHints.newPassword], decoration: const InputDecoration(labelText: 'New password'), validator: (value) => value == null || value.length < 6 ? 'Use at least 6 characters' : utf8.encode(value).length > 72 ? 'Password must be at most 72 UTF-8 bytes' : null),
            TextFormField(controller: _confirmation, enabled: !_busy, obscureText: true, decoration: const InputDecoration(labelText: 'Confirm new password'), validator: (value) => value != _password.text ? 'Passwords do not match' : null),
            FilledButton(onPressed: _busy ? null : _reset, child: const Text('Reset password')),
          ])) else ...[
            const Text('Verify your provider identity, then confirm linking it to this account.'),
            if (!widget.flow.browser.supported) const Text('Open this invitation in the web application to use SSO.'),
            FilledButton(onPressed: _busy || !widget.flow.browser.supported ? null : _continue, child: const Text('Continue with provider')),
          ],
        ] else if (!_busy) const Text('Request a new invitation from your administrator.'),
        TextButton(onPressed: _busy ? null : () async { if (!_busy && await FormNavigation.canExit(context) && mounted && !_busy) context.go('/login'); }, child: const Text('Cancel and return to sign-in')),
      ]),
    ))))));
  }
}
