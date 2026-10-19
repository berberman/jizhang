import 'dart:convert';

import 'package:flutter/material.dart';

import '../../core/api_client.dart';
import '../../core/auth_flow_controller.dart';
import '../../core/models.dart';
import '../../core/session_controller.dart';
import '../../shared/widgets.dart';

class AccountSecuritySection extends StatefulWidget {
  const AccountSecuritySection({super.key, required this.session,
    required this.canLeave, required this.beforeExternalNavigation,
    required this.onPendingChanged, this.authFlow, this.disabled = false});

  final SessionController session;
  final AuthFlowController? authFlow;
  final Future<bool> Function() canLeave;
  final Future<bool> Function() beforeExternalNavigation;
  final ValueChanged<bool> onPendingChanged;
  final bool disabled;

  @override
  State<AccountSecuritySection> createState() => _AccountSecuritySectionState();
}

class _AccountSecuritySectionState extends State<AccountSecuritySection> {
  AccountAuthentication? _authentication;
  bool _loading = true;
  bool _pending = false;
  String? _error;
  int _request = 0;

  @override
  void initState() {
    super.initState();
    widget.authFlow?.addListener(_flowChanged);
    _load();
  }

  void _flowChanged() {
    if (!mounted) return;
    setState(() {});
    widget.onPendingChanged(_busy);
  }

  bool _current(int generation) => mounted && generation == widget.session.generation;
  bool get _busy => _pending || (widget.authFlow?.pending ?? false);
  bool get _disabled => widget.disabled || _busy;
  bool get _browserSupported => widget.authFlow?.browser.supported ?? false;
  bool get _freshOidc {
    final authentication = _authentication;
    final at = authentication?.oidcReauthenticatedAt;
    if (authentication == null || at == null || !authentication.providerEnabled ||
        authentication.identity == null || authentication.sessionMethod != SessionMethod.oidc) return false;
    final age = DateTime.now().toUtc().difference(at);
    return age >= Duration.zero && age < const Duration(minutes: 10);
  }

  Future<void> _load() async {
    final request = ++_request;
    final generation = widget.session.generation;
    setState(() { _loading = true; _error = null; });
    try {
      final value = await widget.session.repository.getAuthentication();
      if (!_current(generation) || request != _request) return;
      setState(() { _authentication = value; _loading = false; });
    } catch (error) {
      if (!_current(generation) || request != _request) return;
      setState(() { _loading = false; _error = errorMessage(error); });
    }
  }

  void _setPending(bool value) {
    setState(() => _pending = value);
    widget.onPendingChanged(_busy);
  }

  Future<void> _external(AuthFlowPurpose purpose) async {
    if (_disabled || !_browserSupported) return;
    final generation = widget.session.generation;
    if (!await widget.beforeExternalNavigation() || !_current(generation) || _disabled) return;
    _setPending(true);
    try {
      await widget.authFlow!.start(purpose: purpose, returnTo: '/account');
      if (_current(generation)) setState(() => _error = widget.authFlow!.error);
    } finally {
      if (mounted) _setPending(false);
    }
  }

  // Password proof is a separate server-verified step. New password fields do
  // not exist until this succeeds; a failed proof retains its input.
  Future<String?> _verifyPassword({Future<void> Function(String)? action}) async {
    final controller = TextEditingController();
    final generation = widget.session.generation;
    String? error;
    bool pending = false;
    final route = DialogRoute<String>(context: context, barrierDismissible: false,
      builder: (dialogContext) => StatefulBuilder(builder: (context, update) => Actions(
        actions: {DismissIntent: CallbackAction<DismissIntent>(onInvoke: (_) {
          if (!pending) Navigator.pop(dialogContext);
          return null;
        })},
        child: FocusScope(autofocus: true, child: PopScope(
        canPop: !pending,
        child: AlertDialog(title: const Text('Verify your password'),
          content: SingleChildScrollView(child: Column(mainAxisSize: MainAxisSize.min,
            crossAxisAlignment: CrossAxisAlignment.stretch, spacing: 12, children: [
              const Text('Verify your current password before continuing.'),
              TextField(controller: controller, enabled: !pending, obscureText: true,
                autofocus: true, decoration: const InputDecoration(labelText: 'Current password')),
              if (error != null) ErrorPanel(error: error!),
            ])),
          actions: [
            TextButton(onPressed: pending ? null : () => Navigator.pop(dialogContext), child: const Text('Cancel')),
            FilledButton(onPressed: pending ? null : () async {
              update(() { pending = true; error = null; });
              try {
                if (action == null) {
                  await widget.session.repository.reauthenticatePassword(controller.text);
                } else {
                  await action(controller.text);
                }
                if (!dialogContext.mounted) return;
                Navigator.pop(dialogContext, _current(generation) ? controller.text : null);
              } catch (failure) {
                if (!dialogContext.mounted) return;
                if (!_current(generation)) { Navigator.pop(dialogContext); return; }
                update(() { pending = false; error = errorMessage(failure); });
              }
            }, child: Text(pending ? 'Verifying…' : 'Verify')),
          ]),
      )))));
    final result = await Navigator.of(context).push(route);
    await route.completed;
    controller.dispose();
    return result;
  }

  Future<void> _password() async {
    final authentication = _authentication;
    if (_disabled || authentication == null) return;
    if (!authentication.hasPassword && !_freshOidc) {
      await _offerVerification();
      return;
    }
    _setPending(true);
    final generation = widget.session.generation;
    final currentPassword = authentication.hasPassword ? await _verifyPassword() : null;
    if (!_current(generation)) { if (mounted) _setPending(false); return; }
    if (authentication.hasPassword && currentPassword == null) { _setPending(false); return; }
    await _newPassword(currentPassword);
    if (mounted) _setPending(false);
  }

  Future<void> _offerVerification() async {
    final canVerify = _browserSupported && (_authentication?.providerEnabled ?? false);
    final accepted = await showDialog<bool>(context: context, builder: (context) => AlertDialog(
      title: const Text('Reauthentication required'),
      content: Text(canVerify
        ? 'Verify your linked SSO identity before setting or removing a password. You will return to Account to continue. Password verification alone cannot authorize this action.'
        : 'A recent verification with your linked SSO provider is required. Use the web app with an enabled provider to continue.'),
      actions: [
        TextButton(onPressed: () => Navigator.pop(context, false), child: const Text('Cancel')),
        if (canVerify) FilledButton(onPressed: () => Navigator.pop(context, true), child: const Text('Verify with provider')),
      ]));
    if (accepted == true && mounted) await _external(AuthFlowPurpose.reauth);
  }

  Future<void> _newPassword(String? currentPassword) async {
    final password = TextEditingController();
    final confirmation = TextEditingController();
    final form = GlobalKey<FormState>();
    final generation = widget.session.generation;
    bool pending = false;
    bool requiresVerification = false;
    bool navigateToVerification = false;
    String? error;
    final route = DialogRoute<void>(context: context, barrierDismissible: false,
      builder: (dialogContext) => StatefulBuilder(builder: (context, update) => Actions(
        actions: {DismissIntent: CallbackAction<DismissIntent>(onInvoke: (_) {
          if (!pending) Navigator.pop(dialogContext);
          return null;
        })},
        child: FocusScope(autofocus: true, child: PopScope(
        canPop: !pending,
        child: AlertDialog(title: Text(currentPassword == null ? 'Set password' : 'Change password'),
          content: SingleChildScrollView(child: Form(key: form, child: Column(
            mainAxisSize: MainAxisSize.min, crossAxisAlignment: CrossAxisAlignment.stretch, spacing: 12,
            children: [
              const Text('This signs out all application sessions. Sign in again with your new password or linked SSO identity.'),
              if (!requiresVerification) ...[
              TextFormField(controller: password, enabled: !pending && !requiresVerification,
                obscureText: true, autofocus: true, decoration: const InputDecoration(labelText: 'New password'),
                validator: (value) => (value ?? '').runes.length < 6 ? 'Use at least 6 characters'
                  : utf8.encode(value!).length > 72 ? 'Password must be at most 72 UTF-8 bytes' : null),
              TextFormField(controller: confirmation, enabled: !pending && !requiresVerification,
                obscureText: true, decoration: const InputDecoration(labelText: 'Confirm new password'),
                validator: (value) => value != password.text ? 'Passwords do not match' : null),
              ],
              if (error != null) ErrorPanel(error: error!),
            ]))),
          actions: [
            TextButton(onPressed: pending ? null : () => Navigator.pop(dialogContext), child: const Text('Cancel')),
            if (requiresVerification && _browserSupported)
              FilledButton(onPressed: () { navigateToVerification = true; Navigator.pop(dialogContext); }, child: const Text('Verify with provider')),
            if (!requiresVerification) FilledButton(onPressed: pending ? null : () async {
              if (!form.currentState!.validate()) return;
              update(() { pending = true; error = null; });
              if (!await widget.canLeave() || !_current(generation)) {
                if (dialogContext.mounted) update(() => pending = false);
                return;
              }
              try {
                await widget.session.repository.changePassword(currentPassword: currentPassword, newPassword: password.text);
                if (dialogContext.mounted) Navigator.pop(dialogContext);
                if (_current(generation)) widget.session.clearAfterCredentialChange();
              } catch (failure) {
                if (!dialogContext.mounted) return;
                if (!_current(generation)) { Navigator.pop(dialogContext); return; }
                update(() {
                  pending = false; error = errorMessage(failure);
                  requiresVerification = failure is ApiException && failure.statusCode == 403;
                  if (requiresVerification) {
                    password.clear();
                    confirmation.clear();
                  }
                });
              }
            }, child: Text(pending ? 'Saving…' : 'Save password')),
          ]),
      )))));
    await Navigator.of(context).push(route);
    await route.completed;
    password.dispose();
    confirmation.dispose();
    if (navigateToVerification && _current(generation)) {
      _setPending(false);
      await _external(AuthFlowPurpose.reauth);
    }
  }

  Future<void> _remove({required bool identity}) async {
    final authentication = _authentication;
    if (_disabled || authentication == null) return;
    if (identity && !authentication.hasPassword) {
      setState(() => _error = 'Cannot remove the last sign-in method. Set a local password before unlinking SSO.');
      return;
    }
    if (!identity && !_freshOidc) { await _offerVerification(); return; }
    final generation = widget.session.generation;
    if (!await confirmAction(context, identity ? 'Unlink SSO identity?' : 'Remove password?',
      identity ? 'Your local password will remain. All sessions and pending sign-in grants will be revoked; ledger history is preserved.'
        : 'You will need your linked SSO identity to sign in. All application sessions and pending sign-in grants will be revoked.',
      action: identity ? 'Unlink identity' : 'Remove password') || !_current(generation)) return;
    _setPending(true);
    if (!await widget.canLeave() || !_current(generation)) { if (mounted) _setPending(false); return; }
    if (identity) {
      final password = await _verifyPassword(action: widget.session.repository.unlinkIdentity);
      if (password != null && _current(generation)) widget.session.clearAfterCredentialChange();
      if (mounted) _setPending(false);
      return;
    }
    bool verify = false;
    try {
      await widget.session.repository.removePassword();
      if (_current(generation)) widget.session.clearAfterCredentialChange();
    } catch (failure) {
      if (_current(generation)) {
        setState(() => _error = errorMessage(failure));
        verify = failure is ApiException && failure.statusCode == 403;
      }
    } finally {
      if (mounted) _setPending(false);
    }
    if (verify && _current(generation)) await _offerVerification();
  }

  Future<void> _logout({bool provider = false}) async {
    if (_disabled) return;
    final generation = widget.session.generation;
    if (!await widget.canLeave() || !_current(generation)) return;
    if (provider && !await confirmAction(context, 'Sign out of provider too?',
      'Your application session is revoked immediately. Whether other applications are signed out depends on the provider’s configured logout flow.', action: 'Sign out')) return;
    if (!_current(generation)) return;
    await widget.session.logout(provider: provider);
  }

  @override
  void dispose() {
    widget.authFlow?.removeListener(_flowChanged);
    _request++;
    super.dispose();
  }

  @override
  Widget build(BuildContext context) {
    final authentication = _authentication;
    final identity = authentication?.identity;
    final keyPrefix = 'account:${widget.session.generation}:${widget.session.user?.userId}';
    final destructiveStyle = TextButton.styleFrom(
      foregroundColor: Theme.of(context).colorScheme.error,
    );
    return Column(crossAxisAlignment: CrossAxisAlignment.stretch, spacing: 12, children: [
      Card(child: Padding(padding: const EdgeInsets.all(16), child: Column(
        crossAxisAlignment: CrossAxisAlignment.stretch, spacing: 12, children: [
          const SectionHeading(title: 'Sign-in methods'),
          if (_loading) const LinearProgressIndicator(),
          if (_error != null) ErrorPanel(error: _error!),
          if (!_loading && (authentication == null || _error != null))
            Align(alignment: Alignment.centerLeft, child: TextButton(onPressed: _disabled ? null : _load, child: const Text('Retry sign-in methods'))),
          if (authentication != null) ...[
            ExpansionTile(
              key: PageStorageKey('$keyPrefix:password'),
              maintainState: true,
              tilePadding: const EdgeInsets.symmetric(horizontal: 12),
              childrenPadding: const EdgeInsets.all(12),
              title: const Text('Password'),
              subtitle: Text(authentication.hasPassword ? 'Configured' : 'Not set'),
              children: [
                Column(crossAxisAlignment: CrossAxisAlignment.stretch, spacing: 8, children: [
                  Wrap(spacing: 8, runSpacing: 8, children: [
                    OutlinedButton(onPressed: _disabled ? null : _password, child: Text(authentication.hasPassword ? 'Change password' : 'Set password')),
                    if (authentication.hasPassword)
                      TextButton(onPressed: _disabled ? null : () => _verifyOnly(), child: const Text('Verify current password')),
                  ]),
                  if (authentication.hasPassword && identity != null && authentication.providerEnabled) ...[
                    const Divider(),
                    Align(alignment: Alignment.centerLeft, child: TextButton(
                      style: destructiveStyle,
                      onPressed: _disabled ? null : () => _remove(identity: false),
                      child: const Text('Remove password'))),
                  ],
                ]),
              ],
            ),
            ExpansionTile(
              key: PageStorageKey('$keyPrefix:provider'),
              maintainState: true,
              tilePadding: const EdgeInsets.symmetric(horizontal: 12),
              childrenPadding: const EdgeInsets.all(12),
              title: Text(identity?.providerName ?? 'SSO provider'),
              subtitle: Column(crossAxisAlignment: CrossAxisAlignment.start, children: [
                Text(identity != null ? 'Linked' : 'Not linked'),
                if (!_browserSupported) const Text('Provider sign-in and verification are available in the web app.'),
                if (!authentication.providerEnabled) const Text('Provider sign-in is currently unavailable.'),
              ]),
              children: [
                Column(crossAxisAlignment: CrossAxisAlignment.stretch, spacing: 8, children: [
                  if (identity != null) ...[
                    if (authentication.providerEnabled && _browserSupported)
                      Align(alignment: Alignment.centerLeft, child: OutlinedButton(onPressed: _disabled ? null : () => _external(AuthFlowPurpose.reauth), child: const Text('Verify with provider'))),
                    ExpansionTile(
                      key: PageStorageKey('$keyPrefix:identity-details'),
                      maintainState: true,
                      tilePadding: const EdgeInsets.symmetric(horizontal: 12),
                      childrenPadding: const EdgeInsets.all(12),
                      title: const Text('Identity details'),
                      children: [
                        Column(crossAxisAlignment: CrossAxisAlignment.stretch, spacing: 8, children: [
                          SelectableText('Issuer: ${identity.issuer}'),
                          SelectableText('Subject: ${identity.subject}'),
                          if (identity.claimedEmail != null) SelectableText('Provider email: ${identity.claimedEmail} · ${identity.claimedEmailVerified ? 'Verified' : 'Unverified'}'),
                          SelectableText('Linked: ${identity.linkedAt.toLocal()}'),
                          if (identity.lastLoginAt != null) SelectableText('Last SSO sign-in: ${identity.lastLoginAt!.toLocal()}'),
                        ]),
                      ],
                    ),
                    const Divider(),
                    Align(alignment: Alignment.centerLeft, child: TextButton(
                      style: destructiveStyle,
                      onPressed: _disabled ? null : () => _remove(identity: true),
                      child: const Text('Unlink SSO identity'))),
                  ] else if (authentication.providerEnabled && _browserSupported)
                    Align(alignment: Alignment.centerLeft, child: OutlinedButton(onPressed: _disabled ? null : () => _link(), child: const Text('Link SSO identity'))),
                ]),
              ],
            ),
          ],
        ],
      ))),
      Card(child: Padding(padding: const EdgeInsets.all(16), child: Column(
        crossAxisAlignment: CrossAxisAlignment.stretch, spacing: 12, children: [
          const SectionHeading(title: 'Session'),
          if (authentication != null)
            Text('Current session: ${authentication.sessionMethod == SessionMethod.oidc ? 'SSO' : 'Password'}'),
          Wrap(spacing: 8, runSpacing: 8, children: [
            OutlinedButton(onPressed: _disabled ? null : _logout, child: const Text('Sign out')),
            if (authentication != null && authentication.canSignOutProvider && _browserSupported)
              TextButton(onPressed: _disabled ? null : () => _logout(provider: true), child: Text('Sign out of ${identity?.providerName ?? 'provider'} too')),
          ]),
        ],
      ))),
    ]);
  }

  Future<void> _verifyOnly() async {
    if (_disabled) return;
    _setPending(true);
    final generation = widget.session.generation;
    final password = await _verifyPassword();
    if (mounted) _setPending(false);
    if (password != null && _current(generation)) {
      showSuccess(context, 'Password verified');
      await _load();
    }
  }

  Future<void> _link() async {
    final authentication = _authentication;
    if (_disabled || authentication == null) return;
    // Link start requires recent generic proof, unlike OIDC-specific removals.
    if (DateTime.now().toUtc().difference(authentication.authenticatedAt) >= const Duration(minutes: 10)) {
      if (!authentication.hasPassword) {
        setState(() => _error = 'Reauthentication required. Sign in again before linking an identity.');
        return;
      }
      _setPending(true);
      final generation = widget.session.generation;
      final password = await _verifyPassword();
      if (mounted) _setPending(false);
      if (password == null || !_current(generation)) return;
    }
    await _external(AuthFlowPurpose.link);
    if (mounted && widget.authFlow?.error == 'Reauthentication required') {
      setState(() => _error = 'Reauthentication required. Verify your current password, then try linking again.');
      if (authentication.hasPassword && await confirmAction(context, 'Reauthentication required',
        'Verify your current password before linking the identity.', action: 'Verify password') && mounted) {
        await _verifyOnly();
      }
    }
  }
}
