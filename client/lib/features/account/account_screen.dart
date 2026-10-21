import 'package:flutter/material.dart';
import 'package:go_router/go_router.dart';

import '../../core/auth_flow_controller.dart';
import '../../core/models.dart';
import '../../core/session_controller.dart';
import '../../shared/branding.dart';
import '../../shared/form_controller.dart';
import '../../shared/widgets.dart';
import '../../shared/legal.dart';
import 'account_security_section.dart';

class AccountScreen extends StatefulWidget {
  const AccountScreen({super.key, required this.session, this.authFlow});
  final SessionController session;
  final AuthFlowController? authFlow;
  @override
  State<AccountScreen> createState() => _AccountScreenState();
}

class _AccountScreenState extends State<AccountScreen> {
  late final _draft = _AccountFormController(widget.session, () => _securityPending);
  final _form = GlobalKey<FormState>();
  final _username = TextEditingController();
  final _displayName = TextEditingController();
  UserProfile? _loaded;
  bool _loading = true;
  bool _deleting = false;
  bool _securityPending = false;
  int _request = 0;
  String? _error;

  @override
  void initState() {
    super.initState();
    _load();
  }

  bool get _busy => _loading || _deleting || _draft.pending || _securityPending;

  void _reset(UserProfile profile) {
    _loaded = profile;
    _username.text = profile.user.username;
    _displayName.text = profile.user.displayName;
    _draft.reset();
  }

  Future<bool> _discard() => _draft.canLeave(() => confirmAction(context,
    'Discard unsaved changes?', 'Your changes have not been saved.', action: 'Discard'));

  Future<void> _load() async {
    final request = ++_request;
    final generation = widget.session.generation;
    setState(() { _loading = true; _error = null; });
    await widget.session.refreshAccount();
    if (!mounted || request != _request || generation != widget.session.generation) return;
    setState(() {
      _loading = false;
      if (widget.session.status == SessionStatus.authenticated && widget.session.profile != null) {
        _reset(widget.session.profile!);
      } else {
        _error = widget.session.message ?? 'Could not load account.';
      }
    });
  }

  Future<void> _refresh() async {
    if (_busy || !await _discard() || !mounted) return;
    if (_loaded != null) setState(() => _reset(_loaded!));
    await _load();
  }

  Future<void> _save() async {
    if (_busy || !_form.currentState!.validate()) return;
    setState(() => _error = null);
    final generation = widget.session.generation;
    final saved = await _draft.save(() => widget.session.updateProfile(_username.text, _displayName.text));
    if (!mounted || !saved || generation != widget.session.generation) return;
    setState(() => _reset(widget.session.profile!));
    showSuccess(context, 'Profile saved');
  }

  Future<bool> _securityCanLeave() async {
    if (_loading || _deleting || _draft.pending) return false;
    if (!_draft.dirty) return true;
    // Do not reset the profile draft on a failed credential mutation.
    return confirmAction(context, 'Discard unsaved changes?',
      'A successful security change will sign you out and discard your profile changes.', action: 'Continue');
  }

  Future<bool> _beforeExternalNavigation() async {
    if (_loading || _deleting || _draft.pending) return false;
    final wasDirty = _draft.dirty;
    if (!await _discard() || !mounted) return false;
    final allowed = await FormNavigation.canExit(context);
    // Starting a provider flow can still fail before navigation. Keep the
    // draft guarded until the browser actually leaves this page.
    if (mounted && wasDirty) _draft.markDirty();
    return allowed;
  }

  Future<void> _delete() async {
    if (_busy || !await _discard() || !mounted) return;
    if (_loaded != null) setState(() => _reset(_loaded!));
    if (!await confirmAction(context, 'Delete account?',
      'Delete @${widget.session.user?.username}? This cannot be undone. Referenced group or expense history can prevent account deletion.') || !mounted) return;
    setState(() { _deleting = true; _error = null; });
    await widget.session.deleteAccount();
    if (mounted) setState(() { _deleting = false; _error = widget.session.message; });
  }

  @override
  void dispose() {
    _request++;
    _draft.dispose();
    _username.dispose();
    _displayName.dispose();
    super.dispose();
  }

  PageStorageKey<String> _sectionKey(String section) => PageStorageKey(
    'account:${widget.session.generation}:${widget.session.user?.userId}:$section');

  @override
  Widget build(BuildContext context) => FormLeaveGuard(controller: _draft,
    child: ListenableBuilder(listenable: _draft, builder: (context, _) => FormSurface(
      title: 'Account', child: Form(key: _form, child: Column(crossAxisAlignment: CrossAxisAlignment.stretch, children: [
        Row(children: [
          Expanded(child: Align(alignment: Alignment.centerLeft, child: TextButton.icon(
            onPressed: _busy ? null : () => context.go('/groups'),
            icon: const Icon(Icons.arrow_back), label: const Text('Back to groups')))),
          IconButton(tooltip: 'Refresh account', onPressed: _busy ? null : _refresh,
            icon: const Icon(Icons.refresh)),
        ]),
        if (_loading) const LinearProgressIndicator(),
        if (_error != null) ErrorPanel(error: _error!),
        if (_loaded case final profile?) Card(child: Padding(
          padding: const EdgeInsets.all(16),
          child: Column(crossAxisAlignment: CrossAxisAlignment.stretch, children: [
            const SectionHeading(title: 'Profile'),
            const SizedBox(height: 12),
            IdentityLabel(user: profile.user),
            const SizedBox(height: 16),
            TextFormField(controller: _username, enabled: !_busy,
              decoration: const InputDecoration(labelText: 'Username', prefixText: '@'),
              onChanged: (_) => _draft.markDirty(),
              validator: (value) => value == null || value.trim().isEmpty || value.trim().runes.length > 50 ? 'Use a username of 1–50 characters' : null),
            const SizedBox(height: 12),
            TextFormField(controller: _displayName, enabled: !_busy,
              decoration: const InputDecoration(labelText: 'Display name'),
              onChanged: (_) => _draft.markDirty(),
              validator: (value) => value == null || value.trim().isEmpty || value.trim().runes.length > 100 ? 'Use a display name of 1–100 characters' : null),
            const SizedBox(height: 12),
            InputDecorator(decoration: const InputDecoration(labelText: 'Email', helperText: 'Email is read-only.'),
              child: SelectableText('${profile.email} · ${profile.emailVerified ? 'Verified' : 'Unverified'}')),
            const SizedBox(height: 12),
            ExpansionTile(key: _sectionKey('details'), maintainState: true,
              tilePadding: const EdgeInsets.symmetric(horizontal: 12),
              childrenPadding: const EdgeInsets.all(12), title: const Text('Account details'),
              expandedCrossAxisAlignment: CrossAxisAlignment.stretch,
              children: [SelectableText('Account ID: ${profile.user.userId}')]),
            if (_draft.error != null) ErrorPanel(error: _draft.error!),
            if (_draft.dirty || _draft.pending) ...[
              const SizedBox(height: 12),
              Wrap(spacing: 8, runSpacing: 8, children: [
                FilledButton(onPressed: _busy ? null : _save, child: Text(_draft.pending ? 'Saving…' : 'Save profile')),
                TextButton(onPressed: _busy ? null : () => setState(() => _reset(_loaded!)), child: const Text('Cancel')),
              ]),
            ],
          ]),
        )),
        const SizedBox(height: 8),
        AccountSecuritySection(session: widget.session, authFlow: widget.authFlow,
          disabled: _loading || _deleting || _draft.pending,
          canLeave: _securityCanLeave, beforeExternalNavigation: _beforeExternalNavigation,
          onPendingChanged: (pending) { if (mounted) setState(() => _securityPending = pending); }),
        const SizedBox(height: 8),
        Card(child: ExpansionTile(key: _sectionKey('deletion'), maintainState: true,
          title: const Text('Delete account'),
          shape: const Border(),
          collapsedShape: const Border(),
          expandedCrossAxisAlignment: CrossAxisAlignment.stretch,
          childrenPadding: const EdgeInsets.all(16),
          children: [
            const Text('Referenced group or expense history may prevent deletion. Ledger history is preserved.'),
            const SizedBox(height: 12),
            Align(alignment: Alignment.centerLeft, child: OutlinedButton(
              style: OutlinedButton.styleFrom(foregroundColor: Theme.of(context).colorScheme.error),
              onPressed: _busy ? null : _delete, child: Text(_deleting ? 'Deleting…' : 'Delete account'))),
          ],
        )),
        const SizedBox(height: 16),
        LegalLinks(disabled: _busy || (widget.authFlow?.pending ?? false)),
        const AppVersionLabel(),
      ])),
    )));
}

class _AccountFormController extends FormController {
  _AccountFormController(super.session, this.securityPending);
  final bool Function() securityPending;

  @override
  Future<bool> canLeave(Future<bool> Function() confirm) {
    if (securityPending()) return Future.value(false);
    return super.canLeave(confirm);
  }
}
