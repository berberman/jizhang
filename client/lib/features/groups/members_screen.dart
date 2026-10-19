import 'package:flutter/material.dart';
import 'package:go_router/go_router.dart';

import '../../core/api_client.dart';
import '../../core/jizhang_repository.dart';
import '../../core/models.dart';
import '../../core/session_controller.dart';
import '../../shared/form_controller.dart';
import '../../shared/entity_picker.dart';
import '../../shared/widgets.dart';
import 'group_screen.dart';
import 'group_currency_settings.dart';

class MembersScreen extends StatefulWidget {
  const MembersScreen({super.key, required this.repository, required this.session, required this.groupId});
  final JizhangRepository repository;
  final SessionController session;
  final String groupId;
  @override
  State<MembersScreen> createState() => _MembersScreenState();
}

class _MembersScreenState extends State<MembersScreen> {
  late final _draft = FormController(widget.session);
  final _name = TextEditingController();
  final _renameForm = GlobalKey<FormState>();
  bool _initialized = false;
  String? _newOwner;
  User? _candidate;
  CurrencyMode _currencyMode = CurrencyMode.single;
  String _currencyCode = 'USD';
  @override
  void dispose() { _draft.dispose(); _name.dispose(); super.dispose(); }

  Future<void> _mutate(Future<ExpenseGroup> Function() action, String message) async {
    final scope = GroupScope.of(context);
    ExpenseGroup? updated;
    if (await _draft.save(() async {
      try {
        updated = await action();
      } on ApiException catch (error) {
        if (error.statusCode == 403 || error.statusCode == 409) await scope.refreshGroup();
        rethrow;
      }
    }) && mounted) {
      scope.updateGroup(updated!);
      showSuccess(context, message);
    }
  }

  Future<void> _chooseMember(ExpenseGroup group) async {
    final generation = widget.session.generation;
    final selected = await showEntityPicker<User>(context,
      session: widget.session, title: 'Add member', searchLabel: 'Search users',
      idOf: (user) => user.userId, titleOf: (user) => '@${user.username}',
      subtitleOf: (user) => user.displayName,
      leading: (_) => const CircleAvatar(child: Icon(Icons.person_outline)),
      selectedId: _candidate?.userId,
      loadPage: (query) => widget.repository.listMemberCandidates(group.groupId, query),
    );
    if (!mounted || generation != widget.session.generation || selected == null ||
        _draft.pending || selected.userId == _candidate?.userId) return;
    setState(() => _candidate = selected);
    _draft.markDirty();
  }

  Future<void> _addMember(ExpenseGroup group) async {
    final generation = widget.session.generation;
    final selected = _candidate;
    if (selected == null) return;
    await _mutate(() => widget.repository.addMember(group.groupId, selected.userId),
      'Member added or reactivated');
    if (mounted && generation == widget.session.generation && _draft.error == null) {
      setState(() => _candidate = null);
    }
  }

  Future<void> _remove(User member, ExpenseGroup group) async {
    final scope = GroupScope.of(context);
    final leaving = member.userId == widget.session.user?.userId;
    if (!await confirmAction(context, leaving ? 'Leave group?' : 'Remove ${userLabel(member)}?',
      '${leaving ? 'Your' : '${userLabel(member)}’s'} group access will end. Historical expenses and balances are preserved.', action: leaving ? 'Leave' : 'Remove')) { return; }
    final saved = await _draft.save(() async {
      await widget.repository.removeMember(group.groupId, member.userId);
      if (!leaving && mounted) await scope.refreshGroup();
    });
    if (!saved || !mounted) return;
    if (leaving) { context.go('/groups'); }
    else { setState(() {}); showSuccess(context, 'Member removed; history preserved'); }
  }

  Future<void> _delete(ExpenseGroup group) async {
    if (!await confirmAction(context, 'Delete ${group.groupName}?',
      'This permanently deletes this group and its expenses, receipts, and memberships.')) { return; }
    if (await _draft.save(() => widget.repository.deleteGroup(group.groupId)) && mounted) context.go('/groups');
  }

  @override
  Widget build(BuildContext context) {
    final group = GroupScope.of(context).group;
    if (_newOwner != null && !group.members.any((member) => member.userId == _newOwner && member.userId != group.owner.userId)) {
      _newOwner = null;
    }
    if (!_initialized) {
      _initialized = true; _name.text = group.groupName;
      _currencyMode = group.currencyMode; _currencyCode = group.defaultCurrency.code;
    }
    final owner = group.owner.userId == widget.session.user?.userId;
    return FormLeaveGuard(controller: _draft,
      child: ListenableBuilder(listenable: _draft, builder: (context, _) => Column(
        crossAxisAlignment: CrossAxisAlignment.stretch, children: [
          Text('Members & settings', style: Theme.of(context).textTheme.headlineSmall),
          const SizedBox(height: 16),
          if (_draft.error != null) ErrorPanel(error: _draft.error!),
          for (final member in group.members) Card(child: Padding(
            padding: const EdgeInsets.all(12),
            child: LayoutBuilder(builder: (context, size) => Row(children: [
              Expanded(child: Column(crossAxisAlignment: CrossAxisAlignment.start, children: [
                IdentityLabel(user: member),
                const SizedBox(height: 4),
                Badge(backgroundColor: Theme.of(context).colorScheme.surfaceContainerHighest,
                  textColor: Theme.of(context).colorScheme.onSurface,
                  label: Text(member.userId == group.owner.userId ? 'Owner' : 'Member')),
              ])),
              if (member.userId != group.owner.userId && (owner || member.userId == widget.session.user?.userId))
                ConstrainedBox(constraints: BoxConstraints(maxWidth: size.maxWidth * .4),
                  child: TextButton(onPressed: _draft.pending ? null : () => _remove(member, group),
                    child: Text(member.userId == widget.session.user?.userId ? 'Leave group' : 'Remove member'))),
            ])),
          )),
          if (owner) ...[
            const SizedBox(height: 16),
            const SectionHeading(title: 'Add member'),
            const SizedBox(height: 8),
            InputDecorator(
              decoration: const InputDecoration(labelText: 'New member'),
              child: TextButton.icon(
                onPressed: _draft.pending ? null : () => _chooseMember(group),
                icon: const Icon(Icons.person_add_outlined),
                label: Text(_candidate == null ? 'Choose a user' : userLabel(_candidate!)),
              ),
            ),
            const SizedBox(height: 8),
            Align(alignment: Alignment.centerLeft, child: FilledButton(
              onPressed: _draft.pending || _candidate == null ||
                  group.members.any((user) => user.userId == _candidate!.userId)
                ? null : () => _addMember(group),
              child: const Text('Add member'),
            )),
            const SizedBox(height: 16),
            const Divider(),
            const SectionHeading(title: 'Group settings'),
            const SizedBox(height: 12),
            Form(key: _renameForm, child: Column(crossAxisAlignment: CrossAxisAlignment.start, children: [
              TextFormField(controller: _name, enabled: !_draft.pending, decoration: const InputDecoration(labelText: 'Group name'),
                onChanged: (_) => _draft.markDirty(), validator: (value) => value == null || value.trim().isEmpty || value.length > 100 ? 'Use a group name of 1–100 characters' : null),
              const SizedBox(height: 12),
              GroupCurrencySettings(repository: widget.repository, session: widget.session,
                mode: group.currencyLocked ? group.currencyMode : _currencyMode,
                currencyCode: group.currencyLocked ? group.defaultCurrency.code : _currencyCode,
                locked: group.currencyLocked, enabled: !_draft.pending,
                onModeChanged: (value) { setState(() => _currencyMode = value); _draft.markDirty(); },
                onCurrencyChanged: (value) { setState(() => _currencyCode = value); _draft.markDirty(); }),
              const SizedBox(height: 8),
              OutlinedButton(onPressed: _draft.pending ? null : () {
                if (_renameForm.currentState!.validate()) _mutate(() => widget.repository.updateGroupSettings(
                  group.groupId, name: _name.text,
                  currencyMode: group.currencyLocked ? group.currencyMode : _currencyMode,
                  defaultCurrency: group.currencyLocked ? group.defaultCurrency.code : _currencyCode), 'Group settings saved');
              }, child: const Text('Save settings')),
            ])),
            const SizedBox(height: 16),
            const SectionHeading(title: 'Ownership'),
            const SizedBox(height: 8),
            UserPickerField(
              session: widget.session, label: 'New owner', selectedUserId: _newOwner,
              knownUsers: {for (final user in group.members) user.userId: user},
              choices: group.members, excludedUserId: group.owner.userId,
              enabled: !_draft.pending,
              onChanged: (value) { setState(() => _newOwner = value); _draft.markDirty(); },
            ),
            const SizedBox(height: 8),
            Align(alignment: Alignment.centerLeft, child: OutlinedButton(onPressed: _draft.pending || _newOwner == null ? null : () async {
              if (!await confirmAction(context, 'Transfer ownership?', 'Make ${userLabel(group.members.firstWhere((member) => member.userId == _newOwner))} the owner of ${group.groupName}? You will lose owner-only actions.', action: 'Transfer')) return;
              if (!mounted) return;
              await _mutate(() => widget.repository.transferOwnership(group.groupId, _newOwner!), 'Ownership transferred');
            }, child: const Text('Transfer ownership'))),
            const SizedBox(height: 16),
            const Divider(),
            const Text('Transfer ownership to another active member before leaving.'),
            const SizedBox(height: 12),
            Align(alignment: Alignment.centerLeft, child: OutlinedButton(
              style: OutlinedButton.styleFrom(foregroundColor: const Color(0xFFB42318)),
              onPressed: _draft.pending ? null : () => _delete(group), child: const Text('Delete group'))),
          ],
        ],
      )),
    );
  }
}
