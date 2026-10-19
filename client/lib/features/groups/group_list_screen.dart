import 'package:flutter/material.dart';
import 'package:flutter/services.dart';
import 'package:go_router/go_router.dart';

import '../../core/jizhang_repository.dart';
import '../../core/models.dart';
import '../../core/pagination_controller.dart';
import '../../core/session_controller.dart';
import '../../shared/form_controller.dart';
import '../../shared/widgets.dart';
import 'group_currency_settings.dart';

class GroupListScreen extends StatefulWidget {
  const GroupListScreen({super.key, required this.repository, required this.session, this.create = false});
  final JizhangRepository repository;
  final SessionController session;
  final bool create;
  @override
  State<GroupListScreen> createState() => _GroupListScreenState();
}

class _GroupListScreenState extends State<GroupListScreen> {
  late final _pager = PaginationController<ExpenseGroup>(load: widget.repository.listGroups, query: widget.session.groupListQuery);
  late final _draft = FormController(widget.session);
  final _name = TextEditingController();
  final _form = GlobalKey<FormState>();
  CurrencyMode _currencyMode = CurrencyMode.single;
  String _currencyCode = 'USD';
  void _rememberQuery() { widget.session.groupListQuery = _pager.query; }
  @override
  void initState() {
    super.initState();
    _pager.addListener(_rememberQuery);
    if (!widget.create) _pager.refresh(clamp: true);
  }
  @override
  void dispose() { _pager.dispose(); _draft.dispose(); _name.dispose(); super.dispose(); }
  Future<void> _create() async {
    if (!_form.currentState!.validate()) return;
    ExpenseGroup? group;
    final saved = await _draft.save(() async { group = await widget.repository.createGroup(_name.text,
      currencyMode: _currencyMode, defaultCurrency: _currencyCode); });
    if (saved && mounted && group != null) {
      _draft.reset(); context.go('/groups/${group!.groupId}/records');
    }
  }
  @override
  Widget build(BuildContext context) {
    if (widget.create) {
      return CallbackShortcuts(bindings: {
        const SingleActivator(LogicalKeyboardKey.escape): () { if (!_draft.pending) context.go('/groups'); },
      }, child: FormLeaveGuard(controller: _draft, child: ListenableBuilder(listenable: _draft,
      builder: (context, _) => FormSurface(title: 'Create group', child: Form(key: _form, child: Column(
        crossAxisAlignment: CrossAxisAlignment.stretch, children: [
          TextFormField(controller: _name, enabled: !_draft.pending, decoration: const InputDecoration(labelText: 'Group name'),
            onChanged: (_) => _draft.markDirty(),
            validator: (value) => value == null || value.trim().isEmpty || value.length > 100 ? 'Use a group name of 1–100 characters' : null,
            onFieldSubmitted: (_) => _create()),
          const SizedBox(height: 12),
          GroupCurrencySettings(repository: widget.repository, session: widget.session,
            mode: _currencyMode, currencyCode: _currencyCode, enabled: !_draft.pending,
            onModeChanged: (value) { setState(() => _currencyMode = value); _draft.markDirty(); },
            onCurrencyChanged: (value) { setState(() => _currencyCode = value); _draft.markDirty(); }),
          const SizedBox(height: 16), if (_draft.error != null) ErrorPanel(error: _draft.error!),
          Wrap(spacing: 16, children: [
            FilledButton(onPressed: _draft.pending ? null : _create, child: Text(_draft.pending ? 'Creating…' : 'Create group')),
            TextButton(onPressed: _draft.pending ? null : () => context.go('/groups'), child: const Text('Cancel')),
          ]),
        ],
      ))),
    )));
    }
    return ListenableBuilder(listenable: _pager, builder: (context, _) => Column(
      crossAxisAlignment: CrossAxisAlignment.stretch, children: [
        Wrap(alignment: WrapAlignment.spaceBetween, spacing: 16, runSpacing: 8,
          crossAxisAlignment: WrapCrossAlignment.center, children: [
          Text('Groups', style: Theme.of(context).textTheme.headlineSmall),
          FilledButton.icon(onPressed: () => context.go('/groups/new'), icon: const Icon(Icons.add), label: const Text('Create group')),
        ]),
        ListToolbar(query: _pager.query.query, sort: _pager.query.sort, label: 'groups', onSearch: _pager.search, onSort: _pager.setSort),
        if (_pager.loading) const LinearProgressIndicator(),
        if (_pager.error != null) ErrorPanel(error: _pager.error!, onRetry: () => _pager.refresh()),
        if (_pager.error != null && _pager.page != null) const Text('Showing previously loaded groups.'),
        if (!_pager.loading && _pager.error == null && _pager.page?.items.isEmpty == true)
          EmptyState(
            title: _pager.query.query.trim().isEmpty ? 'No groups yet' : 'No matching groups',
            message: _pager.query.query.trim().isEmpty
              ? 'Create a group to start sharing expenses.'
              : 'Try another group name or clear your search.',
          ),
        for (final group in _pager.page?.items ?? <ExpenseGroup>[])
          Card(clipBehavior: Clip.antiAlias, child: InkWell(
            onTap: () => context.go('/groups/${group.groupId}/records'),
            child: Padding(padding: const EdgeInsets.all(12), child: Row(
              crossAxisAlignment: CrossAxisAlignment.start, children: [
                const Padding(padding: EdgeInsets.only(top: 4), child: Icon(Icons.group_outlined)),
                const SizedBox(width: 12),
                Expanded(child: Column(crossAxisAlignment: CrossAxisAlignment.start, children: [
                  Text(group.groupName, style: Theme.of(context).textTheme.titleMedium),
                  Text(groupCurrencyLabel(group)),
                  const SizedBox(height: 8),
                  IdentityLabel(user: group.owner),
                  const SizedBox(height: 8),
                  Wrap(spacing: 8, runSpacing: 8, crossAxisAlignment: WrapCrossAlignment.center, children: [
                    Text('${group.members.length} active members · ID ${group.groupId.substring(0, 8)}'),
                    DecoratedBox(decoration: BoxDecoration(
                      color: Theme.of(context).colorScheme.surfaceContainerHighest,
                      borderRadius: BorderRadius.circular(8)),
                      child: Padding(padding: const EdgeInsets.symmetric(horizontal: 8, vertical: 4),
                        child: Text(group.owner.userId == widget.session.user?.userId ? 'Owner' : 'Member'))),
                  ]),
                ])),
                const SizedBox(width: 8),
                const Icon(Icons.chevron_right),
              ],
            )),
          )),
        const SizedBox(height: 16), PageControls(controller: _pager),
      ],
    ));
  }
}
