import 'package:flutter/material.dart';
import 'package:go_router/go_router.dart';

import '../../core/api_client.dart';
import '../../core/jizhang_repository.dart';
import '../../core/models.dart';
import '../../core/session_controller.dart';
import '../../shared/entity_picker.dart';
import '../../shared/record_navigation.dart';
import '../../shared/widgets.dart';
import 'group_currency_settings.dart';

class GroupScope extends InheritedWidget {
  const GroupScope({super.key, required this.group, required this.updateGroup,
    required this.refreshGroup, required this.recordChanges, required super.child});
  final ExpenseGroup group;
  final void Function(ExpenseGroup) updateGroup;
  final Future<void> Function() refreshGroup;
  final RecordChangeTracker recordChanges;

  static GroupScope of(BuildContext context) {
    final scope = context.dependOnInheritedWidgetOfExactType<GroupScope>();
    assert(scope != null, 'GroupScope requires a GroupWorkspace ancestor');
    return scope!;
  }

  @override
  bool updateShouldNotify(GroupScope oldWidget) => !identical(group, oldWidget.group);
}

class GroupWorkspace extends StatefulWidget {
  const GroupWorkspace({super.key, required this.repository, required this.session,
    required this.groupId, required this.location, required this.child});
  final JizhangRepository repository;
  final SessionController session;
  final String groupId;
  final Uri location;
  final Widget child;

  @override
  State<GroupWorkspace> createState() => _GroupWorkspaceState();
}

class _GroupWorkspaceState extends State<GroupWorkspace> {
  final ScrollController _navigationScroll = ScrollController();
  final RecordChangeTracker _recordChanges = RecordChangeTracker();
  ExpenseGroup? _group;
  Object? _error;
  bool _loading = true;
  int _request = 0;
  late int _generation;

  @override
  void initState() {
    super.initState();
    _generation = widget.session.generation;
    widget.session.addListener(_sessionChanged);
    _refreshGroup();
  }

  void _sessionChanged() {
    if (widget.session.generation == _generation) return;
    _request++;
    setState(() { _group = null; _error = null; _loading = false; });
  }

  bool _current(int request) => mounted && request == _request &&
      widget.session.generation == _generation && widget.session.isAuthenticated;

  Future<void> _refreshGroup() async {
    final request = ++_request;
    setState(() { _loading = true; _error = null; });
    try {
      final group = await widget.repository.getGroup(widget.groupId);
      if (!_current(request)) return;
      setState(() { _group = group; _loading = false; });
    } catch (error) {
      if (!_current(request)) return;
      final accessError = error is ApiException &&
          (error.statusCode == 401 || error.statusCode == 403 || error.statusCode == 404);
      setState(() {
        if (accessError) _group = null;
        _error = error;
        _loading = false;
      });
      if (error is ApiException && error.statusCode == 403) {
        // Keep the original access failure even when the list refresh also fails.
        try { await widget.repository.listGroups(const PageQuery()); } catch (_) {}
      }
    }
  }

  void _updateGroup(ExpenseGroup group) {
    if (!mounted || widget.session.generation != _generation ||
        !widget.session.isAuthenticated || group.groupId != widget.groupId) return;
    _request++;
    setState(() { _group = group; _error = null; _loading = false; });
  }

  @override
  void dispose() {
    _request++;
    widget.session.removeListener(_sessionChanged);
    _navigationScroll.dispose();
    super.dispose();
  }

  String get _section {
    final segments = widget.location.pathSegments;
    final section = segments.length > 2 ? segments[2] : 'records';
    return switch (section) {
      'receipts' => 'receipts',
      'balances' => 'balances',
      'statistics' => 'statistics',
      'members' => 'members',
      'import' => 'import',
      _ => 'records',
    };
  }

  Future<void> _switchGroup() async {
    if (FormNavigation.active?.pending == true) return;
    final generation = widget.session.generation;
    final selected = await showEntityPicker<ExpenseGroup>(context,
      session: widget.session, title: 'Switch group', searchLabel: 'Search groups',
      idOf: (group) => group.groupId, titleOf: (group) => group.groupName,
      subtitleOf: (group) => '${userLabel(group.owner)} · ${group.members.length} members · ${group.groupId.substring(0, 8)}',
      leading: (_) => const CircleAvatar(child: Icon(Icons.group_outlined)),
      selectedId: widget.groupId, loadPage: widget.repository.listGroups, sort: 'groupName',
    );
    if (!mounted || generation != widget.session.generation || selected == null ||
        selected.groupId == widget.groupId || FormNavigation.active?.pending == true) return;
    final preserved = _section == 'statistics' ? <String, String>{
      for (final key in ['from', 'to', 'bucket', 'view', 'memberMeasure'])
        if (widget.location.queryParameters[key] != null) key: widget.location.queryParameters[key]!,
    } : null;
    context.go(Uri(path: '/groups/${selected.groupId}/$_section', queryParameters: preserved).toString());
  }

  Widget _navigationButton(String section, String label, IconData icon) {
    final selected = _section == section;
    return Semantics(
      selected: selected,
      child: TextButton.icon(
        style: TextButton.styleFrom(
          minimumSize: const Size(48, 48),
          padding: const EdgeInsets.symmetric(horizontal: 12, vertical: 12),
          shape: RoundedRectangleBorder(borderRadius: BorderRadius.circular(8)),
          backgroundColor: selected ? Theme.of(context).colorScheme.secondaryContainer : null,
        ),
        onPressed: () {
          final target = '/groups/${widget.groupId}/$section';
          if (widget.location.path != target) context.go(target);
        },
        icon: Icon(icon, size: 20),
        label: Text(label),
      ),
    );
  }

  @override
  Widget build(BuildContext context) {
    final group = _group;
    if (group == null) {
      if (_loading) return const Center(child: CircularProgressIndicator());
      if (_error == null) return const SizedBox.shrink();
      return PageBody(
        storageKey: 'group-error:$_generation:${widget.groupId}',
        child: ErrorPanel(error: _error!, onRetry: _refreshGroup, backToGroups: true),
      );
    }
    return GroupScope(
      group: group,
      updateGroup: _updateGroup,
      refreshGroup: _refreshGroup,
      recordChanges: _recordChanges,
      child: Column(crossAxisAlignment: CrossAxisAlignment.stretch, children: [
        Align(
          alignment: Alignment.topCenter,
          child: ConstrainedBox(
            constraints: BoxConstraints(maxWidth: MediaQuery.sizeOf(context).width >= 1100 ? 1200 : 960),
            child: Padding(
              padding: const EdgeInsets.symmetric(horizontal: 16, vertical: 12),
              child: Column(crossAxisAlignment: CrossAxisAlignment.stretch, children: [
                Row(crossAxisAlignment: CrossAxisAlignment.start, children: [
                  Expanded(child: Column(crossAxisAlignment: CrossAxisAlignment.start, children: [
                    Text(group.groupName, style: Theme.of(context).textTheme.headlineSmall),
                    Text('Owner: ${userLabel(group.owner)} · ${group.members.length} active members'),
                    Text(groupCurrencyLabel(group)),
                  ])),
                  IconButton(
                    tooltip: 'Switch group', onPressed: _switchGroup,
                    icon: const Icon(Icons.swap_horiz),
                  ),
                  IconButton(
                    tooltip: 'Refresh group',
                    onPressed: _loading ? null : _refreshGroup,
                    icon: const Icon(Icons.refresh),
                  ),
                ]),
                if (_error != null) ...[
                  const Text('Showing previously loaded group details.'),
                  ErrorPanel(error: _error!, onRetry: _refreshGroup),
                ],
                const SizedBox(height: 8),
                Scrollbar(
                  controller: _navigationScroll,
                  thumbVisibility: true,
                  trackVisibility: true,
                  notificationPredicate: (notification) => notification.depth == 0,
                  child: SingleChildScrollView(
                    controller: _navigationScroll,
                    scrollDirection: Axis.horizontal,
                    padding: const EdgeInsets.only(bottom: 12),
                    child: IntrinsicHeight(child: Row(crossAxisAlignment: CrossAxisAlignment.stretch, children: [
                      _navigationButton('records', 'Records', Icons.receipt_long_outlined),
                      _navigationButton('receipts', 'Receipts', Icons.folder_outlined),
                      _navigationButton('balances', 'Balances', Icons.balance_outlined),
                      _navigationButton('statistics', 'Statistics', Icons.bar_chart_outlined),
                      _navigationButton('members', 'Members', Icons.people_outline),
                      const VerticalDivider(width: 16, indent: 8, endIndent: 8),
                      _navigationButton('import', 'Import CSV', Icons.upload_file),
                    ])),
                  ),
                ),
              ]),
            ),
          ),
        ),
        if (_loading) const LinearProgressIndicator(minHeight: 2),
        Expanded(child: Semantics(container: true, child: widget.child)),
      ]),
    );
  }
}
