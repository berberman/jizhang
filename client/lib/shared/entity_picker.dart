import 'dart:async';
import 'dart:math' as math;

import 'package:flutter/material.dart';

import '../core/models.dart' as model;
import '../core/pagination_controller.dart';
import '../core/session_controller.dart';
import 'widgets.dart';

Future<T?> showEntityPicker<T>(
  BuildContext context, {
  required SessionController session,
  required String title,
  required String searchLabel,
  required String Function(T) idOf,
  required String Function(T) titleOf,
  required String Function(T) subtitleOf,
  required Widget Function(T) leading,
  String? selectedId,
  List<T>? items,
  Future<model.Page<T>> Function(model.PageQuery)? loadPage,
  String sort = 'username',
  bool Function(T)? isEnabled,
  String Function(T)? disabledReason,
}) {
  if ((items == null) == (loadPage == null)) {
    throw ArgumentError('Provide exactly one of items or loadPage.');
  }
  return showDialog<T>(
    context: context,
    builder: (context) => _EntityPicker<T>(
      session: session,
      title: title,
      searchLabel: searchLabel,
      idOf: idOf,
      titleOf: titleOf,
      subtitleOf: subtitleOf,
      leading: leading,
      selectedId: selectedId,
      items: items,
      loadPage: loadPage,
      sort: sort,
      isEnabled: isEnabled,
      disabledReason: disabledReason,
    ),
  );
}

class _EntityPicker<T> extends StatefulWidget {
  const _EntityPicker({
    required this.session,
    required this.title,
    required this.searchLabel,
    required this.idOf,
    required this.titleOf,
    required this.subtitleOf,
    required this.leading,
    required this.selectedId,
    required this.items,
    required this.loadPage,
    required this.sort,
    required this.isEnabled,
    required this.disabledReason,
  });

  final SessionController session;
  final String title, searchLabel, sort;
  final String Function(T) idOf, titleOf, subtitleOf;
  final Widget Function(T) leading;
  final String? selectedId;
  final List<T>? items;
  final Future<model.Page<T>> Function(model.PageQuery)? loadPage;
  final bool Function(T)? isEnabled;
  final String Function(T)? disabledReason;

  @override
  State<_EntityPicker<T>> createState() => _EntityPickerState<T>();
}

class _EntityPickerState<T> extends State<_EntityPicker<T>> {
  final _search = TextEditingController();
  final _searchFocus = FocusNode();
  PaginationController<T>? _pages;
  Timer? _debounce;
  late final int _sessionGeneration;
  String _localQuery = '';
  String? _selectedLabel;
  bool _pendingSearch = false;
  bool _closing = false;

  bool get _current => mounted && !_closing &&
      widget.session.generation == _sessionGeneration;
  bool get _busy => _pendingSearch || (_pages?.loading ?? false);
  bool get _canSelect => _current && !_busy && _pages?.error == null;
  String get _entities => widget.sort.contains('groupName') ? 'groups' : 'users';

  @override
  void initState() {
    super.initState();
    _sessionGeneration = widget.session.generation;
    widget.session.addListener(_sessionChanged);
    _rememberSelection(widget.items ?? <T>[]);
    if (widget.loadPage != null) {
      _pages = PaginationController<T>(
        load: widget.loadPage!,
        query: model.PageQuery(sort: widget.sort),
      )..addListener(_pageChanged);
      _pages!.refresh();
    }
  }

  void _rememberSelection(List<T> items) {
    for (final item in items) {
      if (widget.idOf(item) == widget.selectedId) {
        _selectedLabel = '${widget.titleOf(item)} · ${widget.subtitleOf(item)}';
        break;
      }
    }
  }

  void _pageChanged() {
    if (!_current) return;
    _rememberSelection(_pages?.page?.items ?? <T>[]);
    setState(() {});
  }

  void _sessionChanged() {
    if (widget.session.generation == _sessionGeneration || _closing) return;
    _closing = true;
    _debounce?.cancel();
    _pages?.clear();
    scheduleMicrotask(() {
      if (!mounted) return;
      final route = ModalRoute.of(context);
      if (route == null) return;
      final navigator = Navigator.of(context);
      if (route.isCurrent) {
        navigator.pop<T>();
      } else {
        navigator.removeRoute(route);
      }
    });
  }

  void _searchChanged(String value) {
    if (!_current) return;
    _debounce?.cancel();
    // clear invalidates any in-flight page before the debounce starts.
    // Selection metadata lives separately, never in the result page.
    _pendingSearch = true;
    _pages?.clear();
    setState(() {});
    _debounce = Timer(const Duration(milliseconds: 250), _runSearch);
  }

  void _runSearch() {
    if (!_current) return;
    _debounce?.cancel();
    setState(() {
      _pendingSearch = false;
      _localQuery = _search.text.trim().toLowerCase();
    });
    _pages?.search(_search.text);
  }

  List<T> get _rows {
    if (_pages != null) return _pages!.page?.items ?? <T>[];
    final handleOnly = _localQuery.startsWith('@');
    final query = handleOnly ? _localQuery.substring(1) : _localQuery;
    return widget.items!.where((item) {
      final title = widget.titleOf(item).toLowerCase();
      return title.contains(query) ||
          (!handleOnly && widget.subtitleOf(item).toLowerCase().contains(query));
    }).toList();
  }

  void _choose(T item) {
    if (!_canSelect || !(widget.isEnabled?.call(item) ?? true)) return;
    _closing = true;
    Navigator.of(context).pop<T>(item);
  }

  @override
  void dispose() {
    _debounce?.cancel();
    widget.session.removeListener(_sessionChanged);
    _pages?.removeListener(_pageChanged);
    _pages?.dispose();
    _search.dispose();
    _searchFocus.dispose();
    super.dispose();
  }

  @override
  Widget build(BuildContext context) {
    final rows = _rows;
    final error = _pages?.error;
    final count = _pages?.page?.pageInfo.total ?? rows.length;
    final selectedId = widget.selectedId;
    final body = Column(
      crossAxisAlignment: CrossAxisAlignment.stretch,
      children: [
        Padding(
          padding: const EdgeInsets.fromLTRB(16, 8, 8, 8),
          child: Row(children: [
            Expanded(child: Text(widget.title,
                style: Theme.of(context).textTheme.titleLarge)),
            IconButton(tooltip: 'Close',
                onPressed: () => Navigator.of(context).pop<T>(),
                icon: const Icon(Icons.close)),
          ]),
        ),
        Padding(
          padding: const EdgeInsets.symmetric(horizontal: 16),
          child: SearchBar(
            controller: _search,
            focusNode: _searchFocus,
            autoFocus: true,
            hintText: widget.searchLabel,
            leading: const Icon(Icons.search),
            onChanged: _searchChanged,
            onSubmitted: (_) => _runSearch(),
            trailing: [IconButton(
              tooltip: 'Clear search',
              onPressed: () {
                _search.clear();
                _searchChanged('');
                _runSearch();
                _searchFocus.requestFocus();
              },
              icon: const Icon(Icons.clear),
            )],
          ),
        ),
        Padding(
          padding: const EdgeInsets.fromLTRB(16, 8, 16, 8),
          child: Column(crossAxisAlignment: CrossAxisAlignment.start, children: [
            if (selectedId != null)
              Text('Selected: ${_selectedLabel ?? 'ID ${_shortId(selectedId)}'}'),
            Semantics(liveRegion: true, child: Text(
              _busy ? 'Loading…' : error != null ? 'Could not load $_entities'
                  : '$count ${count == 1 ? _entities.substring(0, _entities.length - 1) : _entities}',
            )),
          ]),
        ),
        if (_busy) const LinearProgressIndicator(),
        Expanded(
          child: error != null
              ? SingleChildScrollView(child: ErrorPanel(
                  error: error, onRetry: _runSearch))
              : rows.isEmpty
                  ? Center(child: Padding(
                      padding: const EdgeInsets.all(16),
                      child: Text(_busy ? 'Loading…' : _search.text.trim().isEmpty
                          ? 'No eligible $_entities' : 'No matching $_entities'),
                    ))
                  : ListView.builder(
                      itemCount: rows.length,
                      itemBuilder: (context, index) {
                        final item = rows[index];
                        final enabled = _canSelect &&
                            (widget.isEnabled?.call(item) ?? true);
                        final reason = widget.disabledReason?.call(item);
                        return ListTile(
                          key: ValueKey(widget.idOf(item)),
                          minVerticalPadding: 12,
                          minTileHeight: 56,
                          enabled: enabled,
                          leading: widget.leading(item),
                          title: Text(widget.titleOf(item)),
                          subtitle: Text([
                            widget.subtitleOf(item),
                            if (reason != null) reason,
                          ].where((text) => text.isNotEmpty).join('\n')),
                          selected: widget.idOf(item) == selectedId,
                          trailing: widget.idOf(item) == selectedId
                              ? const Icon(Icons.check, semanticLabel: 'Selected') : null,
                          onTap: enabled ? () => _choose(item) : null,
                        );
                      },
                    ),
        ),
        if (_pages != null)
          Padding(
            padding: const EdgeInsets.all(16),
            child: PageControls<T>(controller: _pages!),
          ),
      ],
    );
    final size = MediaQuery.sizeOf(context);
    if (size.width < 600) {
      return Dialog.fullscreen(child: SafeArea(child: body));
    }
    return Dialog(
      insetPadding: const EdgeInsets.all(16),
      child: SizedBox(
        width: math.min(560.0, size.width - 32),
        height: math.min(640.0, size.height - 32),
        child: body,
      ),
    );
  }
}

String _shortId(String id) => id.length <= 8 ? id : id.substring(0, 8);

class UserPickerField extends StatefulWidget {
  const UserPickerField({
    super.key,
    required this.session,
    required this.label,
    required this.selectedUserId,
    required this.knownUsers,
    required this.choices,
    required this.onChanged,
    this.enabled = true,
    this.excludedUserId,
  });

  final SessionController session;
  final String label;
  final String? selectedUserId;
  final Map<String, model.User> knownUsers;
  final List<model.User> choices;
  final ValueChanged<String> onChanged;
  final bool enabled;
  final String? excludedUserId;

  @override
  State<UserPickerField> createState() => _UserPickerFieldState();
}

class _UserPickerFieldState extends State<UserPickerField> {
  bool _open = false;

  Future<void> _choose() async {
    if (!widget.enabled || _open) return;
    final generation = widget.session.generation;
    setState(() => _open = true);
    try {
      final selected = await showEntityPicker<model.User>(
        context,
        session: widget.session,
        title: widget.label,
        searchLabel: 'Search handle or display name',
        idOf: (user) => user.userId,
        titleOf: (user) => '@${user.username}',
        subtitleOf: (user) => user.displayName,
        leading: (_) => const Icon(Icons.person_outline),
        selectedId: widget.selectedUserId,
        items: widget.choices,
        isEnabled: (user) => user.userId != widget.excludedUserId,
        disabledReason: (user) => user.userId == widget.excludedUserId
            ? 'Not eligible for this selection' : '',
      );
      if (!mounted || generation != widget.session.generation ||
          !widget.enabled || selected == null ||
          selected.userId == widget.selectedUserId ||
          selected.userId == widget.excludedUserId ||
          !widget.choices.any((user) => user.userId == selected.userId)) return;
      widget.onChanged(selected.userId);
    } finally {
      if (mounted) setState(() => _open = false);
    }
  }

  @override
  Widget build(BuildContext context) {
    final id = widget.selectedUserId;
    model.User? selected = id == null ? null : widget.knownUsers[id];
    if (selected == null && id != null) {
      for (final user in widget.choices) {
        if (user.userId == id) {
          selected = user;
          break;
        }
      }
    }
    final former = id != null &&
        !widget.choices.any((user) => user.userId == id);
    final label = id == null ? 'Choose user' : selected == null
        ? 'Unavailable user · ${_shortId(id)}'
        : '@${selected.username} · ${selected.displayName}';
    return InputDecorator(
      decoration: InputDecoration(labelText: widget.label, enabled: widget.enabled),
      child: OutlinedButton(
        onPressed: widget.enabled && !_open ? _choose : null,
        style: OutlinedButton.styleFrom(
          minimumSize: const Size.fromHeight(48),
          padding: const EdgeInsets.symmetric(horizontal: 12, vertical: 8),
        ),
        child: Row(children: [
          const Icon(Icons.person_outline),
          const SizedBox(width: 8),
          Expanded(child: Column(
            crossAxisAlignment: CrossAxisAlignment.start,
            children: [Text(label), if (former) const Text('Former member')],
          )),
          const SizedBox(width: 8),
          Text(id == null ? 'Choose' : 'Change'),
        ]),
      ),
    );
  }
}
