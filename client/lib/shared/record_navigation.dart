import 'package:flutter/material.dart';
import 'package:go_router/go_router.dart';

import '../core/session_controller.dart';
import 'record_history_unsupported.dart'
    if (dart.library.js_interop) 'record_history_web.dart' as platform;
import 'widgets.dart';

Future<void> returnFromRecord(BuildContext context, SessionController session,
    {required String fallback}) async {
  final generation = session.generation;
  final router = GoRouter.of(context);
  if (!router.canPop()) {
    router.go(fallback);
    return;
  }
  if (!await FormNavigation.canExit(context) || !context.mounted ||
      generation != session.generation || !session.isAuthenticated) return;
  final route = ModalRoute.of(context);
  final dismissed = router.routeInformationProvider.value.uri.toString();
  Router.neglect(context, () => router.pop());
  if (route == null) return;
  await route.popped;
  final predecessor = router.routerDelegate.currentConfiguration;
  await WidgetsBinding.instance.endOfFrame;
  if (generation != session.generation || !session.isAuthenticated ||
      !identical(predecessor, router.routerDelegate.currentConfiguration)) return;
  // Replace the dismissed URL first, then consume its entry; Forward cannot reopen an editor.
  platform.consumeRecordHistoryEntry(router.routeInformationProvider.value.uri.toString(), dismissed,
    () => generation == session.generation && session.isAuthenticated &&
      identical(predecessor, router.routerDelegate.currentConfiguration));
}


class RecordChangeTracker {
  int _revision = 0;
  int get revision => _revision;
  void markChanged() => _revision++;
}

class RecordReturnBoundary extends StatefulWidget {
  const RecordReturnBoundary({super.key, required this.session,
    required this.changes, required this.onReturn, required this.child});
  final SessionController session;
  final RecordChangeTracker changes;
  final Future<void> Function(bool changed) onReturn;
  final Widget child;

  @override
  State<RecordReturnBoundary> createState() => _RecordReturnBoundaryState();
}

class _RecordReturnBoundaryState extends State<RecordReturnBoundary> {
  late final int _generation;
  late int _seenRevision;
  final FocusNode _contentFocus = FocusNode(skipTraversal: true);
  bool? _wasCurrent;
  int _returnRequest = 0;
  ScrollPosition? _position;
  double? _offset;
  FocusNode? _focused;
  @override
  void initState() {
    super.initState();
    _generation = widget.session.generation;
    _seenRevision = widget.changes.revision;
  }

  @override
  void didChangeDependencies() {
    super.didChangeDependencies();
    final current = ModalRoute.isCurrentOf(context);
    if (current == null || current == _wasCurrent) return;
    final previous = _wasCurrent;
    _wasCurrent = current;
    if (previous == true && !current) {
      _returnRequest++;
      _position = Scrollable.maybeOf(context, axis: Axis.vertical)?.position;
      _offset = _position?.pixels;
      _focused = FocusScope.of(context).focusedChild;
    } else if (previous == false && current) {
      final request = ++_returnRequest;
      WidgetsBinding.instance.addPostFrameCallback((_) => _returned(request));
    }
  }

  bool _current(int request) => mounted && request == _returnRequest &&
      widget.session.generation == _generation && widget.session.isAuthenticated &&
      ModalRoute.of(context)?.isCurrent == true;

  Future<void> _returned(int request) async {
    if (!_current(request)) return;
    final revision = widget.changes.revision;
    final changed = revision != _seenRevision;
    await widget.onReturn(changed);
    if (!_current(request)) return;
    _seenRevision = revision;
    await WidgetsBinding.instance.endOfFrame;
    if (!_current(request)) return;
    final position = _position;
    final offset = _offset;
    if (position != null && offset != null && position.hasPixels &&
        position.hasContentDimensions &&
        identical(Scrollable.maybeOf(context, axis: Axis.vertical)?.position, position) &&
        (changed || position.pixels != offset.clamp(position.minScrollExtent, position.maxScrollExtent))) {
      position.jumpTo(offset.clamp(position.minScrollExtent, position.maxScrollExtent));
    }
    final focused = _focused;
    if (focused != null && focused.context != null && focused.canRequestFocus &&
        focused.enclosingScope == _contentFocus.enclosingScope) {
      focused.requestFocus();
    } else {
      _contentFocus.requestFocus();
    }
    // EditableText can reveal its caret a frame after route focus returns.
    await WidgetsBinding.instance.endOfFrame;
    if (!_current(request)) return;
    if (position != null && offset != null && position.hasPixels && position.hasContentDimensions &&
        identical(Scrollable.maybeOf(context, axis: Axis.vertical)?.position, position)) {
      final target = offset.clamp(position.minScrollExtent, position.maxScrollExtent);
      if (position.pixels != target || position.isScrollingNotifier.value) position.jumpTo(target);
    }
  }

  @override
  void dispose() {
    _returnRequest++;
    _contentFocus.dispose();
    super.dispose();
  }

  @override
  Widget build(BuildContext context) => Focus(
    focusNode: _contentFocus, includeSemantics: false, child: widget.child);
}
