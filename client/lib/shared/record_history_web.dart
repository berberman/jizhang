import 'dart:js_interop';

import 'package:web/web.dart' as web;

void consumeRecordHistoryEntry(String destination, String dismissed, bool Function() current,
    String Function() currentLocation) {
  var remaining = web.window.history.length;
  late final JSFunction listener;
  listener = ((web.PopStateEvent event) {
    if (!current()) {
      event.stopImmediatePropagation();
      web.window.removeEventListener('popstate', listener, true.toJS);
      // A queued Back must not replace a newer tab or authenticated session route.
      web.window.history.replaceState(web.window.history.state, '', '#${currentLocation()}');
      return;
    }
    final hash = web.window.location.hash;
    final route = hash.isEmpty ? '' : hash.substring(1);
    if (route == destination || route != dismissed || --remaining <= 0) {
      web.window.removeEventListener('popstate', listener, true.toJS);
      return;
    }
    // A saved editor's replaced Forward entry can duplicate its detail predecessor.
    event.stopImmediatePropagation();
    web.window.history.back();
  }).toJS;
  web.window.addEventListener('popstate', listener, true.toJS);
  web.window.history.back();
}
