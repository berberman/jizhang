import 'dart:js_interop';

import 'package:web/web.dart' as web;

void consumeRecordHistoryEntry(String destination, String dismissed, bool Function() current) {
  var remaining = web.window.history.length;
  late final JSFunction listener;
  listener = ((web.PopStateEvent event) {
    final hash = web.window.location.hash;
    final route = hash.isEmpty ? '' : hash.substring(1);
    if (!current() || route == destination || route != dismissed || --remaining <= 0) {
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
