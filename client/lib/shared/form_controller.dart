import 'package:flutter/foundation.dart';

import '../core/api_client.dart';
import '../core/session_controller.dart';

class FormController extends ChangeNotifier {
  FormController(this.session) : _sessionGeneration = session.generation {
    session.addListener(_sessionChanged);
  }

  final SessionController session;
  bool dirty = false;
  bool pending = false;
  String? error;
  int _sessionGeneration;
  int _generation = 0;
  bool _disposed = false;

  void _sessionChanged() {
    if (_sessionGeneration == session.generation) return;
    _sessionGeneration = session.generation;
    reset();
  }

  void markDirty() {
    if (_disposed) return;
    dirty = true;
    error = null;
    notifyListeners();
  }

  Future<bool> save(Future<void> Function() action) async {
    if (_disposed || pending) return false;
    final generation = ++_generation;
    pending = true;
    error = null;
    notifyListeners();
    try {
      await action();
      if (_disposed || generation != _generation) return false;
      pending = false;
      dirty = false;
      notifyListeners();
      return true;
    } catch (failure) {
      if (_disposed || generation != _generation) return false;
      pending = false;
      error = failure is ApiException
          ? failure.message
          : 'Could not save changes. Please try again.';
      notifyListeners();
      return false;
    }
  }

  Future<bool> canLeave(Future<bool> Function() confirm) async {
    if (_disposed) return true;
    if (pending) return false;
    if (!dirty) return true;
    final generation = _generation;
    final discard = await confirm();
    if (_disposed || generation != _generation) return !dirty;
    if (!discard) return false;
    reset();
    return true;
  }

  void reset() {
    if (_disposed) return;
    _generation++;
    dirty = false;
    pending = false;
    error = null;
    notifyListeners();
  }

  @override
  void dispose() {
    _disposed = true;
    _generation++;
    session.removeListener(_sessionChanged);
    super.dispose();
  }
}
