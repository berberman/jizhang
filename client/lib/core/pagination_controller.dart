import 'package:flutter/foundation.dart';

import 'api_client.dart';
import 'models.dart';

class PaginationController<T> extends ChangeNotifier {
  PaginationController({
    required Future<Page<T>> Function(PageQuery) load,
    this.query = const PageQuery(),
  }) : _load = load;

  final Future<Page<T>> Function(PageQuery) _load;
  PageQuery query;
  Page<T>? page;
  String? error;
  int? errorStatusCode;
  bool loading = false;
  bool disposed = false;
  int _generation = 0;

  Future<void> refresh({bool clamp = false}) async {
    if (disposed) return;
    final generation = ++_generation;
    loading = true;
    error = null;
    errorStatusCode = null;
    notifyListeners();
    try {
      var result = await _load(query);
      if (disposed || generation != _generation) return;
      if (clamp && result.items.isEmpty && result.pageInfo.offset > 0) {
        final info = result.pageInfo;
        final limit = info.limit;
        final offset = info.total == 0 || limit <= 0
            ? 0
            : ((info.total - 1) ~/ limit) * limit;
        if (offset < info.offset) {
          query = query.copyWith(offset: offset);
          result = await _load(query);
          if (disposed || generation != _generation) return;
        }
      }
      page = result;
      query = query.copyWith(
        offset: result.pageInfo.offset,
        limit: result.pageInfo.limit,
      );
      loading = false;
      notifyListeners();
    } catch (failure) {
      if (disposed || generation != _generation) return;
      errorStatusCode = failure is ApiException ? failure.statusCode : null;
      error = failure is ApiException
          ? failure.message
          : 'Could not load data. Please try again.';
      loading = false;
      notifyListeners();
    }
  }

  Future<void> search(String value) {
    query = query.copyWith(query: value, offset: 0);
    return refresh();
  }

  Future<void> setSort(String value) {
    query = query.copyWith(sort: value, offset: 0);
    return refresh();
  }

  Future<void> next() async {
    final info = page?.pageInfo;
    if (disposed || loading || info == null || !info.hasNext) return;
    query = query.copyWith(offset: info.offset + info.limit);
    await refresh();
  }

  Future<void> previous() async {
    final info = page?.pageInfo;
    if (disposed || loading || info == null || !info.hasPrev) return;
    final offset = info.offset - info.limit;
    query = query.copyWith(offset: offset < 0 ? 0 : offset);
    await refresh();
  }

  void clear() {
    if (disposed) return;
    _generation++;
    page = null;
    error = null;
    errorStatusCode = null;
    loading = false;
    query = query.copyWith(offset: 0);
    notifyListeners();
  }

  @override
  void dispose() {
    disposed = true;
    _generation++;
    super.dispose();
  }
}
