import 'dart:async';

import 'package:flutter_test/flutter_test.dart';
import 'package:jizhang_client/core/api_client.dart';
import 'package:jizhang_client/core/models.dart';
import 'package:jizhang_client/core/pagination_controller.dart';

Page<String> result(
  List<String> items, {
  int offset = 0,
  int total = 1,
  int limit = 20,
}) => Page<String>(
  items: items,
  pageInfo: PageInfo(
    offset: offset,
    limit: limit,
    total: total,
    hasNext: offset + limit < total,
    hasPrev: offset > 0,
  ),
  appliedQuery: null,
  appliedSort: null,
);

void main() {
  test('late search result cannot replace current page or loading state', () async {
    final first = Completer<Page<String>>();
    final second = Completer<Page<String>>();
    final controller = PaginationController<String>(
      load: (query) => query.query == 'new' ? second.future : first.future,
    );
    addTearDown(controller.dispose);
    final oldRequest = controller.refresh();
    final currentRequest = controller.search('new');
    first.complete(result(['old']));
    await oldRequest;
    expect(controller.page, isNull);
    expect(controller.loading, isTrue);
    second.complete(result(['new']));
    await currentRequest;
    expect(controller.page!.items, ['new']);
    expect(controller.query.query, 'new');
    expect(controller.query.offset, 0);
    expect(controller.loading, isFalse);
  });

  test('late failed request does not overwrite a newer successful result', () async {
    final old = Completer<Page<String>>();
    final controller = PaginationController<String>(
      load: (query) async => query.query == 'new'
          ? result(['new'])
          : await old.future,
    );
    addTearDown(controller.dispose);
    final obsolete = controller.refresh();
    await controller.search('new');
    old.completeError(const ApiException(statusCode: 500, message: 'Old failure'));
    await obsolete;
    expect(controller.page!.items, ['new']);
    expect(controller.error, isNull);
  });

  test('deletion on last page clamps and fetches the last valid page', () async {
    var deleted = false;
    final controller = PaginationController<String>(
      query: const PageQuery(offset: 20),
      load: (query) async {
        if (!deleted) return result(['last'], offset: 20, total: 21);
        if (query.offset == 20) return result([], offset: 20, total: 20);
        return result(['remaining'], total: 20);
      },
    );
    addTearDown(controller.dispose);
    await controller.refresh();
    expect(controller.page!.items, ['last']);
    deleted = true;
    await controller.refresh(clamp: true);
    expect(controller.query.offset, 0);
    expect(controller.page!.items, ['remaining']);
    expect(controller.page!.pageInfo.hasPrev, isFalse);
    expect(controller.page!.pageInfo.hasNext, isFalse);
  });

  test('deleting final item returns to the empty first page', () async {
    final controller = PaginationController<String>(
      query: const PageQuery(offset: 40),
      load: (query) async => result([], offset: query.offset, total: 0),
    );
    addTearDown(controller.dispose);
    await controller.refresh(clamp: true);
    expect(controller.query.offset, 0);
    expect(controller.page!.pageInfo.offset, 0);
    expect(controller.page!.items, isEmpty);
  });

  test('failed refresh preserves existing page with visible error', () async {
    var offline = false;
    final controller = PaginationController<String>(load: (_) async {
      if (offline) {
        throw const ApiException(
          statusCode: null, message: 'Could not reach the server',
        );
      }
      return result(['saved']);
    });
    addTearDown(controller.dispose);
    await controller.refresh();
    offline = true;
    await controller.refresh();
    expect(controller.page!.items, ['saved']);
    expect(controller.error, 'Could not reach the server');
    expect(controller.loading, isFalse);
  });

  test('server boundaries govern paging and search and sort reset offset', () async {
    final controller = PaginationController<String>(
      load: (query) async => result(
        ['row-${query.offset}'], offset: query.offset, total: 21,
      ),
    );
    addTearDown(controller.dispose);
    await controller.refresh();
    await controller.previous();
    expect(controller.page!.items, ['row-0']);
    await controller.next();
    expect(controller.page!.items, ['row-20']);
    await controller.next();
    expect(controller.page!.items, ['row-20']);
    await controller.search('Trip');
    expect(controller.query.offset, 0);
    await controller.next();
    await controller.setSort('-groupName');
    expect(controller.query.offset, 0);
    expect(controller.query.query, 'Trip');
  });

  test('clear and dispose invalidate pending results', () async {
    final response = Completer<Page<String>>();
    final controller = PaginationController<String>(load: (_) => response.future);
    final pending = controller.refresh();
    controller.clear();
    response.complete(result(['obsolete']));
    await pending;
    expect(controller.page, isNull);
    expect(controller.loading, isFalse);
    controller.dispose();

    final late = Completer<Page<String>>();
    final disposed = PaginationController<String>(load: (_) => late.future);
    final request = disposed.refresh();
    disposed.dispose();
    late.complete(result(['obsolete']));
    await request;
    expect(disposed.page, isNull);
  });
}
