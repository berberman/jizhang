import 'package:flutter/material.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:http/http.dart' as http;
import 'package:jizhang_client/core/exchange_rate_controller.dart';
import 'package:jizhang_client/core/models.dart';
import 'package:jizhang_client/features/records/record_editor.dart';
import 'package:jizhang_client/features/records/record_search.dart';
import 'package:jizhang_client/shared/currency_amount_fields.dart';

import 'session_and_forms_test.dart' as fixtures;

void expectSeparated(WidgetTester tester, Finder before, Finder after) {
  expect(before, findsOneWidget);
  expect(after, findsOneWidget);
  expect(tester.getRect(before).overlaps(tester.getRect(after)), isFalse);
}

Widget surface(Widget child) => MaterialApp(home: Scaffold(body:
  SingleChildScrollView(padding: const EdgeInsets.all(16), child: child)));

void main() {
  for (final width in [320.0, 390.0, 768.0, 1440.0]) {
    for (final scale in [1.0, 2.0]) {
      testWidgets('Records controls reflow without collisions at width $width and scale $scale', (tester) async {
        await tester.binding.setSurfaceSize(Size(width, 1000));
        addTearDown(() => tester.binding.setSurfaceSize(null));
        final session = fixtures.roleSession();
        addTearDown(session.dispose);
        final alice = User.fromJson(fixtures.aliceJson);
        final group = ExpenseGroup(groupId: 'toolbar-group', groupName: 'Trip', owner: alice,
          members: [alice], currencyMode: CurrencyMode.single, defaultCurrency: fixtures.usd,
          currencyLocked: false);
        var applied = '';
        var sortValue = '-date';
        final searches = <String>[];
        await tester.pumpWidget(surface(Builder(builder: (context) => MediaQuery(
          data: MediaQuery.of(context).copyWith(textScaler: TextScaler.linear(scale)),
          child: StatefulBuilder(builder: (context, update) => RecordSearchToolbar(
            repository: session.repository, session: session, group: group, query: applied, sort: sortValue,
            onSearch: (value) => update(() { applied = value; searches.add(value); }),
            onSort: (value) => update(() => sortValue = value)))))));
        final toolbar = find.byType(RecordSearchToolbar);
        final input = find.widgetWithText(TextField, 'Search records');
        final search = find.widgetWithText(FilledButton, 'Search');
        final clear = find.widgetWithText(TextButton, 'Clear');
        final addFilter = find.widgetWithText(OutlinedButton, 'Add filter');
        final field = find.widgetWithText(DropdownButtonFormField<String>, 'Sort records');
        final direction = find.widgetWithText(DropdownButtonFormField<bool>, 'Direction');
        final actions = [search, clear, addFilter];
        final controls = [input, ...actions, field, direction];
        void checkGeometry([List<Finder> extra = const []]) {
          final bounds = tester.getRect(toolbar);
          final rects = [for (final finder in [...controls, ...extra]) tester.getRect(finder)];
          for (var i = 0; i < rects.length; i++) {
            expect(rects[i].left, greaterThanOrEqualTo(bounds.left - 0.01));
            expect(rects[i].right, lessThanOrEqualTo(bounds.right + 0.01));
            for (var j = i + 1; j < rects.length; j++) {
              expect(rects[i].overlaps(rects[j]), isFalse, reason: 'Controls $i and $j overlap');
            }
          }
          expect(tester.takeException(), isNull);
        }
        checkGeometry();
        await tester.enterText(input, 'type:transfer note:invoice');
        await tester.ensureVisible(search);
        await tester.tap(search); await tester.pumpAndSettle();
        expect(applied, 'type:transfer note:invoice');
        final chips = [find.widgetWithText(InputChip, 'type: transfer'), find.widgetWithText(InputChip, 'note: invoice')];
        checkGeometry(chips);
        expectSeparated(tester, chips.first, field);
        await tester.ensureVisible(input);
        await tester.enterText(input, 'note:"unfinished');
        await tester.ensureVisible(search);
        await tester.tap(search); await tester.pumpAndSettle();
        expect(searches, ['type:transfer note:invoice']);
        final error = find.text('Unterminated quoted value.');
        final notice = find.text('Previous results are unchanged until a valid search is applied.');
        expect(error, findsOneWidget);
        checkGeometry([notice]);
        final errorRect = tester.getRect(error);
        final errorBounds = tester.getRect(toolbar);
        expect(errorRect.left, greaterThanOrEqualTo(errorBounds.left));
        expect(errorRect.right, lessThanOrEqualTo(errorBounds.right));
        for (final action in [...actions, field, direction, notice]) {
          expect(errorRect.overlaps(tester.getRect(action)), isFalse);
        }
        expectSeparated(tester, notice, field);
        await tester.ensureVisible(field);
        await tester.tap(field); await tester.pumpAndSettle();
        final amount = find.text('Amount (grouped by currency)').last;
        await tester.ensureVisible(amount);
        await tester.tap(amount); await tester.pumpAndSettle();
        checkGeometry([notice]);
        await tester.pumpWidget(const SizedBox.shrink());
      });
    }
  }

  for (final size in [const Size(390, 844), const Size(1440, 1000)]) {
    testWidgets('Split search and participant results stay separated at $size', (tester) async {
      await tester.binding.setSurfaceSize(size);
      addTearDown(() => tester.binding.setSurfaceSize(null));
      final session = fixtures.roleSession();
      addTearDown(session.dispose);
      await fixtures.openRoleEditor(tester, session, 'expenses/new');
      final controller = tester.widget<SplitFields>(find.byType(SplitFields)).controller;
      final search = find.widgetWithText(TextField, 'Search participants');
      await tester.ensureVisible(search);
      await tester.pumpAndSettle();
      final count = find.text('2 participants selected');
      final dropdown = find.byType(DropdownButtonFormField<SplitMode>);
      final segmented = find.byType(SegmentedButton<SplitMode>);
      final method = dropdown.evaluate().isNotEmpty ? dropdown : segmented;
      expectSeparated(tester, method, count);
      expectSeparated(tester, count, search);
      expectSeparated(tester, search, find.byKey(const ValueKey('candidate:${fixtures.aliceId}')));
      if (segmented.evaluate().isNotEmpty) {
        expectSeparated(tester, find.text('Split method'), segmented);
      }
      await tester.enterText(search, '@bob');
      await tester.pumpAndSettle();
      expect(find.byKey(const ValueKey('candidate:${fixtures.aliceId}')), findsNothing);
      final bob = find.byKey(const ValueKey('candidate:${fixtures.bobId}'));
      expectSeparated(tester, search, bob);
      await tester.ensureVisible(bob);
      await tester.tap(bob);
      await tester.pumpAndSettle();
      expect(controller.selected, {fixtures.aliceId});
      expectSeparated(tester, method, find.text('1 participants selected'));
      expectSeparated(tester, find.text('1 participants selected'), search);
      expectSeparated(tester, search, bob);
      await tester.pumpWidget(const SizedBox.shrink());
    });


    testWidgets('Exchange rate helper status and actions stay separated at $size', (tester) async {
      await tester.binding.setSurfaceSize(size);
      addTearDown(() => tester.binding.setSurfaceSize(null));
      final session = fixtures.makeSession((_) async => http.Response('Rate unavailable', 503));
      addTearDown(session.dispose);
      final alice = User.fromJson(fixtures.aliceJson);
      final group = ExpenseGroup(groupId: 'group-id', groupName: 'Trip', owner: alice,
        members: [alice], currencyMode: CurrencyMode.single, defaultCurrency: fixtures.usd,
        currencyLocked: false);
      final controller = ExchangeRateController(repository: session.repository, session: session,
        group: group, sourceCurrency: 'EUR');
      addTearDown(controller.dispose);
      await tester.pumpWidget(surface(ExchangeRateFields(controller: controller)));
      final field = find.widgetWithText(TextField, 'Exchange rate');
      final actions = find.ancestor(of: find.widgetWithText(TextButton, 'Refresh rate'), matching: find.byType(Wrap));
      expectSeparated(tester, find.text('1 EUR = … USD'), field);
      expectSeparated(tester, field, actions);
      await tester.enterText(field, '1.25');
      await tester.pumpAndSettle();
      expectSeparated(tester, find.text('1 EUR = 1.25 USD'), field);
      expectSeparated(tester, field, find.text('Manual rate'));
      expectSeparated(tester, find.text('Manual rate'), actions);
      await tester.tap(find.widgetWithText(TextButton, 'Refresh rate'));
      await tester.pumpAndSettle();
      expect(controller.error, isNotNull);
      final error = find.text(controller.error!);
      expectSeparated(tester, field, error);
      final retryActions = find.ancestor(of: find.widgetWithText(TextButton, 'Retry'), matching: find.byType(Wrap));
      expectSeparated(tester, error, retryActions);
      await tester.enterText(field, '1.5');
      await tester.pumpAndSettle();
      expect(error, findsNothing);
      expectSeparated(tester, field, find.text('Manual rate'));
      expectSeparated(tester, find.text('Manual rate'), actions);
      await tester.pumpWidget(const SizedBox.shrink());
    });
  }
}
