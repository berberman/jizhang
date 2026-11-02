import 'dart:convert';

import 'package:flutter/material.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:http/http.dart' as http;
import 'package:jizhang_client/features/groups/group_screen.dart';
import 'package:jizhang_client/features/receipts/receipt_editor.dart';
import 'package:jizhang_client/features/records/record_editor.dart';
import 'package:jizhang_client/shared/widgets.dart';

import 'session_and_forms_test.dart' as fixtures;

void main() {
  for (final scenario in [
    (size: const Size(195, 422), textScale: 1.0),
    (size: const Size(390, 420), textScale: 1.0),
    (size: const Size(390, 844), textScale: 2.0),
  ]) {
    testWidgets('receipt actions remain reachable at ${scenario.size} with ${scenario.textScale} text', (tester) async {
      await tester.binding.setSurfaceSize(scenario.size);
      addTearDown(() => tester.binding.setSurfaceSize(null));
      final group = {
        'groupId': 'group-id',
        'groupName': 'Receipt import browser smoke with a long shared ledger name',
        'owner': fixtures.aliceJson,
        'members': [fixtures.aliceJson, fixtures.bobJson],
        'currencyMode': 'single',
        'defaultCurrency': fixtures.usdJson,
        'currencyLocked': false,
      };
      final session = fixtures.makeSession((request) async {
        if (request.url.path.endsWith('/auth/login')) return fixtures.signedIn();
        if (request.url.path.endsWith('/groups/group-id')) return http.Response(jsonEncode(group), 200);
        if (request.url.path.endsWith('/currencies')) return http.Response(jsonEncode([fixtures.usdJson]), 200);
        if (request.url.path.endsWith('/receipt-extractions/options')) {
          return http.Response(jsonEncode({
            'available': false, 'model': 'gpt-5.4-mini-2026-03-17', 'reasoningEffort': 'low',
            'maxImageBytes': 0, 'acceptedMediaTypes': ['image/jpeg'],
          }), 200);
        }
        return http.Response('Not found', 404);
      });
      addTearDown(session.dispose);
      await session.login('alice', 'password');
      await tester.pumpWidget(MaterialApp(home: Builder(builder: (context) => MediaQuery(
        data: MediaQuery.of(context).copyWith(textScaler: TextScaler.linear(scenario.textScale)),
        child: Scaffold(appBar: AppBar(title: const Text('Jizhang')), body: GroupWorkspace(
          repository: session.repository, session: session, groupId: 'group-id',
          location: Uri.parse('/groups/group-id/receipts/new'),
          child: PageBody(storageKey: 'receipt-zoom-test', child: ReceiptEditor(
            repository: session.repository, session: session, groupId: 'group-id')))),
      ))));
      await tester.pumpAndSettle();
      final title = find.widgetWithText(TextField, 'Receipt title');
      await tester.ensureVisible(title); await tester.enterText(title, '会议 <b>Market</b>');
      await tester.pumpAndSettle();
      final add = find.widgetWithText(OutlinedButton, 'Add expense');
      await tester.ensureVisible(add); await tester.pumpAndSettle();
      final addBounds = tester.getRect(add);
      expect(addBounds.top, greaterThanOrEqualTo(56));
      expect(addBounds.bottom, lessThanOrEqualTo(scenario.size.height));
      await tester.tap(add); await tester.pumpAndSettle();
      expect(find.byType(SplitFields), findsOneWidget);
      final card = find.byWidgetPredicate((widget) => widget is ExpansionTile && widget.key is ObjectKey && (widget.key as ObjectKey).value is SplitFieldsController);
      await tester.ensureVisible(card);
      await tester.tap(find.descendant(of: card, matching: find.byType(ListTile)).first);
      await tester.pumpAndSettle();
      expect(find.byType(SplitFields), findsNothing);
      final save = find.widgetWithText(FilledButton, 'Save receipt');
      await tester.ensureVisible(save); await tester.pumpAndSettle();
      final saveBounds = tester.getRect(save);
      expect(saveBounds.top, greaterThanOrEqualTo(56));
      expect(saveBounds.bottom, lessThanOrEqualTo(scenario.size.height));
      expect(saveBounds.height, greaterThanOrEqualTo(44));
      expect(saveBounds.left, greaterThanOrEqualTo(0));
      expect(saveBounds.right, lessThanOrEqualTo(scenario.size.width));
      await tester.tap(save); await tester.pumpAndSettle();
      expect(find.byType(SplitFields), findsOneWidget);
      await tester.pumpWidget(const SizedBox());
    });
  }
}
