import 'dart:async';
import 'dart:convert';

import 'package:flutter/material.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:http/http.dart' as http;
import 'package:http/testing.dart';
import 'package:jizhang_client/core/api_client.dart';
import 'package:jizhang_client/core/jizhang_repository.dart';
import 'package:jizhang_client/core/models.dart';
import 'package:jizhang_client/core/session_controller.dart';
import 'package:jizhang_client/features/receipts/receipt_import_review.dart';
import 'package:jizhang_client/features/receipts/receipt_batch_edit.dart';
import 'package:jizhang_client/features/records/record_editor.dart';
import 'package:jizhang_client/shared/widgets.dart';

import 'receipt_import_test.dart' show FakeReceiptMedia, jsonResponse, mountEditor, options, tapText, testImage, extraction, chooseNoTip, openExpenseCards;
import 'session_and_forms_test.dart' show roleSession, aliceJson, bobJson, aliceId, bobId, expenseFixture;

const eur = CurrencyInfo(code: 'EUR', name: 'Euro', minorUnits: 2);

void main() {
  late SessionController session;
  testWidgets('pending receipt saves block batch and selection actions', (tester) async {
    final pending = Completer<http.Response>();
    final receipt = {'receiptId': 'receipt-id', 'groupId': 'group-id', 'title': 'Synthetic Market groceries', 'note': '', 'source': null,
      'uploadedBy': aliceJson, 'createdAt': '2026-10-01T00:00:00Z', 'records': [expenseFixture()]};
    final editorSession = roleSession(records: (request) async {
      if (request.url.path.endsWith('/receipt-extractions/options')) return jsonResponse(options(available: false));
      if (request.url.path.endsWith('/receipts/receipt-id') && request.method == 'PUT') return pending.future;
      if (request.url.path.endsWith('/receipts/receipt-id')) return jsonResponse(receipt);
      return null;
    });
    final router = await mountEditor(tester, editorSession, FakeReceiptMedia(), receiptId: 'receipt-id');
    await tapText(tester, 'Edit multiple'); await tapText(tester, 'Select all');
    await tester.ensureVisible(find.text('Save receipt')); await tester.tap(find.text('Save receipt')); await tester.pump();
    expect(tester.widget<OutlinedButton>(find.widgetWithText(OutlinedButton, 'Edit selected')).onPressed, isNull);
    expect(tester.widget<TextButton>(find.widgetWithText(TextButton, 'Done')).onPressed, isNull);
    expect(tester.widget<Checkbox>(find.byWidgetPredicate((widget) => widget is Checkbox && widget.semanticLabel == 'Select expense 1')).onChanged, isNull);
    expect(tester.widget<TextField>(find.widgetWithText(TextField, 'Receipt title')).enabled, isFalse);
    pending.complete(http.Response('Save unavailable', 503)); await tester.pumpAndSettle();
    expect(tester.widget<OutlinedButton>(find.widgetWithText(OutlinedButton, 'Edit selected')).onPressed, isNotNull);
    expect(find.text('1 selected'), findsOneWidget);
    expect(tester.widget<TextField>(find.widgetWithText(TextField, 'Receipt title')).controller!.text, 'Synthetic Market groceries');
    expect(tester.widget<TextField>(find.widgetWithText(TextField, 'Receipt title')).enabled, isTrue);
    await tester.pumpWidget(const SizedBox()); router.dispose(); editorSession.dispose();
  });

  testWidgets('import date batches retain payer confirmation while split changes and regeneration reset it', (tester) async {
    final editorSession = roleSession(records: (request) async {
      if (request.url.path.endsWith('/receipt-extractions/options')) return jsonResponse(options());
      if (request.url.path.endsWith('/receipt-extractions')) return jsonResponse(extraction());
      return null;
    });
    final router = await mountEditor(tester, editorSession, FakeReceiptMedia()..next = testImage);
    await tester.enterText(find.widgetWithText(TextField, 'Receipt title'), 'Synthetic Market groceries');
    await tapText(tester, 'Upload photo'); await tapText(tester, 'Read receipt');
    final review = tester.widget<ReceiptImportReviewFields>(find.byType(ReceiptImportReviewFields)).controller;
    chooseNoTip(review); review.changed(); await tester.pumpAndSettle();
    await tapText(tester, 'Continue'); await openExpenseCards(tester);
    await tapText(tester, 'Edit multiple'); await tapText(tester, 'Select all'); await tapText(tester, 'I confirm the displayed payers and splits');
    CheckboxListTile confirmation() => tester.widget<CheckboxListTile>(find.widgetWithText(CheckboxListTile, 'I confirm the displayed payers and splits'));
    expect(confirmation().value, isTrue);
    await tapText(tester, 'Edit selected'); await tapText(tester, 'Change date');
    await tester.enterText(find.descendant(of: find.byType(AlertDialog), matching: find.byType(TextField)), '2026-10-28');
    await tapText(tester, 'Apply to selected');
    expect(confirmation().value, isTrue);
    expect(tester.widget<SplitFields>(find.byType(SplitFields)).controller.toDraft().date, '2026-10-28');
    await tapText(tester, 'Edit selected'); await tapText(tester, 'Change splits');
    final participant = find.descendant(of: find.byType(AlertDialog), matching: find.widgetWithText(CheckboxListTile, '@bob · Bob'));
    await tester.ensureVisible(participant); await tester.tap(participant); await tester.pumpAndSettle();
    await tapText(tester, 'Apply to selected'); expect(confirmation().value, isFalse);
    final field = tester.widget<SplitFields>(find.byType(SplitFields)).controller;
    expect(field.toDraft().splits.single.userId, bobId); expect(field.preview(), {bobId: BigInt.from(2970)});
    await tapText(tester, 'Edit amounts'); await tapText(tester, 'Continue to review');
    await tapText(tester, 'Continue'); await openExpenseCards(tester);
    await tapText(tester, 'Edit multiple');
    expect(find.text('0 selected'), findsOneWidget); expect(confirmation().value, isFalse);
    expect(tester.widget<SplitFields>(find.byType(SplitFields)).controller, isNot(same(field)));
    await tapText(tester, 'Select all'); await tapText(tester, 'Edit selected');
    editorSession.expire(); await tester.pumpAndSettle();
    expect(find.byType(AlertDialog), findsNothing); expect(find.byType(SplitFields), findsNothing);
    await tester.pumpWidget(const SizedBox()); router.dispose(); editorSession.dispose();
  });

  late JizhangRepository repository;
  late ExpenseGroup group;
  final fields = <SplitFieldsController>[];
  var requests = 0;
  setUp(() {
    requests = 0;
    repository = JizhangRepository(ApiClient(baseUri: Uri.parse('https://example.test'), accessToken: () => null,
      httpClient: MockClient((_) async { requests++; return http.Response('Unexpected request', 500); })));
    session = SessionController(repository);
    group = ExpenseGroup.fromJson({'groupId': 'group-id', 'groupName': 'Trip', 'owner': aliceJson,
      'members': [aliceJson, bobJson], 'currencyMode': 'single',
      'defaultCurrency': {'code': 'USD', 'name': 'US Dollar', 'minorUnits': 2}, 'currencyLocked': false});
  });
  tearDown(() { for (final field in fields) { field.dispose(); } fields.clear(); session.dispose(); });
  SplitFieldsController row({String title = 'Expense', String amount = '10.00', CurrencyInfo? currency, ExchangeRate? rate}) {
    final value = SplitFieldsController(repository: repository, session: session, group: group,
      currentUserId: aliceId, savedCurrency: currency, savedRate: rate);
    value.title.text = title; value.note.text = 'Keep this note'; value.amount.text = amount; value.date.text = '2026-10-01';
    fields.add(value); return value;
  }
  List<SplitDraft> splits(int a, int b) => [SplitDraft(userId: aliceId, value: BigInt.from(a)), SplitDraft(userId: bobId, value: BigInt.from(b))];

  test('percentage edits preserve money, rate provenance and every unselected field', () {
    final rate = ExchangeRate(source: 'reference', rate: '1.2', rateDate: '2026-10-01',
      fetchedAt: DateTime.utc(2026, 10, 1), quoteId: 'saved-quote');
    final first = row(currency: eur, rate: rate), second = row(amount: '20.00', currency: eur, rate: rate), untouched = row();
    final before = untouched.toDraft().toJson();
    final changes = ReceiptBatchChanges(payerUserId: bobId, accountingDate: '2026-10-28',
      mode: SplitMode.percentage, splits: splits(5000, 5000));
    changes.apply(group, [first, second]);
    for (final field in [first, second]) {
      final saved = field.toDraft();
      expect(field.splitInput(aliceId).text, '50.00');
      expect(saved.toJson()['splits'], [{'userId': aliceId, 'value': '5000'}, {'userId': bobId, 'value': '5000'}]);
      expect(field.preview(), {aliceId: field.amountMinor ~/ BigInt.two, bobId: field.amountMinor ~/ BigInt.two});
      expect(field.preview(settlement: true), {aliceId: field.settlementMinor ~/ BigInt.two, bobId: field.settlementMinor ~/ BigInt.two});
      expect(saved.exchangeRate!.toJson(), {'source': 'reference', 'quoteId': 'saved-quote'});
      expect(field.exchangeRate.source, 'reference'); expect(field.exchangeRate.rateDate, '2026-10-01');
      expect(saved.currency, 'EUR'); expect(saved.title, 'Expense'); expect(saved.note, 'Keep this note');
      expect(saved.actorUserId, bobId); expect(saved.date, '2026-10-28');
    }
    expect(first.toDraft().amountMinor, BigInt.from(1000)); expect(second.toDraft().amountMinor, BigInt.from(2000));
    expect(untouched.toDraft().toJson(), before); expect(requests, 0);
  });

  test('preflight rejects every invalid exact proposal before changing any selected field', () {
    final first = row(), second = row(amount: '11.00');
    final before = fields.map((field) => field.toDraft().toJson()).toList();
    final changes = ReceiptBatchChanges(payerUserId: bobId, accountingDate: '2026-10-28', mode: SplitMode.exact, splits: splits(500, 500));
    expect(() => changes.apply(group, [first, second]), throwsFormatException);
    expect(fields.map((field) => field.toDraft().toJson()), before);
    second.amount.text = '10.00';
    expect(() => ReceiptBatchChanges(payerUserId: bobId, mode: SplitMode.exact, splits: splits(500, 499)).apply(group, [first, second]), throwsFormatException);
    expect(first.actorUserId, aliceId); expect(second.actorUserId, aliceId);
    second.amount.clear(); expect(() => changes.apply(group, [first, second]), throwsFormatException);
    expect(first.actorUserId, aliceId);
    second.amount.text = '10.00'; second.currency = eur;
    expect(() => changes.apply(group, [first, second]), throwsFormatException); expect(first.actorUserId, aliceId);
  });

  test('exact amounts use currency precision and survive serialization and allocation', () {
    final first = row(), second = row();
    ReceiptBatchChanges(mode: SplitMode.exact, splits: splits(333, 667)).apply(group, [first, second]);
    for (final field in [first, second]) {
      expect(field.splitInput(aliceId).text, '3.33');
      expect(field.toDraft().splits.map((split) => split.value), [BigInt.from(333), BigInt.from(667)]);
      expect(field.preview(), {aliceId: BigInt.from(333), bobId: BigInt.from(667)});
    }
    const jpy = CurrencyInfo(code: 'JPY', name: 'Yen', minorUnits: 0);
    final yen = row(currency: jpy, amount: '1000'); yen.exchangeRate.setManualRate('0.01');
    ReceiptBatchChanges(mode: SplitMode.exact, splits: splits(333, 667)).apply(group, [yen]);
    expect(yen.splitInput(aliceId).text, '333'); expect(yen.toDraft().splits.first.value, BigInt.from(333));
  });

  test('equal shares percentage do not serialize selected or unrelated incomplete drafts', () {
    final selected = row(title: '', amount: ''), unrelated = row(title: '', amount: 'bad');
    unrelated.setSplitMode(SplitMode.shares); unrelated.setSplitValue(aliceId, 'unfinished');
    ReceiptBatchChanges(payerUserId: bobId, accountingDate: '2024-02-29', mode: SplitMode.shares, splits: splits(1, 2)).apply(group, [selected]);
    expect(selected.actorUserId, bobId); expect(selected.date.text, '2024-02-29');
    expect(selected.splitDrafts.map((split) => split.value), [BigInt.one, BigInt.two]);
    ReceiptBatchChanges(mode: SplitMode.percentage, splits: splits(2500, 7500)).apply(group, [selected]);
    expect(selected.splitDrafts.map((split) => split.value), [BigInt.from(2500), BigInt.from(7500)]);
    ReceiptBatchChanges(mode: SplitMode.equal, splits: splits(1, 1)).apply(group, [selected]);
    expect(selected.splitMode, SplitMode.equal); expect(selected.amount.text, '');
    expect(unrelated.amount.text, 'bad'); expect(unrelated.splitInput(aliceId).text, 'unfinished'); expect(requests, 0);
  });

  test('invalid shares percentages dates and stale membership cause zero mutations', () {
    final first = row(), second = row();
    final before = fields.map((field) => field.toDraft().toJson()).toList();
    final invalid = [
      ReceiptBatchChanges(payerUserId: bobId, mode: SplitMode.percentage, splits: splits(5000, 4999)),
      ReceiptBatchChanges(payerUserId: bobId, mode: SplitMode.shares, splits: splits(0, 0)),
      ReceiptBatchChanges(payerUserId: bobId, mode: SplitMode.shares, splits: splits(32768, 1)),
      ReceiptBatchChanges(payerUserId: bobId, mode: SplitMode.shares, splits: splits(-1, 1)),
      ReceiptBatchChanges(payerUserId: bobId, accountingDate: '2026-02-29'),
      ReceiptBatchChanges(payerUserId: bobId, accountingDate: '26-10-28'),
      ReceiptBatchChanges(payerUserId: 'not-a-member'),
      ReceiptBatchChanges(mode: SplitMode.equal, splits: [SplitDraft(userId: aliceId, value: BigInt.one), SplitDraft(userId: aliceId, value: BigInt.one)]),
    ];
    for (final changes in invalid) {
      expect(() => changes.apply(group, [first, second]), throwsFormatException);
      expect(fields.map((field) => field.toDraft().toJson()), before);
    }
    final changedGroup = ExpenseGroup(groupId: group.groupId, groupName: group.groupName, owner: group.owner,
      members: [group.owner], currencyMode: group.currencyMode, defaultCurrency: group.defaultCurrency, currencyLocked: group.currencyLocked);
    expect(() => ReceiptBatchChanges(payerUserId: bobId).apply(changedGroup, [first, second]), throwsFormatException);
    expect(() => ReceiptBatchChanges(mode: SplitMode.equal, splits: splits(1, 1)).apply(changedGroup, [first, second]), throwsFormatException);
    expect(fields.map((field) => field.toDraft().toJson()), before);
  });

  test('payer and date retain mode input caches and split edits retain other modes', () {
    final field = row(); field.setSplitMode(SplitMode.shares); field.setSplitValue(aliceId, 'bad');
    field.setSplitMode(SplitMode.percentage); field.setSplitValue(aliceId, 'unfinished');
    field.setSplitMode(SplitMode.equal);
    ReceiptBatchChanges(payerUserId: bobId, accountingDate: '2026-10-28').apply(group, [field]);
    field.setSplitMode(SplitMode.shares); expect(field.splitInput(aliceId).text, 'bad');
    ReceiptBatchChanges(mode: SplitMode.shares, splits: splits(2, 1)).apply(group, [field]);
    expect(field.preview(), {aliceId: BigInt.from(667), bobId: BigInt.from(333)});
    field.setSplitMode(SplitMode.percentage); expect(field.splitInput(aliceId).text, 'unfinished');
  });

  testWidgets('selection is identity keyed UI state, not a financial change', (tester) async {
    final receipt = {'receiptId': 'receipt-id', 'groupId': 'group-id', 'title': 'Synthetic Market groceries', 'note': 'Saved note', 'source': null,
      'uploadedBy': aliceJson, 'createdAt': '2026-10-01T00:00:00Z',
      'records': [for (var n = 0; n < 3; n++) expenseFixture(id: 'expense-$n', title: 'Same title')]};
    final editorSession = roleSession(records: (request) async {
      if (request.url.path.endsWith('/receipt-extractions/options')) return jsonResponse(options(available: false));
      if (request.url.path.endsWith('/receipts/receipt-id')) return jsonResponse(receipt);
      return null;
    });
    final router = await mountEditor(tester, editorSession, FakeReceiptMedia(), receiptId: 'receipt-id');
    final form = tester.widget<FormLeaveGuard>(find.byType(FormLeaveGuard)).controller;
    expect(form.dirty, isFalse);
    await openExpenseCards(tester); await tapText(tester, 'Edit multiple');
    await tapText(tester, 'Select all'); expect(find.text('3 selected'), findsOneWidget); expect(form.dirty, isFalse);
    await tapText(tester, 'Done'); expect(form.dirty, isFalse);
    await tapText(tester, 'Edit multiple'); expect(find.text('0 selected'), findsOneWidget); expect(form.dirty, isFalse);
    final checkbox = find.byWidgetPredicate((widget) => widget is Checkbox && widget.semanticLabel == 'Select expense 2');
    await tester.ensureVisible(checkbox); await tester.tap(checkbox); await tester.pumpAndSettle();
    final controllers = tester.widgetList<SplitFields>(find.byType(SplitFields)).map((widget) => widget.controller).toList();
    controllers[1].title.text = 'Renamed'; await tester.pumpAndSettle();
    expect(find.text('1 selected'), findsOneWidget); expect(form.dirty, isFalse);
    await tapText(tester, 'Remove expense 1'); expect(find.text('1 selected'), findsOneWidget);
    await tapText(tester, 'Remove expense 1'); expect(find.text('0 selected'), findsOneWidget);
    await tester.pumpWidget(const SizedBox()); router.dispose(); editorSession.dispose();
  });

  testWidgets('dialog preflight is atomic and failed saves retain edited selected allocations', (tester) async {
    final writes = <Map<String, dynamic>>[];
    final receipt = {'receiptId': 'receipt-id', 'groupId': 'group-id', 'title': 'Synthetic Market groceries', 'note': 'Saved note', 'source': null,
      'uploadedBy': aliceJson, 'createdAt': '2026-10-01T00:00:00Z',
      'records': [for (var n = 0; n < 3; n++) expenseFixture(id: 'expense-$n')]};
    final editorSession = roleSession(records: (request) async {
      if (request.url.path.endsWith('/receipt-extractions/options')) return jsonResponse(options(available: false));
      if (request.url.path.endsWith('/receipts/receipt-id') && request.method == 'PUT') {
        writes.add(jsonDecode(request.body) as Map<String, dynamic>); return http.Response('Try again later', 503);
      }
      if (request.url.path.endsWith('/receipts/receipt-id')) return jsonResponse(receipt);
      return null;
    });
    final router = await mountEditor(tester, editorSession, FakeReceiptMedia(), receiptId: 'receipt-id');
    await openExpenseCards(tester); await tapText(tester, 'Edit multiple');
    final rows = tester.widgetList<SplitFields>(find.byType(SplitFields)).map((widget) => widget.controller).toList();
    final untouched = rows.last.toDraft().toJson();
    for (final ordinal in [1, 2]) {
      final checkbox = find.byWidgetPredicate((widget) => widget is Checkbox && widget.semanticLabel == 'Select expense $ordinal');
      await tester.ensureVisible(checkbox); await tester.tap(checkbox); await tester.pumpAndSettle();
    }
    await tapText(tester, 'Edit selected');
    expect(tester.widget<FilledButton>(find.widgetWithText(FilledButton, 'Apply to selected')).onPressed, isNull);
    await tapText(tester, 'Change splits');
    await tester.tap(find.byType(DropdownButtonFormField<SplitMode>)); await tester.pumpAndSettle();
    await tapText(tester, 'Percentage');
    final dialog = find.byType(AlertDialog);
    for (final user in ['@alice · Alice', '@bob · Bob']) {
      final choice = find.descendant(of: dialog, matching: find.widgetWithText(CheckboxListTile, user));
      await tester.ensureVisible(choice); await tester.tap(choice); await tester.pumpAndSettle();
    }
    final values = find.descendant(of: dialog, matching: find.byType(TextField));
    await tester.enterText(values.at(0), '50'); await tester.enterText(values.at(1), '49');
    await tapText(tester, 'Apply to selected'); expect(find.byType(AlertDialog), findsOneWidget);
    expect(rows[0].splitMode, SplitMode.equal); expect(rows[1].splitMode, SplitMode.equal);
    await tester.enterText(values.at(1), '50'); await tapText(tester, 'Apply to selected');
    expect(rows[0].preview(), {aliceId: BigInt.from(600), bobId: BigInt.from(600)});
    expect(rows[1].preview(), {aliceId: BigInt.from(600), bobId: BigInt.from(600)});
    expect(rows.last.toDraft().toJson(), untouched);
    await tapText(tester, 'Save receipt');
    expect(writes.single['records'][0]['splits'], [{'userId': aliceId, 'value': '5000'}, {'userId': bobId, 'value': '5000'}]);
    expect(writes.single['records'][1]['splitMode'], 'percentage'); expect(writes.single['records'][2], untouched);
    expect(rows[0].toDraft().splits.first.value, BigInt.from(5000)); expect(find.text('2 selected'), findsOneWidget);
    await tester.pumpWidget(const SizedBox()); router.dispose(); editorSession.dispose();
  });
}
