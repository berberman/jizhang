import 'dart:async';
import 'dart:convert';

import 'package:flutter/gestures.dart';
import 'package:flutter/material.dart';
import 'package:flutter/services.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:go_router/go_router.dart';
import 'package:http/http.dart' as http;
import 'package:jizhang_client/core/models.dart';
import 'package:jizhang_client/core/session_controller.dart';
import 'package:jizhang_client/core/receipt_ai_models.dart';
import 'package:jizhang_client/features/groups/group_screen.dart';
import 'package:jizhang_client/features/receipts/receipt_editor.dart';
import 'package:jizhang_client/features/receipts/receipt_import_controller.dart';
import 'package:jizhang_client/features/receipts/receipt_import_review.dart';
import 'package:jizhang_client/features/receipts/receipt_media.dart';
import 'package:jizhang_client/features/records/record_editor.dart';
import 'package:jizhang_client/shared/record_navigation.dart';
import 'package:jizhang_client/shared/widgets.dart';

import 'receipt_import_math_test.dart' show evidenceJson, observation, reviewJson;
import 'session_and_forms_test.dart' show roleSession, usd, aliceJson, bobJson, aliceId, bobId, expenseFixture;

const extractionId = '00000000-0000-0000-0000-000000000099';
const testImage = ReceiptImageInput(dataBase64: '/9j/4AAQSkZJRgABAQAAAQABAAD/2wBDAAMCAgICAgMCAgIDAwMDBAYEBAQEBAgGBgUGCQgKCgkICQkKDA8MCgsOCwkJDRENDg8QEBEQCgwSExIQEw8QEBD/2wBDAQMDAwQDBAgEBAgQCwkLEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBD/wAARCAACAAIDASIAAhEBAxEB/8QAHwAAAQUBAQEBAQEAAAAAAAAAAAECAwQFBgcICQoL/8QAtRAAAgEDAwIEAwUFBAQAAAF9AQIDAAQRBRIhMUEGE1FhByJxFDKBkaEII0KxwRVS0fAkM2JyggkKFhcYGRolJicoKSo0NTY3ODk6Q0RFRkdISUpTVFVWV1hZWmNkZWZnaGlqc3R1dnd4eXqDhIWGh4iJipKTlJWWl5iZmqKjpKWmp6ipqrKztLW2t7i5usLDxMXGx8jJytLT1NXW19jZ2uHi4+Tl5ufo6erx8vP09fb3+Pn6/8QAHwEAAwEBAQEBAQEBAQAAAAAAAAECAwQFBgcICQoL/8QAtREAAgECBAQDBAcFBAQAAQJ3AAECAxEEBSExBhJBUQdhcRMiMoEIFEKRobHBCSMzUvAVYnLRChYkNOEl8RcYGRomJygpKjU2Nzg5OkNERUZHSElKU1RVVldYWVpjZGVmZ2hpanN0dXZ3eHl6goOEhYaHiImKkpOUlZaXmJmaoqOkpaanqKmqsrO0tba3uLm6wsPExcbHyMnK0tPU1dbX2Nna4uPk5ebn6Onq8vP09fb3+Pn6/9oADAMBAAIRAxEAPwD9U6KKKAP/2Q==');

class FakeReceiptMedia implements ReceiptMedia {
  ReceiptImageInput? next;
  int picks = 0, stops = 0;
  @override
  bool get supported => true;
  @override
  bool get cameraSupported => false;
  @override
  Future<ReceiptImageInput?> pickImage({required int maxBytes}) async { picks++; return next; }
  @override
  Future<ReceiptImageInput?> takePhoto(BuildContext context, {required int maxBytes}) async => next;
  @override
  void dispose() { stops++; }
}

Map<String, Object?> options({bool available = true, int cap = 5242880}) => {
  'available': available, 'model': 'gpt-5.4-mini-2026-03-17', 'reasoningEffort': 'low',
  'maxImageBytes': cap, 'acceptedMediaTypes': ['image/jpeg'],
};
Map<String, Object?> extraction({Map<String, Object?>? evidence, String? title = 'Synthetic Market groceries', String id = extractionId}) => {
  'extractionId': id, 'model': 'gpt-5.4-mini-2026-03-17', 'promptVersion': 'receipt-v2', 'title': title,
  'evidence': evidence ?? evidenceJson(), 'issues': <Object>[],
};
http.Response jsonResponse(Object value, [int code = 200]) => http.Response(jsonEncode(value), code,
  headers: {'content-type': 'application/json; charset=utf-8'});
void chooseNoTip(ReceiptImportController c) {
  c.tipDecision = 'none'; c.actualTip.text = '0';
}
Future<BuildContext> mountContext(WidgetTester tester) async {
  late BuildContext result;
  await tester.pumpWidget(MaterialApp(home: Builder(builder: (context) { result = context; return const SizedBox(); })));
  return result;
}
Future<void> tapText(WidgetTester tester, String text) async {
  final target = find.text(text).last;
  await tester.ensureVisible(target); await tester.tap(target); await tester.pumpAndSettle();
}
Future<void> openExpenseCards(WidgetTester tester) async {
  final cards = find.byWidgetPredicate((widget) => widget is ExpansionTile && widget.key is ObjectKey && (widget.key as ObjectKey).value is SplitFieldsController);
  for (final element in cards.evaluate().toList()) {
    final tile = element.widget as ExpansionTile;
    if (tile.controller?.isExpanded ?? false) continue;
    await tester.ensureVisible(find.byWidget(tile));
    await tester.tap(find.descendant(of: find.byWidget(tile), matching: find.byType(ListTile)).first);
    await tester.pumpAndSettle();
  }
}
ExpenseGroup group([String id = 'group-id']) => ExpenseGroup.fromJson({'groupId': id, 'groupName': 'Trip',
  'owner': aliceJson, 'members': [aliceJson, bobJson], 'currencyMode': 'single',
  'defaultCurrency': {'code': usd.code, 'name': usd.name, 'minorUnits': usd.minorUnits}, 'currencyLocked': false});

Future<GoRouter> mountEditor(WidgetTester tester, SessionController session, FakeReceiptMedia media, {String? receiptId}) async {
  await session.login('alice', 'password');
  final router = GoRouter(routes: [GoRoute(path: '/', builder: (context, state) => Scaffold(
    body: SingleChildScrollView(padding: const EdgeInsets.all(16), child: GroupScope(group: group(),
      updateGroup: (_) {}, refreshGroup: () async {}, recordChanges: RecordChangeTracker(),
      child: ReceiptEditor(repository: session.repository, session: session, groupId: 'group-id', receiptId: receiptId, media: media)))))]);
  await tester.pumpWidget(MaterialApp.router(routerConfig: router)); await tester.pumpAndSettle(); return router;
}

final _receiptPhotoTransform = find.byKey(const ValueKey('receipt-photo-transform'));

class _ReceiptPhotoHarness {
  final page = ScrollController(initialScrollOffset: 240);
  final provider = ValueNotifier(MemoryImage(testImage.bytes));
  final width = ValueNotifier(560.0);

  Finder get viewport => find.ancestor(of: _receiptPhotoTransform, matching: find.byType(ClipRect)).first;
  Matrix4 matrix(WidgetTester tester) => tester.widget<Transform>(_receiptPhotoTransform).transform.clone();
  Rect bounds(WidgetTester tester) => tester.getRect(viewport);

  Future<void> mount(WidgetTester tester) async {
    addTearDown(() async {
      await tester.pumpWidget(const SizedBox());
      page.dispose(); provider.dispose(); width.dispose();
    });
    await tester.pumpWidget(MaterialApp(home: Scaffold(body: Shortcuts(
      shortcuts: const {
        SingleActivator(LogicalKeyboardKey.arrowDown): ScrollIntent(direction: AxisDirection.down),
        SingleActivator(LogicalKeyboardKey.arrowUp): ScrollIntent(direction: AxisDirection.up),
      },
      child: SingleChildScrollView(controller: page, physics: const ClampingScrollPhysics(), child: Column(children: [
        const SizedBox(height: 320),
        Center(child: ValueListenableBuilder<double>(valueListenable: width, builder: (context, value, _) => SizedBox(
          width: value,
          child: ValueListenableBuilder<MemoryImage>(valueListenable: provider,
            builder: (context, value, _) => ReceiptImagePreview(provider: value)),
        ))),
        const SizedBox(height: 1200),
      ])),
    ))));
    await tester.pumpAndSettle();
    expect(page.offset, 240);
    final screen = Offset.zero & tester.view.physicalSize / tester.view.devicePixelRatio;
    final preview = tester.getRect(find.byType(ReceiptImagePreview));
    expect(preview.top, greaterThanOrEqualTo(screen.top));
    expect(preview.bottom, lessThanOrEqualTo(screen.bottom));
  }

  Future<void> focus(WidgetTester tester) async {
    Focus.of(tester.element(viewport)).requestFocus();
    await tester.pumpAndSettle();
  }

  Future<void> button(WidgetTester tester, String label) async {
    final offset = page.offset;
    await tester.tap(find.widgetWithText(TextButton, label));
    await tester.pumpAndSettle();
    expect(page.offset, closeTo(offset, 0.001));
    expectBounded(tester);
  }

  void expectBounded(WidgetTester tester) {
    final value = matrix(tester);
    final scale = value.getMaxScaleOnAxis();
    final translation = value.getTranslation();
    final size = tester.getSize(viewport);
    expect(value.storage.every((entry) => entry.isFinite), isTrue);
    expect(scale, inInclusiveRange(1.0, 8.0));
    expect(translation.x, inInclusiveRange(size.width * (1 - scale) - 0.001, 0.001));
    expect(translation.y, inInclusiveRange(size.height * (1 - scale) - 0.001, 0.001));
    if (scale == 1) expect(value, Matrix4.identity());
  }
}

Future<({TestGesture first, TestGesture second})> _startReceiptPhotoPinch(
  WidgetTester tester, _ReceiptPhotoHarness photo,
) async {
  final center = photo.bounds(tester).center;
  final offset = photo.page.offset;
  final first = await tester.startGesture(center - const Offset(40, 0));
  final second = await tester.startGesture(center + const Offset(40, 0));
  for (final span in [80.0, 120.0, 160.0]) {
    await first.moveTo(center - Offset(span, 0));
    await second.moveTo(center + Offset(span, 0));
    await tester.pump();
    expect(photo.page.offset, closeTo(offset, 0.001));
    photo.expectBounded(tester);
  }
  expect(photo.matrix(tester).getMaxScaleOnAxis(), greaterThan(2));
  return (first: first, second: second);
}

Future<void> _scrollReceiptPhotoWithTouch(WidgetTester tester, _ReceiptPhotoHarness photo) async {
  final offset = photo.page.offset;
  final gesture = await tester.startGesture(photo.bounds(tester).center);
  await gesture.moveBy(const Offset(0, -40));
  await gesture.moveBy(const Offset(0, -60));
  await tester.pump();
  expect(photo.page.offset, greaterThan(offset));
  expect(photo.matrix(tester), Matrix4.identity());
  await gesture.up();
  await tester.pumpAndSettle();
}

Finder _receiptReviewInput(TextEditingController controller) => find.byWidgetPredicate(
  (widget) => widget is TextField && widget.controller == controller,
);

Future<void> _editReceiptReviewInput(WidgetTester tester, TextEditingController controller, String value) async {
  final field = _receiptReviewInput(controller);
  await tester.ensureVisible(field); await tester.enterText(field, value); await tester.pumpAndSettle();
}

Future<void> _chooseReceiptReviewOption(WidgetTester tester, String label, String value) async {
  final field = find.widgetWithText(DropdownButtonFormField<String>, label);
  await tester.ensureVisible(field); await tester.tap(field); await tester.pumpAndSettle();
  await tapText(tester, value);
}

Future<void> _openReceiptDatePicker(WidgetTester tester) async {
  final button = find.byTooltip('Choose receipt date');
  await tester.ensureVisible(button); await tester.tap(button); await tester.pumpAndSettle();
  expect(find.byType(CalendarDatePicker), findsOneWidget);
}

Future<void> _enterReceiptPickerDate(WidgetTester tester, DateTime date) async {
  final dialog = find.byType(DatePickerDialog);
  final localizations = MaterialLocalizations.of(tester.element(dialog));
  await tester.tap(find.byTooltip(localizations.inputDateModeButtonLabel)); await tester.pumpAndSettle();
  final field = find.descendant(of: find.byType(InputDatePickerFormField), matching: find.byType(TextField));
  await tester.enterText(field, localizations.formatCompactDate(date)); await tester.pumpAndSettle();
}

Future<void> _finishReceiptDatePicker(WidgetTester tester, {bool cancel = false}) async {
  final dialog = find.byType(DatePickerDialog);
  final localizations = MaterialLocalizations.of(tester.element(dialog));
  await tester.tap(find.descendant(of: dialog, matching: find.widgetWithText(TextButton,
    cancel ? localizations.cancelButtonLabel : localizations.okButtonLabel)));
  await tester.pumpAndSettle();
  expect(find.byType(DatePickerDialog), findsNothing);
}

class _ReceiptChoiceHarness {
  _ReceiptChoiceHarness({Map<String, Object?>? review, Map<String, Object?>? evidence}) {
    session = roleSession(records: (request) async {
      if (request.url.path.endsWith('/receipt-extractions/options')) return jsonResponse(options());
      if (request.url.path.endsWith('/receipt-extractions')) { reads++; return jsonResponse(extraction(evidence: evidence)); }
      return null;
    });
    controller = ReceiptImportController(repository: session.repository, session: session, groupId: 'group-id', media: media);
    if (review != null) {
      controller.catalog = [usd]; controller.restoreReview(ReceiptImportReview.fromJson(review), accepted: false);
    }
  }

  late final SessionController session;
  late final ReceiptImportController controller;
  final media = FakeReceiptMedia()..next = testImage;
  int reads = 0, changes = 0, uses = 0;
  List<BigInt>? amounts;

  Future<void> read(WidgetTester tester) async {
    final context = await mountContext(tester);
    await controller.loadOptions(); await controller.selectImage(context); await controller.extract();
    expect(reads, 1);
  }

  Widget fields(ReceiptImportController current, {bool reviewOnly = false}) => ListenableBuilder(listenable: current, builder: (context, _) =>
    reviewOnly && current.stage != ReceiptImportStage.review ? const SizedBox() :
      ReceiptImportReviewFields(controller: current, onChanged: () => changes++, onUse: () {
        amounts = current.acceptReview().map((row) => row.amountMinor).toList(); uses++;
      }));

  Future<void> mount(WidgetTester tester) async {
    await tester.pumpWidget(MaterialApp(home: Scaffold(body: SingleChildScrollView(child: fields(controller)))));
    await tester.pumpAndSettle();
  }

  void dispose() { controller.dispose(); session.dispose(); }
}

class _CameraReceiptMedia extends FakeReceiptMedia {
  int captures = 0;
  @override
  bool get cameraSupported => true;
  @override
  Future<ReceiptImageInput?> takePhoto(BuildContext context, {required int maxBytes}) async {
    captures++; return next;
  }
}

const _photoActions = ['View photo', 'Review photo', 'Read again', 'Replace photo', 'Take photo', 'Remove photo'];
Finder _photoAction(String label) => find.widgetWithText(OutlinedButton, label);

void _expectPhotoActions(WidgetTester tester, List<String> available) {
  for (final label in _photoActions) {
    expect(_photoAction(label), available.contains(label) ? findsOneWidget : findsNothing, reason: label);
  }
}

bool _photoActionHasFocus(String label) {
  var found = false;
  FocusManager.instance.primaryFocus?.context?.visitAncestorElements((element) {
    if (_photoAction(label).evaluate().contains(element)) { found = true; return false; }
    return true;
  });
  return found;
}

Future<void> _tabToPhotoAction(WidgetTester tester, String label) async {
  await tester.ensureVisible(_photoAction(label));
  for (var count = 0; count < 80 && !_photoActionHasFocus(label); count++) {
    await tester.sendKeyEvent(LogicalKeyboardKey.tab); await tester.pumpAndSettle();
  }
  expect(_photoActionHasFocus(label), isTrue, reason: '$label is keyboard reachable');
}

class _PhotoActionHarness {
  _PhotoActionHarness({this.saved = false}) {
    session = roleSession(records: (request) async {
      final path = request.url.path;
      if (path.endsWith('/receipt-extractions/options')) return jsonResponse(options());
      if (path.endsWith('/receipt-extractions')) {
        reads++; return pendingRead?.future ?? Future.value(jsonResponse(extraction(title: suggestion, id: '00000000-0000-0000-0000-${reads.toString().padLeft(12, '0')}')));
      }
      if (path.endsWith('/receipts/receipt-id/source')) {
        sourceReads++; return jsonResponse(historical.toJson());
      }
      if (path.endsWith('/receipts/receipt-id') && request.method == 'GET') {
        return jsonResponse({'receiptId': 'receipt-id', 'groupId': 'group-id', 'title': 'Synthetic Market groceries', 'note': 'Saved receipt note',
          'uploadedBy': aliceJson, 'createdAt': '2026-10-01T00:00:00Z', 'records': [expenseFixture()],
          'source': {'mediaType': 'image/jpeg', 'width': 2, 'height': 2, 'byteCount': testImage.bytes.length,
            'sha256': List.filled(64, 'a').join(), 'hasReview': true}});
      }
      if ((request.method == 'POST' && path.endsWith('/receipts')) ||
          (request.method == 'PUT' && path.endsWith('/receipts/receipt-id'))) {
        writes.add(jsonDecode(request.body) as Map<String, dynamic>);
        return http.Response('Temporary save failure', 503);
      }
      return null;
    });
  }

  final bool saved;
  late final SessionController session;
  final media = _CameraReceiptMedia()..next = testImage;
  final writes = <Map<String, dynamic>>[];
  int reads = 0, sourceReads = 0;
  String? suggestion = 'Synthetic Market groceries';
  Completer<http.Response>? pendingRead;
  final historical = ReceiptSourceInput(image: testImage, extractionId: extractionId,
    evidence: ReceiptEvidence.fromJson(evidenceJson()),
    review: ReceiptImportReview.fromJson(reviewJson(items: [], adjustments: [], overrides: {'mode': 'total'})));

  Finder get note => find.byWidgetPredicate((widget) => widget is TextField && widget.decoration?.labelText == 'Receipt note (optional)');
  ReceiptImportController review(WidgetTester tester) => tester.widget<ReceiptImportReviewFields>(find.byType(ReceiptImportReviewFields)).controller;

  Future<void> mount(WidgetTester tester, {bool untouchedTitle = false}) async {
    final router = await mountEditor(tester, session, media, receiptId: saved ? 'receipt-id' : null);
    addTearDown(() async { await tester.pumpWidget(const SizedBox()); router.dispose(); session.dispose(); });
    if (!saved && !untouchedTitle) await tester.enterText(find.widgetWithText(TextField, 'Receipt title'), 'Synthetic Market groceries');
  }

  Future<void> extract(WidgetTester tester) async {
    await tapText(tester, 'Upload photo'); await tapText(tester, 'Read receipt');
    expect(reads, 1);
  }

  Future<SplitFields> accept(WidgetTester tester) async {
    await extract(tester);
    chooseNoTip(review(tester)); review(tester).changed(); await tester.pumpAndSettle();
    await tapText(tester, 'Continue'); await openExpenseCards(tester);
    return tester.widget<SplitFields>(find.byType(SplitFields));
  }

  Future<void> setNote(WidgetTester tester) async {
    await tester.ensureVisible(note); await tester.enterText(note, 'Private receipt\n保留'); await tester.pumpAndSettle();
  }

  void expectNote(WidgetTester tester) {
    expect(tester.widget<TextField>(note).controller!.text, 'Private receipt\n保留');
    expect(tester.widget<TextField>(find.widgetWithText(TextField, 'Receipt title')).controller!.text, 'Synthetic Market groceries');
  }
}


void main() {
  test('extraction title is required nullable normalized and separate from evidence', () {
    final json = extraction(title: ' 会议 <b>Market</b> ');
    final result = ReceiptExtractionResponse.fromJson(json);
    expect(result.title, '会议 <b>Market</b>'); expect(result.promptVersion, 'receipt-v2');
    expect(result.evidence.toJson(), evidenceJson());
    expect(result.toJson()['title'], result.title);
    expect(ReceiptExtractionResponse.fromJson(extraction(title: null)).toJson()['title'], isNull);
    expect(ReceiptExtractionResponse.fromJson(extraction(title: List.filled(512, '😀').join())).title!.runes.length, 512);
    for (final invalid in <Object?>['', '   ', '\tMarket', 'Market\u0085', 'Market\u2028', 'Market\u2029', List.filled(513, '😀').join(), 1, true]) {
      expect(() => ReceiptExtractionResponse.fromJson({...json, 'title': invalid}), throwsFormatException);
    }
    expect(() => ReceiptExtractionResponse.fromJson({...json}..remove('title')), throwsFormatException);
    expect(() => ReceiptExtractionResponse.fromJson({...json, 'extra': true}), throwsFormatException);
    expect(() => ReceiptExtractionResponse.fromJson({...json, 'evidence': {}}), throwsFormatException);
    expect(() => ReceiptExtractionResponse.fromJson(evidenceJson()), throwsFormatException);
  });

  testWidgets('suggestion fills untouched title and survives reread replacement removal and failure', (tester) async {
    final h = _PhotoActionHarness(); await h.mount(tester, untouchedTitle: true); await h.setNote(tester);
    final title = find.widgetWithText(TextField, 'Receipt title');
    expect(tester.widget<TextField>(title).controller!.text, isEmpty);
    await h.extract(tester); h.expectNote(tester);
    final c = h.review(tester);
    expect(c.suggestedTitle, 'Synthetic Market groceries'); expect(c.evidence!.toJson(), evidenceJson());
    chooseNoTip(c); c.changed(); await tester.pumpAndSettle();
    expect(c.buildReview().toJson().containsKey('title'), isFalse);
    h.suggestion = 'Another visible purchase';
    await tapText(tester, 'Photo options'); await tapText(tester, 'Read again');
    expect(h.reads, 2); h.expectNote(tester);
    await tapText(tester, 'Replace photo');
    expect(c.suggestedTitle, isNull); h.expectNote(tester); expect(h.reads, 2);
    await tapText(tester, 'Read receipt'); expect(h.reads, 3); h.expectNote(tester);
    h.pendingRead = Completer<http.Response>();
    await tester.ensureVisible(_photoAction('Read again')); await tester.tap(_photoAction('Read again')); await tester.pump();
    h.pendingRead!.complete(http.Response('Temporary extraction failure', 503)); await tester.pumpAndSettle();
    h.expectNote(tester); expect(h.reads, 4);
    await tapText(tester, 'Remove photo'); await tapText(tester, 'Remove photo');
    h.expectNote(tester); expect(c.suggestedTitle, isNull);
    await tapText(tester, 'Save receipt');
    expect(h.writes.single['title'], 'Synthetic Market groceries'); h.expectNote(tester);
    expect(h.reads, 4);
  });

  testWidgets('clearing a suggested title is manual and never refills from another reading', (tester) async {
    final h = _PhotoActionHarness(); await h.mount(tester, untouchedTitle: true); await h.extract(tester);
    final title = find.widgetWithText(TextField, 'Receipt title');
    await tester.ensureVisible(title); await tester.enterText(title, ''); await tester.pumpAndSettle();
    final c = h.review(tester); c.changed(); await tester.pumpAndSettle();
    expect(tester.widget<TextField>(title).controller!.text, isEmpty);
    await tapText(tester, 'Photo options'); await tapText(tester, 'Read again');
    expect(tester.widget<TextField>(title).controller!.text, isEmpty);
    await tapText(tester, 'Replace photo'); await tapText(tester, 'Read receipt');
    expect(tester.widget<TextField>(title).controller!.text, isEmpty); expect(h.reads, 3);
  });

  testWidgets('saved title is retained through a different extraction suggestion', (tester) async {
    final h = _PhotoActionHarness(saved: true)..suggestion = 'Another visible purchase';
    await h.mount(tester); await tapText(tester, 'Photo options'); await tapText(tester, 'Read again');
    await tapText(tester, 'Continue to review');
    expect(h.reads, 1); expect(h.review(tester).suggestedTitle, 'Another visible purchase');
    expect(tester.widget<TextField>(find.widgetWithText(TextField, 'Receipt title')).controller!.text, 'Synthetic Market groceries');
  });

  for (final manualTitle in ['Typed title', '']) {
    testWidgets('manual title "$manualTitle" prevents refill on reread and replacement', (tester) async {
      final h = _PhotoActionHarness(); await h.mount(tester, untouchedTitle: true);
      final title = find.widgetWithText(TextField, 'Receipt title');
      await tester.enterText(title, 'Typed title');
      if (manualTitle.isEmpty) await tester.enterText(title, '');
      await h.extract(tester);
      expect(tester.widget<TextField>(title).controller!.text, manualTitle);
      await tapText(tester, 'Photo options'); await tapText(tester, 'Read again');
      expect(tester.widget<TextField>(title).controller!.text, manualTitle);
      await tapText(tester, 'Replace photo'); await tapText(tester, 'Read receipt');
      expect(tester.widget<TextField>(title).controller!.text, manualTitle);
      expect(h.reads, 3); expect(h.writes, isEmpty);
    });
  }

  testWidgets('null suggestion leaves title editable and later extraction may fill untouched blank', (tester) async {
    final h = _PhotoActionHarness()..suggestion = null;
    await h.mount(tester, untouchedTitle: true); await h.extract(tester);
    final title = find.widgetWithText(TextField, 'Receipt title');
    final c = h.review(tester); chooseNoTip(c); c.changed(); await tester.pumpAndSettle();
    final review = c.buildReview().toJson();
    await tapText(tester, 'Continue');
    expect(find.text('Receipt title is required.'), findsOneWidget);
    expect(tester.widget<TextField>(title).focusNode!.hasFocus, isTrue);
    expect(c.acceptedReview, isNull); expect(c.buildReview().toJson(), review);
    expect(h.writes, isEmpty); expect(h.reads, 1);
    c.suggestedTitle = 'Late notification is not a new extraction'; c.changed(); await tester.pumpAndSettle();
    expect(tester.widget<TextField>(title).controller!.text, isEmpty);
    h.suggestion = 'Synthetic Market groceries';
    await tapText(tester, 'Photo options'); await tapText(tester, 'Read again');
    expect(tester.widget<TextField>(title).controller!.text, 'Synthetic Market groceries'); expect(h.reads, 2);
    chooseNoTip(c); c.changed(); await tester.pumpAndSettle(); await tapText(tester, 'Continue');
    expect(c.acceptedReview, isNotNull); expect(c.source!.toJson().containsKey('title'), isFalse);
  });

  testWidgets('controller clears suggestions with review without changing financial source or allocations', (tester) async {
    final h = _ReceiptChoiceHarness(); await h.read(tester);
    final c = h.controller;
    expect(c.suggestedTitle, 'Synthetic Market groceries');
    chooseNoTip(c); final allocations = c.acceptReview();
    expect(allocations.single.amountMinor, BigInt.from(2970));
    expect(c.source!.evidence!.toJson(), evidenceJson());
    expect(c.source!.toJson().containsKey('title'), isFalse);
    c.useManualAttachment(); expect(c.suggestedTitle, isNull);
    await c.extract(); expect(c.suggestedTitle, 'Synthetic Market groceries');
    final context = tester.element(find.byType(SizedBox));
    await c.selectImage(context); expect(c.suggestedTitle, isNull);
    await c.extract(); expect(c.suggestedTitle, 'Synthetic Market groceries');
    c.clear(); expect(c.suggestedTitle, isNull);
    await c.selectImage(context); await c.extract(); expect(c.suggestedTitle, isNotNull);
    h.session.expire(); expect(c.suggestedTitle, isNull);
    h.dispose();
  });

  testWidgets('receipt Save rejects invalid titles without changing the editable draft', (tester) async {
    final h = _PhotoActionHarness(); await h.mount(tester);
    await h.setNote(tester);
    final title = find.widgetWithText(TextField, 'Receipt title');
    final field = tester.widget<TextField>(title);
    for (final invalid in ['', '   ', '\tMarket', 'Market\u0085', 'Market\u2028', 'Market\u2029', List.filled(513, '😀').join()]) {
      field.controller!.text = invalid; field.onChanged!(invalid); await tester.pumpAndSettle();
      await tapText(tester, 'Save receipt');
      expect(h.writes, isEmpty); expect(field.controller!.text, invalid);
      expect(field.focusNode!.hasFocus, isTrue);
      expect(tester.getRect(title).overlaps(Offset.zero & tester.view.physicalSize / tester.view.devicePixelRatio), isTrue);
      expect(tester.widget<TextField>(h.note).controller!.text, 'Private receipt\n保留');
    }
    const literal = '会议 <b>Market</b>';
    await tester.enterText(title, ' $literal '); await tapText(tester, 'Save receipt');
    expect(h.writes.single['title'], literal); expect(h.writes.single['note'], 'Private receipt\n保留');
    expect(h.writes.single['records'], isEmpty); expect(h.writes.single.containsKey('source'), isFalse);
    expect(field.controller!.text, ' $literal ');
    expect(find.text('Temporary save failure'), findsOneWidget);
    final form = tester.widget<FormLeaveGuard>(find.byType(FormLeaveGuard)).controller;
    expect(form.dirty, isTrue);
    final canLeave = form.canLeave(() => confirmAction(tester.element(title), 'Discard unsaved changes?', 'Discard?', action: 'Discard'));
    await tester.pumpAndSettle(); await tapText(tester, 'Cancel'); expect(await canLeave, isFalse);
    expect(field.controller!.text, ' $literal ');
    final emoji = List.filled(512, '😀').join();
    await tester.enterText(title, emoji); await tapText(tester, 'Save receipt');
    expect(h.writes, hasLength(2)); expect(h.writes.last['title'], emoji);
    expect(field.controller!.text, emoji);
  });

  testWidgets('Continue requires a title and title-only edits retain accepted financial review', (tester) async {
    final h = _PhotoActionHarness(); await h.mount(tester);
    final title = find.widgetWithText(TextField, 'Receipt title');
    await tester.enterText(title, '');
    await h.extract(tester);
    final c = h.review(tester); chooseNoTip(c); c.changed(); await tester.pumpAndSettle();
    final evidence = c.evidence;
    final image = c.image;
    final reviewJson = c.buildReview().toJson();
    await tapText(tester, 'Continue');
    expect(c.acceptedReview, isNull); expect(c.evidence, same(evidence)); expect(c.image, same(image));
    expect(c.buildReview().toJson(), reviewJson); expect(h.writes, isEmpty); expect(h.reads, 1);
    expect(find.text('Receipt title is required.'), findsOneWidget);
    expect(tester.widget<TextField>(title).focusNode!.hasFocus, isTrue);
    expect(tester.getRect(title).overlaps(Offset.zero & tester.view.physicalSize / tester.view.devicePixelRatio), isTrue);
    await tester.enterText(title, 'Synthetic Market groceries'); await tapText(tester, 'Continue');
    await openExpenseCards(tester);
    final accepted = c.acceptedReview;
    final expense = tester.widget<SplitFields>(find.byType(SplitFields)).controller;
    final allocation = expense.toDraft().toJson();
    await tapText(tester, 'I confirm the displayed payers and splits');
    await tester.ensureVisible(title); await tester.enterText(title, '会议 <b>Market</b>'); await tester.pumpAndSettle();
    expect(c.acceptedReview, same(accepted)); expect(expense.toDraft().toJson(), allocation);
    expect(c.acceptedReview!.totalTitle, reviewJson['totalTitle']);
    await tapText(tester, 'Save receipt');
    expect(h.writes.single['title'], '会议 <b>Market</b>');
    expect(h.writes.single['source']['review'], accepted!.toJson());
    expect(h.writes.single['records'][0], allocation); expect(h.reads, 1);
    expect(tester.widget<TextField>(title).controller!.text, '会议 <b>Market</b>');
  });

  testWidgets('receipt session teardown clears title note and title focus', (tester) async {
    final h = _PhotoActionHarness(); await h.mount(tester); await h.setNote(tester);
    final title = find.widgetWithText(TextField, 'Receipt title');
    await tester.ensureVisible(title); await tester.tap(title); await tester.pumpAndSettle();
    final field = tester.widget<TextField>(title), note = tester.widget<TextField>(h.note);
    expect(field.focusNode!.hasFocus, isTrue);
    h.session.expire(); await tester.pumpAndSettle();
    expect(field.controller!.text, isEmpty); expect(note.controller!.text, isEmpty);
    expect(field.focusNode!.hasFocus, isFalse); expect(h.writes, isEmpty);
  });

  testWidgets('fresh preview actions retain the selected photo on chooser and camera cancellation', (tester) async {
    final h = _PhotoActionHarness(); await h.mount(tester);
    await h.setNote(tester); await tapText(tester, 'Upload photo');
    expect(h.reads, 0); expect(h.writes, isEmpty);
    await tapText(tester, 'Photo options');
    _expectPhotoActions(tester, ['Replace photo', 'Take photo', 'Remove photo']);
    h.media.next = null;
    await tapText(tester, 'Replace photo'); expect(h.media.picks, 2);
    await tapText(tester, 'Take photo'); expect(h.media.captures, 1);
    await tapText(tester, 'Remove photo'); await tapText(tester, 'Cancel');
    h.expectNote(tester); expect(find.byType(ReceiptImagePreview), findsOneWidget);
    expect(find.text('Read receipt'), findsOneWidget); expect(h.reads, 0); expect(h.writes, isEmpty);
    await tapText(tester, 'Read receipt'); expect(h.reads, 1);
    expect(h.review(tester).image!.toJson(), testImage.toJson());
  });

  testWidgets('extracted review actions keep corrected fields until an explicit read again', (tester) async {
    final h = _PhotoActionHarness(); await h.mount(tester); await h.extract(tester);
    final c = h.review(tester);
    await _editReceiptReviewInput(tester, c.merchant, 'Corrected merchant');
    await _editReceiptReviewInput(tester, c.items.single.title, 'Corrected item');
    await h.setNote(tester); await tapText(tester, 'Photo options');
    _expectPhotoActions(tester, ['Read again', 'Replace photo', 'Take photo', 'Remove photo']);
    final image = c.image;
    h.media.next = null;
    await tapText(tester, 'Replace photo'); await tapText(tester, 'Take photo');
    await tapText(tester, 'Remove photo'); await tester.sendKeyEvent(LogicalKeyboardKey.escape); await tester.pumpAndSettle();
    expect(find.byType(AlertDialog), findsNothing);
    expect(c.merchant.text, 'Corrected merchant'); expect(c.items.single.title.text, 'Corrected item');
    expect(c.image, same(image)); h.expectNote(tester); expect(h.reads, 1); expect(h.writes, isEmpty);
    await tapText(tester, 'Read again'); expect(h.reads, 2); expect(h.writes, isEmpty);
  });

  testWidgets('accepted expenses survive canceled photo actions and review navigation without another read', (tester) async {
    final h = _PhotoActionHarness(); await h.mount(tester);
    final fields = await h.accept(tester); final c = fields.controller;
    c.title.text = 'Edited expense'; c.setActorUserId(bobId); fields.onChanged(); await tester.pumpAndSettle();
    await h.setNote(tester); await tapText(tester, 'Photo options');
    _expectPhotoActions(tester, ['Review photo', 'Read again', 'Replace photo', 'Take photo', 'Remove photo']);
    final picks = h.media.picks;
    for (final label in ['Review photo', 'Read again', 'Replace photo', 'Take photo', 'Remove photo']) {
      await tapText(tester, label); await tapText(tester, 'Cancel');
      expect(tester.widget<SplitFields>(find.byType(SplitFields)).controller, same(c));
      expect(c.title.text, 'Edited expense'); expect(c.actorUserId, bobId); expect(c.amount.text, '29.70');
      h.expectNote(tester); expect(h.reads, 1); expect(h.writes, isEmpty);
    }
    expect(h.media.picks, picks); expect(h.media.captures, 0);
    h.media.next = null;
    for (final label in ['Replace photo', 'Take photo']) {
      await tapText(tester, label); await tapText(tester, 'Continue to review');
      expect(tester.widget<SplitFields>(find.byType(SplitFields)).controller, same(c));
      expect(c.title.text, 'Edited expense'); h.expectNote(tester);
    }
    expect(h.media.picks, picks + 1); expect(h.media.captures, 1);
    await tapText(tester, 'Review photo'); await tapText(tester, 'Continue to review');
    expect(h.reads, 1); expect(h.review(tester).image!.toJson(), testImage.toJson());
    await tapText(tester, 'Cancel review changes and keep displayed expenses');
    expect(tester.widget<SplitFields>(find.byType(SplitFields)).controller, same(c));
    expect(c.title.text, 'Edited expense'); expect(c.actorUserId, bobId); h.expectNote(tester);
    await tapText(tester, 'Read again'); await tapText(tester, 'Continue to review');
    expect(h.reads, 2); expect(h.writes, isEmpty);
  });

  testWidgets('saved photo actions use one source callback and keep dirty expenses through canceled operations', (tester) async {
    final h = _PhotoActionHarness(saved: true); await h.mount(tester); await openExpenseCards(tester);
    final fields = tester.widget<SplitFields>(find.byType(SplitFields));
    fields.controller.title.text = 'Dirty saved expense'; fields.controller.amount.text = '13.00';
    fields.onChanged(); await tester.pumpAndSettle(); await h.setNote(tester);
    await tapText(tester, 'Photo options'); _expectPhotoActions(tester, _photoActions);
    expect(h.sourceReads, 0); expect(h.reads, 0); expect(h.writes, isEmpty);
    await _tabToPhotoAction(tester, 'View photo');
    await tester.sendKeyEvent(LogicalKeyboardKey.enter); await tester.pumpAndSettle();
    expect(h.sourceReads, 1); expect(find.byType(ReceiptImagePreview), findsOneWidget);
    await tester.sendKeyEvent(LogicalKeyboardKey.escape); await tester.pumpAndSettle();
    expect(find.byType(Dialog), findsNothing); expect(_photoActionHasFocus('View photo'), isTrue);
    await tester.sendKeyEvent(LogicalKeyboardKey.tab); await tester.pumpAndSettle();
    expect(_photoActionHasFocus('Review photo'), isTrue);
    await tester.sendKeyEvent(LogicalKeyboardKey.space); await tester.pumpAndSettle();
    expect(find.text('Replace displayed expenses?'), findsOneWidget);
    await tester.sendKeyEvent(LogicalKeyboardKey.escape); await tester.pumpAndSettle();
    expect(_photoActionHasFocus('Review photo'), isTrue); expect(h.sourceReads, 1);
    for (final label in ['Read again', 'Remove photo']) {
      await tapText(tester, label); await tapText(tester, 'Cancel');
    }
    h.media.next = null;
    await tapText(tester, 'Replace photo'); await tapText(tester, 'Take photo');
    expect(h.media.picks, 1); expect(h.media.captures, 1);
    expect(fields.controller.title.text, 'Dirty saved expense'); expect(fields.controller.amount.text, '13.00');
    h.expectNote(tester); expect(h.sourceReads, 1); expect(h.reads, 0); expect(h.writes, isEmpty);
    await tapText(tester, 'Review photo'); await tapText(tester, 'Continue to review');
    expect(h.sourceReads, 2); expect(h.reads, 0); expect(h.writes, isEmpty);
    await tapText(tester, 'Cancel review changes and keep displayed expenses');
    await openExpenseCards(tester);
    expect(tester.widget<SplitFields>(find.byType(SplitFields)).controller, same(fields.controller));
    expect(fields.controller.amount.text, '13.00'); h.expectNote(tester);
    _expectPhotoActions(tester, _photoActions);
    await tapText(tester, 'Read again'); await tapText(tester, 'Continue to review');
    expect(h.sourceReads, 3); expect(h.reads, 1); expect(h.writes, isEmpty);
  });

  testWidgets('pending read disables photo list actions and cannot start a second read', (tester) async {
    final h = _PhotoActionHarness(); await h.mount(tester); await h.extract(tester);
    await tapText(tester, 'Photo options'); h.pendingRead = Completer<http.Response>();
    await tester.ensureVisible(_photoAction('Read again')); await tester.tap(_photoAction('Read again'));
    await tester.pump(); await tester.pump(const Duration(milliseconds: 100));
    expect(h.reads, 2);
    for (final label in ['Read again', 'Replace photo', 'Take photo', 'Remove photo']) {
      expect(tester.widget<OutlinedButton>(_photoAction(label)).onPressed, isNull);
    }
    await tester.ensureVisible(_photoAction('Read again'));
    await tester.tap(_photoAction('Read again')); await tester.pump(); expect(h.reads, 2);
    expect(h.media.picks, 1); expect(h.media.captures, 0); expect(h.writes, isEmpty);
    h.pendingRead!.complete(jsonResponse(extraction())); await tester.pumpAndSettle();
    expect(tester.widget<OutlinedButton>(_photoAction('Read again')).onPressed, isNotNull);
    expect(h.reads, 2);
  });

  testWidgets('zoomed receipt photo pans with focused arrow keys without pan buttons', (tester) async {
    final photo = _ReceiptPhotoHarness();
    await photo.mount(tester);
    await photo.focus(tester);
    final initialOffset = photo.page.offset;
    await tester.sendKeyEvent(LogicalKeyboardKey.arrowDown); await tester.pumpAndSettle();
    expect(photo.matrix(tester), Matrix4.identity());
    expect(photo.page.offset, greaterThan(initialOffset));
    photo.page.jumpTo(initialOffset); await tester.pumpAndSettle();
    await photo.button(tester, 'Zoom in');
    await photo.focus(tester);
    final offset = photo.page.offset;
    final before = photo.matrix(tester).getTranslation();
    await tester.sendKeyEvent(LogicalKeyboardKey.arrowRight); await tester.pump();
    expect(photo.matrix(tester).getTranslation().x, closeTo(before.x - 60, 0.001));
    await tester.sendKeyEvent(LogicalKeyboardKey.arrowDown); await tester.pump();
    expect(photo.matrix(tester).getTranslation().y, closeTo(before.y - 60, 0.001));
    for (final key in [LogicalKeyboardKey.arrowRight, LogicalKeyboardKey.arrowDown]) {
      for (var n = 0; n < 20; n++) { await tester.sendKeyEvent(key); }
    }
    await tester.pumpAndSettle();
    final size = tester.getSize(photo.viewport);
    final lower = photo.matrix(tester).getTranslation();
    expect(lower.x, closeTo(-size.width, 0.001));
    expect(lower.y, closeTo(-size.height, 0.001));
    for (final key in [LogicalKeyboardKey.arrowLeft, LogicalKeyboardKey.arrowUp]) {
      for (var n = 0; n < 20; n++) { await tester.sendKeyEvent(key); }
    }
    await tester.pumpAndSettle();
    final upper = photo.matrix(tester).getTranslation();
    expect(upper.x, closeTo(0, 0.001)); expect(upper.y, closeTo(0, 0.001));
    expect(photo.page.offset, closeTo(offset, 0.001));
    photo.expectBounded(tester);
    FocusManager.instance.primaryFocus?.unfocus(); await tester.pump();
    final unfocused = photo.matrix(tester);
    await tester.sendKeyEvent(LogicalKeyboardKey.arrowRight); await tester.pumpAndSettle();
    expect(photo.matrix(tester), unfocused);
  });

  testWidgets('mouse and trackpad wheel scroll the page over photo edges and controls at either zoom', (tester) async {
    final photo = _ReceiptPhotoHarness();
    await photo.mount(tester);
    for (final zoomed in [false, true]) {
      if (zoomed) await photo.button(tester, 'Zoom in');
      for (final kind in [PointerDeviceKind.mouse, PointerDeviceKind.trackpad]) {
        for (final target in ['center', 'left edge', 'right edge', 'zoom control']) {
          for (final dy in [40.0, -40.0]) {
            final rect = photo.bounds(tester);
            final position = switch (target) {
              'center' => rect.center,
              'left edge' => Offset(rect.left + 1, rect.center.dy),
              'right edge' => Offset(rect.right - 1, rect.center.dy),
              _ => tester.getCenter(find.widgetWithText(TextButton, 'Zoom in')),
            };
            final before = photo.matrix(tester);
            final offset = photo.page.offset;
            await tester.sendEventToBinding(PointerScrollEvent(kind: kind, position: position, scrollDelta: Offset(0, dy)));
            await tester.pumpAndSettle();
            expect(photo.matrix(tester), before, reason: '$kind $target at zoomed=$zoomed');
            expect(photo.page.offset, dy > 0 ? greaterThan(offset) : lessThan(offset));
          }
        }
      }
    }
  });

  testWidgets('one finger scrolls at fit size and a later second finger cannot steal the page drag', (tester) async {
    final photo = _ReceiptPhotoHarness();
    await photo.mount(tester);
    await _scrollReceiptPhotoWithTouch(tester, photo);
    photo.page.jumpTo(240); await tester.pumpAndSettle();
    final center = photo.bounds(tester).center;
    final first = await tester.startGesture(center);
    await first.moveBy(const Offset(0, -40));
    await first.moveBy(const Offset(0, -40));
    await tester.pump();
    expect(photo.page.offset, greaterThan(240));
    final offset = photo.page.offset;
    final second = await tester.startGesture(center + const Offset(40, -40));
    await first.moveBy(const Offset(-40, -30));
    await second.moveBy(const Offset(80, -30));
    await first.moveBy(const Offset(0, -40));
    await tester.pump();
    expect(photo.matrix(tester), Matrix4.identity());
    expect(photo.page.offset, greaterThan(offset));
    await second.up(); await first.up(); await tester.pumpAndSettle();
  });

  testWidgets('two-finger pinch owns the photo through lifting one finger and subsequent zoom out', (tester) async {
    final photo = _ReceiptPhotoHarness();
    await photo.mount(tester);
    final offset = photo.page.offset;
    final pinch = await _startReceiptPhotoPinch(tester, photo);
    final before = photo.matrix(tester);
    await pinch.second.up();
    await pinch.first.moveBy(const Offset(0, -20));
    await pinch.first.moveBy(const Offset(0, -40));
    await tester.pump();
    expect(photo.matrix(tester).getTranslation().y, lessThan(before.getTranslation().y));
    expect(photo.page.offset, closeTo(offset, 0.001));
    photo.expectBounded(tester);
    await pinch.first.up(); await tester.pumpAndSettle();
    final actualScale = photo.matrix(tester).getMaxScaleOnAxis();
    await photo.button(tester, 'Zoom out');
    expect(photo.matrix(tester).getMaxScaleOnAxis(), closeTo(actualScale * 0.5, 0.001));
  });

  testWidgets('pinch reaching fit size retains ownership until release then fresh touch scrolls', (tester) async {
    final photo = _ReceiptPhotoHarness();
    await photo.mount(tester);
    final center = photo.bounds(tester).center;
    final offset = photo.page.offset;
    final pinch = await _startReceiptPhotoPinch(tester, photo);
    await pinch.first.moveTo(center - const Offset(20, 0));
    await pinch.second.moveTo(center + const Offset(20, 0));
    await tester.pump();
    expect(photo.matrix(tester), Matrix4.identity());
    await pinch.first.moveBy(const Offset(0, -30));
    await pinch.second.moveBy(const Offset(0, -30));
    await tester.pump();
    expect(photo.matrix(tester), Matrix4.identity());
    expect(photo.page.offset, closeTo(offset, 0.001));
    await pinch.second.up();
    await pinch.first.moveBy(const Offset(0, -20));
    await pinch.first.moveBy(const Offset(0, -40));
    await tester.pump();
    expect(photo.matrix(tester), Matrix4.identity());
    expect(photo.page.offset, closeTo(offset, 0.001));
    await pinch.first.up(); await tester.pumpAndSettle();
    await _scrollReceiptPhotoWithTouch(tester, photo);
  });

  testWidgets('cancelled photo pinch releases ownership for a fresh fit-size page drag', (tester) async {
    final photo = _ReceiptPhotoHarness();
    await photo.mount(tester);
    final pinch = await _startReceiptPhotoPinch(tester, photo);
    await pinch.first.cancel(); await pinch.second.cancel(); await tester.pumpAndSettle();
    await photo.button(tester, 'Reset image zoom');
    await _scrollReceiptPhotoWithTouch(tester, photo);
  });

  testWidgets('browser pinch scales about its local position and clamps without scrolling the page', (tester) async {
    final photo = _ReceiptPhotoHarness();
    await photo.mount(tester);
    final rect = photo.bounds(tester);
    final local = Offset(rect.width * 0.25, rect.height * 0.75);
    final offset = photo.page.offset;
    Future<void> scale(double factor) async {
      await tester.sendEventToBinding(PointerScaleEvent(kind: PointerDeviceKind.trackpad,
        position: rect.topLeft + local, scale: factor));
      await tester.pumpAndSettle();
      expect(photo.page.offset, closeTo(offset, 0.001));
      photo.expectBounded(tester);
    }
    await scale(1.5);
    expect(photo.matrix(tester).getMaxScaleOnAxis(), closeTo(1.5, 0.001));
    expect(photo.matrix(tester).getTranslation().x, closeTo(local.dx * -0.5, 0.001));
    expect(photo.matrix(tester).getTranslation().y, closeTo(local.dy * -0.5, 0.001));
    await scale(2);
    expect(photo.matrix(tester).getMaxScaleOnAxis(), closeTo(3, 0.001));
    await scale(100);
    expect(photo.matrix(tester).getMaxScaleOnAxis(), 8);
    await scale(0.001);
    expect(photo.matrix(tester), Matrix4.identity());
  });

  testWidgets('native trackpad pan zoom transforms only the photo and ends cleanly', (tester) async {
    final photo = _ReceiptPhotoHarness();
    await photo.mount(tester);
    final center = photo.bounds(tester).center;
    final offset = photo.page.offset;
    final gesture = await tester.startGesture(center, kind: PointerDeviceKind.trackpad);
    await gesture.panZoomUpdate(center, scale: 1.2);
    await gesture.panZoomUpdate(center, scale: 2.4);
    await tester.pump();
    expect(photo.matrix(tester).getMaxScaleOnAxis(), greaterThan(1));
    expect(photo.page.offset, closeTo(offset, 0.001));
    final before = photo.matrix(tester);
    await gesture.panZoomUpdate(center, scale: 2.4, pan: const Offset(-40, -30), rotation: 0.5);
    await tester.pump();
    expect(photo.matrix(tester).getTranslation().x, lessThan(before.getTranslation().x));
    expect(photo.matrix(tester).getTranslation().y, lessThan(before.getTranslation().y));
    expect(photo.matrix(tester).entry(0, 1), 0); expect(photo.matrix(tester).entry(1, 0), 0);
    expect(photo.page.offset, closeTo(offset, 0.001));
    photo.expectBounded(tester);
    await gesture.panZoomEnd(); await tester.pumpAndSettle();
    await photo.button(tester, 'Reset image zoom');
    await _scrollReceiptPhotoWithTouch(tester, photo);
  });

  testWidgets('zoom buttons use actual scale preserve the viewport center and never jump the page', (tester) async {
    final photo = _ReceiptPhotoHarness();
    await photo.mount(tester);
    final rect = photo.bounds(tester);
    await tester.sendEventToBinding(PointerScaleEvent(kind: PointerDeviceKind.trackpad,
      position: rect.topLeft + Offset(rect.width * 0.25, rect.height * 0.25), scale: 3));
    await tester.pumpAndSettle();
    await photo.button(tester, 'Zoom out');
    expect(photo.matrix(tester).getMaxScaleOnAxis(), closeTo(1.5, 0.001));
    final before = photo.matrix(tester);
    final center = Offset(rect.width / 2, rect.height / 2);
    final oldTranslation = before.getTranslation();
    final scene = (center - Offset(oldTranslation.x, oldTranslation.y)) / before.getMaxScaleOnAxis();
    await photo.button(tester, 'Zoom in');
    final after = photo.matrix(tester);
    expect(after.getMaxScaleOnAxis(), closeTo(3, 0.001));
    expect(after.getTranslation().x, closeTo(center.dx - scene.dx * 3, 0.001));
    expect(after.getTranslation().y, closeTo(center.dy - scene.dy * 3, 0.001));
    for (final expected in [6.0, 8.0, 8.0]) {
      await photo.button(tester, 'Zoom in');
      expect(photo.matrix(tester).getMaxScaleOnAxis(), expected);
    }
    for (final expected in [4.0, 2.0, 1.0, 1.0]) {
      await photo.button(tester, 'Zoom out');
      expect(photo.matrix(tester).getMaxScaleOnAxis(), expected);
    }
    expect(photo.matrix(tester), Matrix4.identity());
    await photo.button(tester, 'Zoom in');
    await photo.button(tester, 'Reset image zoom');
    expect(photo.matrix(tester), Matrix4.identity());
  });

  testWidgets('a drag begun zoomed pans only the photo and provider replacement resets its transform', (tester) async {
    final photo = _ReceiptPhotoHarness();
    await photo.mount(tester);
    await photo.button(tester, 'Zoom in');
    final offset = photo.page.offset;
    final before = photo.matrix(tester);
    final gesture = await tester.startGesture(photo.bounds(tester).center);
    await gesture.moveBy(const Offset(-40, -30));
    await gesture.moveBy(const Offset(-40, -30));
    await tester.pump();
    expect(photo.matrix(tester).getTranslation().x, lessThan(before.getTranslation().x));
    expect(photo.matrix(tester).getTranslation().y, lessThan(before.getTranslation().y));
    expect(photo.page.offset, closeTo(offset, 0.001));
    photo.expectBounded(tester);
    await gesture.up(); await tester.pumpAndSettle();
    photo.width.value = 240;
    photo.provider.value = MemoryImage(testImage.bytes);
    await tester.pumpAndSettle();
    expect(photo.matrix(tester), Matrix4.identity());
    expect(photo.page.offset, closeTo(offset, 0.001));
    expect(tester.takeException(), isNull);
  });

  testWidgets('resizing a zoomed photo clamps immediately settles and retains an active drag', (tester) async {
    final photo = _ReceiptPhotoHarness();
    await photo.mount(tester);
    await photo.button(tester, 'Zoom in'); await photo.button(tester, 'Zoom in');
    await photo.focus(tester);
    for (var n = 0; n < 40; n++) {
      await tester.sendKeyEvent(LogicalKeyboardKey.arrowRight);
      await tester.sendKeyEvent(LogicalKeyboardKey.arrowDown);
    }
    await tester.pumpAndSettle();
    final offset = photo.page.offset;
    photo.width.value = 240;
    await tester.pump();
    photo.expectBounded(tester);
    expect(photo.matrix(tester).getTranslation().x, closeTo(-720, 0.001));
    final clamped = photo.matrix(tester);
    await tester.pump(); await tester.pump(const Duration(milliseconds: 100));
    expect(photo.matrix(tester), clamped);
    expect(tester.binding.hasScheduledFrame, isFalse);
    expect(tester.takeException(), isNull);
    final gesture = await tester.startGesture(photo.bounds(tester).center);
    await gesture.moveBy(const Offset(40, 0)); await tester.pump();
    photo.width.value = 320;
    await tester.pump();
    final before = photo.matrix(tester);
    await gesture.moveBy(const Offset(40, 30)); await tester.pump();
    expect(photo.matrix(tester).getTranslation().x, greaterThan(before.getTranslation().x));
    expect(photo.matrix(tester).getTranslation().y, greaterThan(before.getTranslation().y));
    expect(photo.page.offset, closeTo(offset, 0.001));
    photo.expectBounded(tester);
    await gesture.up(); await tester.pumpAndSettle();
    expect(tester.binding.hasScheduledFrame, isFalse);
    expect(tester.takeException(), isNull);
  });

  for (final scenario in [
    (size: const Size(195, 422), textScale: 1.0),
    (size: const Size(390, 420), textScale: 1.0),
    (size: const Size(390, 844), textScale: 2.0),
  ]) {
    testWidgets('review disclosures retain populated fields without overlap at $scenario', (tester) async {
      await tester.binding.setSurfaceSize(scenario.size);
      addTearDown(() => tester.binding.setSurfaceSize(null));
      final session = roleSession();
      final c = ReceiptImportController(repository: session.repository, session: session, groupId: 'group-id', media: FakeReceiptMedia());
      c.catalog = [usd]; c.restoreReview(ReceiptImportReview.fromJson(reviewJson()));
      c.items.first.quantity.text = '2'; c.items.first.unit.text = 'kg'; c.items.first.unitPrice.text = '5.00';
      final accepted = c.acceptedReview;
      await tester.pumpWidget(MaterialApp(home: Builder(builder: (context) => MediaQuery(
        data: MediaQuery.of(context).copyWith(textScaler: TextScaler.linear(scenario.textScale)),
        child: Scaffold(body: SingleChildScrollView(child: ListenableBuilder(listenable: c,
          builder: (context, _) => ReceiptImportReviewFields(controller: c, onUse: () {}, onChanged: () {}))))))));
      await tester.pumpAndSettle();
      final item = find.byKey(ValueKey<Object>(c.items.first));
      final header = find.descendant(of: item, matching: find.byType(ListTile)).first;
      await tester.ensureVisible(header); await tester.tap(header); await tester.pumpAndSettle();
      final quantity = find.byWidgetPredicate((widget) => widget is TextField && widget.controller == c.items.first.quantity);
      await tester.ensureVisible(quantity); await tester.tap(quantity); await tester.pumpAndSettle();
      expect(tester.getRect(quantity).overlaps(tester.getRect(header)), isFalse);
      await tester.ensureVisible(header); await tester.tap(header); await tester.pumpAndSettle();
      await tester.tap(header); await tester.pumpAndSettle();
      expect(c.items.first.quantity.text, '2'); expect(c.items.first.unit.text, 'kg'); expect(c.items.first.unitPrice.text, '5.00');
      await tapText(tester, 'Receipt details');
      final receipt = find.byKey(const ValueKey<Object>('receipt'));
      final receiptHeader = find.descendant(of: receipt, matching: find.byType(ListTile)).first;
      final mode = find.widgetWithText(DropdownButtonFormField<String>, 'Record as');
      await tester.ensureVisible(mode); await tester.pumpAndSettle();
      expect(tester.getRect(mode).overlaps(tester.getRect(receiptHeader)), isFalse);
      await tapText(tester, 'Receipt details'); await tapText(tester, 'Receipt details');
      expect(c.mode, 'itemized'); expect(c.acceptedReview, same(accepted));
      expect(tester.takeException(), isNull);
      await tester.pumpWidget(const SizedBox()); c.dispose(); session.dispose();
    });
  }

  testWidgets('Continue reveals and focuses invalid optional item details', (tester) async {
    final session = roleSession();
    final c = ReceiptImportController(repository: session.repository, session: session, groupId: 'group-id', media: FakeReceiptMedia());
    c.catalog = [usd]; c.restoreReview(ReceiptImportReview.fromJson(reviewJson()));
    c.items.first.quantity.text = 'unreadable';
    var accepted = false;
    await tester.pumpWidget(MaterialApp(home: Scaffold(body: SingleChildScrollView(child:
      ReceiptImportReviewFields(controller: c, onUse: () => accepted = true, onChanged: () {})))));
    await tester.pumpAndSettle();
    final details = find.byKey(ValueKey<Object>(c.items.first));
    expect(tester.widget<ExpansionTile>(details).controller!.isExpanded, isFalse);
    await tapText(tester, 'Continue');
    expect(accepted, isFalse);
    expect(tester.widget<ExpansionTile>(details).controller!.isExpanded, isTrue);
    final quantity = find.byWidgetPredicate((widget) => widget is TextField && widget.controller == c.items.first.quantity);
    expect(tester.widget<TextField>(quantity).focusNode!.hasFocus, isTrue);
    expect(tester.getRect(quantity).overlaps(Offset.zero & tester.view.physicalSize / tester.view.devicePixelRatio), isTrue);
    await tester.pumpWidget(const SizedBox()); c.dispose(); session.dispose();
  });

  testWidgets('blank receipt date picker cancel and Escape preserve the draft and acknowledgement', (tester) async {
    final h = _ReceiptChoiceHarness(evidence: evidenceJson()
      ..['issues'] = [{'field': 'merchant', 'reason': 'Check merchant'}]);
    await h.read(tester);
    final c = h.controller; chooseNoTip(c); c.acknowledgeWarnings(true);
    final accepted = c.buildReview(); c.acceptedReview = accepted; c.date.clear();
    final acknowledged = {...c.acknowledgedWarnings}; expect(acknowledged, isNotEmpty);
    await h.mount(tester);
    for (final escape in [false, true]) {
      await _openReceiptDatePicker(tester);
      expect(c.date.text, isEmpty); expect(h.changes, 0);
      if (escape) {
        await tester.sendKeyEvent(LogicalKeyboardKey.escape); await tester.pumpAndSettle();
        expect(find.byType(DatePickerDialog), findsNothing);
      } else {
        await _finishReceiptDatePicker(tester, cancel: true);
      }
      expect(c.date.text, isEmpty); expect(c.acknowledgedWarnings, acknowledged);
      expect(c.acceptedReview, same(accepted)); expect(h.changes, 0); expect(h.reads, 1);
    }
    await tester.pumpWidget(const SizedBox()); h.dispose();
  });

  testWidgets('invalid typed receipt date stays editable and Continue focuses its raw value', (tester) async {
    final h = _ReceiptChoiceHarness(); await h.read(tester);
    final c = h.controller; chooseNoTip(c); await h.mount(tester);
    await _editReceiptReviewInput(tester, c.date, '2026-02-30');
    await tapText(tester, 'Continue');
    expect(h.uses, 0); expect(c.acceptedReview, isNull); expect(c.date.text, '2026-02-30');
    expect(tester.widget<TextField>(_receiptReviewInput(c.date)).focusNode!.hasFocus, isTrue);
    expect(c.buildReview, throwsFormatException);
    await _openReceiptDatePicker(tester);
    expect(c.date.text, '2026-02-30');
    await _enterReceiptPickerDate(tester, DateTime(2028, 2, 29));
    final changes = h.changes;
    await _finishReceiptDatePicker(tester);
    expect(c.date.text, '2028-02-29'); expect(h.changes, changes + 1);
    await tapText(tester, 'Continue');
    expect(h.uses, 1); expect(c.acceptedReview!.date, '2028-02-29'); expect(h.reads, 1);
    await tester.pumpWidget(const SizedBox()); h.dispose();
  });

  testWidgets('a different picked receipt date invalidates once and the same picked date is a no-op', (tester) async {
    final h = _ReceiptChoiceHarness(evidence: evidenceJson()
      ..['issues'] = [{'field': 'merchant', 'reason': 'Check merchant'}]);
    await h.read(tester);
    final c = h.controller; chooseNoTip(c); c.acknowledgeWarnings(true); c.acceptedReview = c.buildReview();
    await h.mount(tester);
    var notifications = 0;
    c.addListener(() => notifications++);
    await _openReceiptDatePicker(tester);
    await _enterReceiptPickerDate(tester, DateTime(2028, 2, 29));
    await _finishReceiptDatePicker(tester);
    expect(c.date.text, '2028-02-29'); expect(c.acknowledgedWarnings, isEmpty); expect(c.acceptedReview, isNull);
    expect(h.changes, 1); expect(notifications, 1); expect(h.uses, 0); expect(h.reads, 1);
    c.acknowledgeWarnings(true);
    final accepted = c.buildReview(); c.acceptedReview = accepted;
    final acknowledged = {...c.acknowledgedWarnings}; expect(acknowledged, isNotEmpty);
    notifications = 0;
    await tester.pumpAndSettle();
    await _openReceiptDatePicker(tester);
    await _enterReceiptPickerDate(tester, DateTime(2028, 2, 29));
    await _finishReceiptDatePicker(tester);
    expect(c.date.text, '2028-02-29'); expect(c.acknowledgedWarnings, acknowledged); expect(c.acceptedReview, same(accepted));
    expect(h.changes, 1); expect(notifications, 0); expect(h.reads, 1);
    await tester.pumpWidget(const SizedBox()); h.dispose();
  });

  for (final invalidation in ['controller', 'photo', 'session', 'navigation', 'review stage', 'evidence', 'typed date']) {
    testWidgets('late receipt date selection after $invalidation changes is ignored', (tester) async {
      final h = _ReceiptChoiceHarness(); await h.read(tester);
      final c = h.controller; chooseNoTip(c);
      final current = ValueNotifier(c);
      final navigator = GlobalKey<NavigatorState>();
      await tester.pumpWidget(MaterialApp(navigatorKey: navigator, home: const Scaffold(body: Text('Outside receipt'))));
      final route = MaterialPageRoute<void>(builder: (context) => Scaffold(body: SingleChildScrollView(
        child: ValueListenableBuilder<ReceiptImportController>(valueListenable: current,
          builder: (context, value, _) => h.fields(value, reviewOnly: true)))));
      unawaited(navigator.currentState!.push(route)); await tester.pumpAndSettle();
      await _openReceiptDatePicker(tester);
      await _enterReceiptPickerDate(tester, DateTime(2028, 2, 29));
      ReceiptImportController? replacement;
      switch (invalidation) {
        case 'controller':
          replacement = ReceiptImportController(repository: h.session.repository, session: h.session,
            groupId: 'other-group', media: FakeReceiptMedia());
          replacement.catalog = [usd]; replacement.restoreReview(ReceiptImportReview.fromJson(reviewJson()), accepted: false);
          current.value = replacement;
        case 'photo':
          await c.selectImage(navigator.currentContext!);
        case 'session':
          h.session.expire();
        case 'navigation':
          navigator.currentState!.removeRoute(route);
        case 'review stage':
          c.acceptReview(); c.returnToReview();
        case 'evidence':
          c.evidence = ReceiptEvidence.fromJson(evidenceJson()); c.changed();
        case 'typed date':
          c.date.text = '2027-01-02'; c.changed();
      }
      await tester.pumpAndSettle();
      final expectedDate = c.date.text, expectedReplacementDate = replacement?.date.text;
      final changes = h.changes;
      await _finishReceiptDatePicker(tester);
      expect(c.date.text, expectedDate); expect(replacement?.date.text, expectedReplacementDate);
      expect(c.date.text, isNot('2028-02-29')); expect(h.changes, changes); expect(h.reads, 1);
      expect(tester.takeException(), isNull);
      await tester.pumpWidget(const SizedBox()); current.dispose(); replacement?.dispose(); h.dispose();
    });
  }

  testWidgets('total source keeps independent references and requires a deliberate matching total', (tester) async {
    final h = _ReceiptChoiceHarness(review: reviewJson(total: 3000, items: [], adjustments: [], overrides: {
      'mode': 'total', 'finalPaidMinor': '3500',
    }));
    final c = h.controller; await h.mount(tester);
    await _chooseReceiptReviewOption(tester, 'Total source', 'Amount paid');
    expect(c.totalBasis, 'final_paid'); expect(c.total.text, '30.00');
    expect(c.printedTotal.text, '30.00'); expect(c.finalPaid.text, '35.00');
    expect(c.acceptedReview, isNull); expect(h.uses, 0);
    await tapText(tester, 'Continue');
    expect(h.uses, 0); expect(c.acceptReview, throwsFormatException);
    expect(tester.widget<TextField>(_receiptReviewInput(c.finalPaid)).focusNode!.hasFocus, isTrue);
    await _editReceiptReviewInput(tester, c.finalPaid, '');
    await tapText(tester, 'Continue');
    expect(h.uses, 0); expect(c.acceptReview, throwsFormatException);
    expect(tester.widget<TextField>(_receiptReviewInput(c.finalPaid)).focusNode!.hasFocus, isTrue);
    await _editReceiptReviewInput(tester, c.finalPaid, '35.00');
    await _chooseReceiptReviewOption(tester, 'Total source', 'Bill total');
    expect(c.total.text, '30.00');
    await tapText(tester, 'Continue');
    expect(h.uses, 1); expect(h.amounts, [BigInt.from(3000)]);
    c.returnToReview(); await tester.pumpAndSettle();
    await _chooseReceiptReviewOption(tester, 'Total source', 'Amount paid');
    await _editReceiptReviewInput(tester, c.total, '35.00');
    expect(c.totalBasis, 'user_entered'); expect(c.printedTotal.text, '30.00'); expect(c.finalPaid.text, '35.00');
    expect(c.acceptedReview, isNull);
    await _chooseReceiptReviewOption(tester, 'Total source', 'Amount paid');
    await tapText(tester, 'Continue');
    expect(h.uses, 2); expect(h.amounts, [BigInt.from(3500)]);
    expect(c.acceptedReview!.totalBasis, 'final_paid');
    expect(c.acceptedReview!.printedTotalMinor, BigInt.from(3000)); expect(c.acceptedReview!.finalPaidMinor, BigInt.from(3500));
    expect(h.reads, 0);
    await tester.pumpWidget(const SizedBox()); h.dispose();
  });

  testWidgets('known tip records Total once and item mode retains drafts and requires tip charge eligibility', (tester) async {
    final h = _ReceiptChoiceHarness(review: reviewJson(total: 3500, adjustments: [
      {'id': 'tip', 'kind': 'actual_tip', 'amountMinor': '500', 'treatment': 'add', 'itemIds': <String>[]},
    ]));
    final c = h.controller;
    c.items.first.quantity.text = '2'; c.items.first.unit.text = 'kg'; c.items.first.unitPrice.text = '5.00';
    final items = c.items.toList(), charges = c.adjustments.toList();
    await h.mount(tester);
    await tapText(tester, 'Receipt details');
    await _chooseReceiptReviewOption(tester, 'Record as', 'One expense');
    expect(c.items, orderedEquals(items)); expect(c.adjustments, orderedEquals(charges)); expect(c.acceptedReview, isNull);
    await _chooseReceiptReviewOption(tester, 'Tip', 'Amount known');
    await _editReceiptReviewInput(tester, c.actualTip, '5.00');
    await tapText(tester, 'Continue');
    expect(h.uses, 1); expect(h.amounts, [BigInt.from(3500)]);
    expect(c.acceptedReview!.totalMinor, BigInt.from(3500)); expect(c.acceptedReview!.actualTipMinor, BigInt.from(500));
    expect(c.acceptedReview!.toJson()['items'], isEmpty); expect(c.acceptedReview!.toJson()['adjustments'], isEmpty);
    expect(c.acceptedReview!.acknowledgedWarnings, isEmpty);
    c.returnToReview(); await tester.pumpAndSettle();
    await _chooseReceiptReviewOption(tester, 'Record as', 'Item expenses');
    expect(c.items, orderedEquals(items)); expect(c.adjustments, orderedEquals(charges));
    expect(c.items.first.quantity.text, '2'); expect(c.items.first.unit.text, 'kg'); expect(c.items.first.unitPrice.text, '5.00');
    expect(c.adjustments.single.itemIds, isEmpty);
    await _editReceiptReviewInput(tester, c.actualTip, '4.00');
    await tapText(tester, 'Continue');
    expect(h.uses, 1); expect(c.acceptReview, throwsFormatException);
    await _editReceiptReviewInput(tester, c.actualTip, '5.00');
    await tapText(tester, 'Continue');
    expect(h.uses, 1); expect(c.acceptReview, throwsFormatException);
    final charge = find.byKey(ObjectKey(c.adjustments.single));
    final allItems = find.descendant(of: charge, matching: find.text('All items'));
    await tester.ensureVisible(allItems); await tester.tap(allItems); await tester.pumpAndSettle();
    expect(c.adjustments.single.itemIds, c.items.map((row) => row.id).toSet());
    await tapText(tester, 'Continue');
    expect(h.uses, 2); expect(h.amounts, [BigInt.from(1167), BigInt.from(2333)]);
    final review = c.acceptedReview!;
    expect(review.adjustments, hasLength(1)); expect(review.adjustments.single.kind, 'actual_tip');
    expect(review.adjustments.single.amountMinor, BigInt.from(500)); expect(review.actualTipMinor, BigInt.from(500));
    expect(review.items.first.quantity, '2'); expect(review.items.first.unit, 'kg'); expect(review.items.first.unitPrice, '5.00');
    expect(review.acknowledgedWarnings, isEmpty); expect(h.reads, 0);
    await tester.pumpWidget(const SizedBox()); h.dispose();
  });

  testWidgets('bill only retains Total clears paid and tip and removes the contradictory paid choice', (tester) async {
    final h = _ReceiptChoiceHarness(review: reviewJson(total: 3500, items: [], adjustments: [], overrides: {
      'mode': 'total', 'totalBasis': 'final_paid', 'printedTotalMinor': '3000', 'finalPaidMinor': '3500',
      'tipDecision': 'confirmed', 'actualTipMinor': '500',
    }));
    final c = h.controller; await h.mount(tester);
    expect(c.buildReview().acknowledgedWarnings, isEmpty);
    await _chooseReceiptReviewOption(tester, 'Tip', 'Bill only; tip unknown');
    expect(c.total.text, '35.00'); expect(c.totalBasis, 'user_entered'); expect(c.printedTotal.text, '30.00');
    expect(c.finalPaid.text, isEmpty); expect(c.actualTip.text, isEmpty);
    expect(_receiptReviewInput(c.finalPaid), findsNothing); expect(_receiptReviewInput(c.actualTip), findsNothing);
    final source = find.widgetWithText(DropdownButtonFormField<String>, 'Total source');
    await tester.ensureVisible(source); await tester.tap(source); await tester.pumpAndSettle();
    expect(find.widgetWithText(DropdownMenuItem<String>, 'Amount paid'), findsNothing);
    await tapText(tester, 'Bill total');
    expect(c.total.text, '35.00'); expect(c.printedTotal.text, '30.00');
    await tapText(tester, 'Continue');
    expect(h.uses, 0); expect(c.acceptReview, throwsFormatException);
    expect(tester.widget<TextField>(_receiptReviewInput(c.printedTotal)).focusNode!.hasFocus, isTrue);
    await _chooseReceiptReviewOption(tester, 'Total source', 'Entered total');
    final wire = c.buildReview().toJson();
    expect(wire['totalMinor'], '3500'); expect(wire['printedTotalMinor'], '3000'); expect(wire['totalBasis'], 'user_entered');
    expect(wire['actualTipMinor'], isNull); expect(wire['finalPaidMinor'], isNull);
    expect(wire['acknowledgedWarnings'], ['bill_only:tipDecision']);
    await tapText(tester, 'Continue'); expect(h.uses, 1); expect(h.amounts, [BigInt.from(3500)]);
    c.returnToReview(); await tester.pumpAndSettle();
    await _chooseReceiptReviewOption(tester, 'Tip', 'Amount known');
    expect(_receiptReviewInput(c.finalPaid), findsOneWidget); expect(c.finalPaid.text, isEmpty);
    expect(_receiptReviewInput(c.actualTip), findsOneWidget); expect(c.actualTip.text, isEmpty);
    expect(c.buildReview().acknowledgedWarnings, isEmpty);
    await tapText(tester, 'Continue'); expect(h.uses, 1); expect(c.acceptReview, throwsFormatException);
    expect(tester.widget<TextField>(_receiptReviewInput(c.actualTip)).focusNode!.hasFocus, isTrue);
    await _editReceiptReviewInput(tester, c.finalPaid, '42.00');
    await _chooseReceiptReviewOption(tester, 'Tip', 'No tip');
    expect(c.finalPaid.text, '42.00'); expect(c.total.text, '35.00'); expect(c.actualTip.text, '0');
    await tapText(tester, 'Continue');
    expect(h.uses, 2); expect(h.amounts, [BigInt.from(3500)]);
    expect(c.acceptedReview!.actualTipMinor, BigInt.zero); expect(c.acceptedReview!.finalPaidMinor, BigInt.from(4200));
    expect(c.acceptedReview!.acknowledgedWarnings, isEmpty); expect(h.reads, 0);
    await tester.pumpWidget(const SizedBox()); h.dispose();
  });

  testWidgets('warning acknowledgement is exact, uncapped, and invalidated by edits', (tester) async {
    final original = evidenceJson()
      ..['issues'] = [for (var n = 0; n < 400; n++) {'field': 'merchant', 'reason': 'Check printed merchant $n'}]
      ..['suggested_tips'] = [{'rate': observation(null), 'amount': observation('unreadable')}];
    final session = roleSession(records: (request) async {
      if (request.url.path.endsWith('/receipt-extractions/options')) return jsonResponse(options());
      if (request.url.path.endsWith('/receipt-extractions')) return jsonResponse(extraction(evidence: original));
      return null;
    });
    final c = ReceiptImportController(repository: session.repository, session: session, groupId: 'group-id', media: FakeReceiptMedia()..next = testImage);
    final context = await mountContext(tester); await c.loadOptions(); await c.selectImage(context); await c.extract();
    chooseNoTip(c);
    final required = c.requiredWarnings.map((issue) => issue.warningToken).toSet();
    expect(required, hasLength(401)); expect(c.acceptReview, throwsFormatException);
    c.acknowledgeWarnings(true); expect(c.acknowledgedWarnings, required);
    expect(c.acceptReview().single.amountMinor, BigInt.from(2970));
    expect(c.source!.review!.acknowledgedWarnings.toSet(), required);
    c.merchant.text = 'Corrected merchant'; c.changed();
    expect(c.acknowledgedWarnings, isEmpty); expect(c.acceptReview, throwsFormatException);
    c.items.single.amount.text = 'invalid'; c.changed(); c.acknowledgeWarnings(true);
    expect(c.acknowledgedWarnings, isEmpty);
    c.items.single.amount.text = '29.70'; c.changed(); c.acknowledgeWarnings(true);
    expect(c.acceptReview().single.amountMinor, BigInt.from(2970));
    c.dispose(); session.dispose();
  });

  testWidgets('fresh charge review requires tip and per-charge eligibility before golden allocation', (tester) async {
    final original = evidenceJson();
    final firstItem = Map<String, Object?>.from((original['items'] as List).single as Map);
    original['items'] = [
      {...firstItem, 'id': 'a', 'description': 'First item', 'line_total': observation('10.00')},
      {...firstItem, 'id': 'b', 'description': 'Second item', 'line_total': observation('20.00')},
    ];
    original['adjustments'] = [
      {'id': 'discount', 'kind': 'discount', 'scope': 'receipt', 'item_id': null, 'amount': observation('3.00'), 'treatment': 'subtract', 'evidence': 'Discount'},
      {'id': 'tax', 'kind': 'tax', 'scope': 'receipt', 'item_id': null, 'amount': observation('2.70'), 'treatment': 'add', 'evidence': 'Tax'},
    ];
    final session = roleSession(records: (request) async {
      if (request.url.path.endsWith('/receipt-extractions/options')) return jsonResponse(options());
      if (request.url.path.endsWith('/receipt-extractions')) return jsonResponse(extraction(evidence: original));
      return null;
    });
    final c = ReceiptImportController(repository: session.repository, session: session, groupId: 'group-id', media: FakeReceiptMedia()..next = testImage);
    final context = await mountContext(tester); await c.loadOptions(); await c.selectImage(context); await c.extract();
    var uses = 0;
    List<String>? allocations;
    await tester.pumpWidget(MaterialApp(home: Scaffold(body: SingleChildScrollView(child:
      ListenableBuilder(listenable: c, builder: (context, _) => ReceiptImportReviewFields(controller: c,
        onUse: () { allocations = c.acceptReview().map((row) => '${row.itemId}:${row.amountMinor}').toList(); uses++; }, onChanged: () {}))))));
    await tester.pumpAndSettle();
    expect(c.tipDecision, isEmpty); expect(c.acceptedReview, isNull);
    expect(c.adjustments.every((row) => row.itemIds.isEmpty), isTrue);
    final tip = find.widgetWithText(DropdownButtonFormField<String>, 'Tip');
    await tester.ensureVisible(tip); await tester.tap(tip); await tester.pumpAndSettle();
    await tapText(tester, 'No tip');
    expect(c.tipDecision, 'none'); expect(c.acceptedReview, isNull);
    final discountCard = find.byKey(ObjectKey(c.adjustments.first));
    final taxCard = find.byKey(ObjectKey(c.adjustments.last));
    Future<void> allItems(Finder card) async {
      final action = find.descendant(of: card, matching: find.text('All items'));
      await tester.ensureVisible(action); await tester.tap(action); await tester.pumpAndSettle();
    }
    await allItems(discountCard);
    expect(c.adjustments.first.itemIds, c.items.map((row) => row.id).toSet()); expect(c.adjustments.last.itemIds, isEmpty);
    expect(c.acceptedReview, isNull);
    await allItems(taxCard);
    await tapText(tester, 'Receipt details'); await tapText(tester, 'Receipt details');
    expect(c.acceptedReview, isNull); expect(uses, 0);
    await tapText(tester, 'Continue');
    expect(uses, 1);
    expect(allocations, ['item_0:990', 'item_1:1980']);
    expect(c.source!.review!.adjustments.map((row) => row.toJson()), [
      {'id': 'charge_2', 'kind': 'discount', 'amountMinor': '300', 'treatment': 'subtract', 'itemIds': ['item_0', 'item_1']},
      {'id': 'charge_3', 'kind': 'tax', 'amountMinor': '270', 'treatment': 'add', 'itemIds': ['item_0', 'item_1']},
    ]);
    expect(c.source!.evidence!.toJson(), original);
    await tester.pumpWidget(const SizedBox()); c.dispose(); session.dispose();
  });

  testWidgets('numeric warning checkbox gates Continue and editing clears acknowledgement', (tester) async {
    final session = roleSession();
    final c = ReceiptImportController(repository: session.repository, session: session, groupId: 'group-id', media: FakeReceiptMedia());
    c.catalog = [usd]; c.restoreReview(ReceiptImportReview.fromJson(reviewJson()));
    c.items.first.quantity.text = '2'; c.items.first.unitPrice.text = '4.00'; c.changed();
    var uses = 0;
    await tester.pumpWidget(MaterialApp(home: Scaffold(body: SingleChildScrollView(child:
      ListenableBuilder(listenable: c, builder: (context, _) => ReceiptImportReviewFields(controller: c,
        onUse: () { c.acceptReview(); uses++; }, onChanged: () {}))))));
    await tester.pumpAndSettle();
    CheckboxListTile acknowledgement() => tester.widget<CheckboxListTile>(find.widgetWithText(CheckboxListTile, 'I have checked these details'));
    expect(acknowledgement().value, isFalse);
    expect(c.requiredWarnings.map((issue) => issue.warningToken), ['quantity_price_mismatch:items.a']);
    await tapText(tester, 'Continue'); expect(uses, 0);
    await tapText(tester, 'I have checked these details'); expect(acknowledgement().value, isTrue);
    expect(c.acknowledgedWarnings, {'quantity_price_mismatch:items.a'});
    await tapText(tester, 'Continue'); expect(uses, 1);
    final merchant = find.byWidgetPredicate((widget) => widget is TextField && widget.controller == c.merchant);
    await tester.ensureVisible(merchant); await tester.enterText(merchant, 'Edited merchant'); await tester.pumpAndSettle();
    expect(acknowledgement().value, isFalse); expect(c.acknowledgedWarnings, isEmpty);
    await tapText(tester, 'Continue'); expect(uses, 1);
    await tester.pumpWidget(const SizedBox()); c.dispose(); session.dispose();
  });

  testWidgets('picker cancel and preview never start a paid extraction', (tester) async {
    var calls = 0;
    final session = roleSession(records: (request) async {
      if (request.url.path.endsWith('/receipt-extractions/options')) return jsonResponse(options());
      if (request.url.path.endsWith('/receipt-extractions')) { calls++; return jsonResponse(extraction()); }
      return null;
    });
    final media = FakeReceiptMedia();
    final c = ReceiptImportController(repository: session.repository, session: session, groupId: 'group-id', media: media);
    final context = await mountContext(tester); await c.loadOptions();
    expect(await c.selectImage(context), isFalse); expect(c.stage, ReceiptImportStage.manual); expect(calls, 0);
    media.next = testImage;
    expect(await c.selectImage(context), isTrue); expect(c.stage, ReceiptImportStage.preview); expect(calls, 0);
    await c.extract(); expect(calls, 1); expect(c.stage, ReceiptImportStage.review);
    expect(c.tipDecision, isEmpty); expect(c.actualTip.text, isEmpty);
    c.dispose(); session.dispose();
  });

  testWidgets('session invalidation discards late extraction and releases private image', (tester) async {
    final pending = Completer<http.Response>();
    final started = Completer<void>();
    final session = roleSession(records: (request) async {
      if (request.url.path.endsWith('/receipt-extractions/options')) return jsonResponse(options());
      if (request.url.path.endsWith('/receipt-extractions')) { started.complete(); return pending.future; }
      return null;
    });
    final media = FakeReceiptMedia()..next = testImage;
    final c = ReceiptImportController(repository: session.repository, session: session, groupId: 'group-id', media: media);
    final context = await mountContext(tester); await c.loadOptions(); await c.selectImage(context);
    final reading = c.extract(); await started.future; await c.extract(); session.expire();
    pending.complete(jsonResponse(extraction())); await reading;
    expect(c.image, isNull); expect(c.imageProvider, isNull); expect(c.evidence, isNull);
    expect(c.suggestedTitle, isNull);
    expect(c.items, isEmpty); expect(c.stage, ReceiptImportStage.manual); expect(media.stops, greaterThan(0));
    c.dispose(); session.dispose();
  });

  testWidgets('changing groups disposes the old import and ignores its late extraction', (tester) async {
    final pending = Completer<http.Response>();
    final started = Completer<void>();
    final session = roleSession(records: (request) async {
      if (request.url.path.endsWith('/receipt-extractions/options')) return jsonResponse(options());
      if (request.url.path.endsWith('/receipt-extractions')) { started.complete(); return pending.future; }
      return null;
    });
    await session.login('alice', 'password');
    final firstMedia = FakeReceiptMedia()..next = testImage;
    final secondMedia = FakeReceiptMedia();
    final selectedGroup = ValueNotifier('group-id');
    await tester.pumpWidget(MaterialApp(home: Scaffold(body: SingleChildScrollView(child: ValueListenableBuilder<String>(
      valueListenable: selectedGroup, builder: (context, id, _) => GroupScope(group: group(id), updateGroup: (_) {},
        refreshGroup: () async {}, recordChanges: RecordChangeTracker(), child: ReceiptEditor(repository: session.repository,
          session: session, groupId: id, media: id == 'group-id' ? firstMedia : secondMedia)))))));
    await tester.pumpAndSettle(); await tapText(tester, 'Upload photo');
    await tester.ensureVisible(find.text('Read receipt')); await tester.tap(find.text('Read receipt'));
    await tester.pump(); await started.future;
    selectedGroup.value = 'other-group'; await tester.pumpAndSettle();
    pending.complete(jsonResponse(extraction())); await tester.pumpAndSettle();
    expect(firstMedia.stops, greaterThan(0)); expect(find.byType(ReceiptImportReviewFields), findsNothing);
    expect(find.byType(ReceiptImagePreview), findsNothing); expect(find.byType(SplitFields), findsNothing);
    await tester.pumpWidget(const SizedBox()); selectedGroup.dispose(); session.dispose();
  });

  testWidgets('corrected evidence remains separate and charge eligibility gates allocation without repeating extraction', (tester) async {
    var calls = 0;
    final original = evidenceJson();
    (original['items'] as List).single['line_total'] = observation('1O.00');
    original['adjustments'] = [{'id': 'tax', 'kind': 'tax', 'scope': 'receipt', 'item_id': null,
      'amount': observation('0.70'), 'treatment': 'add', 'evidence': 'Tax'}];
    final session = roleSession(records: (request) async {
      if (request.url.path.endsWith('/receipt-extractions/options')) return jsonResponse(options());
      if (request.url.path.endsWith('/receipt-extractions')) { calls++; return jsonResponse(extraction(evidence: original)); }
      return null;
    });
    final media = FakeReceiptMedia()..next = testImage;
    final c = ReceiptImportController(repository: session.repository, session: session, groupId: 'group-id', media: media);
    final context = await mountContext(tester); await c.loadOptions(); await c.selectImage(context); await c.extract();
    chooseNoTip(c); c.items.single.amount.text = '29.00';
    final charge = c.adjustments.single;
    charge.itemIds.clear();
    expect(c.acceptReview, throwsFormatException);
    charge.itemIds.add(c.items.single.id);
    expect(c.acceptReview().single.amountMinor, BigInt.from(2970)); expect(calls, 1);
    expect(c.source!.evidence!.items.single.lineTotal.value, '1O.00');
    expect(c.source!.review!.items.single.amountMinor, BigInt.from(2900));
    expect(identical(c.source!.evidence, c.evidence), isTrue);
    c.dispose(); session.dispose();
  });

  testWidgets('total mode tolerates unusable itemization but never invents date or tip', (tester) async {
    final original = evidenceJson(); original['transaction_date'] = observation(null);
    (original['items'] as List).single['line_total'] = observation('bad');
    final session = roleSession(records: (request) async {
      if (request.url.path.endsWith('/receipt-extractions/options')) return jsonResponse(options());
      if (request.url.path.endsWith('/receipt-extractions')) return jsonResponse(extraction(evidence: original));
      return null;
    });
    final c = ReceiptImportController(repository: session.repository, session: session, groupId: 'group-id', media: FakeReceiptMedia()..next = testImage);
    final context = await mountContext(tester); await c.loadOptions(); await c.selectImage(context); await c.extract();
    c.mode = 'total'; chooseNoTip(c); expect(c.date.text, isEmpty); expect(c.acceptReview, throwsFormatException);
    c.date.text = '2026-10-21'; c.tipDecision = 'bill_only'; c.actualTip.text = '5.00'; c.finalPaid.text = '34.70';
    expect(c.acceptReview().single.amountMinor, BigInt.from(2970));
    expect(c.acceptedReview!.items, isEmpty); expect(c.acceptedReview!.adjustments, isEmpty);
    expect(c.acceptedReview!.actualTipMinor, isNull); expect(c.acceptedReview!.finalPaidMinor, isNull);
    expect(c.acceptedReview!.acknowledgedWarnings, ['bill_only:tipDecision']);
    final source = c.source!.toJson();
    expect(source['image'], testImage.toJson()); expect(source['extractionId'], extractionId);
    expect(source['evidence'], original); expect(source['review'], c.acceptedReview!.toJson());
    c.dispose(); session.dispose();
  });

  testWidgets('unavailable extraction still attaches manually and zero cap blocks selection', (tester) async {
    var cap = 1000, calls = 0;
    final session = roleSession(records: (request) async {
      if (request.url.path.endsWith('/receipt-extractions/options')) return jsonResponse(options(available: false, cap: cap));
      if (request.url.path.endsWith('/receipt-extractions')) calls++;
      return null;
    });
    final media = FakeReceiptMedia()..next = testImage;
    final c = ReceiptImportController(repository: session.repository, session: session, groupId: 'group-id', media: media);
    final context = await mountContext(tester); await c.loadOptions(); await c.selectImage(context); await c.extract();
    expect(calls, 0); c.useManualAttachment(); expect(c.source!.review, isNull); expect(c.source!.extractionId, isNull);
    c.clear(); cap = 0; await c.loadOptions(); expect(await c.selectImage(context), isFalse); expect(media.picks, 1);
    c.dispose(); session.dispose();
  });

  testWidgets('extraction and save failures retain corrected fields and exact source payload', (tester) async {
    final writes = <Map<String, dynamic>>[];
    var attempts = 0;
    final session = roleSession(records: (request) async {
      if (request.url.path.endsWith('/receipt-extractions/options')) return jsonResponse(options());
      if (request.url.path.endsWith('/receipt-extractions')) {
        attempts++;
        return attempts == 1 ? http.Response('Temporary extraction failure', 503) : jsonResponse(extraction());
      }
      if (request.method == 'POST' && request.url.path.endsWith('/receipts')) {
        writes.add(jsonDecode(request.body) as Map<String, dynamic>); return http.Response('Receipt exceeds upload limit', 413);
      }
      return null;
    });
    final media = FakeReceiptMedia()..next = testImage;
    final router = await mountEditor(tester, session, media);
    await tester.enterText(find.widgetWithText(TextField, 'Receipt title'), 'Synthetic Market groceries');
    await tapText(tester, 'Upload photo'); expect(attempts, 0);
    await tapText(tester, 'Read receipt'); expect(attempts, 1); expect(find.text('Read receipt'), findsOneWidget);
    expect(tester.widget<TextField>(find.widgetWithText(TextField, 'Receipt title')).controller!.text, 'Synthetic Market groceries');
    await tapText(tester, 'Read receipt'); expect(attempts, 2);
    final review = tester.widget<ReceiptImportReviewFields>(find.byType(ReceiptImportReviewFields)).controller;
    chooseNoTip(review); review.items.single.title.text = 'Corrected literal <b>title</b>'; review.changed(); await tester.pumpAndSettle();
    await tapText(tester, 'Continue');
    expect(find.byType(SplitFields), findsNothing);
    await openExpenseCards(tester);
    final fields = tester.widget<SplitFields>(find.byType(SplitFields));
    expect(fields.moneyLocked, isTrue); expect(fields.controller.date.text, '2026-10-21');
    await tapText(tester, 'Save receipt'); expect(writes, isEmpty);
    await tapText(tester, 'I confirm the displayed payers and splits');
    fields.controller.title.text = 'Editable expense title'; fields.onChanged(); await tester.pumpAndSettle();
    await tapText(tester, 'Save receipt'); expect(writes.length, 1);
    expect(writes.single['title'], 'Synthetic Market groceries');
    expect(tester.widget<TextField>(find.widgetWithText(TextField, 'Receipt title')).controller!.text, 'Synthetic Market groceries');
    expect(writes.single['source']['review']['items'][0]['title'], 'Corrected literal <b>title</b>');
    expect(writes.single['records'][0]['title'], 'Editable expense title');
    expect(writes.single['records'][0]['amountMinor'], '2970');
    expect(writes.single['records'][0]['byUserId'], aliceId);
    expect(review.items.single.title.text, 'Corrected literal <b>title</b>'); expect(review.image, isNotNull);
    fields.controller.setActorUserId(bobId); fields.onChanged(); await tester.pumpAndSettle();
    await tapText(tester, 'Save receipt'); expect(writes.length, 1);
    await tapText(tester, 'Edit amounts'); await tapText(tester, 'Cancel');
    expect(tester.widget<SplitFields>(find.byType(SplitFields)).controller, same(fields.controller));
    await tapText(tester, 'Edit amounts'); await tapText(tester, 'Continue to review');
    review.items.single.amount.text = 'broken'; review.changed(); await tester.pumpAndSettle();
    await tapText(tester, 'Cancel review changes and keep displayed expenses');
    expect(tester.widget<SplitFields>(find.byType(SplitFields)).controller, same(fields.controller));
    expect(fields.controller.actorUserId, bobId); expect(fields.controller.amount.text, '29.70');
    expect(review.items.single.amount.text, '29.70'); expect(attempts, 2);
    await tester.pumpWidget(const SizedBox()); router.dispose(); session.dispose();
  });

  testWidgets('saved source is historical, omitted unchanged and removed only after explicit save', (tester) async {
    final writes = <Map<String, dynamic>>[];
    var sourceReads = 0;
    final historical = ReceiptSourceInput(image: testImage, extractionId: extractionId,
      evidence: ReceiptEvidence.fromJson(evidenceJson()), review: ReceiptImportReview.fromJson(reviewJson(items: [], adjustments: [], overrides: {'mode': 'total'})));
    final receipt = {'receiptId': 'receipt-id', 'groupId': 'group-id', 'title': 'Synthetic Market groceries', 'note': 'Saved note',
      'uploadedBy': aliceJson, 'createdAt': '2026-10-01T00:00:00Z', 'records': [expenseFixture()],
      'source': {'mediaType': 'image/jpeg', 'width': 2, 'height': 2, 'byteCount': testImage.bytes.length, 'sha256': List.filled(64, 'a').join(), 'hasReview': true}};
    final session = roleSession(records: (request) async {
      if (request.url.path.endsWith('/receipt-extractions/options')) return jsonResponse(options(available: false));
      if (request.url.path.endsWith('/receipts/receipt-id/source')) {
        sourceReads++;
        return sourceReads == 1 ? http.Response('Source temporarily unavailable', 503) : jsonResponse(historical.toJson());
      }
      if (request.url.path.endsWith('/receipts/receipt-id') && request.method == 'GET') return jsonResponse(receipt);
      if (request.url.path.endsWith('/receipts/receipt-id') && request.method == 'PUT') {
        writes.add(jsonDecode(request.body) as Map<String, dynamic>); return http.Response('Temporary save failure', 503);
      }
      return null;
    });
    final router = await mountEditor(tester, session, FakeReceiptMedia(), receiptId: 'receipt-id');
    await openExpenseCards(tester);
    expect(sourceReads, 0);
    final fields = tester.widget<SplitFields>(find.byType(SplitFields)); expect(fields.moneyLocked, isFalse);
    fields.controller.amount.text = '13.00'; fields.onChanged(); await tester.pumpAndSettle();
    await tapText(tester, 'Save receipt'); expect(writes.single.containsKey('source'), isFalse);
    expect(find.text('View photo'), findsNothing);
    await tapText(tester, 'Photo options'); await tapText(tester, 'View photo'); expect(sourceReads, 1);
    expect(fields.controller.amount.text, '13.00');
    await tapText(tester, 'Retry loading source'); expect(sourceReads, 2);
    expect(fields.controller.date.text, '2026-10-01'); expect(fields.controller.amount.text, '13.00');
    await tapText(tester, 'Close source');
    await tapText(tester, 'Remove photo'); await tapText(tester, 'Remove photo');
    expect(writes.length, 1); await tapText(tester, 'Save receipt'); expect(writes.last['source'], isNull);
    expect(writes.last['records'][0]['amountMinor'], '1300');
    await tester.pumpWidget(const SizedBox()); router.dispose(); session.dispose();
  });

  testWidgets('session expiry closes a pending source dialog and rejects its late private snapshot', (tester) async {
    final pending = Completer<http.Response>();
    var sourceReads = 0;
    final historical = ReceiptSourceInput(image: testImage, extractionId: extractionId,
      evidence: ReceiptEvidence.fromJson(evidenceJson()), review: ReceiptImportReview.fromJson(reviewJson(items: [], adjustments: [], overrides: {'mode': 'total', 'merchant': 'Private historical merchant'})));
    final receipt = {'receiptId': 'receipt-id', 'groupId': 'group-id', 'title': 'Synthetic Market groceries', 'note': 'Saved receipt',
      'uploadedBy': aliceJson, 'createdAt': '2026-10-01T00:00:00Z', 'records': [expenseFixture()],
      'source': {'mediaType': 'image/jpeg', 'width': 2, 'height': 2, 'byteCount': testImage.bytes.length, 'sha256': List.filled(64, 'a').join(), 'hasReview': true}};
    final session = roleSession(records: (request) async {
      if (request.url.path.endsWith('/receipt-extractions/options')) return jsonResponse(options(available: false));
      if (request.url.path.endsWith('/receipts/receipt-id/source')) { sourceReads++; return pending.future; }
      if (request.url.path.endsWith('/receipts/receipt-id')) return jsonResponse(receipt);
      return null;
    });
    final router = await mountEditor(tester, session, FakeReceiptMedia(), receiptId: 'receipt-id');
    await tapText(tester, 'Photo options');
    await tester.ensureVisible(find.text('View photo'));
    await tester.tap(find.text('View photo')); await tester.pump();
    expect(sourceReads, 1);
    session.expire(); await tester.pumpAndSettle();
    expect(find.byType(Dialog), findsNothing);
    pending.complete(jsonResponse(historical.toJson())); await tester.pumpAndSettle();
    expect(find.textContaining('Private historical merchant'), findsNothing);
    expect(find.byType(SplitFields), findsNothing);
    await tester.pumpWidget(const SizedBox()); router.dispose(); session.dispose();
  });
}
