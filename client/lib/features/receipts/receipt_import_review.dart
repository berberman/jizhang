import 'package:flutter/gestures.dart';
import 'package:flutter/material.dart';
import 'package:flutter/services.dart';

import '../../core/jizhang_repository.dart';
import '../../core/receipt_ai_models.dart';
import '../../core/receipt_import_math.dart';
import '../../core/money.dart';
import '../../core/session_controller.dart';
import '../../shared/widgets.dart';
import 'receipt_import_controller.dart';

class ReceiptImportReviewFields extends StatefulWidget {
  const ReceiptImportReviewFields({super.key, required this.controller, required this.onUse, required this.onChanged});
  final ReceiptImportController controller;
  final VoidCallback onUse, onChanged;
  @override
  State<ReceiptImportReviewFields> createState() => _ReceiptImportReviewFieldsState();
}

class _ReceiptImportReviewFieldsState extends State<ReceiptImportReviewFields> {
  final Map<Object, FocusNode> _focusNodes = {};
  final Map<Object, ExpansibleController> _disclosures = {};
  final Map<Object, PageStorageBucket> _disclosureStorage = {};
  bool _submitted = false;
  ReceiptImportController get c => widget.controller;
  FocusNode _focus(Object key) => _focusNodes.putIfAbsent(key, FocusNode.new);
  ExpansibleController _disclosure(Object key) => _disclosures.putIfAbsent(key, ExpansibleController.new);
  @override
  void dispose() {
    for (final node in _focusNodes.values) { node.dispose(); }
    for (final controller in _disclosures.values) { controller.dispose(); }
    super.dispose();
  }
  void _change(VoidCallback action) {
    action(); c.changed(); widget.onChanged();
    WidgetsBinding.instance.addPostFrameCallback((_) {
      if (!mounted) return;
      final rows = {...c.items, ...c.adjustments};
      final retired = _disclosures.keys.where((key) => key is ReceiptImportRow ? !rows.contains(key) : key is (String, ReceiptImportRow) && !rows.contains(key.$2)).toList();
      for (final key in retired) { _disclosures.remove(key)?.dispose(); _disclosureStorage.remove(key); }
      final fields = {c.merchant, c.date, c.total, c.totalTitle, c.actualTip, c.printedTotal, c.finalPaid,
        for (final row in rows) ...[row.title, row.amount, row.quantity, row.unit, row.unitPrice]};
      final retiredFocus = _focusNodes.keys.where((key) => key is TextEditingController ? !fields.contains(key) : key is (String, ReceiptImportRow) && !rows.contains(key.$2)).toList();
      for (final key in retiredFocus) { _focusNodes.remove(key)?.dispose(); }
    });
  }
  Widget _input(String label, TextEditingController field, {VoidCallback? update}) => Padding(
    padding: const EdgeInsets.only(bottom: 12), child: TextField(controller: field, focusNode: _focus(field),
      decoration: InputDecoration(label: Text(label)), onChanged: (_) => _change(() { update?.call(); })));
  Widget _dateInput() => Padding(
    padding: const EdgeInsets.only(bottom: 12), child: TextField(controller: c.date, focusNode: _focus(c.date),
      keyboardType: TextInputType.datetime, decoration: InputDecoration(label: const Text('Date'), hintText: 'YYYY-MM-DD',
        suffixIcon: IconButton(tooltip: 'Choose receipt date', icon: const Icon(Icons.calendar_month),
          onPressed: c.busy ? null : () async {
            final controller = c;
            if (controller.stage != ReceiptImportStage.review) return;
            final route = ModalRoute.of(context);
            final field = controller.date;
            final original = field.text;
            final text = original.trim();
            final parsed = validReceiptDate(text) ? DateTime.parse(text) : null;
            final generation = controller.session.generation;
            final image = controller.image;
            final provider = controller.imageProvider;
            final evidence = controller.evidence;
            final extractionId = controller.extractionId;
            var current = true;
            void invalidate() { current = false; }
            controller.addListener(invalidate);
            DateTime? selected;
            try {
              selected = await showDatePicker(context: context,
                initialDate: parsed != null && parsed.year >= 1 ? parsed : DateTime.now(),
                firstDate: DateTime(1), lastDate: DateTime(9999, 12, 31), initialEntryMode: DatePickerEntryMode.calendar);
            } finally {
              controller.removeListener(invalidate);
            }
            if (!mounted || !current || route?.isCurrent == false || !identical(c, controller) || generation != controller.session.generation ||
                controller.stage != ReceiptImportStage.review || !identical(controller.image, image) ||
                !identical(controller.imageProvider, provider) || !identical(controller.evidence, evidence) ||
                controller.extractionId != extractionId || !identical(controller.date, field) || field.text != original || selected == null) return;
            final iso = '${selected.year.toString().padLeft(4, '0')}-${selected.month.toString().padLeft(2, '0')}-${selected.day.toString().padLeft(2, '0')}';
            if (iso == text) return;
            _change(() => field.text = iso);
          })), onChanged: (_) => _change(() {})));
  Widget _explanation(String text) => Padding(padding: const EdgeInsets.only(bottom: 12),
    child: Text(text, style: Theme.of(context).textTheme.bodySmall));
  Widget _choice(String label, String value, Map<String, String> choices, ValueChanged<String> update, {Object? focusKey}) => Padding(
    padding: const EdgeInsets.only(bottom: 12), child: DropdownButtonFormField<String>(
      key: ValueKey((label, focusKey, value)), initialValue: choices.containsKey(value) ? value : null,
      focusNode: _focus(focusKey ?? label), itemHeight: null, isDense: false, isExpanded: true, decoration: InputDecoration(labelText: label),
      items: [for (final entry in choices.entries) DropdownMenuItem(value: entry.key, child: Text(entry.value))],
      onChanged: (selected) { if (selected != null) _change(() => update(selected)); }));
  Widget _details(Object key, String title, List<Widget> children, {bool initiallyExpanded = false}) => PageStorage(
    bucket: _disclosureStorage.putIfAbsent(key, PageStorageBucket.new), child: ExpansionTile(
      key: ValueKey(key), controller: _disclosure(key), maintainState: true, initiallyExpanded: initiallyExpanded,
      tilePadding: EdgeInsets.zero, childrenPadding: const EdgeInsets.fromLTRB(12, 16, 12, 12),
      expandedCrossAxisAlignment: CrossAxisAlignment.stretch, title: Text(title), children: children));
  static const _kinds = {'discount': 'Discount', 'tax': 'Tax', 'service_charge': 'Service charge', 'actual_tip': 'Tip', 'rounding': 'Rounding', 'other': 'Other charge'};
  Widget _item(ReceiptImportRow row, int index) => Card(key: ObjectKey(row), child: Padding(
    padding: const EdgeInsets.all(12), child: Column(crossAxisAlignment: CrossAxisAlignment.stretch, children: [
      Row(children: [Expanded(child: Text('Item ${index + 1}', style: Theme.of(context).textTheme.titleMedium)),
        IconButton(tooltip: 'Remove item ${index + 1}', onPressed: () => _change(() => c.removeItem(row)), icon: const Icon(Icons.remove_circle_outline))]),
      _input('Item', row.title), _input('Amount', row.amount),
      _details(row, 'Item details', [
        _ReviewPair(first: _input('Quantity (optional)', row.quantity), second: _input('Unit (optional)', row.unit)),
        _input('Unit price (optional)', row.unitPrice),
      ]),
    ])));
  Widget _adjustment(ReceiptImportRow row, int index) => Card(key: ObjectKey(row), child: Padding(
    padding: const EdgeInsets.all(12), child: Column(crossAxisAlignment: CrossAxisAlignment.stretch, children: [
      Text(_kinds[row.kind] ?? 'Charge ${index + 1}', style: Theme.of(context).textTheme.titleMedium),
      const SizedBox(height: 12), _input('Amount', row.amount),
      _choice('How to apply', row.treatment, const {'add': 'Add to items', 'subtract': 'Subtract from items',
        'included_in_line_total': 'Already included', 'informational': 'Reference only'}, (value) => row.treatment = value, focusKey: ('treatment', row)),
      if (row.treatment == 'add' || row.treatment == 'subtract') ...[
        Text('Applies to: ${row.itemIds.length} of ${c.items.length} items'),
        Wrap(spacing: 8, children: [
          TextButton(onPressed: () => _change(() { row.itemIds..clear()..addAll(c.items.map((item) => item.id)); }), child: const Text('All items')),
          TextButton(onPressed: () => _disclosure(('eligibility', row)).expand(), child: Text(row.itemIds.isEmpty ? 'Choose items' : 'Change')),
        ]),
        _details(('eligibility', row), 'Applies to', [
          for (var i = 0; i < c.items.length; i++) CheckboxListTile(contentPadding: EdgeInsets.zero,
            focusNode: i == 0 ? _focus(('eligibility', row)) : null,
            title: Text(c.items[i].title.text.trim().isEmpty ? 'Item ${i + 1}' : c.items[i].title.text), value: row.itemIds.contains(c.items[i].id),
            onChanged: (selected) => _change(() { if (selected ?? false) { row.itemIds.add(c.items[i].id); } else { row.itemIds.remove(c.items[i].id); } })),
        ]),
      ],
      _details(row, 'Charge details', [
        _choice('Kind', row.kind, _kinds, (value) => row.kind = value, focusKey: ('kind', row)),
        Align(alignment: Alignment.centerLeft, child: TextButton(onPressed: () => _change(() => c.removeAdjustment(row)), child: const Text('Remove charge'))),
      ]),
    ])));
  bool _invalidMoney(TextEditingController field, {bool optional = false, bool positive = false}) {
    if (optional && field.text.trim().isEmpty) return false;
    try { parseMajorAmount(field.text, minorDigits: c.selectedCurrency!.minorUnits, allowZero: !positive); return false; }
    on FormatException { return true; }
  }
  bool _matchesTotal(TextEditingController reference) {
    try {
      return parseMajorAmount(reference.text, minorDigits: c.selectedCurrency!.minorUnits, allowZero: true) ==
        parseMajorAmount(c.total.text, minorDigits: c.selectedCurrency!.minorUnits, allowZero: true);
    } on FormatException { return false; }
  }
  void _focusUnresolved() {
    Object key = c.total;
    Object? disclosure;
    if (c.selectedCurrency == null) { key = 'Currency'; }
    else if (!validReceiptDate(c.date.text.trim())) { key = c.date; }
    else if (_invalidMoney(c.total, positive: true)) { key = c.total; }
    else if (c.tipDecision.isEmpty) { key = 'Tip'; }
    else if (c.tipDecision == 'confirmed' && _invalidMoney(c.actualTip)) { key = c.actualTip; }
    else if (c.totalBasis.isEmpty) { key = 'Total source'; disclosure = 'receipt'; }
    else if (_invalidMoney(c.printedTotal, optional: true)) { key = c.printedTotal; disclosure = 'receipt'; }
    else if (_invalidMoney(c.finalPaid, optional: true)) { key = c.finalPaid; disclosure = 'receipt'; }
    else if (c.totalBasis == 'printed_total' && !_matchesTotal(c.printedTotal)) { key = c.printedTotal; disclosure = 'receipt'; }
    else if (c.totalBasis == 'final_paid' && !_matchesTotal(c.finalPaid)) { key = c.finalPaid; disclosure = 'receipt'; }
    else if (c.mode == 'total' && c.totalTitle.text.trim().isEmpty) { key = c.totalTitle; disclosure = 'receipt'; }
    else if (c.mode == 'itemized') {
      if (c.items.isEmpty) { key = 'Record as'; disclosure = 'receipt'; }
      for (final row in c.adjustments.reversed) {
        if (!_kinds.containsKey(row.kind)) { key = ('kind', row); disclosure = row; }
        if (!const ['add', 'subtract', 'included_in_line_total', 'informational'].contains(row.treatment)) { key = ('treatment', row); disclosure = null; }
        if ((row.treatment == 'add' || row.treatment == 'subtract') && row.itemIds.isEmpty) { key = ('eligibility', row); disclosure = ('eligibility', row); }
        if (_invalidMoney(row.amount)) { key = row.amount; disclosure = null; }
      }
      for (final row in c.items.reversed) {
        for (final field in [row.unitPrice, row.quantity]) {
          if (field.text.trim().isEmpty) continue;
          try { parseReceiptDecimal(field.text.trim()); } on FormatException { key = field; disclosure = row; }
        }
        if (_invalidMoney(row.amount)) { key = row.amount; disclosure = null; }
        if (row.title.text.trim().isEmpty) { key = row.title; disclosure = null; }
      }
    }
    if (disclosure != null) _disclosure(disclosure).expand();
    WidgetsBinding.instance.addPostFrameCallback((_) {
      if (!mounted) return;
      final node = _focus(key); node.requestFocus();
      if (node.context != null) Scrollable.ensureVisible(node.context!, alignment: 0.15);
    });
  }
  String _warningLabel(ReceiptReviewIssue issue) {
    final field = issue.field;
    if (field == 'merchant') return 'Merchant';
    if (issue.code.startsWith('model_issue_')) {
      if (field.startsWith('items.')) return 'Original receipt item';
      if (field.startsWith('adjustments.')) return 'Original receipt charge';
    }
    for (var i = 0; i < c.items.length; i++) { if (field == 'items.${c.items[i].id}' || field.startsWith('items.${c.items[i].id}.')) return 'Item ${i + 1}'; }
    for (final row in c.adjustments) { if (field == 'adjustments.${row.id}' || field.startsWith('adjustments.${row.id}.')) return _kinds[row.kind] ?? 'Charge'; }
    if (field.startsWith('items.')) return 'Original receipt item';
    if (field.startsWith('adjustments.')) return 'Original receipt charge';
    if (field.contains('suggested_tip') || field.contains('suggestedTip')) return 'Suggested tip';
    if (field.contains('tip') || field.contains('Tip')) return 'Tip';
    if (field.contains('currency')) return 'Currency';
    if (field.contains('date')) return 'Date';
    if (field.contains('total') || field.contains('Total')) return 'Total';
    return 'Receipt';
  }
  @override
  Widget build(BuildContext context) {
    final warnings = c.requiredWarnings;
    String? problem;
    List<ReceiptImportAmount>? amounts;
    bool parseable = false;
    try {
      final review = c.buildReview(); parseable = true;
      amounts = validateReceiptImportReview(c.catalog, c.evidence, review);
    } on FormatException catch (failure) { problem = failure.message; }
    final checked = warnings.every((issue) => c.acknowledgedWarnings.contains(issue.warningToken));
    final missingWarnings = parseable && warnings.isNotEmpty && !checked;
    if (missingWarnings && problem != null && problem!.startsWith('Acknowledge')) problem = null;
    final evidenceCurrency = c.evidence?.toJson()['currency'];
    final changedCurrency = evidenceCurrency is Map && evidenceCurrency['code'] != c.currency && c.currency.isNotEmpty;
    return Column(crossAxisAlignment: CrossAxisAlignment.stretch, children: [
      const SectionHeading(title: 'Review receipt'), const SizedBox(height: 12),
      _input('Merchant', c.merchant, update: () { if (!c.totalTitleEdited) c.totalTitle.text = c.merchant.text.trim().isEmpty ? 'Receipt expense' : c.merchant.text.trim(); }),
      _ReviewPair(first: _dateInput(), second: _choice('Currency', c.currency,
        {for (final currency in c.catalog) currency.code: '${currency.code} · ${currency.name}'}, (value) => c.currency = value)),
      if (changedCurrency) const Padding(padding: EdgeInsets.only(bottom: 12), child: Text('Amounts are not converted.')),
      _input('Total', c.total, update: () => c.totalBasis = 'user_entered'),
      _explanation('Amount to record for this receipt, including any known tip.'),
      _explanation(switch (c.totalBasis) {
        'printed_total' => 'Based on: Bill total',
        'final_paid' => 'Based on: Amount paid',
        'user_entered' => 'Based on: Entered total',
        _ => 'Choose a total source in Receipt details.',
      }),
      _choice('Tip', c.tipDecision, const {'none': 'No tip', 'confirmed': 'Amount known', 'bill_only': 'Bill only; tip unknown'}, (value) {
        c.tipDecision = value;
        if (value == 'none') c.actualTip.text = '0';
        if (value == 'bill_only') { c.actualTip.clear(); c.finalPaid.clear(); if (c.totalBasis == 'final_paid') c.totalBasis = 'user_entered'; }
      }),
      _explanation(switch (c.tipDecision) {
        'none' => 'Records a zero tip.',
        'confirmed' => 'Enter the tip included in Total.',
        'bill_only' => 'Record the bill amount in Total. Choosing this clears Tip amount and Amount paid.',
        _ => 'Choose whether you left a tip and how much.',
      }),
      if (c.tipDecision == 'confirmed') ...[
        _input('Tip amount', c.actualTip),
        if (c.mode == 'itemized') _explanation('The Tip charges in Charges and discounts must add up to this amount.'),
      ],
      if (c.mode == 'itemized') ...[
        const SectionHeading(title: 'Items'),
        for (var i = 0; i < c.items.length; i++) _item(c.items[i], i),
        OutlinedButton(onPressed: () { c.addItem(); widget.onChanged(); }, child: const Text('Add item')),
        if (c.adjustments.isNotEmpty) ...[const SizedBox(height: 12), const SectionHeading(title: 'Charges and discounts')],
        for (var i = 0; i < c.adjustments.length; i++) _adjustment(c.adjustments[i], i),
        TextButton(onPressed: () { c.addAdjustment(); widget.onChanged(); }, child: const Text('Add charge')),
      ],
      _details('receipt', 'Receipt details', [
        _choice('Record as', c.mode, const {'itemized': 'Item expenses', 'total': 'One expense'}, (value) => c.mode = value),
        if (c.mode == 'itemized') _explanation('Creates expenses from the items, charges and discounts.'),
        if (c.mode == 'total') _explanation('Records Total as one expense.'),
        if (c.mode == 'total') _input('Expense title', c.totalTitle, update: () => c.totalTitleEdited = true),
        _choice('Total source', c.totalBasis, {'printed_total': 'Bill total',
          if (c.tipDecision != 'bill_only') 'final_paid': 'Amount paid', 'user_entered': 'Entered total'}, (value) => c.totalBasis = value),
        if (c.totalBasis == 'printed_total') _explanation('Use the bill total printed on the receipt. Enter the same amount in Total and Bill total.'),
        if (c.totalBasis == 'final_paid') _explanation('Use the final amount you paid. Enter the same amount in Total and Amount paid.'),
        if (c.totalBasis == 'user_entered') _explanation('Records the amount you entered in Total.'),
        _input(c.totalBasis == 'printed_total' ? 'Bill total' : 'Bill total (optional)', c.printedTotal),
        _explanation(c.totalBasis == 'printed_total' ? 'Enter the same amount as Total.' : 'Printed on the receipt.'),
        if (c.tipDecision == 'bill_only') _explanation('Amount paid: unknown.') else ...[
          _input(c.totalBasis == 'final_paid' ? 'Amount paid' : 'Amount paid (optional)', c.finalPaid),
          _explanation(c.totalBasis == 'final_paid' ? 'Enter the same amount as Total.' : 'Final amount you paid.'),
        ],
      ], initiallyExpanded: c.items.isEmpty),
      if (warnings.isNotEmpty) _details('warnings', 'Check these details', [
        for (final issue in warnings) Padding(padding: const EdgeInsets.symmetric(vertical: 4), child: Text('${_warningLabel(issue)}: ${issue.message}')),
        if (parseable) CheckboxListTile(contentPadding: EdgeInsets.zero, title: const Text('I have checked these details'), value: checked,
          onChanged: (value) { c.acknowledgeWarnings(value ?? false); widget.onChanged(); }),
      ], initiallyExpanded: true),
      if (missingWarnings) const Text('Check the details below before continuing.'),
      if (!missingWarnings && amounts != null) Text('${amounts.length} expenses · ${c.selectedCurrency!.format(amounts.fold(BigInt.zero, (sum, amount) => sum + amount.amountMinor))}'),
      if (_submitted && problem != null) Semantics(liveRegion: true, child: Text(problem!)),
      const SizedBox(height: 12), FilledButton(onPressed: c.busy ? null : () {
        setState(() => _submitted = true);
        if (problem != null || missingWarnings) { _focusUnresolved(); return; }
        widget.onUse();
      }, child: const Text('Continue')),
    ]);
  }
}

class _ReviewPair extends StatelessWidget {
  const _ReviewPair({required this.first, required this.second});
  final Widget first, second;
  @override
  Widget build(BuildContext context) => LayoutBuilder(builder: (context, constraints) => constraints.maxWidth >= 600
    ? Row(crossAxisAlignment: CrossAxisAlignment.start, children: [Expanded(child: first), const SizedBox(width: 12), Expanded(child: second)])
    : Column(crossAxisAlignment: CrossAxisAlignment.stretch, children: [first, second]));
}

class ReceiptImagePreview extends StatefulWidget {
  const ReceiptImagePreview({super.key, required this.provider});
  final MemoryImage provider;
  @override
  State<ReceiptImagePreview> createState() => _ReceiptImagePreviewState();
}
class _ReceiptPhotoScaleGestureRecognizer extends ScaleGestureRecognizer {
  _ReceiptPhotoScaleGestureRecognizer({Object? debugOwner})
    : super(debugOwner: debugOwner, trackpadScrollCausesScale: false);

  late ValueGetter<double> currentScale;
  bool _canPanSinglePointer = false;
  bool _ownsGesture = false;

  void _startPointerSequence() {
    if (pointerCount == 0) {
      _canPanSinglePointer = currentScale() > 1;
      _ownsGesture = false;
    }
  }

  @override
  void addAllowedPointer(PointerDownEvent event) {
    _startPointerSequence();
    super.addAllowedPointer(event);
  }

  @override
  void addAllowedPointerPanZoom(PointerPanZoomStartEvent event) {
    _startPointerSequence();
    super.addAllowedPointerPanZoom(event);
  }

  @override
  void acceptGesture(int pointer) {
    _ownsGesture = true;
    super.acceptGesture(pointer);
  }

  @override
  void resolve(GestureDisposition disposition) {
    super.resolve(disposition == GestureDisposition.accepted && pointerCount == 1 &&
      !_canPanSinglePointer && !_ownsGesture ? GestureDisposition.rejected : disposition);
  }
}

class _ReceiptImagePreviewState extends State<ReceiptImagePreview> {
  final _transform = TransformationController();
  Size _viewport = Size.zero;
  double _gestureStartScale = 1;
  Offset _gestureScenePoint = Offset.zero;
  bool _imageFocused = false;

  Offset _translation(Matrix4 matrix) => Offset(matrix.entry(0, 3), matrix.entry(1, 3));

  Matrix4 _boundedTransform(double scale, Offset translation, Size viewport) {
    final nextScale = scale.clamp(1.0, 8.0).toDouble();
    return Matrix4.diagonal3Values(nextScale, nextScale, 1)
      ..setEntry(0, 3, translation.dx.clamp(viewport.width * (1 - nextScale), 0.0).toDouble())
      ..setEntry(1, 3, translation.dy.clamp(viewport.height * (1 - nextScale), 0.0).toDouble());
  }

  Matrix4 _clampedTransform(Matrix4 matrix, Size viewport) {
    final scale = matrix.getMaxScaleOnAxis();
    final translation = _translation(matrix);
    if (scale >= 1 && scale <= 8 &&
        translation.dx >= viewport.width * (1 - scale) && translation.dx <= 0 &&
        translation.dy >= viewport.height * (1 - scale) && translation.dy <= 0) return matrix;
    return _boundedTransform(scale, translation, viewport);
  }

  Matrix4 get _currentTransform => _clampedTransform(_transform.value, _viewport);
  double get _currentScale => _currentTransform.getMaxScaleOnAxis();

  void _writeTransform(double scale, Offset translation) {
    final next = _boundedTransform(scale, translation, _viewport);
    if (_transform.value != next) _transform.value = next;
  }

  void _zoomAt(double factor, Offset focalPoint) {
    final matrix = _currentTransform;
    final scale = matrix.getMaxScaleOnAxis();
    final scenePoint = (focalPoint - _translation(matrix)) / scale;
    final nextScale = (scale * factor).clamp(1.0, 8.0).toDouble();
    _writeTransform(nextScale, focalPoint - scenePoint * nextScale);
  }

  void _zoom(double factor) => _zoomAt(factor, _viewport.center(Offset.zero));

  void _pan(double dx, double dy) {
    final matrix = _currentTransform;
    _writeTransform(matrix.getMaxScaleOnAxis(), _translation(matrix) + Offset(dx, dy));
  }

  void _onScaleStart(ScaleStartDetails details) {
    final matrix = _currentTransform;
    _gestureStartScale = matrix.getMaxScaleOnAxis();
    _gestureScenePoint = (details.localFocalPoint - _translation(matrix)) / _gestureStartScale;
  }

  void _onScaleUpdate(ScaleUpdateDetails details) {
    final nextScale = (_gestureStartScale * details.scale).clamp(1.0, 8.0).toDouble();
    _writeTransform(nextScale, details.localFocalPoint - _gestureScenePoint * nextScale);
  }

  void _onPointerSignal(PointerSignalEvent event) {
    if (event is PointerScaleEvent) {
      GestureBinding.instance.pointerSignalResolver.register(event, (_) {
        _zoomAt(event.scale, event.localPosition);
      });
    }
  }

  void _updateViewport(Size viewport) {
    _viewport = viewport;
    if (_clampedTransform(_transform.value, viewport) == _transform.value) return;
    final provider = widget.provider;
    WidgetsBinding.instance.addPostFrameCallback((_) {
      if (!mounted || !identical(widget.provider, provider) || _viewport != viewport) return;
      final next = _clampedTransform(_transform.value, viewport);
      if (next != _transform.value) _transform.value = next;
    });
  }

  @override
  void didUpdateWidget(covariant ReceiptImagePreview oldWidget) {
    super.didUpdateWidget(oldWidget);
    if (oldWidget.provider != widget.provider) _transform.value = Matrix4.identity();
  }

  @override
  void dispose() { _transform.dispose(); super.dispose(); }

  @override
  Widget build(BuildContext context) => Column(crossAxisAlignment: CrossAxisAlignment.stretch, children: [
    SizedBox(height: 360, child: LayoutBuilder(builder: (context, constraints) {
      final viewport = Size(constraints.hasBoundedWidth ? constraints.maxWidth : constraints.minWidth,
        constraints.maxHeight);
      _updateViewport(viewport);
      return SizedBox.fromSize(size: viewport, child: Semantics(
        label: 'Receipt photo; use arrow keys to pan when zoomed',
        child: Focus(
          onFocusChange: (value) => setState(() => _imageFocused = value),
          onKeyEvent: (node, event) {
            if (!node.hasPrimaryFocus || event is! KeyDownEvent || _currentScale <= 1) return KeyEventResult.ignored;
            final key = event.logicalKey;
            if (key == LogicalKeyboardKey.arrowLeft) { _pan(60, 0); }
            else if (key == LogicalKeyboardKey.arrowRight) { _pan(-60, 0); }
            else if (key == LogicalKeyboardKey.arrowUp) { _pan(0, 60); }
            else if (key == LogicalKeyboardKey.arrowDown) { _pan(0, -60); }
            else { return KeyEventResult.ignored; }
            return KeyEventResult.handled;
          },
          child: DecoratedBox(
            position: DecorationPosition.foreground,
            decoration: BoxDecoration(border: Border.all(
              color: _imageFocused ? Theme.of(context).colorScheme.primary : Colors.transparent, width: 2)),
            child: ClipRect(child: Listener(
              behavior: HitTestBehavior.opaque,
              onPointerSignal: _onPointerSignal,
              child: RawGestureDetector(
                behavior: HitTestBehavior.opaque,
                gestures: <Type, GestureRecognizerFactory>{
                  _ReceiptPhotoScaleGestureRecognizer: GestureRecognizerFactoryWithHandlers<_ReceiptPhotoScaleGestureRecognizer>(
                    () => _ReceiptPhotoScaleGestureRecognizer(debugOwner: this),
                    (instance) => instance
                      ..currentScale = (() => _currentScale)
                      ..trackpadScrollCausesScale = false
                      ..onStart = _onScaleStart
                      ..onUpdate = _onScaleUpdate,
                  ),
                },
                child: AnimatedBuilder(
                  animation: _transform,
                  builder: (context, child) => Transform(
                    key: const ValueKey('receipt-photo-transform'),
                    transform: _clampedTransform(_transform.value, viewport),
                    child: child,
                  ),
                  child: SizedBox.fromSize(size: viewport, child: Image(
                    image: widget.provider, fit: BoxFit.contain, semanticLabel: 'Receipt photo',
                    errorBuilder: (_, error, stack) => const Text('This photo could not be displayed. Choose another photo.'),
                  )),
                ),
              ),
            )),
          ),
        ),
      ));
    })),
    Wrap(spacing: 8, runSpacing: 8, children: [
      TextButton(onPressed: () => _zoom(2), child: const Text('Zoom in')),
      TextButton(onPressed: () => _zoom(0.5), child: const Text('Zoom out')),
      TextButton(onPressed: () => _transform.value = Matrix4.identity(), child: const Text('Reset image zoom')),
    ]),
  ]);
}

Future<void> showReceiptSource(BuildContext context, {required JizhangRepository repository,
  required SessionController session, required String groupId, required String receiptId}) => showDialog<void>(
    context: context, builder: (_) => _ReceiptSourceDialog(repository: repository, session: session, groupId: groupId, receiptId: receiptId));

class _ReceiptSourceDialog extends StatefulWidget {
  const _ReceiptSourceDialog({required this.repository, required this.session, required this.groupId, required this.receiptId});
  final JizhangRepository repository;
  final SessionController session;
  final String groupId, receiptId;
  @override
  State<_ReceiptSourceDialog> createState() => _ReceiptSourceDialogState();
}
class _ReceiptSourceDialogState extends State<_ReceiptSourceDialog> {
  ReceiptSourceInput? _source;
  MemoryImage? _provider;
  String? _error;
  bool _pending = true;
  late final int _sessionGeneration;
  int _generation = 0;
  @override
  void initState() {
    super.initState();
    _sessionGeneration = widget.session.generation;
    widget.session.addListener(_sessionChanged);
    _load();
  }
  void _sessionChanged() { if (_sessionGeneration != widget.session.generation && mounted) { _generation++; _release(); Navigator.pop(context); } }
  void _release() { _provider?.evict(); _provider = null; _source = null; }
  Future<void> _load() async {
    final generation = ++_generation;
    setState(() { _pending = true; _error = null; });
    try {
      final source = await widget.repository.getReceiptSource(widget.groupId, widget.receiptId);
      if (!mounted || generation != _generation || _sessionGeneration != widget.session.generation) return;
      setState(() { _source = source; _provider = MemoryImage(source.image.bytes); _pending = false; });
    } catch (failure) {
      if (!mounted || generation != _generation || _sessionGeneration != widget.session.generation) return;
      setState(() { _pending = false; _error = errorMessage(failure); });
    }
  }
  @override
  void dispose() { _generation++; widget.session.removeListener(_sessionChanged); _release(); super.dispose(); }
  Widget _historicalReview(ReceiptImportReview review) => ExpansionTile(title: const Text('Import details'), children: [
    Text('Merchant: ${review.merchant}'), Text('Date: ${review.date}'), Text('Currency: ${review.currency}'),
    Text('Record as: ${review.mode == 'itemized' ? 'Item expenses' : 'One expense'}'),
  ]);
  @override
  Widget build(BuildContext context) => Dialog(child: ConstrainedBox(constraints: const BoxConstraints(maxWidth: 1000), child: Column(mainAxisSize: MainAxisSize.min, children: [
    Flexible(child: SingleChildScrollView(padding: const EdgeInsets.all(20), child: Column(crossAxisAlignment: CrossAxisAlignment.stretch, children: [
      Text('Receipt source', style: Theme.of(context).textTheme.titleLarge), const SizedBox(height: 12),
      if (_pending) Semantics(liveRegion: true, child: const Text('Loading source image…')),
      if (_error != null) ...[Text(_error!), TextButton(onPressed: _load, child: const Text('Retry loading source'))],
      if (_provider != null) ReceiptImagePreview(provider: _provider!),
      if (_source?.review != null) _historicalReview(_source!.review!),
    ]))),
    Padding(padding: const EdgeInsets.all(12), child: TextButton(onPressed: () => Navigator.pop(context), child: const Text('Close source'))),
  ])));
}
