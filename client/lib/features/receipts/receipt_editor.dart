import 'package:flutter/material.dart';

import '../../core/api_client.dart';
import '../../core/jizhang_repository.dart';
import '../../core/models.dart';
import '../../core/receipt_ai_models.dart';
import '../../core/receipt_import_math.dart';
import '../../core/session_controller.dart';
import '../../shared/form_controller.dart';
import '../../shared/record_navigation.dart';
import '../../shared/widgets.dart';
import '../groups/group_screen.dart';
import '../records/record_editor.dart';
import 'receipt_import_controller.dart';
import 'receipt_import_review.dart';
import 'receipt_media.dart';
import 'receipt_batch_edit.dart';

class ReceiptEditor extends StatelessWidget {
  const ReceiptEditor({super.key, required this.repository, required this.session,
    required this.groupId, this.receiptId, this.media});
  final JizhangRepository repository;
  final SessionController session;
  final String groupId;
  final String? receiptId;
  final ReceiptMedia? media;
  @override
  Widget build(BuildContext context) {
    final group = GroupScope.of(context).group;
    if (receiptId == null) return _ReceiptForm(key: ValueKey((groupId, session.generation)), repository: repository, session: session, group: group, media: media);
    return LoadView<Receipt>(load: () => repository.getReceipt(groupId, receiptId!),
      builder: (receipt) => _ReceiptForm(key: ValueKey((groupId, receiptId, session.generation)), repository: repository, session: session, group: group, receipt: receipt, media: media));
  }
}

class _ReceiptForm extends StatefulWidget {
  const _ReceiptForm({super.key, required this.repository, required this.session, required this.group, this.receipt, this.media});
  final JizhangRepository repository;
  final SessionController session;
  final ExpenseGroup group;
  final Receipt? receipt;
  final ReceiptMedia? media;
  @override
  State<_ReceiptForm> createState() => _ReceiptFormState();
}

class _ReceiptFormState extends State<_ReceiptForm> {
  late final FormController _form = FormController(widget.session);
  late final TextEditingController _title = TextEditingController(text: widget.receipt?.title ?? '');
  late final TextEditingController _note = TextEditingController(text: widget.receipt?.note ?? '');
  late final ReceiptImportController _import = ReceiptImportController(repository: widget.repository,
    session: widget.session, groupId: widget.group.groupId, media: widget.media ?? createReceiptMedia());
  final List<SplitFieldsController> _items = [];
  final Set<SplitFieldsController> _pendingDisposal = {};
  final Set<SplitFieldsController> _selectedItems = Set.identity();
  final Map<SplitFieldsController, String> _importItemIds = {};
  final Map<SplitFieldsController, String> _roles = {};
  final Map<SplitFieldsController, ExpansibleController> _cardControllers = {};
  final Map<SplitFieldsController, FocusScopeNode> _cardFocus = {};
  final Map<SplitFieldsController, PageStorageBucket> _cardStorage = {};
  final PageStorageBucket _photoOptionsStorage = PageStorageBucket();
  final PageStorageBucket _photoPrivacyStorage = PageStorageBucket();
  final PageStorageBucket _photoPreviewStorage = PageStorageBucket();
  final FocusNode _titleFocus = FocusNode();
  final FocusNode _noteFocus = FocusNode();
  bool _editingMultiple = false;
  bool _titleEdited = false;
  String? _lastTitleSuggestionExtractionId;
  bool get _previewing => _import.stage == ReceiptImportStage.preview;
  ExpansibleController _cardController(SplitFieldsController item) =>
    _cardControllers.putIfAbsent(item, ExpansibleController.new);
  FocusScopeNode _focusScope(SplitFieldsController item) =>
    _cardFocus.putIfAbsent(item, FocusScopeNode.new);
  bool _payersConfirmed = false, _removeSource = false;
  ReceiptImportReview? _previousReview;
  bool _reviewingSavedSource = false;
  late int _sessionGeneration;
  bool get _busy => _form.pending || _import.busy;
  bool get _newReviewedSource => _import.acceptedReview != null;
  bool get _reviewing => _import.stage == ReceiptImportStage.review;
  @override
  void initState() {
    super.initState();
    _sessionGeneration = widget.session.generation;
    widget.session.addListener(_sessionChanged);
    _import.addListener(_importChanged); _import.loadOptions();
    _form.addListener(_formChanged);
    for (final record in widget.receipt?.records ?? <LedgerRecord>[]) {
      if (record is ExpenseRecord) {
        _items.add(SplitFieldsController(repository: widget.repository, session: widget.session, group: widget.group, draft: SplitRecordDraft.fromRecord(record),
          savedCurrency: record.currency, savedRate: record.exchangeRate,
          currentUserId: widget.session.user?.userId, historicalUsers: [record.paidBy, ...record.splits.map((split) => split.user)]));
      }
    }
  }
  void _importChanged() {
    final extractionId = _import.extractionId;
    if (_import.stage == ReceiptImportStage.review && extractionId != null && extractionId != _lastTitleSuggestionExtractionId) {
      _lastTitleSuggestionExtractionId = extractionId;
      final suggestion = _import.suggestedTitle;
      if (!_titleEdited && _title.text.trim().isEmpty && suggestion != null) {
        _title.text = suggestion;
        _form.markDirty();
      }
    }
    _form.discardMessage = _import.stage == ReceiptImportStage.extracting
      ? 'Your unsaved changes will be lost. Receipt extraction may still finish on the server and charge this attempt even if you leave.'
      : 'Your unsaved changes will be lost.';
    if (mounted) setState(() {});
  }
  void _formChanged() {
    if (!_form.dirty && !_form.pending && _import.stage == ReceiptImportStage.extracting) _import.cancelExtraction();
  }
  void _sessionChanged() {
    if (_sessionGeneration == widget.session.generation) return;
    _sessionGeneration = widget.session.generation;
    _title.clear(); _titleFocus.unfocus();
    _titleEdited = false; _lastTitleSuggestionExtractionId = null;
    _note.clear(); _previousReview = null; _payersConfirmed = false; _removeSource = false;
    _reviewingSavedSource = false;
    _editingMultiple = false;
    final previousItems = List<SplitFieldsController>.of(_items);
    if (mounted) setState(() { _items.clear(); _selectedItems.clear(); _importItemIds.clear(); _roles.clear(); });
    for (final item in previousItems) { _retire(item); }
  }
  @override
  void didUpdateWidget(covariant _ReceiptForm oldWidget) {
    super.didUpdateWidget(oldWidget);
    if (!identical(oldWidget.group, widget.group)) {
      for (final item in _items) { item.updateGroup(widget.group); }
      _payersConfirmed = false;
    }
  }
  @override
  void dispose() {
    widget.session.removeListener(_sessionChanged);
    _form.removeListener(_formChanged);
    _import.removeListener(_importChanged); _import.dispose();
    _form.dispose(); _title.dispose(); _note.dispose();
    _drainRetired();
    _titleFocus.dispose();
    _noteFocus.dispose();
    for (final controller in _cardControllers.values) { controller.dispose(); }
    for (final node in _cardFocus.values) { node.dispose(); }
    for (final item in _items) { item.dispose(); }
    super.dispose();
  }
  void _add() {
    if (_newReviewedSource) { _returnToReview(); return; }
    final item = SplitFieldsController(repository: widget.repository, session: widget.session, group: widget.group,
      currentUserId: widget.session.user?.userId);
    _cardController(item).expand();
    setState(() => _items.add(item));
    _form.markDirty();
  }
  void _drainRetired() {
    final retired = List<SplitFieldsController>.of(_pendingDisposal);
    _pendingDisposal.clear();
    for (final item in retired) {
      _cardControllers.remove(item)?.dispose(); _cardFocus.remove(item)?.dispose(); item.dispose();
      _cardStorage.remove(item);
    }
  }
  void _retire(SplitFieldsController item) {
    item.deactivate();
    _pendingDisposal.add(item);
    WidgetsBinding.instance.addPostFrameCallback((_) => _drainRetired());
  }
  void _remove(SplitFieldsController item) {
    if (_busy || !_items.contains(item)) return;
    if (_newReviewedSource) { _returnToReview(); return; }
    setState(() { _items.remove(item); _selectedItems.remove(item); _roles.remove(item); _importItemIds.remove(item); });
    _retire(item); _form.markDirty();
  }
  String _roleFingerprint(SplitFieldsController item) => '${item.actorUserId}|${item.splitMode.name}|${item.selected.map((id) => '$id:${item.splitMode == SplitMode.equal ? '1' : item.splitInput(id).text}').join('|')}';
  void _expenseChanged(SplitFieldsController item) {
    final next = _roleFingerprint(item);
    if (_roles[item] != next) { setState(() => _payersConfirmed = false); _roles[item] = next; }
    _form.markDirty();
  }
  Future<void> _batchEdit() async {
    if (_busy || _selectedItems.isEmpty) return;
    final generation = widget.session.generation;
    final selected = _items.where(_selectedItems.contains).toList();
    final changes = await showReceiptBatchEditor(context, session: widget.session, group: widget.group, selected: selected);
    if (!mounted || generation != widget.session.generation || _busy || changes == null) return;
    if (selected.any((item) => !_items.contains(item))) return;
    try {
      for (final item in selected) { _roles.putIfAbsent(item, () => _roleFingerprint(item)); }
      changes.apply(widget.group, selected, onChanged: _expenseChanged);
      setState(() {});
    } on FormatException catch (error) { setState(() => _form.error = error.message); }
  }
  Future<bool> _confirmReplacement() async {
    if (_items.isEmpty) return true;
    final generation = widget.session.generation;
    final result = await confirmAction(context, 'Replace displayed expenses?',
      'Using reviewed expenses replaces the displayed expenses and their payer, split and exchange-rate choices.', action: 'Continue to review');
    return mounted && generation == widget.session.generation && result;
  }
  Future<void> _selectImage({bool camera = false}) async {
    if (_busy) return;
    if (_import.image != null && !await _confirmReplacement()) return;
    if (!mounted) return;
    if (await _import.selectImage(context, camera: camera)) { _previousReview = null; _reviewingSavedSource = false; _removeSource = false; _form.markDirty(); }
  }
  Future<void> _extract() async {
    if (_busy || !await _confirmReplacement()) return;
    if (!mounted) return;
    if (_import.image == null) {
      if (widget.receipt == null || !await _import.openSavedSource(widget.receipt!.receiptId)) return;
      if (!mounted) return;
    }
    _previousReview = null; _form.markDirty(); await _import.extract();
  }
  Future<void> _returnToReview() async {
    if (_busy || !await _confirmReplacement()) return;
    if (!mounted) return;
    _previousReview = _import.acceptedReview;
    _reviewingSavedSource = false;
    _editingMultiple = false; _selectedItems.clear();
    _import.returnToReview();
  }
  void _cancelReview() {
    if (_reviewingSavedSource) {
      _reviewingSavedSource = false; _previousReview = null;
      _import.clear();
      return;
    }
    if (_previousReview == null) return;
    final review = _previousReview!; _previousReview = null;
    _import.restoreReview(review);
  }
  Future<void> _replaceFromSavedImage() async {
    if (_busy || widget.receipt == null || !await _confirmReplacement()) return;
    if (!mounted) return;
    if (await _import.openSavedSource(widget.receipt!.receiptId)) {
      _previousReview = null; _reviewingSavedSource = true;
      _editingMultiple = false; _selectedItems.clear();
      _form.markDirty();
    }
  }
  Future<void> _useReviewed() async {
    if (_busy) return;
    try { _validateTitle(); }
    on FormatException catch (failure) { setState(() => _form.error = failure.message); return; }
    _form.error = null;
    List<ReceiptImportAmount> amounts;
    try { amounts = _import.acceptReview(); }
    on FormatException catch (failure) { setState(() => _import.error = failure.message); return; }
    final review = _import.acceptedReview!;
    final previous = List<SplitFieldsController>.of(_items);
    setState(() {
      _items.clear(); _selectedItems.clear(); _importItemIds.clear(); _roles.clear(); _previousReview = null; _payersConfirmed = false;
      _reviewingSavedSource = false;
      _editingMultiple = false;
      for (final amount in amounts) {
        final item = SplitFieldsController(repository: widget.repository, session: widget.session, group: widget.group,
          currentUserId: widget.session.user?.userId, savedCurrency: _import.selectedCurrency,
          draft: SplitRecordDraft(kind: RecordKind.expense, title: amount.title, amountMinor: amount.amountMinor,
            currency: review.currency, actorUserId: widget.group.members.any((member) => member.userId == widget.session.user?.userId)
              ? widget.session.user!.userId : '', date: review.date, splitMode: SplitMode.equal,
            splits: [for (final member in widget.group.members) SplitDraft(userId: member.userId, value: BigInt.one)]));
        _items.add(item); _importItemIds[item] = amount.itemId; _roles[item] = _roleFingerprint(item);
      }
    });
    for (final item in previous) { _retire(item); }
    _form.markDirty();
  }
  Future<void> _detachSource() async {
    if (_busy) return;
    final generation = widget.session.generation;
    if (!await confirmAction(context, 'Remove photo?', 'Remove the photo and its saved reading when you save. Keep the expenses.', action: 'Remove photo')) return;
    if (!mounted || generation != widget.session.generation) return;
    _import.clear(); setState(() { _removeSource = true; _importItemIds.clear(); _previousReview = null; _reviewingSavedSource = false; _payersConfirmed = false; }); _form.markDirty();
  }
  Widget _expenseCard(SplitFieldsController item, int ordinal) => PageStorage(key: ObjectKey(item),
    bucket: _cardStorage.putIfAbsent(item, PageStorageBucket.new), child: Card(child: ExpansionTile(
    key: ObjectKey(item), controller: _cardController(item), maintainState: true,
    leading: _editingMultiple ? Checkbox(value: _selectedItems.contains(item), semanticLabel: 'Select expense $ordinal',
      onChanged: _busy ? null : (value) => setState(() { if (value ?? false) { _selectedItems.add(item); } else { _selectedItems.remove(item); } })) : null,
    title: ListenableBuilder(listenable: item, builder: (context, _) {
      final title = item.title.text.trim(), amount = item.amount.text.trim();
      final splitLabel = switch (item.splitMode) {
        SplitMode.equal => 'Equal', SplitMode.shares => 'Shares',
        SplitMode.percentage => 'Percentages', SplitMode.exact => 'Exact amounts',
      };
      return Column(crossAxisAlignment: CrossAxisAlignment.start, children: [
        Text(title.isEmpty ? 'Expense $ordinal' : title), const SizedBox(height: 4),
        Text('${amount.isEmpty ? 'Amount not set' : '${item.currency.code} $amount'} · Paid by ${item.actorUserId.isEmpty ? 'Not selected' : item.label(item.actorUserId)}', style: Theme.of(context).textTheme.bodyMedium),
        Text('$splitLabel: ${item.selected.isEmpty ? 'No participants' : item.selected.map(item.label).join(', ')}', style: Theme.of(context).textTheme.bodyMedium),
        if (item.exchangeRate.needsConversion) _convertedSummary(item),
      ]);
    }), tilePadding: const EdgeInsets.symmetric(horizontal: 12), childrenPadding: const EdgeInsets.all(12), children: [
      FocusScope(node: _focusScope(item), child: SplitFields(key: ObjectKey(item), session: widget.session, controller: item,
        onChanged: () => _expenseChanged(item), enabled: !_busy, moneyLocked: _newReviewedSource, onReviewMoney: _returnToReview)),
      const SizedBox(height: 12), Align(alignment: Alignment.centerLeft, child: TextButton.icon(
        onPressed: _busy ? null : () => _remove(item), icon: const Icon(Icons.remove_circle_outline), label: Text('Remove expense $ordinal'))),
    ])));
  void _revealInvalidExpense(SplitFieldsController item) {
    _cardController(item).expand();
    WidgetsBinding.instance.addPostFrameCallback((_) {
      if (!mounted || !_items.contains(item)) return;
      _focusScope(item).requestFocus();
      _focusScope(item).nextFocus();
      final cardContext = _focusScope(item).context;
      if (cardContext != null) Scrollable.ensureVisible(cardContext);
    });
  }
  Widget _convertedSummary(SplitFieldsController item) {
    try { return Text('Settlement: ${item.group.defaultCurrency.format(item.settlementMinor)}'); }
    on FormatException { return const Text('Settlement: rate or amount required'); }
  }
  String _validateTitle() {
    try { return normalizeReceiptTitle(_title.text); }
    on FormatException {
      final generation = widget.session.generation;
      WidgetsBinding.instance.addPostFrameCallback((_) {
        if (!mounted || generation != widget.session.generation) return;
        _titleFocus.requestFocus();
        if (_titleFocus.context != null) Scrollable.ensureVisible(_titleFocus.context!);
      });
      rethrow;
    }
  }
  Future<void> _save() async {
    if (_busy) return;
    ReceiptDraft draft;
    try {
      final title = _validateTitle();
      if (_note.text.contains('\u0000')) {
        WidgetsBinding.instance.addPostFrameCallback((_) {
          if (!mounted) return;
          _noteFocus.requestFocus();
          if (_noteFocus.context != null) Scrollable.ensureVisible(_noteFocus.context!);
        });
        throw const FormatException('Note cannot contain NUL');
      }
      if (_reviewing || (_import.evidence != null && !_newReviewedSource)) throw const FormatException('Continue from receipt review before saving.');
      if (_previewing) throw const FormatException('Choose Read receipt or Enter manually before saving.');
      if (_newReviewedSource && !_payersConfirmed) throw const FormatException('Confirm the displayed payers and splits before saving.');
      final records = <SplitRecordDraft>[];
      for (var i = 0; i < _items.length; i++) {
        try { records.add(_items[i].toDraft()); }
        on FormatException catch (error) { _revealInvalidExpense(_items[i]); throw FormatException('Expense ${i + 1}: ${error.message}'); }
      }
      if (_newReviewedSource) {
        final review = _import.acceptedReview!;
        final canonical = validateReceiptImportReview(_import.catalog, _import.evidence, review);
        if (canonical.length != records.length) throw const FormatException('Return to import review to change expense rows.');
        for (var i = 0; i < canonical.length; i++) {
          if (_importItemIds[_items[i]] != canonical[i].itemId || records[i].amountMinor != canonical[i].amountMinor || records[i].currency != review.currency) {
            throw const FormatException('Return to import review to change expense amounts or currency.');
          }
        }
      }
      final source = _import.source;
      draft = ReceiptDraft(title: title, note: _note.text, records: records, sourceChange: source != null
        ? ReceiptSourceChange.replace(source) : _removeSource ? const ReceiptSourceChange.remove() : const ReceiptSourceChange.keep());
    } on FormatException catch (error) { setState(() => _form.error = error.message); return; }
    Receipt? saved;
    final generation = widget.session.generation;
    final previousStage = _import.stage;
    setState(() => _import.stage = ReceiptImportStage.saving);
    bool policyRace = false;
    final success = await _form.save(() async {
      try {
        saved = widget.receipt == null ? await widget.repository.createReceipt(widget.group.groupId, draft)
          : await widget.repository.updateReceipt(widget.group.groupId, widget.receipt!.receiptId, draft);
      } on ApiException catch (e) { policyRace = e.statusCode == 409; rethrow; }
    });
    if (mounted && generation == widget.session.generation) setState(() => _import.stage = previousStage);
    if (mounted && generation == widget.session.generation && (success || policyRace)) await GroupScope.of(context).refreshGroup();
    if (success && mounted && generation == widget.session.generation) {
      GroupScope.of(context).recordChanges.markChanged();
      showSuccess(context, 'Receipt saved. Its expenses and balances have been updated.');
      await _return('/groups/${widget.group.groupId}/receipts/${saved!.receiptId}');
    }
  }
  Future<void> _return(String fallback) async {
    if (_form.pending) return;
    final generation = widget.session.generation;
    final groupId = widget.group.groupId;
    final receiptId = widget.receipt?.receiptId;
    await returnFromRecord(context, widget.session, fallback: fallback,
      canReturn: () => mounted && !_form.pending && generation == widget.session.generation &&
        groupId == widget.group.groupId && receiptId == widget.receipt?.receiptId);
  }
  bool get _hasPhoto => _import.image != null || (widget.receipt?.source != null && !_removeSource);
  Widget _photoAction({required String label, required String description,
    required VoidCallback? onPressed, bool destructive = false}) => OutlinedButton(
    onPressed: onPressed,
    style: OutlinedButton.styleFrom(alignment: Alignment.centerLeft,
      minimumSize: const Size(0, 48), padding: const EdgeInsets.all(12),
      foregroundColor: destructive ? Theme.of(context).colorScheme.error : null),
    child: Semantics(label: label, hint: description, excludeSemantics: true,
      child: Column(mainAxisSize: MainAxisSize.min, crossAxisAlignment: CrossAxisAlignment.stretch, children: [
        Text(label, textAlign: TextAlign.left),
        const SizedBox(height: 4),
        Text(description, textAlign: TextAlign.left, style: TextStyle(
          fontSize: Theme.of(context).textTheme.bodySmall?.fontSize,
          height: Theme.of(context).textTheme.bodySmall?.height, fontWeight: FontWeight.normal)),
      ])));
  Widget _photoSelection({required bool replacing}) {
    if (!replacing) return Wrap(spacing: 8, runSpacing: 8, children: [
      OutlinedButton(onPressed: !_busy && _import.media.supported && _import.maxBytes > 0 ? () => _selectImage() : null,
        child: const Text('Upload photo')),
      OutlinedButton(onPressed: !_busy && _import.media.cameraSupported && _import.maxBytes > 0 ? () => _selectImage(camera: true) : null,
        child: const Text('Take photo')),
    ]);
    return Column(spacing: 8, crossAxisAlignment: CrossAxisAlignment.stretch, children: [
      if (_import.media.supported && _import.maxBytes > 0) _photoAction(label: 'Replace photo',
        description: 'Choose a different photo from your device.', onPressed: _busy ? null : () => _selectImage()),
      if (_import.media.cameraSupported && _import.maxBytes > 0) _photoAction(label: 'Take photo',
        description: 'Use your camera to replace this photo.', onPressed: _busy ? null : () => _selectImage(camera: true)),
    ]);
  }
  Widget _imageTools() => Column(crossAxisAlignment: CrossAxisAlignment.stretch, children: [
    if (!_import.optionsLoaded) Semantics(liveRegion: true, child: const Text('Loading photo options…')),
    if (_import.optionsError != null) ...[Text('Photo options could not be loaded: ${_import.optionsError}'),
      TextButton(onPressed: _busy ? null : _import.loadOptions, child: const Text('Retry photo options'))],
    if (_import.optionsLoaded && _import.maxBytes == 0 && _import.optionsError == null)
      const Text('Photo uploads are unavailable. You can still add expenses manually.'),
    if (_import.optionsLoaded && !_import.available && _import.maxBytes > 0)
      const Text('Receipt reading is unavailable. You can attach a photo and enter expenses manually.'),
    if (!_import.media.supported) const Text('Photo selection is unavailable on this platform. Add expenses manually.'),
    if (_import.media.supported && !_import.media.cameraSupported)
      const Text('Camera capture needs a supported browser on HTTPS or localhost. Upload a photo instead.'),
    if (!_hasPhoto) _photoSelection(replacing: false),
    if (_hasPhoto) PageStorage(bucket: _photoOptionsStorage, child: ExpansionTile(
      key: const PageStorageKey('receipt-photo-options'), title: const Text('Photo options'),
      tilePadding: EdgeInsets.zero, expandedCrossAxisAlignment: CrossAxisAlignment.stretch,
      children: [Column(spacing: 8, crossAxisAlignment: CrossAxisAlignment.stretch, children: [
        if (widget.receipt?.source != null && !_removeSource) _photoAction(label: 'View photo',
          description: 'Open the saved photo.', onPressed: _busy ? null : () => showReceiptSource(context,
            repository: widget.repository, session: widget.session, groupId: widget.group.groupId, receiptId: widget.receipt!.receiptId)),
        if (_newReviewedSource) _photoAction(label: 'Review photo',
          description: 'Edit the current receipt details.', onPressed: _busy ? null : _returnToReview)
        else if (_import.image == null && widget.receipt?.source != null) _photoAction(label: 'Review photo',
          description: 'Edit the saved receipt details.', onPressed: _busy ? null : _replaceFromSavedImage),
        if (!_previewing && _import.available) _photoAction(label: 'Read again',
          description: 'Send this photo to OpenAI again; API charges may apply.', onPressed: _busy ? null : _extract),
        if ((_import.media.supported || _import.media.cameraSupported) && _import.maxBytes > 0)
          _photoSelection(replacing: true),
        Padding(padding: const EdgeInsets.only(top: 8), child: _photoAction(label: 'Remove photo',
          description: 'Remove the photo when you save; keep the expenses.',
          onPressed: _busy ? null : _detachSource, destructive: true)),
      ])])),
    if (_removeSource) const Text('Photo will be removed when you save.'),
    if (_import.stage == ReceiptImportStage.selecting) Semantics(liveRegion: true, child: const Text('Opening receipt photo…')),
    if (_import.image != null) ...[
      if (_previewing) ...[
        const SizedBox(height: 12),
        const Text('Sends this photo to OpenAI; API charges may apply.'),
        const SizedBox(height: 12),
        Wrap(spacing: 8, runSpacing: 8, children: [
          FilledButton(onPressed: !_busy && _import.available ? _extract : null, child: const Text('Read receipt')),
          TextButton(onPressed: _busy ? null : () { _import.useManualAttachment(); _form.markDirty(); }, child: const Text('Enter manually')),
        ]),
      ],
      if (_import.stage == ReceiptImportStage.extracting) ...[
        Semantics(liveRegion: true, child: const Text('Reading receipt…')),
        const Text('This request may still be charged.'),
        TextButton(onPressed: _import.cancelExtraction, child: const Text('Cancel reading')),
      ],
    ],
    if (_hasPhoto) PageStorage(bucket: _photoPrivacyStorage, child: const ExpansionTile(title: Text('Photo privacy'), tilePadding: EdgeInsets.zero, children: [
      Text('Photos are saved with the receipt and visible to its group. OpenAI handles submitted photos under its API terms.'),
    ])),
    if (_import.error != null) Semantics(liveRegion: true, child: Padding(padding: const EdgeInsets.symmetric(vertical: 12), child: Text(_import.error!))),
  ]);
  Widget _expenses() => Column(crossAxisAlignment: CrossAxisAlignment.stretch, children: [
    if (_newReviewedSource) const Text('Check who paid and how each expense is split.'),
    if (_items.isNotEmpty) Wrap(spacing: 8, runSpacing: 8, crossAxisAlignment: WrapCrossAlignment.center, children: [
      if (!_editingMultiple) TextButton(onPressed: _busy ? null : () => setState(() => _editingMultiple = true), child: const Text('Edit multiple')),
      if (_editingMultiple) ...[
        SizedBox(width: 180, child: CheckboxListTile(contentPadding: EdgeInsets.zero, controlAffinity: ListTileControlAffinity.leading,
          title: const Text('Select all'), value: _items.isNotEmpty && _selectedItems.length == _items.length,
          onChanged: _busy ? null : (value) => setState(() { if (value ?? false) { _selectedItems.addAll(_items); } else { _selectedItems.clear(); } }))),
        Semantics(liveRegion: true, child: Text('${_selectedItems.length} selected')),
        OutlinedButton(onPressed: _busy || _selectedItems.isEmpty ? null : _batchEdit, child: const Text('Edit selected')),
        TextButton(onPressed: _busy ? null : () => setState(() { _editingMultiple = false; _selectedItems.clear(); }), child: const Text('Done')),
      ],
    ]),
    for (var i = 0; i < _items.length; i++) _expenseCard(_items[i], i + 1),
    OutlinedButton.icon(onPressed: _busy ? null : _add, icon: const Icon(Icons.add), label: const Text('Add expense')),
    if (_newReviewedSource) CheckboxListTile(contentPadding: EdgeInsets.zero,
      title: const Text('I confirm the displayed payers and splits'), value: _payersConfirmed,
      onChanged: _busy ? null : (value) { setState(() => _payersConfirmed = value ?? false); _form.markDirty(); }),
  ]);
  Widget _content() {
    final fields = Column(crossAxisAlignment: CrossAxisAlignment.stretch, children: [
      _imageTools(), const SizedBox(height: 16),
      if (_reviewing) ...[
        ReceiptImportReviewFields(controller: _import, onUse: _useReviewed, onChanged: _form.markDirty),
        if (_previousReview != null || _reviewingSavedSource) TextButton(onPressed: _cancelReview, child: const Text('Cancel review changes and keep displayed expenses')),
      ] else if (!_previewing && _import.stage != ReceiptImportStage.extracting) _expenses(),
    ]);
    final provider = _import.imageProvider;
    if (provider == null) return fields;
    return LayoutBuilder(builder: (context, constraints) {
      final preview = ReceiptImagePreview(provider: provider);
      if (constraints.maxWidth >= 1000) return Row(crossAxisAlignment: CrossAxisAlignment.start, children: [
        Expanded(flex: 35, child: preview), const SizedBox(width: 20), Expanded(flex: 65, child: fields)]);
      return Column(crossAxisAlignment: CrossAxisAlignment.stretch, children: [
        PageStorage(bucket: _photoPreviewStorage, child: ExpansionTile(title: const Text('Photo preview'), initiallyExpanded: _previewing,
          children: [preview])), const SizedBox(height: 12), fields,
      ]);
    });
  }
  @override
  Widget build(BuildContext context) => FormLeaveGuard(controller: _form,
    child: ListenableBuilder(listenable: _form, builder: (context, _) => FormSurface(maxWidth: 1200,
      title: widget.receipt == null ? 'New receipt' : 'Edit receipt',
      child: Column(crossAxisAlignment: CrossAxisAlignment.stretch, children: [
        Text('Uploaded by ${widget.receipt != null ? userLabel(widget.receipt!.uploadedBy) : widget.session.user != null ? userLabel(widget.session.user!) : ''}'),
        if (widget.receipt != null) const Text('Saving replaces all receipt expenses. Their previous record links will no longer be available.'),
        const SizedBox(height: 16),
        TextField(controller: _title, focusNode: _titleFocus, enabled: !_busy,
          decoration: const InputDecoration(label: Text('Receipt title', softWrap: true),
            floatingLabelBehavior: FloatingLabelBehavior.always),
          onChanged: (_) { _titleEdited = true; _form.markDirty(); }),
        const SizedBox(height: 12),
        TextField(controller: _note, focusNode: _noteFocus, enabled: !_busy, minLines: 2, maxLines: 4,
          decoration: const InputDecoration(labelText: 'Receipt note (optional)',
            floatingLabelBehavior: FloatingLabelBehavior.always, hintText: 'Add a note for this receipt'),
          onChanged: (_) => _form.markDirty()),
        const SizedBox(height: 12), AbsorbPointer(absorbing: _form.pending, child: _content()),
        if (_form.error != null) Padding(padding: const EdgeInsets.symmetric(vertical: 12),
          child: Semantics(liveRegion: true, child: Text(_form.error!, semanticsLabel: 'Error: ${_form.error}'))),
        const SizedBox(height: 12), Wrap(spacing: 8, runSpacing: 8, children: [
          if (!_previewing && !_reviewing) FilledButton(onPressed: _busy ? null : _save, child: Text(_form.pending ? 'Saving…' : 'Save receipt')),
          TextButton(onPressed: _form.pending ? null : () => _return(widget.receipt == null
            ? '/groups/${widget.group.groupId}/receipts' : '/groups/${widget.group.groupId}/receipts/${widget.receipt!.receiptId}'), child: const Text('Cancel')),
        ]),
      ]),
    )),
  );
}
