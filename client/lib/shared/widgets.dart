import 'package:flutter/material.dart';
import 'package:go_router/go_router.dart';

import '../core/api_client.dart';
import '../core/models.dart';
import '../core/pagination_controller.dart';
import 'form_controller.dart';

String errorMessage(Object error) => error is ApiException ? error.message : error.toString();

String userLabel(User user) => '@${user.username} · ${user.displayName}';

String splitModeLabel(SplitMode mode) => switch (mode) {
  SplitMode.equal => 'Equal', SplitMode.shares => 'Shares',
  SplitMode.percentage => 'Percentages', SplitMode.exact => 'Exact amounts',
};

String splitInputLabel(SplitRecord record, RecordSplit split) => switch (record.splitMode) {
  SplitMode.equal => record.kind == RecordKind.income ? 'Equal beneficiary' : 'Equal participant',
  SplitMode.shares => 'Shares: ${split.value}',
  SplitMode.percentage => 'Percentage: ${split.value ~/ BigInt.from(100)}.${(split.value % BigInt.from(100)).toString().padLeft(2, '0')}%',
  SplitMode.exact => 'Exact input: ${record.currency.format(split.value)}',
};

List<String> receiptTotals(Receipt receipt) {
  final totals = <String, BigInt>{};
  final currencies = <String, CurrencyInfo>{};
  for (final record in receipt.records) {
    final currency = record.settlementCurrency;
    currencies[currency.code] = currency;
    totals.update(currency.code, (value) => value + record.settlementAmountMinor,
      ifAbsent: () => record.settlementAmountMinor);
  }
  final codes = totals.keys.toList()..sort();
  return [for (final code in codes) currencies[code]!.format(totals[code]!)];
}

class RecordMoney extends StatelessWidget {
  const RecordMoney({super.key, required this.record});
  final LedgerRecord record;
  @override
  Widget build(BuildContext context) => Column(crossAxisAlignment: CrossAxisAlignment.start, children: [
    Text(record.currency.format(record.amountMinor), style: Theme.of(context).textTheme.titleMedium),
    if (record.currency.code != record.settlementCurrency.code)
      Text('Settlement: ${record.settlementCurrency.format(record.settlementAmountMinor)}', style: Theme.of(context).textTheme.bodySmall),
  ]);
}

class SavedExchangeRateDetails extends StatelessWidget {
  const SavedExchangeRateDetails({super.key, required this.record});
  final LedgerRecord record;
  @override
  Widget build(BuildContext context) {
    final rate = record.exchangeRate;
    if (rate == null) return const SizedBox.shrink();
    final date = rate.rateDate == null ? null : DateTime.tryParse(rate.rateDate!);
    final now = DateTime.now().toUtc();
    final stale = date != null && DateTime.utc(now.year, now.month, now.day).difference(date).inDays > 7;
    return Column(crossAxisAlignment: CrossAxisAlignment.start, children: [
      Text('1 ${record.currency.code} = ${rate.rate} ${record.settlementCurrency.code}'),
      Text(rate.source == 'manual' ? 'Manual rate' : 'Frankfurter daily reference · Effective ${rate.rateDate}'),
      if (rate.fetchedAt != null) Text('Fetched ${rate.fetchedAt!.toLocal()}'),
      if (stale) const Text('Stale reference: this saved quote is more than seven days old.'),
    ]);
  }
}

class IdentityLabel extends StatelessWidget {
  const IdentityLabel({super.key, required this.user, this.former = false});
  final User user;
  final bool former;

  @override
  Widget build(BuildContext context) => Row(mainAxisSize: MainAxisSize.min, children: [
    ExcludeSemantics(child: CircleAvatar(radius: 12,
      backgroundColor: Theme.of(context).colorScheme.surfaceContainerHighest,
      child: const Icon(Icons.person_outline, size: 16))),
    const SizedBox(width: 8),
    Flexible(child: Wrap(spacing: 8, runSpacing: 4,
      crossAxisAlignment: WrapCrossAlignment.center, children: [
        Text(userLabel(user)),
        if (former) DecoratedBox(
          decoration: BoxDecoration(color: Theme.of(context).colorScheme.surfaceContainerHighest,
            borderRadius: BorderRadius.circular(8)),
          child: Padding(padding: const EdgeInsets.symmetric(horizontal: 6, vertical: 2),
            child: Text('Former member', style: Theme.of(context).textTheme.labelSmall))),
      ])),
  ]);
}

class SectionHeading extends StatelessWidget {
  const SectionHeading({super.key, required this.title, this.supportingText, this.actions = const []});
  final String title;
  final String? supportingText;
  final List<Widget> actions;

  @override
  Widget build(BuildContext context) => LayoutBuilder(builder: (context, size) {
    final heading = Column(crossAxisAlignment: CrossAxisAlignment.start, children: [
      Text(title, style: Theme.of(context).textTheme.titleMedium),
      if (supportingText != null) ...[
        const SizedBox(height: 4), Text(supportingText!),
      ],
    ]);
    if (actions.isEmpty) return heading;
    final controls = Wrap(spacing: 8, runSpacing: 8, children: actions);
    return size.maxWidth >= 600
      ? Row(children: [Expanded(child: heading), const SizedBox(width: 12), Flexible(child: controls)])
      : Column(crossAxisAlignment: CrossAxisAlignment.start, children: [
          heading, const SizedBox(height: 8), controls,
        ]);
  });
}

class EmptyState extends StatelessWidget {
  const EmptyState({super.key, required this.title, required this.message, this.action});
  final String title, message;
  final Widget? action;

  @override
  Widget build(BuildContext context) => Card(child: Padding(
    padding: const EdgeInsets.all(12),
    child: Row(crossAxisAlignment: CrossAxisAlignment.start, children: [
      const ExcludeSemantics(child: Icon(Icons.inbox_outlined)),
      const SizedBox(width: 12),
      Expanded(child: Column(crossAxisAlignment: CrossAxisAlignment.start, children: [
        Text(title, style: Theme.of(context).textTheme.titleMedium),
        const SizedBox(height: 4), Text(message),
        if (action != null) ...[const SizedBox(height: 8), action!],
      ])),
    ]),
  ));
}

Future<bool> confirmAction(BuildContext context, String title, String message,
    {String action = 'Delete'}) async {
  return await showDialog<bool>(
        context: context,
        builder: (context) => AlertDialog(
          title: Text(title),
          content: Text(message),
          actions: [
            TextButton(onPressed: () => Navigator.pop(context, false), child: const Text('Cancel')),
            FilledButton(onPressed: () => Navigator.pop(context, true), child: Text(action)),
          ],
        ),
      ) ?? false;
}

class FormNavigation {
  static FormController? active;
  static Future<bool> canExit(BuildContext context) async {
    final form = active;
    if (form == null) return true;
    return form.canLeave(() => confirmAction(context, 'Discard unsaved changes?',
        'Your unsaved changes will be lost.', action: 'Discard'));
  }
}

class FormLeaveGuard extends StatefulWidget {
  const FormLeaveGuard({super.key, required this.controller, required this.child});
  final FormController controller;
  final Widget child;
  @override
  State<FormLeaveGuard> createState() => _FormLeaveGuardState();
}

class _FormLeaveGuardState extends State<FormLeaveGuard> {
  @override
  void initState() {
    super.initState();
    FormNavigation.active = widget.controller;
  }
  @override
  void dispose() {
    if (identical(FormNavigation.active, widget.controller)) FormNavigation.active = null;
    super.dispose();
  }
  @override
  Widget build(BuildContext context) => widget.child;
}

class PageBody extends StatelessWidget {
  const PageBody({super.key, required this.storageKey, required this.child});
  final String storageKey;
  final Widget child;

  @override
  Widget build(BuildContext context) => SingleChildScrollView(
    key: PageStorageKey<String>(storageKey),
    padding: const EdgeInsets.all(16),
    child: Align(
      alignment: Alignment.topCenter,
      child: ConstrainedBox(
        constraints: BoxConstraints(
          maxWidth: MediaQuery.sizeOf(context).width >= 1100 ? 1200 : 960,
        ),
        child: child,
      ),
    ),
  );
}

class FormSurface extends StatelessWidget {
  const FormSurface({super.key, required this.title, required this.child});
  final String title;
  final Widget child;
  @override
  Widget build(BuildContext context) => Align(
    alignment: Alignment.topLeft,
    child: ConstrainedBox(
      constraints: const BoxConstraints(maxWidth: 760),
      child: Column(crossAxisAlignment: CrossAxisAlignment.stretch, children: [
        Text(title, style: Theme.of(context).textTheme.headlineSmall),
        const SizedBox(height: 16), child,
      ]),
    ),
  );
}

class ErrorPanel extends StatelessWidget {
  const ErrorPanel({super.key, required this.error, this.onRetry, this.backToGroups = false});
  final Object error;
  final VoidCallback? onRetry;
  final bool backToGroups;
  @override
  Widget build(BuildContext context) => Card(child: Padding(
    padding: const EdgeInsets.all(12),
    child: Column(crossAxisAlignment: CrossAxisAlignment.start, children: [
      Text(errorMessage(error), style: const TextStyle(color: Color(0xFFB42318))),
      const SizedBox(height: 8),
      Wrap(spacing: 8, children: [
        if (onRetry != null) OutlinedButton(onPressed: onRetry, child: const Text('Retry')),
        if (backToGroups) TextButton(onPressed: () => context.go('/groups'), child: const Text('Back to groups')),
      ]),
    ]),
  ));
}

class LoadView<T> extends StatefulWidget {
  const LoadView({super.key, required this.load, required this.builder, this.retainDataOnReload = false});
  final Future<T> Function() load;
  final Widget Function(T) builder;
  final bool retainDataOnReload;
  @override
  LoadViewState<T> createState() => LoadViewState<T>();
}

class LoadViewState<T> extends State<LoadView<T>> {
  T? _data;
  bool _hasData = false;
  bool _loading = true;
  Object? _error;
  int _request = 0;

  @override
  void initState() {
    super.initState();
    reload();
  }

  Future<void> reload() async {
    if (!mounted) return;
    final request = ++_request;
    setState(() {
      _loading = true;
      _error = null;
      if (!widget.retainDataOnReload) {
        _data = null;
        _hasData = false;
      }
    });
    try {
      final data = await widget.load();
      if (!mounted || request != _request) return;
      setState(() {
        _data = data;
        _hasData = true;
        _loading = false;
      });
    } catch (error) {
      if (!mounted || request != _request) return;
      setState(() {
        _error = error;
        _loading = false;
        if (error is ApiException &&
            (error.statusCode == 401 || error.statusCode == 403 || error.statusCode == 404)) {
          _data = null;
          _hasData = false;
        }
      });
    }
  }

  @override
  void dispose() {
    _request++;
    super.dispose();
  }

  @override
  Widget build(BuildContext context) {
    if (!_hasData) {
      if (_loading) return const Center(child: CircularProgressIndicator());
      return ErrorPanel(error: _error!, onRetry: reload, backToGroups: true);
    }
    if (!widget.retainDataOnReload) return widget.builder(_data as T);
    return Column(crossAxisAlignment: CrossAxisAlignment.stretch, children: [
      if (_loading) const LinearProgressIndicator(semanticsLabel: 'Refreshing data'),
      if (_error != null) ...[
        const Text('Showing previously loaded data.'),
        ErrorPanel(error: _error!, onRetry: reload, backToGroups: true),
      ],
      KeyedSubtree(key: const ValueKey('load-view-data'), child: widget.builder(_data as T)),
    ]);
  }
}

class PageControls<T> extends StatelessWidget {
  const PageControls({super.key, required this.controller});
  final PaginationController<T> controller;
  @override
  Widget build(BuildContext context) {
    final info = controller.page?.pageInfo;
    if (info == null) return const SizedBox.shrink();
    return Wrap(spacing: 8, crossAxisAlignment: WrapCrossAlignment.center, children: [
      OutlinedButton(onPressed: !controller.loading && info.hasPrev ? controller.previous : null, child: const Text('Previous')),
      Text('${info.total == 0 ? 0 : info.offset + 1}–${(info.offset + info.limit).clamp(0, info.total)} / ${info.total}'),
      OutlinedButton(onPressed: !controller.loading && info.hasNext ? controller.next : null, child: const Text('Next')),
    ]);
  }
}

class ListToolbar extends StatefulWidget {
  const ListToolbar({super.key, required this.query, required this.sort, required this.label,
    required this.onSearch, required this.onSort});
  final String query, sort, label;
  final Future<void> Function(String) onSearch, onSort;
  @override
  State<ListToolbar> createState() => _ListToolbarState();
}

class _ListToolbarState extends State<ListToolbar> {
  late final TextEditingController _query = TextEditingController(text: widget.query);
  @override
  void dispose() { _query.dispose(); super.dispose(); }
  @override
  Widget build(BuildContext context) => Padding(
    padding: const EdgeInsets.symmetric(vertical: 12),
    child: LayoutBuilder(builder: (context, size) {
      final search = TextField(controller: _query, decoration: InputDecoration(labelText: 'Search ${widget.label}',
        suffixIcon: IconButton(tooltip: 'Search', onPressed: () => widget.onSearch(_query.text), icon: const Icon(Icons.search))),
        onSubmitted: widget.onSearch);
      final sort = DropdownButtonFormField<String>(
        initialValue: widget.sort,
        decoration: const InputDecoration(labelText: 'Sort'),
        items: [DropdownMenuItem(value: widget.sort.replaceFirst('-', ''), child: Text('${widget.label} A–Z')),
          DropdownMenuItem(value: '-${widget.sort.replaceFirst('-', '')}', child: Text('${widget.label} Z–A'))],
        onChanged: (value) { if (value != null) widget.onSort(value); },
      );
      return size.maxWidth < 600 ? Column(children: [search, const SizedBox(height: 12), sort])
        : Row(children: [Expanded(child: search), const SizedBox(width: 12), SizedBox(width: 240, child: sort)]);
    }),
  );
}

void showSuccess(BuildContext context, String message) {
  ScaffoldMessenger.of(context).showSnackBar(SnackBar(content: Text(message)));
}
