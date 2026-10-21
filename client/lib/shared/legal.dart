import 'package:flutter/foundation.dart';
import 'package:flutter/material.dart';
import 'package:flutter/services.dart';
import 'package:go_router/go_router.dart';

bool _licensesRegistered = false;

void registerJizhangLicense() {
  if (_licensesRegistered) return;
  _licensesRegistered = true;
  LicenseRegistry.addLicense(() async* {
    final text = await rootBundle.loadString('assets/legal/mit.txt');
    yield LicenseEntryWithLineBreaks(const ['Jizhang'], text);
  });
}

class LegalLinks extends StatelessWidget {
  const LegalLinks({super.key, this.disabled = false});
  final bool disabled;

  Future<void> _open(BuildContext context, Widget page) => showDialog<void>(
    context: context,
    useRootNavigator: true,
    builder: (context) => Dialog.fullscreen(child: page),
  );

  @override
  Widget build(BuildContext context) => Wrap(
    alignment: WrapAlignment.center,
    spacing: 8,
    runSpacing: 4,
    children: [
      TextButton(
        onPressed: disabled ? null : () => _open(context, const LegalLicensesPage(modal: true)),
        child: const Text('Open-source licenses'),
      ),
    ],
  );
}

void _closeLegal(BuildContext context, bool modal) {
  if (modal) {
    Navigator.of(context).pop();
  } else if (context.canPop()) {
    context.pop();
  } else {
    context.go('/');
  }
}

class LegalLicensesPage extends StatelessWidget {
  const LegalLicensesPage({super.key, this.modal = false});
  final bool modal;

  @override
  Widget build(BuildContext context) => Scaffold(
    bottomNavigationBar: SafeArea(child: Padding(
      padding: const EdgeInsets.all(8),
      child: TextButton(
        onPressed: () => _closeLegal(context, modal),
        child: Text(modal ? 'Close licenses' : 'Back'),
      ),
    )),
    body: const LicensePage(applicationName: 'Jizhang'),
  );
}

