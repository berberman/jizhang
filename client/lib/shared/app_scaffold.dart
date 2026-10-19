import 'package:flutter/material.dart';
import 'package:go_router/go_router.dart';

import '../core/session_controller.dart';
import 'branding.dart';

class AppScaffold extends StatelessWidget {
  const AppScaffold({super.key, required this.session, required this.child});
  final SessionController session;
  final Widget child;

  @override
  Widget build(BuildContext context) => ListenableBuilder(
    listenable: session,
    builder: (context, _) {
      final path = GoRouterState.of(context).uri.path;
      final handle = '@${session.user?.username ?? ''}';
      final accountLabel = 'Account ($handle)';
      void navigate(String destination) {
        if (path != destination) context.go(destination);
      }
      return Scaffold(
        appBar: AppBar(
          automaticallyImplyLeading: false,
          title: Tooltip(
            message: 'Groups',
            excludeFromSemantics: true,
            child: Semantics(
              container: true,
              label: 'Jizhang / Groups',
              button: true,
              onTap: () => navigate('/groups'),
              excludeSemantics: true,
              child: TextButton.icon(
                style: TextButton.styleFrom(minimumSize: const Size(48, 48)),
                onPressed: () => navigate('/groups'),
                icon: const JizhangMark(size: 28),
                label: const Text('Jizhang'),
              ),
            ),
          ),
          actions: [
            if (MediaQuery.sizeOf(context).width >= 600)
              Tooltip(
                message: accountLabel,
                excludeFromSemantics: true,
                child: Semantics(
                  label: accountLabel,
                  button: true,
                  onTap: () => navigate('/account'),
                  excludeSemantics: true,
                  child: TextButton.icon(
                    onPressed: () => navigate('/account'),
                    icon: const Icon(Icons.account_circle_outlined),
                    label: ConstrainedBox(
                      constraints: BoxConstraints(maxWidth: MediaQuery.sizeOf(context).width * .35),
                      child: Text(handle, maxLines: 1, overflow: TextOverflow.ellipsis),
                    ),
                  ),
                ),
              )
            else
              Semantics(
                label: accountLabel,
                button: true,
                onTap: () => navigate('/account'),
                excludeSemantics: true,
                child: IconButton(
                  tooltip: accountLabel,
                  constraints: const BoxConstraints(minWidth: 48, minHeight: 48),
                  onPressed: () => navigate('/account'),
                  icon: const Icon(Icons.account_circle_outlined),
                ),
              ),
            const SizedBox(width: 8),
          ],
        ),
        body: SafeArea(child: child),
      );
    },
  );
}

