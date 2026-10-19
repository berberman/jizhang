import 'package:flutter/material.dart';
import 'package:flutter_svg/flutter_svg.dart';
import 'package:package_info_plus/package_info_plus.dart';

class JizhangMark extends StatelessWidget {
  const JizhangMark({super.key, this.size = 32});

  final double size;

  @override
  Widget build(BuildContext context) {
    return SvgPicture.asset(
      'web/jizhang-mark.svg',
      width: size,
      height: size,
      excludeFromSemantics: true,
    );
  }
}

class AppVersionLabel extends StatefulWidget {
  const AppVersionLabel({super.key});

  @override
  State<AppVersionLabel> createState() => _AppVersionLabelState();
}

class _AppVersionLabelState extends State<AppVersionLabel> {
  late final Future<PackageInfo> _packageInfo = PackageInfo.fromPlatform()
      .timeout(const Duration(seconds: 5));

  @override
  Widget build(BuildContext context) {
    return FutureBuilder<PackageInfo>(
      future: _packageInfo,
      builder: (context, snapshot) {
        var label = 'Version unavailable';
        if (snapshot.connectionState == ConnectionState.waiting) {
          label = 'Loading version…';
        } else if (!snapshot.hasError && snapshot.hasData) {
          final version = snapshot.data!.version.trim();
          final buildNumber = snapshot.data!.buildNumber.trim();
          if (version.isNotEmpty) {
            label = 'v$version';
            if (buildNumber.isNotEmpty) {
              label += ' (build $buildNumber)';
            }
          }
        }
        return Text(
          label,
          style: Theme.of(context).textTheme.bodyMedium?.copyWith(
            color: const Color(0xFF52616B),
          ),
        );
      },
    );
  }
}
