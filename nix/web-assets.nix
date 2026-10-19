{ pkgs, src, lockFile }:

pkgs.flutter.buildFlutterApplication {
  pname = "jizhang-web-assets";
  version = "1.0.0";
  inherit src;
  autoPubspecLock = lockFile;
  targetFlutterPlatform = "web";
  flutterMode = "release";
  flutterBuildFlags = [ "--no-pub" "--base-href" "/" ];
}
