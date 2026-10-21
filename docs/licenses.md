# Open-source notices

Jizhang is licensed under [MIT](../LICENSE). Retain the existing copyright and
license text when copying or distributing the software. Author attribution is
not a statement about an instance's operator or group membership.

## Frontend license access

The Flutter client's **Open-source licenses** action displays Flutter's generated
license registry and a registered copy of the Jizhang MIT notice. It is available
without sign-in and from Account. Its public hash route is `/#/legal/licenses`.
Reading licenses in a modal does not discard the underlying form or sign-in flow.

Deploy the complete Flutter web output, including its generated `assets/NOTICES`.
That file contains notices gathered by the toolchain from Flutter and package
license files, including transitive dependencies. Do not replace it with a
handwritten list of direct dependencies or strip assets when deploying.

The admin console's **Open-source licenses** link opens `/legal/licenses` on the
API origin in a separate tab. Its application JavaScript, CSS, and brand mark are
project code; the notice page includes the project MIT text. This page does not
claim to inventory the Flutter bundle, backend, or host operating system.

The client copy of the project license is
[`client/assets/legal/mit.txt`](../client/assets/legal/mit.txt). Keep it verbatim
with the root `LICENSE`; the admin page also reproduces that notice.

## Distributing binaries and containers

Review the exact Haskell, native-library, web-server, and runtime dependencies in
any binary or container you distribute. Preserve their required copyright/license
notices and provide source, written offers, relinking materials, or other
license-specific materials where required. License obligations can differ between
components; the project's MIT license does not replace them.

A UI link, package-name list, SPDX label, or Nix runtime closure alone is not a
complete compliance bundle. The frontend views do not generate a complete
backend/container third-party notice bundle or establish distribution compliance.
Check the licenses of the artifacts you actually publish.
