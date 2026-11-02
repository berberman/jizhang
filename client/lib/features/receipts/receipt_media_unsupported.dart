import 'package:flutter/material.dart';

import '../../core/receipt_ai_models.dart';
import 'receipt_media.dart';

ReceiptMedia createReceiptMedia() => const UnsupportedReceiptMedia();

class UnsupportedReceiptMedia implements ReceiptMedia {
  const UnsupportedReceiptMedia();
  @override
  bool get supported => false;
  @override
  bool get cameraSupported => false;
  @override
  Future<ReceiptImageInput?> pickImage({required int maxBytes}) async =>
      throw UnsupportedError('Receipt image selection requires a web browser. Enter expenses manually.');
  @override
  Future<ReceiptImageInput?> takePhoto(BuildContext context, {required int maxBytes}) async =>
      throw UnsupportedError('Receipt camera capture requires a web browser. Enter expenses manually.');
  @override
  void dispose() {}
}
