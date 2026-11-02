import 'dart:typed_data';

import 'package:flutter/material.dart';

import '../../core/receipt_ai_models.dart';
import 'receipt_media_unsupported.dart'
    if (dart.library.js_interop) 'receipt_media_web.dart' as platform;

abstract interface class ReceiptMedia {
  bool get supported;
  bool get cameraSupported;
  Future<ReceiptImageInput?> pickImage({required int maxBytes});
  Future<ReceiptImageInput?> takePhoto(BuildContext context, {required int maxBytes});
  void dispose();
}

ReceiptMedia createReceiptMedia() => platform.createReceiptMedia();

class ReceiptMediaException implements Exception {
  const ReceiptMediaException(this.message);
  final String message;
  @override
  String toString() => message;
}

const receiptOriginalMaxBytes = 20 * 1024 * 1024;
const receiptImageLongEdge = 2560;

String? receiptOriginalMediaType(Uint8List prefix) {
  bool starts(List<int> signature, [int offset = 0]) {
    if (prefix.length < offset + signature.length) return false;
    for (var i = 0; i < signature.length; i++) {
      if (prefix[offset + i] != signature[i]) return false;
    }
    return true;
  }
  if (starts(const [0xff, 0xd8, 0xff])) return 'image/jpeg';
  if (starts(const [137, 80, 78, 71, 13, 10, 26, 10])) return 'image/png';
  if (starts(const [82, 73, 70, 70]) && starts(const [87, 69, 66, 80], 8)) return 'image/webp';
  if (starts(const [102, 116, 121, 112], 4)) {
    const brands = {'heic', 'heix', 'hevc', 'hevx', 'heim', 'heis', 'hevm', 'hevs', 'mif1', 'msf1'};
    final end = prefix.length < 64 ? prefix.length : 64;
    for (var i = 8; i + 4 <= end; i += 4) {
      if (i != 12 && brands.contains(String.fromCharCodes(prefix, i, i + 4))) return 'image/heif';
    }
  }
  return null;
}

void validateReceiptOriginal({
  required String name,
  required String? mediaType,
  required int byteCount,
  Uint8List? prefix,
}) {
  if (byteCount > receiptOriginalMaxBytes) {
    throw const ReceiptMediaException('Choose an image smaller than 20 MiB before normalization.');
  }
  final extension = name.split('.').last.toLowerCase();
  final mime = (mediaType ?? '').split(';').first.toLowerCase().trim();
  const extensions = {'jpg', 'jpeg', 'png', 'webp', 'heic', 'heif'};
  const types = {'image/jpeg', 'image/png', 'image/webp', 'image/heic', 'image/heif',
    'image/heic-sequence', 'image/heif-sequence'};
  if (byteCount <= 0 ||
      {'svg', 'svgz', 'gif', 'pdf'}.contains(extension) ||
      {'image/svg+xml', 'image/gif', 'application/pdf'}.contains(mime) ||
      (!types.contains(mime) && !extensions.contains(extension)) ||
      (mime.isNotEmpty && mime != 'application/octet-stream' && !types.contains(mime))) {
    throw const ReceiptMediaException('Choose a JPEG, PNG, WebP, or HEIC/HEIF photo. SVG, GIF, and PDF are not supported.');
  }
  if (prefix != null && receiptOriginalMediaType(prefix) == null) {
    throw const ReceiptMediaException('This file is not a supported photo. Choose a JPEG, PNG, WebP, or HEIC/HEIF image.');
  }
}

({int width, int height}) receiptNormalizedDimensions(int width, int height) {
  if (width <= 0 || height <= 0) {
    throw const ReceiptMediaException('The image has no readable dimensions. Choose another photo.');
  }
  final longest = width > height ? width : height;
  if (longest <= receiptImageLongEdge) return (width: width, height: height);
  final scaledWidth = width * receiptImageLongEdge ~/ longest;
  final scaledHeight = height * receiptImageLongEdge ~/ longest;
  return (width: scaledWidth < 1 ? 1 : scaledWidth, height: scaledHeight < 1 ? 1 : scaledHeight);
}

ReceiptImageInput receiptNormalizedJpeg(String dataUrl, {required int maxBytes}) {
  if (maxBytes <= 0) {
    throw const ReceiptMediaException('Image attachments are disabled by the server upload limit. You can still enter expenses manually.');
  }
  const prefix = 'data:image/jpeg;base64,';
  if (!dataUrl.startsWith(prefix)) {
    throw const ReceiptMediaException('This browser could not encode a JPEG photo. Try uploading a JPEG image instead.');
  }
  final base64 = dataUrl.substring(prefix.length);
  final padding = base64.endsWith('==') ? 2 : (base64.endsWith('=') ? 1 : 0);
  final byteCount = base64.length * 3 ~/ 4 - padding;
  if (byteCount > maxBytes) {
    throw const ReceiptMediaException('The normalized photo exceeds the server upload limit. Choose a smaller image or crop the photo before uploading.');
  }
  if (byteCount <= 0) {
    throw const ReceiptMediaException('The photo is empty. Choose another image.');
  }
  return ReceiptImageInput(dataBase64: base64);
}

// A resource arriving after its owner closes must be released immediately.
class ReceiptMediaResources {
  bool _closed = false;
  final List<VoidCallback> _releases = [];
  bool get closed => _closed;

  void own(VoidCallback release) {
    if (_closed) {
      release();
    } else {
      _releases.add(release);
    }
  }

  void close() {
    if (_closed) return;
    _closed = true;
    while (_releases.isNotEmpty) {
      _releases.removeLast()();
    }
  }
}
