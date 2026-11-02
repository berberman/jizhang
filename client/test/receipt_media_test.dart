import 'dart:convert';
import 'dart:typed_data';

import 'package:flutter_test/flutter_test.dart';
import 'package:jizhang_client/features/receipts/receipt_media.dart';

void main() {
  final mediaFailure = isA<ReceiptMediaException>();

  test('original size rejects empty and oversized files at the 20 MiB boundary', () {
    final prefix = Uint8List.fromList([0xff, 0xd8, 0xff]);
    for (final length in [receiptOriginalMaxBytes - 1, receiptOriginalMaxBytes]) {
      validateReceiptOriginal(name: 'receipt.jpg', mediaType: 'image/jpeg', byteCount: length,
        prefix: prefix);
    }
    for (final length in [0, receiptOriginalMaxBytes + 1]) {
      expect(() => validateReceiptOriginal(name: 'receipt.jpg', mediaType: 'image/jpeg',
        byteCount: length), throwsA(mediaFailure));
    }
  });

  test('sniffing distinguishes JPEG PNG and WebP signatures', () {
    expect(receiptOriginalMediaType(Uint8List.fromList([0xff, 0xd8, 0xff])), 'image/jpeg');
    expect(receiptOriginalMediaType(Uint8List.fromList([137, 80, 78, 71, 13, 10, 26, 10])), 'image/png');
    expect(receiptOriginalMediaType(Uint8List.fromList([
      ...ascii.encode('RIFF'), 0, 0, 0, 0, ...ascii.encode('WEBP'),
    ])), 'image/webp');
  });

  test('unsupported declared types cannot hide behind a photo extension', () {
    for (final type in ['image/svg+xml', 'image/gif', 'application/pdf', 'text/html']) {
      expect(() => validateReceiptOriginal(name: 'renamed.jpg', mediaType: type,
        byteCount: 200), throwsA(mediaFailure), reason: type);
    }
    for (final extension in ['svg', 'svgz', 'gif', 'pdf']) {
      expect(() => validateReceiptOriginal(name: 'renamed.$extension', mediaType: 'image/jpeg',
        byteCount: 200), throwsA(mediaFailure), reason: extension);
    }
  });

  test('supported photo metadata still requires an allowed signature', () {
    for (final disguised in ['<svg></svg>', 'GIF87a', 'GIF89a', '%PDF-1.7', '<html>']) {
      final bytes = Uint8List.fromList(utf8.encode(disguised));
      expect(receiptOriginalMediaType(bytes), isNull);
      expect(() => validateReceiptOriginal(name: 'receipt.png', mediaType: 'image/png',
        byteCount: bytes.length, prefix: bytes), throwsA(mediaFailure));
    }
  });

  test('normalization bounds the long edge without upscaling either orientation', () {
    expect(receiptNormalizedDimensions(4000, 3000), (width: 2560, height: 1920));
    expect(receiptNormalizedDimensions(3000, 4000), (width: 1920, height: 2560));
    expect(receiptNormalizedDimensions(320, 240), (width: 320, height: 240));
    expect(receiptNormalizedDimensions(2560, 10), (width: 2560, height: 10));
    expect(receiptNormalizedDimensions(10000, 1), (width: 2560, height: 1));
    expect(() => receiptNormalizedDimensions(0, 240), throwsA(mediaFailure));
    expect(() => receiptNormalizedDimensions(320, -1), throwsA(mediaFailure));
  });

  test('JPEG transport checks the decoded byte cap and rejects invalid media', () {
    for (final length in [1, 2, 3]) {
      final bytes = Uint8List.fromList(List.generate(length, (i) => i % 256));
      final encoded = base64Encode(bytes);
      expect(receiptNormalizedJpeg('data:image/jpeg;base64,$encoded', maxBytes: length).bytes.length,
        length);
      expect(() => receiptNormalizedJpeg('data:image/jpeg;base64,$encoded', maxBytes: length - 1),
        throwsA(mediaFailure));
    }
    expect(() => receiptNormalizedJpeg('data:image/png;base64,AAAA', maxBytes: 50), throwsA(mediaFailure));
    expect(() => receiptNormalizedJpeg('data:image/jpeg;base64,', maxBytes: 50), throwsA(mediaFailure));
    expect(() => receiptNormalizedJpeg('data:image/jpeg;base64,AAAA', maxBytes: 0), throwsA(mediaFailure));
  });
}
