import 'dart:async';
import 'dart:js_interop';
import 'dart:js_interop_unsafe';
import 'dart:typed_data';

import 'package:file_selector/file_selector.dart';
import 'package:flutter/material.dart';
import 'package:web/web.dart' as web;

import '../../core/receipt_ai_models.dart';
import 'receipt_media.dart';

const _photoTypes = XTypeGroup(
  label: 'Receipt photos',
  extensions: ['jpg', 'jpeg', 'png', 'webp', 'heic', 'heif'],
  mimeTypes: ['image/jpeg', 'image/png', 'image/webp', 'image/heic', 'image/heif',
    'image/heic-sequence', 'image/heif-sequence'],
);

@JS('JizhangReceiptHeif')
external _ReceiptHeifDecoder? get _receiptHeif;

extension type _ReceiptHeifDecoder._(JSObject _) implements JSObject {
  external JSPromise<web.HTMLCanvasElement> decode(JSUint8Array bytes, _ReceiptHeifOptions options);
}

extension type _ReceiptHeifOptions._(JSObject _) implements JSObject {
  external factory _ReceiptHeifOptions({required web.AbortSignal signal});
}

ReceiptMediaException _heifFailure(String? code) => ReceiptMediaException(switch (code) {
  'dimensions' => 'This HEIC/HEIF photo is too large to prepare. Choose a smaller photo.',
  'timeout' => 'Preparing this HEIC/HEIF photo took too long. Choose a smaller photo.',
  'load' => 'HEIC/HEIF support could not be loaded. Reload and try again.',
  _ => 'This HEIC/HEIF photo could not be read. Choose another photo.',
});

ReceiptMedia createReceiptMedia() => WebReceiptMedia();

class WebReceiptMedia with WidgetsBindingObserver implements ReceiptMedia {
  WebReceiptMedia() {
    WidgetsBinding.instance.addObserver(this);
    _leaveListener = ((web.Event _) => _cancelActive()).toJS;
    web.window.addEventListener('pagehide', _leaveListener);
    web.window.addEventListener('popstate', _leaveListener);
  }

  late final JSFunction _leaveListener;
  bool _disposed = false;
  int _generation = 0;
  ReceiptMediaResources? _active;
  VoidCallback? _cancelDialog;

  @override
  bool get supported => !_disposed;
  @override
  bool get cameraSupported => !_disposed && web.window.isSecureContext &&
      !web.window.navigator.getProperty<JSAny?>('mediaDevices'.toJS).isUndefinedOrNull;

  ReceiptMediaResources _begin(int maxBytes) {
    if (_disposed) throw StateError('Receipt media has been disposed');
    if (maxBytes <= 0) {
      throw const ReceiptMediaException('Image attachments are disabled by the server upload limit. You can still enter expenses manually.');
    }
    _cancelActive();
    return _active = ReceiptMediaResources();
  }

  bool _current(ReceiptMediaResources resources) =>
      !_disposed && !resources.closed && identical(_active, resources);

  void _cancelActive() {
    _generation++;
    final cancel = _cancelDialog;
    _cancelDialog = null;
    _active?.close();
    _active = null;
    cancel?.call();
  }

  void _finish(ReceiptMediaResources resources) {
    if (identical(_active, resources)) {
      _cancelDialog = null;
      _active = null;
    }
    resources.close();
  }

  @override
  void didChangeAppLifecycleState(AppLifecycleState state) {
    if (state == AppLifecycleState.detached || state == AppLifecycleState.hidden ||
        state == AppLifecycleState.paused) _cancelActive();
  }

  @override
  Future<ReceiptImageInput?> pickImage({required int maxBytes}) async {
    final resources = _begin(maxBytes);
    try {
      final file = await openFile(acceptedTypeGroups: [_photoTypes]);
      if (file == null) return null;
      final originalUrl = file.path;
      if (originalUrl.startsWith('blob:')) resources.own(() => web.URL.revokeObjectURL(originalUrl));
      if (!_current(resources)) return null;
      final length = await file.length();
      if (!_current(resources)) return null;
      validateReceiptOriginal(name: file.name, mediaType: file.mimeType, byteCount: length);
      final bytes = await file.readAsBytes();
      if (!_current(resources)) return null;
      validateReceiptOriginal(name: file.name, mediaType: file.mimeType,
        byteCount: bytes.length, prefix: bytes);
      if (receiptOriginalMediaType(bytes) == 'image/heif') {
        final canvas = await _decodeHeif(bytes, resources);
        if (canvas == null || !_current(resources)) return null;
        return _normalize(canvas, canvas.width, canvas.height, maxBytes);
      }
      final blob = web.Blob([bytes.toJS].toJS);
      final url = web.URL.createObjectURL(blob);
      resources.own(() => web.URL.revokeObjectURL(url));
      final image = web.HTMLImageElement();
      resources.own(() { image.removeAttribute('src'); image.remove(); });
      // HTML image decoding applies the browser's EXIF orientation before drawing.
      image.src = url;
      await image.decode().toDart;
      if (!_current(resources)) return null;
      return _normalize(image, image.naturalWidth, image.naturalHeight, maxBytes);
    } on ReceiptMediaException {
      if (!_current(resources)) return null;
      rethrow;
    } catch (_) {
      if (!_current(resources)) return null;
      throw const ReceiptMediaException('This browser could not read the photo. Try a JPEG or PNG image instead.');
    } finally {
      _finish(resources);
    }
  }

  Future<web.HTMLCanvasElement?> _decodeHeif(Uint8List bytes, ReceiptMediaResources resources) {
    final result = Completer<web.HTMLCanvasElement?>();
    try {
      final decoder = _receiptHeif;
      if (decoder.isUndefinedOrNull) throw _heifFailure('load');
      final controller = web.AbortController();
      resources.own(() => controller.abort());
      final promise = decoder!.decode(bytes.toJS, _ReceiptHeifOptions(signal: controller.signal));
      promise.callMethod<JSAny?>('then'.toJS,
        ((web.HTMLCanvasElement canvas) {
          resources.own(() {
            canvas.width = 0;
            canvas.height = 0;
            canvas.remove();
          });
          result.complete(_current(resources) ? canvas : null);
        }).toJS,
        ((JSAny? failure) {
          if (!_current(resources)) {
            result.complete(null);
            return;
          }
          final code = !failure.isUndefinedOrNull && failure.typeofEquals('object')
              ? (failure as JSObject).getProperty<JSString?>('code'.toJS)?.toDart
              : null;
          result.completeError(_heifFailure(code));
        }).toJS);
    } on ReceiptMediaException {
      rethrow;
    } catch (_) {
      throw _heifFailure('load');
    }
    return result.future;
  }

  @override
  Future<ReceiptImageInput?> takePhoto(BuildContext context, {required int maxBytes}) async {
    final resources = _begin(maxBytes);
    final generation = _generation;
    final session = _CameraSession(maxBytes: maxBytes, available: cameraSupported);
    resources.own(session.close);
    final navigator = Navigator.of(context, rootNavigator: true);
    final route = DialogRoute<_CameraResult>(
      context: context,
      builder: (_) => _CameraDialog(session: session),
    );
    _cancelDialog = () {
      if (route.isActive) navigator.removeRoute(route);
    };
    _CameraResult? result;
    try {
      result = await navigator.push(route);
      if (!_current(resources) || generation != _generation || !context.mounted) return null;
    } finally {
      _finish(resources);
    }
    if (result?.upload == true) return pickImage(maxBytes: maxBytes);
    return result?.image;
  }

  @override
  void dispose() {
    if (_disposed) return;
    _disposed = true;
    _cancelActive();
    WidgetsBinding.instance.removeObserver(this);
    web.window.removeEventListener('pagehide', _leaveListener);
    web.window.removeEventListener('popstate', _leaveListener);
  }
}

ReceiptImageInput _normalize(web.CanvasImageSource source, int width, int height, int maxBytes) {
  final size = receiptNormalizedDimensions(width, height);
  final canvas = web.HTMLCanvasElement()..width = size.width..height = size.height;
  try {
    final context = canvas.getContext('2d') as web.CanvasRenderingContext2D;
    context.fillStyle = '#ffffff'.toJS;
    context.fillRect(0, 0, size.width, size.height);
    context.drawImage(source, 0, 0, size.width, size.height);
    return receiptNormalizedJpeg(canvas.toDataURL('image/jpeg', 0.92.toJS), maxBytes: maxBytes);
  } finally {
    canvas.width = 0;
    canvas.height = 0;
    canvas.remove();
  }
}

class _CameraResult {
  const _CameraResult({this.image, this.upload = false});
  final ReceiptImageInput? image;
  final bool upload;
}

class _CameraSession extends ChangeNotifier {
  _CameraSession({required this.maxBytes, required this.available});
  final int maxBytes;
  final bool available;
  bool closed = false;
  bool opening = false;
  bool ready = false;
  String? error;
  ReceiptImageInput? photo;
  web.HTMLVideoElement? _video;
  web.MediaStream? _stream;
  ReceiptMediaResources? _streamResources;
  web.HTMLImageElement? _photoElement;
  int _generation = 0;

  void attachVideo(web.HTMLVideoElement video) {
    if (closed) { video.remove(); return; }
    _detachVideo();
    _video = video;
    video
      ..autoplay = true
      ..muted = true
      ..playsInline = true
      ..controls = false
      ..tabIndex = -1
      ..setAttribute('aria-label', 'Live receipt camera preview');
    video.style
      ..width = '100%'
      ..height = '100%'
      ..objectFit = 'contain';
    if (_stream != null) unawaited(_play());
  }

  void attachPhoto(web.HTMLImageElement image, ReceiptImageInput photo) {
    if (closed) { image.remove(); return; }
    _detachPhoto();
    _photoElement = image;
    image
      ..alt = 'Captured receipt photo'
      ..src = 'data:image/jpeg;base64,${photo.dataBase64}';
    image.style..width = '100%'..height = '100%'..objectFit = 'contain';
  }

  void _detachPhoto() {
    final image = _photoElement;
    _photoElement = null;
    if (image == null) return;
    image.removeAttribute('src');
    image.remove();
  }

  Future<void> _play() async {
    final video = _video;
    final stream = _stream;
    final generation = _generation;
    if (closed || video == null || stream == null) return;
    try {
      video.srcObject = stream;
      await video.play().toDart;
      if (closed || generation != _generation || !identical(_video, video)) return;
      ready = video.videoWidth > 0 && video.videoHeight > 0;
      if (!ready) {
        error = 'The camera has not supplied a readable frame. Retake or upload a photo instead.';
        _stopStream();
        _detachVideo();
      }
    } catch (_) {
      if (closed || generation != _generation) return;
      error = 'The camera preview could not start. Retake or upload a photo instead.';
      _stopStream();
      _detachVideo();
    }
    if (!closed) notifyListeners();
  }

  Future<void> start() async {
    if (closed || opening || !available) return;
    _stopStream();
    _detachVideo();
    _detachPhoto();
    photo = null;
    error = null;
    opening = true;
    final generation = ++_generation;
    final resources = ReceiptMediaResources();
    _streamResources = resources;
    notifyListeners();
    try {
      final stream = await web.window.navigator.mediaDevices.getUserMedia(
        web.MediaStreamConstraints(
          audio: false.toJS,
          video: web.MediaTrackConstraints(
            facingMode: web.ConstrainDOMStringParameters(ideal: 'environment'.toJS)),
        ),
      ).toDart;
      resources.own(() {
        for (final track in stream.getTracks().toDart) {
          try { track.stop(); } catch (_) { /* Release the remaining tracks too. */ }
        }
      });
      if (closed || resources.closed || generation != _generation) return;
      _stream = stream;
      opening = false;
      notifyListeners();
      await _play();
    } catch (_) {
      resources.close();
      if (closed || generation != _generation) return;
      opening = false;
      error = 'Camera access was denied or no camera is available. Allow camera access in your browser, or upload a photo instead.';
      notifyListeners();
    }
  }

  void capture() {
    final video = _video;
    if (closed || !ready || video == null) return;
    try {
      photo = _normalize(video, video.videoWidth, video.videoHeight, maxBytes);
      error = null;
      _generation++;
      _stopStream();
      _detachVideo();
    } on ReceiptMediaException catch (failure) {
      error = failure.message;
      _generation++;
      _stopStream();
      _detachVideo();
    } catch (_) {
      error = 'The camera frame could not be captured. Retake or upload a photo instead.';
      _generation++;
      _stopStream();
      _detachVideo();
    }
    notifyListeners();
  }

  void _stopStream() {
    ready = false;
    _streamResources?.close();
    _streamResources = null;
    _stream = null;
  }

  void _detachVideo() {
    final video = _video;
    _video = null;
    if (video == null) return;
    video.pause();
    video.srcObject = null;
    video.remove();
  }

  void close() {
    if (closed) return;
    closed = true;
    _generation++;
    _stopStream();
    _detachVideo();
    _detachPhoto();
    photo = null;
  }

  @override
  void dispose() {
    close();
    super.dispose();
  }
}

class _CameraDialog extends StatefulWidget {
  const _CameraDialog({required this.session});
  final _CameraSession session;
  @override
  State<_CameraDialog> createState() => _CameraDialogState();
}

class _CameraDialogState extends State<_CameraDialog> {
  _CameraSession get session => widget.session;
  @override
  void initState() {
    super.initState();
    session.addListener(_changed);
    unawaited(session.start());
  }

  void _changed() { if (mounted) setState(() {}); }

  void _close([_CameraResult? result]) {
    session.close();
    Navigator.of(context).pop(result);
  }

  @override
  void dispose() {
    session.removeListener(_changed);
    session.dispose();
    super.dispose();
  }

  @override
  Widget build(BuildContext context) {
    final photo = session.photo;
    final buttonStyle = TextButton.styleFrom(minimumSize: const Size(44, 44));
    return PopScope<_CameraResult>(
      onPopInvokedWithResult: (didPop, _) { if (didPop) session.close(); },
      child: AlertDialog(
        title: const Text('Take receipt photo'),
        scrollable: true,
        content: SizedBox(
          width: 640,
          child: Column(mainAxisSize: MainAxisSize.min, crossAxisAlignment: CrossAxisAlignment.start, children: [
            if (!session.available)
              Semantics(liveRegion: true, child: const Text('Camera capture requires a supported browser on HTTPS or localhost. Upload a photo instead.'))
            else ...[
              if (session.opening)
                Semantics(liveRegion: true, child: const Text('Opening camera. Your browser may ask for permission.')),
              if (session.error != null)
                Semantics(liveRegion: true, child: Text(session.error!)),
              if (session.error != null || session.opening) const SizedBox(height: 12),
              if (photo != null)
                AspectRatio(aspectRatio: 4 / 3, child: HtmlElementView.fromTagName(
                  key: const ValueKey('captured-receipt'),
                  tagName: 'img',
                  onElementCreated: (element) => session.attachPhoto(element as web.HTMLImageElement, photo),
                ))
              else if (!session.opening && session.error == null)
                AspectRatio(aspectRatio: 4 / 3, child: HtmlElementView.fromTagName(
                  key: const ValueKey('live-receipt-camera'),
                  tagName: 'video',
                  onElementCreated: (element) => session.attachVideo(element as web.HTMLVideoElement),
                )),
            ],
          ]),
        ),
        actions: [
          TextButton(style: buttonStyle, onPressed: () => _close(), child: const Text('Cancel')),
          TextButton(style: buttonStyle, onPressed: () => _close(const _CameraResult(upload: true)), child: const Text('Upload image')),
          if (session.available && !session.opening && (photo != null || session.error != null))
            TextButton(style: buttonStyle, onPressed: session.start, child: const Text('Retake')),
          if (session.available && photo == null && session.error == null)
            FilledButton(style: FilledButton.styleFrom(minimumSize: const Size(44, 44)),
              onPressed: session.ready ? session.capture : null, child: const Text('Capture')),
          if (photo != null)
            FilledButton(style: FilledButton.styleFrom(minimumSize: const Size(44, 44)),
              onPressed: () => _close(_CameraResult(image: photo)), child: const Text('Use photo')),
        ],
      ),
    );
  }
}
