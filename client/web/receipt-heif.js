(() => {
  'use strict';

  const scriptUrl = document.currentScript?.src;
  const workerUrl = scriptUrl ? new URL('receipt-heif-worker.js', scriptUrl) : null;
  const maxInputBytes = 20 * 1024 * 1024;
  const maxEdge = 16384;
  const maxPixels = 64000000;
  const messages = {
    decode: 'This HEIC/HEIF photo could not be read. Choose another photo.',
    dimensions: 'This HEIC/HEIF photo is too large to prepare. Choose a smaller photo.',
    timeout: 'Preparing this HEIC/HEIF photo took too long. Choose a smaller photo.',
    load: 'HEIC/HEIF support could not be loaded. Reload and try again.',
  };

  function failure(code) {
    const error = new Error(messages[code]);
    error.name = 'ReceiptHeifError';
    error.code = code;
    return error;
  }

  function validDimensions(width, height) {
    return Number.isSafeInteger(width) && Number.isSafeInteger(height)
      && width > 0 && height > 0 && width <= maxEdge && height <= maxEdge
      && width * height <= maxPixels;
  }

  // A full input buffer is transferred, not retained by the caller.
  function decode(bytes, {signal} = {}) {
    return new Promise((resolve, reject) => {
      let worker = null;
      let timer = null;
      let input = null;
      let settled = false;

      function finish(error, canvas) {
        if (settled) return;
        settled = true;
        clearTimeout(timer);
        signal?.removeEventListener('abort', abort);
        if (worker) {
          worker.onmessage = null;
          worker.onerror = null;
          worker.onmessageerror = null;
          worker.terminate();
          worker = null;
        }
        input = null;
        bytes = null;
        if (error) reject(error);
        else resolve(canvas);
      }

      function abort() {
        finish(new DOMException('Photo preparation was cancelled.', 'AbortError'));
      }

      if (signal?.aborted) {
        abort();
        return;
      }
      const isBuffer = bytes instanceof ArrayBuffer;
      if (!isBuffer && !ArrayBuffer.isView(bytes)) {
        finish(failure('decode'));
        return;
      }
      const length = bytes.byteLength;
      if (length > maxInputBytes) {
        finish(failure('dimensions'));
        return;
      }
      if (length === 0) {
        finish(failure('decode'));
        return;
      }
      if (!workerUrl || typeof Worker !== 'function' || typeof WebAssembly !== 'object') {
        finish(failure('load'));
        return;
      }

      signal?.addEventListener('abort', abort, {once: true});
      const deadline = performance.now() + 15000;
      timer = setTimeout(() => finish(failure('timeout')), 15000);
      try {
        if (isBuffer) {
          input = bytes;
        } else if (bytes.buffer instanceof ArrayBuffer
          && bytes.byteOffset === 0 && length === bytes.buffer.byteLength) {
          input = bytes.buffer;
        } else {
          input = new Uint8Array(new Uint8Array(bytes.buffer, bytes.byteOffset, length)).buffer;
        }
        worker = new Worker(workerUrl, {type: 'classic'});
        worker.onmessage = event => {
          if (settled) return;
          if (performance.now() >= deadline) {
            finish(failure('timeout'));
            return;
          }
          let response = event.data;
          if (response?.error) {
            const code = ['decode', 'dimensions', 'load'].includes(response.error)
              ? response.error : 'decode';
            finish(failure(code));
            return;
          }
          if (!validDimensions(response?.width, response?.height)) {
            finish(failure('dimensions'));
            return;
          }
          let rgba = response.rgba;
          if (!(rgba instanceof ArrayBuffer)
            || rgba.byteLength !== response.width * response.height * 4) {
            finish(failure('decode'));
            return;
          }
          let canvas = null;
          try {
            canvas = document.createElement('canvas');
            canvas.width = response.width;
            canvas.height = response.height;
            const context = canvas.getContext('2d');
            if (!context) throw failure('decode');
            context.putImageData(
              new ImageData(new Uint8ClampedArray(rgba), response.width, response.height), 0, 0,
            );
          } catch {
            if (canvas) {
              canvas.width = 0;
              canvas.height = 0;
            }
            finish(failure('decode'));
            return;
          } finally {
            response = null;
            rgba = null;
          }
          if (performance.now() >= deadline) {
            canvas.width = 0;
            canvas.height = 0;
            finish(failure('timeout'));
            return;
          }
          finish(null, canvas);
        };
        worker.onerror = event => {
          event.preventDefault();
          finish(failure('load'));
        };
        worker.onmessageerror = () => finish(failure('decode'));
        worker.postMessage({bytes: input}, [input]);
        input = null;
        bytes = null;
      } catch {
        finish(failure('load'));
      }
    });
  }

  globalThis.JizhangReceiptHeif = Object.freeze({decode});
})();
