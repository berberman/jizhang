(() => {
  'use strict';

  const originalLimit = 20 * 1024 * 1024;
  const longEdge = 2560;
  const supportedTypes = 'image/jpeg,image/png,image/webp,image/heic,image/heif,image/heic-sequence,image/heif-sequence,.jpg,.jpeg,.png,.webp,.heic,.heif';
  let dialogNumber = 0;

  function element(tag, text, className) {
    const result = document.createElement(tag);
    if (text) result.textContent = text;
    if (className) result.className = className;
    return result;
  }

  function limitFrom(options) {
    const limit = options?.maxBytes;
    if (!Number.isSafeInteger(limit) || limit <= 0) throw new Error('Image uploads are unavailable because of the server upload limit. You can still enter expenses manually.');
    return limit;
  }

  function imageType(bytes) {
    const ascii = (start, end) => String.fromCharCode(...bytes.subarray(start, end));
    if (bytes[0] === 0xff && bytes[1] === 0xd8 && bytes[2] === 0xff) return 'image/jpeg';
    if (bytes.length >= 8 && bytes[0] === 0x89 && ascii(1, 8) === 'PNG\r\n\x1a\n') return 'image/png';
    if (ascii(0, 4) === 'RIFF' && ascii(8, 12) === 'WEBP') return 'image/webp';
    if (ascii(4, 8) === 'ftyp') {
      const brands = [];
      for (let index = 8; index + 4 <= bytes.length; index += 4) {
        if (index !== 12) brands.push(ascii(index, index + 4));
      }
      if (brands.some(brand => ['heic', 'heix', 'hevc', 'hevx', 'heim', 'heis', 'hevm', 'hevs', 'mif1', 'msf1'].includes(brand))) return 'image/heif';
    }
    return null;
  }

  function stopStream(stream) {
    if (stream) for (const track of stream.getTracks()) track.stop();
  }

  class ReceiptMedia {
    constructor() {
      this.disposed = false;
      this.urls = new Set();
      this.operation = null;
      this.onLeave = () => this.clear();
      for (const event of ['pagehide', 'hashchange', 'popstate']) window.addEventListener(event, this.onLeave);
    }

    get supported() {
      return !this.disposed && typeof document.createElement('canvas').toBlob === 'function' && typeof URL.createObjectURL === 'function';
    }

    get cameraSupported() {
      return this.supported && window.isSecureContext === true && typeof navigator.mediaDevices?.getUserMedia === 'function';
    }

    begin() {
      if (!this.supported) throw new Error('Image selection is unavailable in this browser. You can still enter expenses manually.');
      this.operation?.cancel();
      const operation = { active: true, cleanup: new Set() };
      operation.cancel = () => {
        if (!operation.active) return;
        operation.active = false;
        for (const cleanup of operation.cleanup) cleanup();
        operation.cleanup.clear();
        if (this.operation === operation) this.operation = null;
      };
      this.operation = operation;
      return operation;
    }

    ownedUrl(blob) {
      const url = URL.createObjectURL(blob);
      this.urls.add(url);
      return url;
    }

    createImageUrl(image) {
      if (this.disposed) throw new Error('This receipt image view has been closed.');
      if (image?.mediaType !== 'image/jpeg' || typeof image.dataBase64 !== 'string') throw new Error('The receipt image is not a JPEG.');
      const binary = atob(image.dataBase64);
      const bytes = new Uint8Array(binary.length);
      for (let index = 0; index < binary.length; index++) bytes[index] = binary.charCodeAt(index);
      return this.ownedUrl(new Blob([bytes], { type: 'image/jpeg' }));
    }

    releaseImageUrl(url) {
      if (this.urls.delete(url)) URL.revokeObjectURL(url);
    }

    clear() {
      this.operation?.cancel();
      for (const url of this.urls) URL.revokeObjectURL(url);
      this.urls.clear();
    }

    dispose() {
      this.disposed = true;
      this.clear();
      for (const event of ['pagehide', 'hashchange', 'popstate']) window.removeEventListener(event, this.onLeave);
    }

    async pickImage(options) {
      const maxBytes = limitFrom(options);
      const operation = this.begin();
      try {
        const file = await new Promise((resolve, reject) => {
          const input = element('input');
          input.type = 'file'; input.accept = supportedTypes; input.hidden = true;
          const finish = file => { if (operation.active) resolve(file); };
          const cleanup = () => {
            input.value = ''; input.remove(); resolve(null);
          };
          operation.cleanup.add(cleanup);
          input.addEventListener('change', () => finish(input.files?.[0] || null));
          input.addEventListener('cancel', event => { event.stopPropagation(); finish(null); });
          const dialogs = document.querySelectorAll('dialog[open]');
          (dialogs[dialogs.length - 1] || document.body).append(input);
          try { input.click(); } catch (_) { reject(new Error('The image picker could not open. Try another browser or enter expenses manually.')); }
        });
        if (!operation.active || !file) return null;
        const image = await this.normalizeFile(file, maxBytes, operation);
        return operation.active ? image : null;
      } finally { operation.cancel(); }
    }

    async normalizeFile(file, maxBytes, operation) {
      if (file.size > originalLimit) throw new Error('Choose an original image no larger than 20 MiB.');
      const extension = file.name.split('.').pop().toLowerCase();
      const mime = file.type.split(';')[0].trim().toLowerCase();
      const extensions = ['jpg', 'jpeg', 'png', 'webp', 'heic', 'heif'];
      const types = ['image/jpeg', 'image/png', 'image/webp', 'image/heic', 'image/heif', 'image/heic-sequence', 'image/heif-sequence'];
      if (file.size <= 0 || !types.includes(mime) && !extensions.includes(extension) || mime && mime !== 'application/octet-stream' && !types.includes(mime))
        throw new Error('Choose a JPEG, PNG, WebP, or HEIC/HEIF photo. SVG, GIF, and PDF files are not supported.');
      const forbidden = /\.(svg|svgz|gif|pdf)$/i.test(file.name) || ['image/svg+xml', 'image/gif', 'application/pdf'].includes(file.type.toLowerCase());
      const type = imageType(new Uint8Array(await file.slice(0, 64).arrayBuffer()));
      if (!operation.active) return null;
      if (forbidden || !type) throw new Error('Choose a JPEG, PNG, WebP, or HEIC/HEIF photo. SVG, GIF, and PDF files are not supported.');
      if (type === 'image/heif') {
        if (!globalThis.JizhangReceiptHeif || typeof AbortController !== 'function')
          throw new Error('HEIC/HEIF support could not be loaded. Reload and try again.');
        const controller = new AbortController(); const abort = () => controller.abort();
        operation.cleanup.add(abort);
        let bytes, canvas, clearCanvas;
        try {
          bytes = await file.arrayBuffer();
          if (!operation.active) return null;
          canvas = await globalThis.JizhangReceiptHeif.decode(bytes, { signal: controller.signal });
          clearCanvas = () => { canvas.width = 0; canvas.height = 0; };
          operation.cleanup.add(clearCanvas);
          if (!operation.active) return null;
          return await this.encode(canvas, canvas.width, canvas.height, maxBytes, operation);
        } catch (error) {
          if (!operation.active || error?.name === 'AbortError') return null;
          throw error;
        } finally {
          operation.cleanup.delete(abort); abort(); bytes = null;
          if (clearCanvas) { operation.cleanup.delete(clearCanvas); clearCanvas(); }
          canvas = null;
        }
      }
      const url = this.ownedUrl(new Blob([file], { type }));
      const image = new Image();
      const release = () => { image.onload = null; image.onerror = null; image.removeAttribute('src'); this.releaseImageUrl(url); };
      let cancelDecode;
      try {
        await new Promise((resolve, reject) => {
          cancelDecode = () => { release(); resolve(); };
          operation.cleanup.add(cancelDecode);
          image.onload = () => resolve();
          image.onerror = () => reject(new Error('This browser could not read that photo. Choose a JPEG or PNG image instead.'));
          // Browser image decoding applies the photo orientation before drawing.
          image.src = url;
        });
        if (!operation.active) return null;
        return await this.encode(image, image.naturalWidth, image.naturalHeight, maxBytes, operation);
      } finally {
        operation.cleanup.delete(cancelDecode);
        release();
      }
    }

    async encode(source, width, height, maxBytes, operation) {
      if (!width || !height) throw new Error('The photo has no readable image dimensions. Choose another image.');
      const scale = Math.min(1, longEdge / Math.max(width, height));
      const canvas = element('canvas');
      canvas.width = Math.max(1, Math.round(width * scale));
      canvas.height = Math.max(1, Math.round(height * scale));
      const clearCanvas = () => { canvas.width = 0; canvas.height = 0; };
      operation.cleanup.add(clearCanvas);
      try {
        const context = canvas.getContext('2d');
        if (!context) throw new Error('This browser cannot prepare receipt photos. Try another browser.');
        context.fillStyle = '#ffffff';
        context.fillRect(0, 0, canvas.width, canvas.height);
        context.drawImage(source, 0, 0, canvas.width, canvas.height);
        const blob = await new Promise(resolve => canvas.toBlob(resolve, 'image/jpeg', 0.92));
        if (!operation.active) return null;
        if (!blob || blob.type !== 'image/jpeg') throw new Error('This browser could not encode the photo as JPEG. Choose another image.');
        if (blob.size > maxBytes) throw new Error('The prepared photo exceeds the server upload limit. Choose a smaller image.');
        const dataBase64 = await new Promise((resolve, reject) => {
          const reader = new FileReader();
          const cancel = () => { if (reader.readyState === FileReader.LOADING) reader.abort(); resolve(null); };
          operation.cleanup.add(cancel);
          const finish = () => { operation.cleanup.delete(cancel); reader.onload = null; reader.onerror = null; reader.onabort = null; };
          reader.onload = () => { const value = reader.result; finish(); resolve(typeof value === 'string' ? value.slice(value.indexOf(',') + 1) : null); };
          reader.onerror = () => { finish(); reject(new Error('The prepared photo could not be read. Choose another image.')); };
          reader.onabort = () => { finish(); resolve(null); };
          reader.readAsDataURL(blob);
        });
        return operation.active && dataBase64 ? { mediaType: 'image/jpeg', dataBase64 } : null;
      } finally { operation.cleanup.delete(clearCanvas); clearCanvas(); }
    }

    async takePhoto(options) {
      const maxBytes = limitFrom(options);
      const operation = this.begin();
      const focus = document.activeElement;
      const dialog = element('dialog', null, 'receipt-camera-dialog');
      const header = element('header', null, 'dialog-header');
      const title = element('h2', 'Take receipt photo');
      title.id = `receipt-camera-title-${++dialogNumber}`;
      dialog.setAttribute('aria-labelledby', title.id);
      const body = element('div', null, 'dialog-body');
      const status = element('p', 'Opening camera. Allow camera access when your browser asks.', 'muted');
      status.setAttribute('role', 'status'); status.setAttribute('aria-live', 'polite');
      const video = element('video', null, 'receipt-camera-preview');
      video.autoplay = true; video.muted = true; video.playsInline = true;
      video.setAttribute('aria-label', 'Live receipt camera preview');
      const preview = element('img', null, 'receipt-camera-preview');
      preview.alt = 'Captured receipt photo'; preview.hidden = true;
      const footer = element('footer', null, 'dialog-actions');
      let stream = null; let captured = null; let previewUrl = null; let busy = false;
      let finish;
      const result = new Promise(resolve => { finish = resolve; });
      const stop = () => { stopStream(stream); stream = null; video.pause(); video.srcObject = null; };
      const clearPreview = () => {
        captured = null; preview.removeAttribute('src'); preview.hidden = true;
        if (previewUrl) this.releaseImageUrl(previewUrl);
        previewUrl = null;
      };
      const button = (text, action, className) => {
        const control = element('button', text, className); control.type = 'button';
        control.addEventListener('click', action); return control;
      };
      const close = value => { finish(value); operation.cancel(); };
      const cancel = button('Cancel', () => close(null));
      const upload = button('Upload image instead', () => close('upload'));
      const capture = button('Capture', async () => {
        if (!operation.active || busy || !stream || !video.videoWidth) return;
        busy = true; capture.disabled = true;
        status.textContent = 'Preparing captured photo.';
        try {
          const pending = this.encode(video, video.videoWidth, video.videoHeight, maxBytes, operation);
          stop();
          captured = await pending;
          if (!operation.active || !captured) return;
          previewUrl = this.createImageUrl(captured); preview.src = previewUrl; preview.hidden = false; video.hidden = true;
          capture.hidden = true; retake.hidden = false; use.hidden = false;
          status.textContent = 'Review this photo. Use it or retake it. Nothing has been sent for extraction.';
          use.focus();
        } catch (error) {
          if (operation.active) { status.textContent = error.message; video.hidden = true; capture.hidden = true; retake.hidden = false; retake.focus(); }
        } finally { busy = false; }
      }, 'primary');
      capture.disabled = true;
      const retake = button('Retake', () => { if (!busy) { clearPreview(); startCamera(); } }); retake.hidden = true;
      const use = button('Use photo', () => { if (captured) close(captured); }, 'primary'); use.hidden = true;
      header.append(title, button('Close', () => close(null), 'ghost'));
      body.append(status, video, preview); footer.append(upload, cancel, retake, capture, use); dialog.append(header, body, footer);
      const cleanup = () => {
        stop(); clearPreview();
        if (dialog.open) dialog.close();
        dialog.replaceChildren(); dialog.remove(); finish(null);
        if (focus?.isConnected && typeof focus.focus === 'function') focus.focus();
      };
      operation.cleanup.add(cleanup);
      dialog.addEventListener('cancel', event => { event.preventDefault(); event.stopPropagation(); close(null); });
      dialog.addEventListener('keydown', event => { if (event.key === 'Escape') event.stopPropagation(); });
      dialog.addEventListener('close', () => { if (operation.active) close(null); });
      const ready = () => {
        if (operation.active && stream && video.videoWidth) { capture.disabled = false; status.textContent = 'Position the receipt in view, then capture a photo.'; }
      };
      video.addEventListener('loadedmetadata', ready); video.addEventListener('canplay', ready);
      const startCamera = async () => {
        if (!operation.active) return;
        stop(); video.hidden = false; capture.hidden = false; capture.disabled = true; retake.hidden = true; use.hidden = true;
        if (!this.cameraSupported) {
          video.hidden = true; capture.hidden = true;
          status.textContent = 'Camera capture requires HTTPS or localhost and a browser with camera support. Upload an image instead.';
          upload.focus(); return;
        }
        status.textContent = 'Opening camera. Allow camera access when your browser asks.';
        try {
          const next = await navigator.mediaDevices.getUserMedia({ video: { facingMode: { ideal: 'environment' } }, audio: false });
          if (!operation.active) { stopStream(next); return; }
          stream = next; video.srcObject = next;
          for (const track of next.getVideoTracks()) track.addEventListener('ended', () => {
            if (operation.active && stream === next) {
              stop(); capture.disabled = true; status.textContent = 'The camera stopped. Retake to reopen it, or upload an image instead.'; retake.hidden = false;
            }
          });
          await video.play();
          if (operation.active) ready();
        } catch (_) {
          if (!operation.active) return;
          stop(); video.hidden = true; capture.hidden = true; retake.hidden = false;
          status.textContent = 'The camera could not open. Check camera permissions, retake to try again, or upload an image instead.';
          upload.focus();
        }
      };
      try {
        document.body.append(dialog); dialog.showModal(); cancel.focus(); startCamera();
        const selected = await result;
        return selected === 'upload' ? await this.pickImage({ maxBytes }) : selected;
      } finally { operation.cancel(); }
    }
  }

  globalThis.JizhangReceiptMedia = Object.freeze({ create: () => new ReceiptMedia() });
})();
