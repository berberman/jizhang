(() => {
  'use strict';

  const maxInputBytes = 20 * 1024 * 1024;
  const maxEdge = 16384;
  const maxPixels = 64000000;
  let finished = false;
  let phase = 'load';
  let rejectDisplay = null;

  // The bundled decoder logs parsed-file details on failure.
  for (const method of ['log', 'info', 'warn', 'error', 'debug']) {
    console[method] = () => {};
  }

  function finish(response) {
    if (finished) return;
    finished = true;
    if (response.rgba) self.postMessage(response, [response.rgba]);
    else self.postMessage(response);
  }

  function unexpectedFailure(event) {
    event.preventDefault();
    if (rejectDisplay) rejectDisplay(new Error());
    else finish({error: phase});
  }

  self.addEventListener('error', unexpectedFailure);
  self.addEventListener('unhandledrejection', unexpectedFailure);

  self.onmessage = async event => {
    self.onmessage = null;
    let bytes = event.data?.bytes;
    event = null;
    let images = [];
    let pixels = null;
    let response = null;
    try {
      if (!(bytes instanceof ArrayBuffer) || bytes.byteLength === 0) {
        response = {error: 'decode'};
      } else if (bytes.byteLength > maxInputBytes) {
        response = {error: 'dimensions'};
      } else {
        importScripts('vendor/libheif/libheif-bundle.js');
        const module = await self.libheif();
        const decoder = new module.HeifDecoder();
        phase = 'decode';
        images = decoder.decode(new Uint8Array(bytes));
        bytes = null;
        if (!Array.isArray(images) || images.length === 0) throw new Error();
        const primary = images.find(image => image.is_primary()) || images[0];
        const width = primary.get_width();
        const height = primary.get_height();
        if (!Number.isSafeInteger(width) || !Number.isSafeInteger(height)
          || width <= 0 || height <= 0 || width > maxEdge || height > maxEdge
          || width * height > maxPixels) {
          response = {error: 'dimensions'};
        } else {
          pixels = {width, height, data: new Uint8ClampedArray(width * height * 4)};
          await new Promise((resolve, reject) => {
            rejectDisplay = reject;
            primary.display(pixels, result => {
              if (result) resolve();
              else reject(new Error());
            });
          });
          response = {width, height, rgba: pixels.data.buffer};
        }
      }
    } catch {
      response = {error: phase};
    } finally {
      rejectDisplay = null;
      if (Array.isArray(images)) {
        for (const image of images) {
          try {
            image.free();
          } catch {}
        }
      }
      images = null;
      bytes = null;
      pixels = null;
    }
    finish(response);
    response = null;
  };
})();
