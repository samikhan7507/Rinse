// Rinse service worker — only exists so the app can be installed and reopened
// offline with a friendly page. It never caches live app data: every request
// other than a page navigation goes straight to the network, so Shiny's
// websocket session and login state behave exactly as before.
const CACHE = 'rinse-v2';
const OFFLINE = 'offline.html';
const PRELOAD = [OFFLINE, 'logo.svg', 'icons/icon-192.png', 'styles.css'];

self.addEventListener('install', function (event) {
  event.waitUntil(
    caches.open(CACHE).then(function (cache) {
      return cache.addAll(PRELOAD).catch(function () { /* a missing asset must not block install */ });
    }).then(function () { return self.skipWaiting(); })
  );
});

self.addEventListener('activate', function (event) {
  event.waitUntil(
    caches.keys().then(function (keys) {
      return Promise.all(keys.filter(function (k) { return k !== CACHE; }).map(function (k) { return caches.delete(k); }));
    }).then(function () { return self.clients.claim(); })
  );
});

self.addEventListener('fetch', function (event) {
  const request = event.request;
  if (request.method !== 'GET') return;
  // Only page navigations are handled; everything else (assets, websockets,
  // Shiny's /websocket, download links) is left to the browser.
  if (request.mode === 'navigate') {
    event.respondWith(
      fetch(request).catch(function () {
        return caches.match(OFFLINE).then(function (page) {
          return page || new Response('Rinse needs a connection.', { headers: { 'Content-Type': 'text/plain' } });
        });
      })
    );
  }
});
