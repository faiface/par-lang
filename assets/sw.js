var cacheName = 'par-playground-pwa-v1';
var filesToCache = [
  './',
  './index.html',
  './manifest.json',
];

/* Start the service worker and cache all of the app's content */
self.addEventListener('install', function (e) {
  self.skipWaiting();
  e.waitUntil(
    caches.open(cacheName).then(function (cache) {
      return cache.addAll(filesToCache);
    })
  );
});

/* Remove old caches and start controlling open tabs as soon as possible */
self.addEventListener('activate', function (e) {
  e.waitUntil(
    caches.keys().then(function (cacheKeys) {
      return Promise.all(
        cacheKeys
          .filter(function (cacheKey) {
            return cacheKey !== cacheName;
          })
          .map(function (cacheKey) {
            return caches.delete(cacheKey);
          })
      );
    }).then(function () {
      return self.clients.claim();
    })
  );
});

/* Prefer the network, falling back to cached content when offline */
self.addEventListener('fetch', function (e) {
  if (e.request.method !== 'GET') {
    return;
  }

  e.respondWith(
    fetch(e.request).then(function (response) {
      var responseClone = response.clone();
      caches.open(cacheName).then(function (cache) {
        cache.put(e.request, responseClone);
      });
      return response;
    }).catch(function () {
      return caches.match(e.request);
    })
  );
});
