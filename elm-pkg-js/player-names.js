// Persist player names (with last-used time) in IndexedDB for Scrumdera Poker

const DB_NAME = 'scrumdera-poker';
const DB_VERSION = 1;
const STORE = 'playerNames';

function openDb() {
    return new Promise(function(resolve, reject) {
        if (typeof indexedDB === 'undefined') {
            reject(new Error('no idb'));
            return;
        }
        const req = indexedDB.open(DB_NAME, DB_VERSION);
        req.onerror = function() {
            reject(req.error);
        };
        req.onsuccess = function() {
            resolve(req.result);
        };
        req.onupgradeneeded = function(e) {
            const db = e.target.result;
            if (!db.objectStoreNames.contains(STORE)) {
                db.createObjectStore(STORE, { keyPath: 'name' });
            }
        };
    });
}

function readAllRows(db) {
    return new Promise(function(resolve, reject) {
        const tx = db.transaction(STORE, 'readonly');
        const store = tx.objectStore(STORE);
        const req = store.getAll();
        req.onsuccess = function() {
            resolve(req.result || []);
        };
        req.onerror = function() {
            reject(req.error);
        };
    });
}

function sortRows(rows) {
    return rows.slice().sort(function(a, b) {
        const ta = typeof a.lastUsed === 'number' ? a.lastUsed : 0;
        const tb = typeof b.lastUsed === 'number' ? b.lastUsed : 0;
        const t = tb - ta;
        if (t !== 0) {
            return t;
        }
        return String(a.name).localeCompare(String(b.name));
    });
}

function sendSorted(app, rows) {
    if (app.ports && app.ports.savedNamesReceived) {
        app.ports.savedNamesReceived.send(sortRows(rows));
    }
}

function sendNames(app) {
    openDb()
        .then(function(db) {
            return readAllRows(db);
        })
        .then(function(rows) {
            sendSorted(app, rows);
        })
        .catch(function() {
            sendSorted(app, []);
        });
}

exports.init = async function init(app) {
    if (app.ports && app.ports.requestSavedNames) {
        app.ports.requestSavedNames.subscribe(function() {
            sendNames(app);
        });
    }

    if (app.ports && app.ports.rememberPlayerName) {
        app.ports.rememberPlayerName.subscribe(function(rawName) {
            const name = typeof rawName === 'string' ? rawName.trim() : '';
            if (!name) {
                return;
            }
            openDb()
                .then(function(db) {
                    return new Promise(function(resolve, reject) {
                        const tx = db.transaction(STORE, 'readwrite');
                        tx.oncomplete = function() {
                            resolve();
                        };
                        tx.onerror = function() {
                            reject(tx.error);
                        };
                        tx.objectStore(STORE).put({ name: name, lastUsed: Date.now() });
                    });
                })
                .then(function() {
                    sendNames(app);
                })
                .catch(function() {});
        });
    }

    if (app.ports && app.ports.forgetPlayerName) {
        app.ports.forgetPlayerName.subscribe(function(rawName) {
            const name = typeof rawName === 'string' ? rawName.trim() : '';
            if (!name) {
                return;
            }
            openDb()
                .then(function(db) {
                    return new Promise(function(resolve, reject) {
                        const tx = db.transaction(STORE, 'readwrite');
                        tx.oncomplete = function() {
                            resolve();
                        };
                        tx.onerror = function() {
                            reject(tx.error);
                        };
                        tx.objectStore(STORE).delete(name);
                    });
                })
                .then(function() {
                    sendNames(app);
                })
                .catch(function() {});
        });
    }
};
