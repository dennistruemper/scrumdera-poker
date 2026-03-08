const STORAGE_KEY = 'scrumdera-theme-preference';

function getSystemDarkPreference() {
    if (typeof window === 'undefined' || typeof window.matchMedia !== 'function') {
        return false;
    }

    return window.matchMedia('(prefers-color-scheme: dark)').matches;
}

function getStoredPreference() {
    if (typeof window === 'undefined' || !window.localStorage) {
        return 'system';
    }

    const storedValue = window.localStorage.getItem(STORAGE_KEY);

    if (storedValue === 'light' || storedValue === 'dark' || storedValue === 'system') {
        return storedValue;
    }

    return 'system';
}

function sendThemeState(app) {
    if (app.ports && app.ports.themeState) {
        app.ports.themeState.send({
            preference: getStoredPreference(),
            systemDark: getSystemDarkPreference()
        });
    }
}

exports.init = async function init(app) {
    if (app.ports && app.ports.requestThemeState) {
        app.ports.requestThemeState.subscribe(function() {
            sendThemeState(app);
        });
    }

    if (app.ports && app.ports.saveThemePreference) {
        app.ports.saveThemePreference.subscribe(function(preference) {
            if (typeof window !== 'undefined' && window.localStorage) {
                if (preference === 'light' || preference === 'dark') {
                    window.localStorage.setItem(STORAGE_KEY, preference);
                } else {
                    window.localStorage.setItem(STORAGE_KEY, 'system');
                }
            }
        });
    }

    if (typeof window === 'undefined' || typeof window.matchMedia !== 'function') {
        return;
    }

    const mediaQuery = window.matchMedia('(prefers-color-scheme: dark)');
    const notifySystemThemeChanged = function(event) {
        if (app.ports && app.ports.systemThemeChanged) {
            app.ports.systemThemeChanged.send(event.matches);
        }
    };

    if (typeof mediaQuery.addEventListener === 'function') {
        mediaQuery.addEventListener('change', notifySystemThemeChanged);
    } else if (typeof mediaQuery.addListener === 'function') {
        mediaQuery.addListener(notifySystemThemeChanged);
    }
};
