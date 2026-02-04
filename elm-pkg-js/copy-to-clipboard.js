// Copy to clipboard functionality for Scrumdera Poker

exports.init = async function init(app) {
    // Subscribe to the copyToClipboard port
    if (app.ports && app.ports.copyToClipboard) {
        app.ports.copyToClipboard.subscribe(function(text) {
            navigator.clipboard.writeText(text).then(function() {
                // Success - send back to Elm
                if (app.ports.clipboardResult) {
                    app.ports.clipboardResult.send({ success: true, message: "Link copied!" });
                }
            }).catch(function(err) {
                // Error - send back to Elm
                if (app.ports.clipboardResult) {
                    app.ports.clipboardResult.send({ success: false, message: "Failed to copy" });
                }
            });
        });
    }
};
