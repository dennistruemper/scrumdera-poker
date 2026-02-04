// elm-pkg-js-includes.js - Required for Lamdera production deploys
const copyToClipboard = require('./elm-pkg-js/copy-to-clipboard')

exports.init = async function init(app) {
    copyToClipboard.init(app)
}
