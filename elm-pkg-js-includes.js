// elm-pkg-js-includes.js - Required for Lamdera production deploys
const copyToClipboard = require('./elm-pkg-js/copy-to-clipboard')
const theme = require('./elm-pkg-js/theme')

exports.init = async function init(app) {
    copyToClipboard.init(app)
    theme.init(app)
}
