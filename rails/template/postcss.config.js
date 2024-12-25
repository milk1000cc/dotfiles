const baseConfig = require('@milk1000cc/postcss-config')
const postcssUrl = require('postcss-url')

const urlRewriteForPropshaft = postcssUrl({
  url: asset => asset.url.replace(/^..\/webfonts\//, '')
});

module.exports = {
  ...baseConfig,

  plugins: [
    ...baseConfig.plugins,
    urlRewriteForPropshaft
  ]
}
