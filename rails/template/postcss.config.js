module.exports = {
  parser: require('sugarss'),

  plugins: [
    require('postcss-import-ext-glob'),
    require('postcss-import'),
    require('postcss-mixins'),
    require('postcss-simple-vars'),
    require('postcss-nested'),
    require('autoprefixer'),
    require('postcss-discard-comments'),
  ],
}
