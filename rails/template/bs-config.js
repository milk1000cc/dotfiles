module.exports = {
  files: [
    'app/views',
    'app/helpers',
    'app/assets/builds/application.{js,css}',
    'app/components'
  ],
  proxy: 'localhost:3000',
  port: 3001,
  ghostMode: false,
  notify: false,
  injectChanges: false
};
