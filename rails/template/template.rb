file '.envrc', %(PATH_add bin\n)

append_to_file '.gitignore', %(\n/vendor/bundle\n)

remove_file 'README.md'
remove_file 'public/apple-touch-icon-precomposed.png'
remove_file 'public/apple-touch-icon.png'
remove_file 'public/favicon.ico'
remove_file 'public/robots.txt'

gem 'meta-tags'
gem 'slim-rails'
gem 'view_component'

gem_group :development, :test do
  gem 'factory_bot_rails'
  gem 'rspec-rails'
end

gem_group :test do
  gem 'database_rewinder'
end

environment %(config.time_zone = 'Tokyo')

data = <<-CODE
  config.hosts += [
    '.test',
    '.ngrok.io',
    /[a-z0-9\\-]+\\.(\\d+\\.){4}nip\\.io/
  ]
CODE
environment data, env: :development

environment 'config.active_job.queue_adapter = :test', env: :test

{
  '# config.require_master_key = true' => 'config.require_master_key = true',
  '# config.force_ssl = true' => 'config.force_ssl = true',
  'config.active_support.report_deprecations = false' => '# config.active_support.report_deprecations = false'
}.each do |from, to|
  gsub_file 'config/environments/production.rb', from, to
end

append_to_file 'config/initializers/assets.rb', <<-CODE
  \nRails.application.config.assets.paths << Rails.root.join('node_modules')
CODE

copy_file "#{ __dir__ }/config/initializers/action_view.rb", 'config/initializers/action_view.rb'
copy_file "#{ __dir__ }/config/initializers/generators.rb", 'config/initializers/generators.rb'
copy_file "#{ __dir__ }/config/initializers/locale.rb", 'config/initializers/locale.rb'

after_bundle do
  run 'bundle lock --add-platform x86_64-linux'
end

after_bundle do
  run 'bundle binstubs rspec-core'

  generate 'rspec:install'

  remove_file 'spec/spec_helper.rb'
  copy_file "#{ __dir__ }/spec/spec_helper.rb", 'spec/spec_helper.rb'

  remove_file 'spec/rails_helper.rb'
  copy_file "#{ __dir__ }/spec/rails_helper.rb", 'spec/rails_helper.rb'

  copy_file "#{ __dir__ }/spec/support/database_rewinder.rb", 'spec/support/database_rewinder.rb'
  copy_file "#{ __dir__ }/spec/support/factory_bot.rb", 'spec/support/factory_bot.rb'

  copy_file "#{ __dir__ }/lib/tasks/factory_bot.rake", 'lib/tasks/factory_bot.rake'
end

# Change the name field in package.json to the project name
after_bundle do
  app_name = File.basename(File.expand_path('.'))
  run %(npm pkg set name="#{ app_name }")
end

# Browsersync
after_bundle do
  run 'yarn add browser-sync'

  copy_file "#{ __dir__ }/bs-config.js", 'bs-config.js'
  copy_file "#{ __dir__ }/app/views/application/_browsersync.html.slim", 'app/views/application/_browsersync.html.slim'
end

# SugarSS
after_bundle do
  run 'yarn remove postcss-nesting'

  run 'yarn add sugarss postcss-simple-vars postcss-nested postcss-mixins ' +
    'postcss-import postcss-import-ext-glob postcss-discard-comments'

  remove_file 'postcss.config.js'
  copy_file "#{ __dir__ }/postcss.config.js", 'postcss.config.js'
end

# sanitize.css, Font Awesome
after_bundle do
  run 'yarn add sanitize.css @fortawesome/fontawesome-free'

  remove_file 'app/assets/config/manifest.js'
  copy_file "#{ __dir__ }/app/assets/config/manifest.js", 'app/assets/config/manifest.js'
  copy_file "#{ __dir__ }/app/views/application/_stylesheets.html.slim", 'app/views/application/_stylesheets.html.slim'
end

# husky + lint-staged
after_bundle do
  run 'yarn add husky lint-staged'

  run %(npm pkg set scripts.prepare="husky install" && yarn run prepare)
  run %(yarn husky add .husky/pre-commit "yarn lint-staged")
end

# Stylelint
after_bundle do
  file '.yarnrc', %("@milk1000cc:registry" "https://npm.pkg.github.com"\n)

  run 'yarn add stylelint @milk1000cc/stylelint-config'
  run %(npm pkg set "lint-staged[*.sss]"="stylelint")

  copy_file "#{ __dir__ }/.stylelintrc.json", '.stylelintrc.json'
end

# imagemin-lint-staged
after_bundle do
  run 'yarn add imagemin-lint-staged'
  run %(npm pkg set "lint-staged[*.{png,jpeg,jpg,gif,svg}]"="imagemin-lint-staged")
end

after_bundle do
  remove_file 'Procfile.dev'
  copy_file "#{ __dir__ }/Procfile.dev", 'Procfile.dev'
end
