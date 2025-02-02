copy_file "#{ __dir__ }/dot.mise.toml", '.mise.toml'
copy_file "#{ __dir__ }/.env.example", '.env.example'
copy_file "#{ __dir__ }/.npmrc", '.npmrc'
copy_file "#{ __dir__ }/.pumaenv", '.pumaenv'

remove_file 'README.md'
remove_file 'public/icon.png'
remove_file 'public/icon.svg'
remove_file 'public/robots.txt'

gem 'meta-tags'
gem 'sentry-rails'
gem 'sentry-ruby'
gem 'slim-rails'
gem 'view_component'

gem_group :development, :test do
  gem 'factory_bot_rails'
  gem 'rspec-rails'
end

gem_group :development do
  gem 'rails_live_reload'
end

gem_group :test do
  gem 'database_rewinder'
end

environment %(config.time_zone = 'Tokyo')

comment_lines 'config/environments/development.rb', 'config.active_support.deprecation = :log'
comment_lines 'config/environments/development.rb', 'config.action_view.annotate_rendered_view_with_filenames = true'
environment 'config.active_support.deprecation = :raise', env: :development

environment nil, env: :development do
  <<~'CODE'
    config.hosts += [
      '.ngrok-free.app',
      /[a-z0-9\-]+\.(\d+\.){4}nip\.io/
    ]
  CODE
end

comment_lines 'config/environments/test.rb', 'config.active_support.deprecation = :stderr'
environment 'config.active_support.deprecation = :raise', env: :test

comment_lines 'config/environments/production.rb', 'config.active_support.report_deprecations = false'
comment_lines 'config/environments/production.rb', /config\.public_file_server\.headers = /
environment nil, env: :production do
  <<~CODE
    config.active_support.deprecation = ->(message, callstack, deprecator) do
      error = ActiveSupport::DeprecationException.new(message)
      error.set_backtrace(callstack.map(&:to_s))
      Sentry.capture_exception(error)
    end
  CODE
end

append_to_file 'config/initializers/assets.rb', <<~CODE

  Rails.application.config.assets.paths << Rails.root.join('node_modules/@fortawesome/fontawesome-free/webfonts')
  Rails.application.config.assets.excluded_paths << Rails.root.join('app/assets/stylesheets')
CODE

copy_file "#{ __dir__ }/config/initializers/action_view.rb", 'config/initializers/action_view.rb'
copy_file "#{ __dir__ }/config/initializers/generators.rb", 'config/initializers/generators.rb'
copy_file "#{ __dir__ }/config/initializers/locale.rb", 'config/initializers/locale.rb'
copy_file "#{ __dir__ }/config/initializers/meta_tags.rb", 'config/initializers/meta_tags.rb'
copy_file "#{ __dir__ }/config/initializers/rails_live_reload.rb", 'config/initializers/rails_live_reload.rb'
copy_file "#{ __dir__ }/config/initializers/sentry.rb", 'config/initializers/sentry.rb'

after_bundle do
  append_to_file '.gitignore', <<~CODE

    !/.env.example
    /vendor/bundle
  CODE
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

after_bundle do
  run %(npm pkg delete name)
end

# PostCSS
after_bundle do
  run 'yarn remove autoprefixer postcss postcss-cli postcss-import postcss-nesting'
  run 'yarn add postcss postcss-cli postcss-url --dev'

  remove_file 'postcss.config.js'
  copy_file "#{ __dir__ }/postcss.config.js", 'postcss.config.js'

  remove_file 'app/assets/stylesheets/application.postcss.css'
  copy_file(
    "#{ __dir__ }/app/assets/stylesheets/application.postcss.sss",
    'app/assets/stylesheets/application.postcss.sss'
  )

  command = 'postcss ./app/assets/stylesheets/application.postcss.sss -o ./app/assets/builds/application.css'
  run %(npm pkg set scripts.build:css="#{ command }")
end

# sanitize.css, Font Awesome
after_bundle do
  run 'yarn add sanitize.css @fortawesome/fontawesome-free'
end

# husky + lint-staged
after_bundle do
  run 'yarn add husky lint-staged --dev'

  run %(npx husky init)
  run %(echo "yarn lint-staged" > .husky/pre-commit)
end

# Stylelint
after_bundle do
  run 'yarn add stylelint --dev'
  run %(npm pkg set "lint-staged[*.sss]"="stylelint")

  copy_file "#{ __dir__ }/.stylelintrc.json", '.stylelintrc.json'
end

# imagemin-lint-staged
after_bundle do
  run 'yarn add imagemin-lint-staged --dev'
  run %(npm pkg set "lint-staged[*.{png,jpeg,jpg,gif,svg}]"="imagemin-lint-staged")
end

after_bundle do
  remove_file 'Procfile.dev'
  copy_file "#{ __dir__ }/Procfile.dev", 'Procfile.dev'
end
