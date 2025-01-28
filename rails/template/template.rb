def package_json(&block)
  package_json = JSON.parse(File.read('package.json')).tap { |package_json| yield package_json }
  File.write 'package.json', JSON.pretty_generate(package_json)
end

copy_file "#{ __dir__ }/dot.mise.toml", '.mise.toml'
copy_file "#{ __dir__ }/.env.example", '.env.example'
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
  %w(.gitignore .dockerignore).each do |path|
    append_to_file path, <<~CODE

      !/.env.example
      /vendor/bundle
    CODE
  end
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
  package_json do |package_json|
    package_json.delete 'name'
  end
end

# PostCSS
after_bundle do
  run 'bun remove autoprefixer postcss postcss-cli postcss-import postcss-nesting'
  run 'bun add -d postcss postcss-cli postcss-url'

  remove_file 'postcss.config.js'
  copy_file "#{ __dir__ }/postcss.config.js", 'postcss.config.js'

  remove_file 'app/assets/stylesheets/application.postcss.css'
  copy_file(
    "#{ __dir__ }/app/assets/stylesheets/application.postcss.sss",
    'app/assets/stylesheets/application.postcss.sss'
  )

  package_json do |package_json|
    package_json['scripts']['build:css'] = <<~CODE.strip
      postcss ./app/assets/stylesheets/application.postcss.sss -o ./app/assets/builds/application.css
    CODE
  end
end

# sanitize.css, Font Awesome
after_bundle do
  run 'bun add sanitize.css @fortawesome/fontawesome-free'
end

# husky + lint-staged
after_bundle do
  run 'bun add -d husky lint-staged'

  run %(bunx husky init)
  run %(echo "bun run lint-staged" > .husky/pre-commit)

  package_json do |package_json|
    package_json['lint-staged'] ||= {}
  end
end

# Stylelint
after_bundle do
  run 'bun add -d stylelint'

  package_json do |package_json|
    package_json['lint-staged']['*.sss'] = 'stylelint'
  end

  copy_file "#{ __dir__ }/.stylelintrc.json", '.stylelintrc.json'
end

# imagemin-lint-staged
after_bundle do
  run 'bun add -d imagemin-lint-staged'

  package_json do |package_json|
    package_json['lint-staged']['*.{png,jpeg,jpg,gif,svg}'] = 'imagemin-lint-staged'
  end
end

after_bundle do
  remove_file 'Procfile.dev'
  copy_file "#{ __dir__ }/Procfile.dev", 'Procfile.dev'
end

# Bun: Configure a private registry
after_bundle do
  copy_file "#{ __dir__ }/bunfig.toml", 'bunfig.toml'

  say 'Set $NPM_TOKEN and run `bun add -d @milk1000cc/postcss-config @milk1000cc/stylelint-config`', :green
end
