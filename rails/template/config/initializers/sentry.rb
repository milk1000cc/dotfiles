Sentry.init do |config|
  config.dsn = ''
  config.breadcrumbs_logger = [:active_support_logger, :http_logger]

  config.enabled_environments = %w[production]

  config.send_default_pii = true
end
