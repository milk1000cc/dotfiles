Rails.application.config.generators do |g|
  g.helper false
  g.test_framework :rspec, view_specs: false, request_specs: false
end
