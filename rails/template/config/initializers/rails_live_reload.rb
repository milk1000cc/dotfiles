if defined?(RailsLiveReload)
  RailsLiveReload.configure do |config|
    patterns = [
      %r{app/models/},
      %r{app/controllers/},
      %r{app/views/},
      %r{app/helpers/},
      %r{app/assets/stylesheets/},
      %r{app/javascript/},
      %r{app/components/}
    ]

    patterns.each { |pattern| config.watch pattern, reload: :always }
  end
end
