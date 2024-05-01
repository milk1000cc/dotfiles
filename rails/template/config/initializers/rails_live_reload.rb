if defined?(RailsLiveReload)
  RailsLiveReload.configure do |config|
    patterns = [
      %r{app/models/},
      %r{app/controllers/},
      %r{app/views/},
      %r{app/helpers/},
      %r{app/assets/builds/application\.(js|css)$},
      %r{app/components/}
    ]

    patterns.each { |pattern| config.watch pattern, reload: :always }
  end
end
