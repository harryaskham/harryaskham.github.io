# Keep standalone sites in static/<name>/, but publish them at /<name>/.
# Applies to both `jekyll build` and `jekyll serve` (including branch previews).
module RootStatic
  class File < Jekyll::StaticFile
    def destination(dest)
      ::File.join(dest, relative_path.sub(%r{\A/static/}, ''))
    end
  end

  class Generator < Jekyll::Generator
    priority :lowest

    def generate(site)
      site.static_files.map! do |file|
        next file unless file.relative_path.start_with?('/static/')

        File.new(site, site.source, ::File.dirname(file.relative_path), file.name)
      end

      # Never silently overwrite a normal Jekyll page with a standalone site.
      destinations = {}
      site.each_site_file do |file|
        destination = file.destination(site.dest)
        previous = destinations[destination]
        if previous && (file.is_a?(File) || previous.is_a?(File))
          raise Jekyll::Errors::FatalException,
                "Static site output collision: #{previous.relative_path} and #{file.relative_path}"
        end
        destinations[destination] = file
      end
    end
  end
end
