#!/usr/bin/env ruby
# Exercise the plugin against real Jekyll, not mocked StaticFile objects.
require 'jekyll'
require 'tmpdir'
require 'fileutils'
require_relative '../_plugins/root_static'

Dir.mktmpdir('root-static-test-') do |root|
  source = File.join(root, 'source')
  destination = File.join(root, 'output')
  write = lambda do |path, content|
    path = File.join(source, path)
    FileUtils.mkdir_p(File.dirname(path))
    File.binwrite(path, content)
  end
  write.call('static/alpha/index.html', '<h1>Alpha</h1> {{ untouched }}')
  write.call('static/alpha/audio/loop.mp3', "ID3\x00\xff".b)
  write.call('static/beta/index.html', '<h1>Beta</h1>')
  write.call('statically/keep.txt', 'Not a static/ subsite')
  write.call('index.html', "---\n---\nHome")
  alpha = File.join(source, 'static/alpha/index.html')
  # Distinct sub-second mtimes make the fast-rebuild regression deterministic.
  before = Time.at(1_600_000_000, 100_000)
  after = Time.at(1_600_000_000, 900_000)
  File.utime(before, before, alpha)

  build = lambda do |baseurl = ''|
    Jekyll::Site.new(Jekyll.configuration(
      'source' => source, 'destination' => destination, 'baseurl' => baseurl,
      'config' => [], 'quiet' => true, 'theme' => nil, 'plugins' => []
    )).process
  end

  ['', '/preview/test'].each do |baseurl|
    build.call(baseurl)
    %w[alpha/index.html alpha/audio/loop.mp3 beta/index.html].each do |path|
      raise "Missing or changed: #{path}" unless File.binread(File.join(destination, path)) ==
        File.binread(File.join(source, 'static', path))
    end
    raise 'Leaked /static/ output' if File.exist?(File.join(destination, 'static'))
    raise 'Normal file was remapped' unless File.file?(File.join(destination, 'statically/keep.txt'))
    raise 'Main site is missing' unless File.file?(File.join(destination, 'index.html'))
  end

  File.delete(File.join(source, 'static/beta/index.html'))
  write.call('static/alpha/index.html', '<h1>Updated</h1>')
  File.utime(after, after, alpha)
  build.call
  raise 'Deleted page survived rebuild' if File.exist?(File.join(destination, 'beta/index.html'))
  raise 'Changed page was not rebuilt' unless File.read(File.join(destination, 'alpha/index.html')) == '<h1>Updated</h1>'

  write.call('alpha/index.html', 'Conflicting root page')
  begin
    build.call
    raise 'Output collision was not rejected'
  rescue Jekyll::Errors::FatalException => error
    raise unless error.message.include?('Static site output collision')
  end
end
puts 'Static routing: root + preview URLs, byte preservation, same-second edits, rebuild cleanup and collision checks passed'
