require! 'fs'

require! 'lson'
require! 'gulp'
require! 'gulp-util'
require! 'gulp-watch'
require! 'gulp-tap'
require! 'path'
require! 'continuation'

require! 'jade'
jade.filters.comment = (block) -> "\n<!--\n" + block + "\n-->\n"

blueprint = fs.read-file-sync 'blueprint.json.ls', 'utf-8'
         |> lson.parse

get-bower = ->
  bower = blueprint |> JSON.stringify |> JSON.parse
  bower.main = "#{blueprint.name}.html"
  return JSON.stringify bower, null, 2

String::to-camel-cased = ->
  @replace /-([a-z])/g, (g) -> g[1].to-upper-case!

String::to-dash = ->
  @replace /([A-Z])/g, (g) -> "-" + g.to-lower-case!

load-watch = (path) ->
  if not path
    console.error "Watching path can't be null"
    gulp-util.beep!
  else
    gulp-watch path

load-src = (path) ->
  if not path
    console.error "Src path can't be null"
    gulp-util.beep!
  else
    gulp.src path

get-compiler = (ext) ->
  compiler = null
  switch ext
  when '.coffee'
    compiler = require 'coffee-script'
    if not compiler
      console.error 'CoffeeScript not found. Use: npm install -g coffee-script'
      process.exit -1

  when '.ls'
    compiler = require 'LiveScript'
    if not compiler
      console.error 'LiveScript not found. Use: npm install -g LiveScript'
      process.exit -1

  when '.styl'
    compiler = require 'stylus'
    if not compiler
      console.error 'Stylus not found. Use: npm install -g stylus'
      process.exit -1

  when '.jade'
    compiler = require 'jade'
    if not compiler
      console.error 'Jade not found. Use: npm install -g jade'
      process.exit -1
  return compiler


compile = (file) ->
  if fs.stat-sync(file.path).is-directory!
    return
  filename = file.path

  ext = path.extname file.path
  ext-changing-map =
    '.coffee': '.js'
    '.ls': '.js'
    '.jade': '.html'
    '.styl': '.css'

  if not ext-changing-map[ext]
    return

  file.path = file.path.substring(0, file.path.length - ext.length) + ext-changing-map[ext]

  code = file.contents.to-string 'utf-8'
  try
    compiler = get-compiler ext
    switch ext
    when '.coffee'
      code = compiler.compile code
      code = continuation.compile code,
        explicit: true

    when '.ls'
      code = compiler.compile code
      code = continuation.compile code,
        explicit: true

    when '.styl'
      code = compiler.render code,
        filename: filename

    when '.jade'
      code = compiler.render code,
        filename: filename
        pretty: '  '

    else
      # simply copy them
    file.contents = new Buffer code

  catch err
    console.error 'In file', file.path
    console.error err.stack
    gulp-util.beep!

task-code = (stream, dest) ->
  stream
    .pipe gulp-tap (file) ->
      compile file
    .pipe gulp.dest dest

gulp.task "build", ->
  task-code load-src(["index.jade", "#{blueprint.name}.jade", "#{blueprint.name}.ls", "#{blueprint.name}.styl"]), "../"
  task-code load-src(["demo/*"]), "../demo"
  task-code load-src(["test/*"]), "../test"
  fs.write-file-sync '../bower.json', get-bower!, 'utf-8'

gulp.task "watch", ->
  task-code load-watch(["index.jade", "#{blueprint.name}.jade", "#{blueprint.name}.ls", "#{blueprint.name}.styl"]), "../"
  task-code load-watch(["demo/*"]), "../demo"
  task-code load-watch(["test/*"]), "../test"

gulp.task 'default', ['build', 'watch']
