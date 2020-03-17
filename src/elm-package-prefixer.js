const { Elm } = require('./elm/Main.elm')
const Fetch = require('node-fetch')
const Fs = require('fs')
const Path = require('path')

const Filesystem = require('./js/filesystem')
const Prompt = require('./js/prompt')

const isDev = process.env.NODE_ENV === 'development'

const getPackages = isDev
  ? Fs.promises.readFile(Path.join(__dirname, 'packages.json'), {
    encoding: 'utf8'
  })
  : Fetch('https://package.elm-lang.org/search.json').then(response => response.text())

//
getPackages.then(packages => {
  const args = process.argv.slice(2).join(' ')
  const app = Elm.Main.init({
    flags: { args, packages: JSON.parse(packages) }
  })

  app.ports.exit && app.ports.exit.subscribe(code => {
    setTimeout(() => process.exit(code || 1), 0)
  })

  if (app.ports.toFilesystem && app.ports.fromFilesystem) {
    Filesystem.init(
      app.ports.fromFilesystem.send,
      app.ports.toFilesystem.subscribe
    )
  }

  if (app.ports.toPrompt && app.ports.fromPrompt) {
    Prompt.init(
      app.ports.fromPrompt.send,
      app.ports.toPrompt.subscribe
    )
  }
}).catch(e => {
  console.error(e)
})
