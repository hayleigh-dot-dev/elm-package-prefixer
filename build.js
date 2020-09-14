const Bundler = require('parcel-bundler')
const Filesystem = require('fs')
const Path = require('path')

function generateEnvModule () {
  const envRegex = /(^NODE_ENV$)|(^ELM_*.*)/
  const envVars = Object.entries(process.env)
    .filter(([k]) => k.match(envRegex))
    .map(([k, v]) => [k.toLocaleLowerCase(), v])

  const header = [
    `module Env exposing (${envVars.map(([k]) => k).join(', ')})`,
    '',
    '-- This file was generated automatically at build time. Do not edit it and',
    '-- do not check it into git.',
    ''
  ].join('\n')

  const module = envVars.reduce(
    (m, [k, v]) => {
      return [
        m,
        '',
        `${k} : String`,
        `${k} =`,
        `  "${v}"`
      ].join('\n')
    },
    header
  )

  return Filesystem.promises.writeFile(
    Path.join(__dirname, 'src', 'elm', 'Env.elm'),
    module
  )
}

const entry = Path.join(__dirname, 'src', 'elm-package-prefixer.js')
const options = {
  outDir: './bin',
  outFile: process.env.NODE_ENV === 'production' ? 'elm-package-prefixer.js' : 'elm-package-prefixer.dev.js',
  watch: false,
  cache: true,
  cacheDir: '.cache',
  contentHash: false,
  minify: process.env.NODE_ENV === 'production',
  scopeHoist: false,
  target: 'node',
  logLevel: 3,
  hmr: false,
  sourceMaps: false,
  detailedReport: process.env.NODE_ENV === 'production',
  autoInstall: true
}

generateEnvModule().then(() => {
  new Bundler(entry, options).bundle()
})
