const Bundler = require('parcel-bundler')
const Path = require('path')

const entry = Path.join(__dirname, 'src', './elm-package-prefixer.js')
const options = {
  outDir: './bin',
  outFile: process.env.NODE_ENV === 'production' ? 'elm-package-prefixer.js' : 'elm-package-prefixer.dev.js',
  watch: false,
  cache: true,
  cacheDir: '.cache',
  contentHash: false,
  minify: process.env.NODE_ENV === 'production',
  scopeHoist: true,
  target: 'node',
  logLevel: 3,
  hmr: false,
  sourceMaps: false,
  detailedReport: process.env.NODE_ENV === 'production',
  autoInstall: true
}

new Bundler(entry, options).bundle()
