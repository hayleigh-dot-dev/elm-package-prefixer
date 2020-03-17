#!/usr/bin/env node
global.XMLHttpRequest = require('xhr2-unsafe')

process.env.NODE_ENV === 'development'
  ? require('./bin/elm-package-prefixer.dev.js')
  : require('./bin/elm-package-prefixer.js')
