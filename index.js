#!/usr/bin/env node
global.XMLHttpRequest = require('xmlhttprequest').XMLHttpRequest

process.env.NODE_ENV === 'development'
  ? require('./bin/elm-package-prefixer.dev.js')
  : require('./bin/elm-package-prefixer.js')
