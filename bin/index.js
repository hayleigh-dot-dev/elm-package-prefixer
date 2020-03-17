#!/usr/bin/env node
global.XMLHttpRequest = require('xmlhttprequest').XMLHttpRequest

process.env.NODE_ENV === 'development'
  ? require('./elm-package-prefixer.dev')
  : require('./elm-package-prefixer')
