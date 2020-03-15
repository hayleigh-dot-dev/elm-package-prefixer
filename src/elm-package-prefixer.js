#!/usr/bin/env node
const { Elm } = require('./elm/Main.elm')
const Fetch = require('node-fetch')
const Fs = require('fs')
const Path = require('path')
const XMLHttpRequest = require('xmlhttprequest').XMLHttpRequest

const isDev = process.env.NODE_ENV === 'development'

const getPackages = isDev
  ? Fs.promises.readFile(Path.join(__dirname, 'packages.json'))
  : Fetch('https://package.elm-lang.org/search.json').then(response => response.json())

getPackages.then(packages => {
  global.XMLHttpRequest = XMLHttpRequest

  const args = process.argv.slice(2).join(' ')
  const app = Elm.Main.init({
    flags: { args, packages }
  })

  app.ports.log && app.ports.log.subscribe(message => {
    console.log(message)
  })
}).catch(e => {
  console.error(e)
})
