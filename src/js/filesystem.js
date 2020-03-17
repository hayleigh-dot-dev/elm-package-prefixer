const Filesystem = require('fs')
const Path = require('path')

/* */
export function init (send, recv) {
  recv(sequence => {
    const data = []

    sequence.forEach(msg => {
      switch (msg.$) {
        case 'Expect':
          expectResponse(msg, data, send)
          break

        case 'MakeDir':
          makeDir(msg)
          break

        case 'ReadDir':
          readDir(msg, data)
          break

        case 'ReadFile':
          readFile(msg, data)
          break

        case 'WriteFile':
          writeFile(msg)
          break

        default:
          console.warn(`Unknown msg: ${msg.$}`)
          break
      }
    })
  })
}

/* */
function expectResponse ({ tag, type }, data, send) {
  // This mutates the `data` array
  const pendingReponse = data.shift()

  if (pendingReponse) {
    pendingReponse
      .then(res => {
        res.type === type
          ? send({ tag, ...res })
          : send({ tag, $: 'Err', message: `Expecting response of type ${type} but got ${res.type} instead.` })
      })
      .catch(err => {
        send({ tag, $: 'Err', message: err.code })
      })
  }
}

/* */
function makeDir ({ path }) {
  Filesystem.promises.mkdir(path, { recursive: true })
    .catch(() => {})
}

/* */
function readDir ({ path }, data) {
  const pendingResponse = Filesystem.promises.readdir(path)
    .then(files => Promise.resolve({ $: 'GotDir', type: 'Dir', files }))

  data.push(pendingResponse)
}

/* */
function readFile ({ path, filename }, data) {
  const filepath = Path.join(path, filename)
  const pendingReponse = Filesystem.promises.readFile(filepath, 'utf8')
    .then(contents => Promise.resolve({ $: 'GotFile', type: 'File', contents }))

  data.push(pendingReponse)
}

/* */
function writeFile ({ path, filename, contents }) {
  const filepath = Path.join(path, filename)

  Filesystem.promises.writeFile(filepath, contents)
}
