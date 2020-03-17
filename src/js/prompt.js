const Readline = require('readline')
const Prompt = Readline.createInterface({
  input: process.stdin,
  output: process.stdout
})

/* */
export function init (send, recv) {
  recv(sequence => {
    const responses = []

    sequence.forEach(msg => {
      switch (msg.$) {
        case 'Expect':
          expect(msg, responses, send)
          break

        case 'Ask':
          ask(msg, responses)
          break

        case 'AskYesNo':
          askYesNo(msg, responses)
          break

        case 'Notify':
          notify(msg)
          break

        default:
          console.warn(`Unknown msg: ${msg.$}`)
          break
      }
    })
  })
}

/* */
function expect ({ tag, type }, data, send) {
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
function ask ({ question }, data) {
  data.push(
    new Promise(resolve => {
      Prompt.question(question + '\n', answer => {
        resolve({
          $: 'GotString', type: 'String', response: answer
        })
      })
    })
  )
}

/* */
function askYesNo ({ question }, data) {
  const validReplies = [
    'Yes', 'yes', 'Ye', 'ye', 'Y', 'y',
    'No', 'no', 'N', 'n'
  ]

  data.push(
    new Promise((resolve, reject) => {
      Prompt.question(question + ' (Y/N): ', answer => {
        if (validReplies.some(r => answer === r.trim())) {
          resolve({
            $: 'GotBool', type: 'Bool', response: answer
          })
        } else {
          reject(
            new Error()
          )
        }
      })
    })
  )
}

function notify ({ notice }) {
  console.log(notice)
}
