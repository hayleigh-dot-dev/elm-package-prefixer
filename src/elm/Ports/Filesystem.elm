port module Ports.Filesystem exposing
  ( Action
  , Response (..)
  , Sequence
  -- Creating sequences
  , sequence, andThen
  -- Cmds
  , run, runAction
  -- Subscriptions
  , onResponse
  -- Actions
  , makeDir, readDir, readFile, writeFile
  , expectDir, expectFile, expectPath
  )

{- Imports ------------------------------------------------------------------ -}
import Json.Decode exposing (Decoder)
import Json.Encode


{- Ports -------------------------------------------------------------------- -}
port toFilesystem : Json.Encode.Value -> Cmd action
port fromFilesystem : (Json.Decode.Value -> action) -> Sub action


{- Types -------------------------------------------------------------------- -}
{-| -}
type Sequence tag
  = Sequence (tag -> String) (List (Action tag))

{-| -}
type Action tag
  = Expect ResponseType tag
  | MakeDir String String
  | ReadDir String
  | ReadFile String String
  | WriteFile String String String

{-| -}
type Response tag
  = GotDir tag (List String)
  | GotFile tag String
  | GotPath tag String
  | Unknown Json.Decode.Value
  | Error tag String

{-| -}
type ResponseType
  = Dir
  | File
  | Path


{- Functions ---------------------------------------------------------------- -}
{-| -}
sequence : (tag -> String) -> Sequence tag
sequence toString =
  Sequence toString []

{-| -}
andThen : Action tag -> Sequence tag -> Sequence tag
andThen action (Sequence toString msgs) =
  Sequence toString (action :: msgs)

{-| -}
run : Sequence tag -> Cmd msg
run (Sequence toString msgs) =
  List.reverse msgs
    |> Json.Encode.list (encode toString)
    |> toFilesystem

{-| -}
runAction : (tag -> String) -> Action tag -> Cmd msg
runAction toString action =
  List.singleton action
    |> Json.Encode.list (encode toString)
    |> toFilesystem

{-| -}
onResponse : (String -> tag) -> (Response tag -> msg) -> Sub msg
onResponse fromString toMsg =
  fromFilesystem (\json ->
    Json.Decode.decodeValue (decoder fromString) json
      |> Result.withDefault (Unknown json)
      |> toMsg
  )


{- Expect ------------------------------------------------------------------- -}
{-| -}
expectDir : tag -> Action tag
expectDir tag =
  Expect Dir tag

{-| -}
expectFile : tag -> Action tag
expectFile tag =
  Expect File tag

{-| -}
expectPath : tag -> Action tag
expectPath tag =
  Expect Path tag


{- Actions ------------------------------------------------------------------ -}
{-| -}
makeDir : String -> String -> Action tag
makeDir path dirname =
  MakeDir path dirname

{-| -}
readDir : String -> Action tag
readDir path =
  ReadDir path

{-| -}
readFile : String -> String -> Action tag
readFile path filename =
  ReadFile path filename

{-| -}
writeFile : String -> String -> String -> Action tag
writeFile path filename contents =
  WriteFile path filename contents


{- Encoders ----------------------------------------------------------------- -}
{-| -}
encode : (tag -> String) -> Action tag -> Json.Encode.Value
encode toString action =
  case action of
    Expect responseType tag ->
      Json.Encode.object
        [ ("$", Json.Encode.string "Expect")
        , ("type", encodeResponseType responseType)
        , ("tag", Json.Encode.string (toString tag))
        ]

    MakeDir path dirname ->
      Json.Encode.object
        [ ("$", Json.Encode.string "MakeDir")
        , ("path", Json.Encode.string path)
        , ("dirname", Json.Encode.string dirname)
        ]


    ReadDir path ->
      Json.Encode.object
        [ ("$", Json.Encode.string "ReadDir")
        , ("path", Json.Encode.string path)
        ]

    ReadFile path filename ->
      Json.Encode.object
        [ ("$", Json.Encode.string "ReadFile")
        , ("path", Json.Encode.string path)
        , ("filename", Json.Encode.string filename)
        ]

    WriteFile path filename contents ->
      Json.Encode.object
        [ ("$", Json.Encode.string "WriteFile")
        , ("path", Json.Encode.string path)
        , ("filename", Json.Encode.string filename)
        , ("contents", Json.Encode.string contents)
        ]

{-| -}
encodeResponseType : ResponseType -> Json.Encode.Value
encodeResponseType responseType =
  case responseType of
    Dir   -> Json.Encode.string "Dir"
    File  -> Json.Encode.string "File"
    Path  -> Json.Encode.string "Path"


{- Decoders ----------------------------------------------------------------- -}
{-| -}
decoder : (String -> tag) -> Decoder (Response tag)
decoder fromString =
  Json.Decode.field "$" Json.Decode.string |> Json.Decode.andThen (\t ->
    case t of
      "GotDir" ->
        Json.Decode.map2 GotDir
          (Json.Decode.field "tag" <| tagDecoder fromString)
          (Json.Decode.field "files" <| Json.Decode.list Json.Decode.string)

      "GotFile" ->
        Json.Decode.map2 GotFile
          (Json.Decode.field "tag" <| tagDecoder fromString)
          (Json.Decode.field "contents" Json.Decode.string)

      "GotPath" ->
        Json.Decode.map2 GotPath
          (Json.Decode.field "tag" <| tagDecoder fromString)
          (Json.Decode.field "path" Json.Decode.string)

      "Err" ->
        Json.Decode.map2 Error
          (Json.Decode.field "tag" <| tagDecoder fromString)
          (Json.Decode.field "message" Json.Decode.string)

      _ ->
        Json.Decode.fail ""
  )

{-| -}
tagDecoder : (String -> tag) -> Decoder tag
tagDecoder fromString =
  Json.Decode.map fromString Json.Decode.string 
