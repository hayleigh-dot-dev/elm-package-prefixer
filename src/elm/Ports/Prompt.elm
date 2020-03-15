port module Ports.Prompt exposing
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
  , ask, askYesNo, notify
  , expectBool, expectString
  )

{- Imports ------------------------------------------------------------------ -}
import Json.Decode exposing (Decoder)
import Json.Encode


{- Ports -------------------------------------------------------------------- -}
port toPrompt : Json.Encode.Value -> Cmd action
port fromPrompt : (Json.Decode.Value -> action) -> Sub action


{- Types -------------------------------------------------------------------- -}
{-| -}
type Sequence tag
  = Sequence (tag -> String) (List (Action tag))

{-| -}
type Action tag
  = Expect ResponseType tag
  | Ask String
  | AskYesNo String
  | Notify String

{-| -}
type Response tag
  = GotBool tag Bool
  | GotString tag String
  | Unknown Json.Decode.Value
  | Error tag String

{-| -}
type ResponseType
  = Bool
  | String


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
    |> toPrompt

{-| -}
runAction : (tag -> String) -> Action tag -> Cmd msg
runAction toString action =
  List.singleton action
    |> Json.Encode.list (encode toString)
    |> toPrompt

{-| -}
onResponse : (String -> tag) -> (Response tag -> msg) -> Sub msg
onResponse fromString toMsg =
  fromPrompt (\json ->
    Json.Decode.decodeValue (decoder fromString) json
      |> Result.withDefault (Unknown json)
      |> toMsg
  )


{- Expect ------------------------------------------------------------------- -}
{-| -}
expectBool : tag -> Action tag
expectBool tag =
  Expect Bool tag

{-| -}
expectString : tag -> Action tag
expectString tag =
  Expect String tag


{- Actions ------------------------------------------------------------------ -}
{-| -}
ask : String -> Action tag
ask question =
  Ask question

{-| -}
askYesNo : String -> Action tag
askYesNo question =
  AskYesNo question

{-| -}
notify : String -> Action tag
notify notice =
  Notify notice


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

    Ask question ->
      Json.Encode.object
        [ ("$", Json.Encode.string "Ask")
        , ("question", Json.Encode.string question)
        ]

    AskYesNo question ->
      Json.Encode.object
        [ ("$", Json.Encode.string "AskYesNo")
        , ("question", Json.Encode.string question)
        ]

    Notify notice ->
      Json.Encode.object
        [ ("$", Json.Encode.string "Notify")
        , ("notice", Json.Encode.string notice)
        ]

{-| -}
encodeResponseType : ResponseType -> Json.Encode.Value
encodeResponseType responseType =
  case responseType of
    Bool    -> Json.Encode.string "Bool"
    String  -> Json.Encode.string "String"


{- Decoders ----------------------------------------------------------------- -}
{-| -}
decoder : (String -> tag) -> Decoder (Response tag)
decoder fromString =
  Json.Decode.field "$" Json.Decode.string |> Json.Decode.andThen (\t ->
    case t of
      "GotBool" ->
        Json.Decode.map2 GotBool
          (Json.Decode.field "tag" <| decodeTag fromString)
          (Json.Decode.field "response" decodeYesNo)

      "GotString" ->
        Json.Decode.map2 GotString
          (Json.Decode.field "tag" <| decodeTag fromString)
          (Json.Decode.field "response" Json.Decode.string)

      "Err" ->
        Json.Decode.map2 Error
          (Json.Decode.field "tag" <| decodeTag fromString)
          (Json.Decode.field "message" Json.Decode.string)

      _ ->
        Json.Decode.fail ""
  )

decodeYesNo : Decoder Bool
decodeYesNo =
  Json.Decode.string |> Json.Decode.andThen (\yesno ->
    case yesno of
      "Yes" -> Json.Decode.succeed True
      "yes" -> Json.Decode.succeed True
      "Ye"  -> Json.Decode.succeed True
      "ye"  -> Json.Decode.succeed True
      "Y"   -> Json.Decode.succeed True
      "y"   -> Json.Decode.succeed True

      "No"  -> Json.Decode.succeed False
      "no"  -> Json.Decode.succeed False
      "N"   -> Json.Decode.succeed False
      "n"   -> Json.Decode.succeed False

      _     -> Json.Decode.fail ""
  )

{-| -}
decodeTag : (String -> tag) -> Decoder tag
decodeTag fromString =
  Json.Decode.map fromString Json.Decode.string 
