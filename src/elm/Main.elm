port module Main exposing (main)

{- Imports ------------------------------------------------------------------ -}
import Arguments exposing (Arguments)
import Json.Decode


{- Ports -------------------------------------------------------------------- -}
{-| -}
port log : String -> Cmd msg


{- Main --------------------------------------------------------------------- -}
{-| Okay this probably looks kind of weird, why do we have a `Maybe Model`? Well,
we have a legitimate scenario where we want the app to immediately terminate
if we get the --help command line argument. There's no need for TEA or for any
model in that scenario, we just want to return the relevant help message back to
javascript and close the app.

Having a `Maybe Model` means we don't have to worry about thinking up dummy values
for a real model, or changing our model to encode some valid states that don't
live long enough to make it to update.

The plumbing below looks kind of weird, but it's necessary so we can keep the
"normal" type signatures for our update and subscriptions functions.
-}
main : Program Flags (Maybe Model) Msg
main =
  Platform.worker
    { init = init
    , update = \msg model -> 
        Maybe.map (update msg) model
          |> Maybe.andThen (Just << Tuple.mapFirst Just)
          |> Maybe.withDefault (Nothing, Cmd.none)
    , subscriptions = \model ->
        Maybe.map subscriptions model
          |> Maybe.withDefault Sub.none
    }


{- Model -------------------------------------------------------------------- -}
{-| -}
type alias Model =
  { arguments : Arguments   -- Arguments passed in on program start
  , createDirectory : Bool  -- Should we create the directory if it doesn't exist?
  , overwriteFiles : Bool   -- Should we continue if files already exist in that directory?
  , packages : List String  -- All the packages that exist on package.elm-lang.org
  }

{-| -}
type alias Flags =
  { args : String
  , packages : Json.Decode.Value
  }

{-| This is that `Maybe Model` quirk again, the init functions are the only place
we need to deal with it so bear with me. Both `initHelp` and `initApp` return that
`Maybe Model` but their reason for doing so is different.

  • initHelp is a "legitimate" termination of the program: the user passed in the
    --help flag so show the help message and just exit.

  • initApp can terminate because of a *fail state*. That happens if the provided
    cli arguments can't be parsed, or not all the required arguments were passed
    in.
-}
init : Flags -> (Maybe Model, Cmd Msg)
init ({ args } as flags) =
  if String.startsWith "--help" args then
    initHelp args

  else 
    initApp flags

{-| -}
initHelp : String -> (Maybe Model, Cmd Msg)
initHelp args =
  ( Nothing
  , Arguments.parseHelp args
      |> Arguments.showHelp
      |> log
  )

{-| Even though we're initialising the "real" app at this point, now we have a
situation where we need to terminate the program because of a fail state. This
happens if we fail to parse the cli args properly.

This is the last time we have to deal with this clunky `Maybe Model` situation,
so hooray!
-}
initApp : Flags -> (Maybe Model, Cmd Msg)
initApp { args, packages } =
  case Arguments.parse args of
    Ok arguments ->
      ( Just
          { arguments = arguments
          , createDirectory = False
          , overwriteFiles = False
          , packages = decodePackageNames packages
          }
      , Cmd.none
      )

    Err _ ->
      ( Nothing
      , Cmd.none
      )

{-| -}
decodePackageNames : Json.Decode.Value -> List String
decodePackageNames packages =
  packages 
    |> Json.Decode.decodeValue (Json.Decode.list <| Json.Decode.field "name" Json.Decode.string)
    |> Result.withDefault []

{- Update ------------------------------------------------------------------- -}
{-| -}
type Msg
  = None

{-| -}
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    None ->
      ( model
      , Cmd.none
      )


{- Subscriptions ------------------------------------------------------------ -}
{-| -}
subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.batch
    [
    ]
