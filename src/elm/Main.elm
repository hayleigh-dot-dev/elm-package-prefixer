port module Main exposing (main)

{- Imports ------------------------------------------------------------------ -}
import Arguments exposing (Arguments)
import Json.Decode

import Ports.Filesystem as Filesystem
import Ports.Prompt as Prompt


{- Ports -------------------------------------------------------------------- -}
{-| -}
port exit : Int -> Cmd msg


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
      |> Prompt.notify
      |> Prompt.runAction never
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
      , checkOutDir arguments.dir
      )

    Err _ ->
      ( Nothing
      , Cmd.none
      )

{-| -}
decodePackageNames : Json.Decode.Value -> List String
decodePackageNames packages =
  packages
    |> Json.Decode.decodeValue
      (Json.Decode.list <| Json.Decode.field "name" Json.Decode.string)
    |> Result.withDefault []


{- Update ------------------------------------------------------------------- -}
{-| -}
type Msg
  = GotFilesystemResponse (Filesystem.Response FilesystemTag)
  | GotPromptResponse (Prompt.Response PromptTag)

{-| -}
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotPromptResponse response ->
      gotPromptResponse response model

    GotFilesystemResponse response ->
      gotFilesystemResponse response model


{- Subscriptions ------------------------------------------------------------ -}
{-| -}
subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.batch
    [ Prompt.onResponse promptTagFromString GotPromptResponse
    , Filesystem.onResponse filesystemTagFromString GotFilesystemResponse
    ]


{- Ports: Filesystem -------------------------------------------------------- -}
{-| The system we have in place for using ports allows us to attach tags to port
calls we expect to return something. We can use whatever we want for the tag
as long as we supply toString and fromString functions.

  • OutDir
    We use this tag to get a response from read the contents of the output
    directory.

  • UnknownFilesystemTag
    For now, the fromString function doesn't return a `Maybe`, so we need to
    have a variant we can return if we get an unknown (or empty) string as the
    tag from javascript.
-}
type FilesystemTag
  = OutDir
  | UnknownFilesystemTag

checkOutDir : String -> Cmd msg
checkOutDir outDir =
  Filesystem.sequence filesystemTagToString
    |> Filesystem.andThen (Filesystem.readDir outDir)
    |> Filesystem.andThen (Filesystem.expectDir OutDir)
    |> Filesystem.run

{-| -}
gotFilesystemResponse : Filesystem.Response FilesystemTag -> Model -> (Model, Cmd Msg)
gotFilesystemResponse response model =
  case response of
    -- If there are files in the directory we should prompt the user to confirm
    -- if they're happy to proceed. We don't actually care what the files are
    -- (maybe we will in the future).
    Filesystem.GotDir OutDir (_ :: _) ->
      ( model
      , askToOverwrite
      )

    -- If the directory doesn't exist we should prompt the user to ask if they
    -- want us to create it for them.
    Filesystem.Error OutDir "ENOENT" ->
      ( model
      , askToCreateDirectory
      )

    _ ->
      ( model
      , Cmd.none
      )

filesystemTagToString : FilesystemTag -> String
filesystemTagToString tag =
  case tag of
    OutDir ->
      "OutDir"

    UnknownFilesystemTag ->
      ""

filesystemTagFromString : String -> FilesystemTag
filesystemTagFromString tag =
  case tag of
    "OutDir" ->
      OutDir

    _ ->
      UnknownFilesystemTag


{- Ports: Prompt ------------------------------------------------------------ -}
{-| -}
type PromptTag
  = CreateDirectory
  | OverwriteFiles
  | UnknownPromptTag

askToCreateDirectory : Cmd msg
askToCreateDirectory =
  Prompt.sequence promptTagToString
    |> Prompt.andThen (Prompt.askYesNo "Looks like the directory doesn't exist, should we create it?")
    |> Prompt.andThen (Prompt.expectBool CreateDirectory)
    |> Prompt.run

{-| -}
askToOverwrite : Cmd msg
askToOverwrite =
  Prompt.sequence promptTagToString
    |> Prompt.andThen (Prompt.askYesNo "Looks like the directory isn't empty, should we conitnue anyway?")
    |> Prompt.andThen (Prompt.expectBool OverwriteFiles)
    |> Prompt.run

{-| -}
gotPromptResponse : Prompt.Response PromptTag -> Model -> (Model, Cmd Msg)
gotPromptResponse response model =
  case response of
    -- The user has said that it's OK to create the directory when it doesn't
    -- exist. If the user supplied an outDir that was a path with multiple
    -- directories that don't exist, we'll create them all.
    Prompt.GotBool CreateDirectory True ->
      ( { model
        | createDirectory = True
        }
      , Cmd.none
      )

    -- The user has said it's *not* OK to create the output directory if
    -- necessary. This means we need to prompt rhem for a new outDir.
    Prompt.GotBool CreateDirectory False ->
      ( model
      , Debug.todo "Prompt user for a new output directory."
      )

    -- 
    Prompt.GotBool OverwriteFiles True ->
      ( { model
        | overwriteFiles = True
        }
      , Cmd.none
      )

    Prompt.GotBool OverwriteFiles False ->
      ( model
      , Debug.todo "Prompt user for a new output directory."
      )

    _ ->
      ( model
      , Cmd.none
      )

{-| -}
promptTagToString : PromptTag -> String
promptTagToString tag =
  case tag of
    CreateDirectory ->
      "CreateDirectory"

    OverwriteFiles ->
      "OverwriteFiles"

    UnknownPromptTag ->
      ""

{-| -}
promptTagFromString : String -> PromptTag
promptTagFromString tag =
  case tag of
    "CreateDirectory" ->
      CreateDirectory

    "OverwriteFiles" ->
      OverwriteFiles

    _ ->
      UnknownPromptTag
