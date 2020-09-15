port module Main exposing (main)

{- Imports ------------------------------------------------------------------ -}

import Arguments exposing (Arguments)
import Elm.Docs
import Env
import Generate
import Http
import Json.Decode
import Ports.Filesystem as Filesystem
import Ports.Prompt as Prompt
import Task



{- Ports -------------------------------------------------------------------- -}


{-| A direct call to process.exit() in javascript land. We can pass in an Int as
the exit code. 0 for success, 1 for failure.
-}
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
        , update =
            \msg model ->
                Maybe.map (update msg) model
                    |> Maybe.andThen (Just << Tuple.mapFirst Just)
                    |> Maybe.withDefault ( Nothing, Cmd.none )
        , subscriptions =
            \model ->
                Maybe.map subscriptions model
                    |> Maybe.withDefault Sub.none
        }



{- Model -------------------------------------------------------------------- -}


{-| -}
type alias Model =
    { arguments : Arguments -- Arguments passed in on program start
    , createDirectory : Bool -- Should we create the directory if it doesn't exist?
    , overwriteFiles : Bool -- Should we continue if files already exist in that directory?
    , packages : List String -- All the packages that exist on package.elm-lang.org
    , modules : List ( String, String ) -- The generated modules, tuple of (path, module).
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

• initApp can terminate because of a _fail state_. That happens if the provided
cli arguments can't be parsed, or not all the required arguments were passed
in.

-}
init : Flags -> ( Maybe Model, Cmd Msg )
init ({ args } as flags) =
    if String.startsWith "--help" args then
        initHelp args

    else
        initApp flags


{-| -}
initHelp : String -> ( Maybe Model, Cmd Msg )
initHelp args =
    ( Nothing
    , Cmd.batch
        [ Arguments.parseHelp args
            |> Arguments.showHelp
            |> Prompt.notify
            |> Prompt.runAction never
        , exit 0
        ]
    )


{-| Even though we're initialising the "real" app at this point, now we have a
situation where we need to terminate the program because of a fail state. This
happens if we fail to parse the cli args properly.

This is the last time we have to deal with this clunky `Maybe Model` situation,
so hooray!

-}
initApp : Flags -> ( Maybe Model, Cmd Msg )
initApp { args, packages } =
    case Arguments.parse args of
        Ok arguments ->
            ( Just
                { arguments = arguments
                , createDirectory = False
                , overwriteFiles = False
                , packages = []
                , modules = []
                }
            , if Env.node_env == "development" then
                -- This process is used to essentially simulate a HTTP request during
                -- development. We don't want to hit the package servers every time we
                -- run the program!
                Json.Decode.decodeValue packageNamesDecoder packages
                    |> Result.mapError (\_ -> Http.NetworkError)
                    |> Task.succeed
                    |> Task.perform GotPackages

              else
                fetchPackageNames
            )

        Err _ ->
            ( Nothing
            , Cmd.batch
                [ notify <|
                    "Error parsing arguments, try running the program again with the "
                        ++ "--help flag to see what I can do."
                , exit 1
                ]
            )


{-| We only care about the package names (for now). We use them to match against
the package name passed in as an argument; there's no sense trying to prefix a
package that doesn't exist.
-}
packageNamesDecoder : Json.Decode.Decoder (List String)
packageNamesDecoder =
    Json.Decode.list <| Json.Decode.field "name" Json.Decode.string


{-| -}
fetchPackageNames : Cmd Msg
fetchPackageNames =
    Http.get
        { url = "https://package.elm-lang.org/search.json"
        , expect = Http.expectJson GotPackages packageNamesDecoder
        }



{- Update ------------------------------------------------------------------- -}


{-| -}
type Msg
    = GotPackages (Result Http.Error (List String))
    | GotPackageDocs (Result Http.Error (List Elm.Docs.Module))
      -- Ports responses
    | GotFilesystemResponse (Filesystem.Response FilesystemTag)
    | GotPromptResponse (Prompt.Response PromptTag)


{-| -}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotPackages (Ok packages) ->
            -- TODO: Should we save the result to a file to be used as cache in the
            -- future?
            ( { model | packages = packages }
            , if List.member model.arguments.name packages then
                checkOutDir model.arguments.dir

              else
                Cmd.batch
                    [ notify <|
                        "Package could not be found on the elm package website, did "
                            ++ "you spell it correctly?"
                    , exit 1
                    ]
            )

        GotPackages (Err _) ->
            ( model
            , Cmd.batch
                [ notify "Error fetching packages from the elm package website."
                , exit 1
                ]
            )

        GotPackageDocs (Ok docs) ->
            let
                modules =
                    List.map (Generate.moduleFromDocs model.arguments.dir model.arguments.prefix) docs
            in
            ( model
            , Cmd.batch
                (modules
                    |> List.map
                        (\{ path, name, body } ->
                            Filesystem.sequence never
                                |> Filesystem.andThen (Filesystem.makeDir path)
                                |> Filesystem.andThen (Filesystem.writeFile path (name ++ ".elm") body)
                                |> Filesystem.run
                        )
                )
            )

        GotPackageDocs (Err _) ->
            ( model
            , Cmd.batch
                [ notify "Error fatching package docs, did you enter the tag correctly?"
                , exit 1
                ]
            )

        GotPromptResponse response ->
            gotPromptResponse response model

        GotFilesystemResponse response ->
            gotFilesystemResponse response model


{-| -}
fetchDocs : String -> String -> Cmd Msg
fetchDocs package tag =
    let
        url =
            "https://package.elm-lang.org/packages/" ++ package ++ "/" ++ tag ++ "/docs.json"
    in
    Http.request
        { method = "GET"
        , headers =
            [ Http.header "accept-encoding" "gzip"
            ]
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectJson GotPackageDocs (Json.Decode.list Elm.Docs.decoder)
        , timeout = Nothing
        , tracker = Nothing
        }



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


{-| -}
checkOutDir : String -> Cmd msg
checkOutDir dir =
    Filesystem.sequence filesystemTagToString
        |> Filesystem.andThen (Filesystem.readDir dir)
        |> Filesystem.andThen (Filesystem.expectDir OutDir)
        |> Filesystem.run


{-| -}
createOutDir : String -> Cmd msg
createOutDir path =
    Filesystem.sequence filesystemTagToString
        |> Filesystem.andThen (Filesystem.makeDir path)
        |> Filesystem.andThen (Filesystem.readDir path)
        |> Filesystem.andThen (Filesystem.expectDir OutDir)
        |> Filesystem.run


{-| -}
gotFilesystemResponse : Filesystem.Response FilesystemTag -> Model -> ( Model, Cmd Msg )
gotFilesystemResponse response model =
    case response of
        -- The directory is empty, we can go ahead and safely get to work!
        Filesystem.GotDir OutDir [] ->
            ( model
            , fetchDocs model.arguments.name model.arguments.tag
            )

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


{-| -}
filesystemTagToString : FilesystemTag -> String
filesystemTagToString tag =
    case tag of
        OutDir ->
            "OutDir"

        UnknownFilesystemTag ->
            ""


{-| -}
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
    | NewPath
    | UnknownPromptTag


{-| -}
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
askForNewDirectory : Cmd msg
askForNewDirectory =
    Prompt.sequence promptTagToString
        |> Prompt.andThen (Prompt.ask "Enter a new path to place the generated files.")
        |> Prompt.andThen (Prompt.expectString NewPath)
        |> Prompt.run


{-| This is essentially an alternative to Debug.log that we use to print some
message to the user without expecting a response.
-}
notify : String -> Cmd msg
notify notice =
    Prompt.notify notice
        |> Prompt.runAction never


{-| -}
gotPromptResponse : Prompt.Response PromptTag -> Model -> ( Model, Cmd Msg )
gotPromptResponse response model =
    case response of
        -- The user has said that it's OK to create the directory when it doesn't
        -- exist. If the user supplied an outDir that was a path with multiple
        -- directories that don't exist, we'll create them all.
        Prompt.GotBool CreateDirectory True ->
            ( { model | createDirectory = True }
            , Cmd.batch
                [ fetchDocs model.arguments.name model.arguments.tag
                , createOutDir model.arguments.dir
                ]
            )

        -- The user has said it's *not* OK to create the output directory if
        -- necessary. This means we need to prompt rhem for a new outDir.
        Prompt.GotBool CreateDirectory False ->
            ( model
            , askForNewDirectory
            )

        Prompt.GotBool OverwriteFiles True ->
            ( { model | overwriteFiles = True }
            , fetchDocs model.arguments.name model.arguments.tag
            )

        Prompt.GotBool OverwriteFiles False ->
            ( model
            , askForNewDirectory
            )

        Prompt.GotString NewPath dir ->
            let
                args =
                    model.arguments

                arguments =
                    { args | dir = dir }
            in
            ( { model | arguments = arguments }
            , createOutDir dir
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

        NewPath ->
            "NewPath"

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

        "NewPath" ->
            NewPath

        _ ->
            UnknownPromptTag
