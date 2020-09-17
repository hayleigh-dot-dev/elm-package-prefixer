module Generate exposing (moduleFromDocs)

{- Imports ------------------------------------------------------------------ -}

import Elm.Docs
import Elm.Type
import Set exposing (Set)



{- Types -------------------------------------------------------------------- -}


{-| -}
type alias Module =
    { path : String
    , name : String
    , body : String
    }



{- Functions ---------------------------------------------------------------- -}


{-| -}
moduleFromDocs : String -> String -> Elm.Docs.Module -> Module
moduleFromDocs dir prefix module_ =
    let
        fullPath =
            String.split "." (prefix ++ "." ++ module_.name)

        path =
            List.reverse fullPath
                |> List.drop 1
                |> List.reverse
                |> String.join "/"

        name =
            List.drop (List.length fullPath - 1) fullPath
                |> String.join "/"

        header =
            String.join "\n"
                [ "module " ++ prefix ++ "." ++ module_.name ++ " exposing (" ++ String.join ", " exposing_ ++ ")"
                , ""
                , "{-|" ++ module_.comment ++ "-}"
                , ""
                , importLines module_
                , ""
                ]

        exposing_ =
            exposingFromBlocks (Elm.Docs.toBlocks module_)

        body =
            String.join "\n\n\n" (bodyFromBlocks module_.name <| Elm.Docs.toBlocks module_)
    in
    { path = dir ++ "/" ++ path
    , name = name
    , body =
        String.join "\n\n"
            [ header
            , body
            ]
    }


{-| -}
importLines : Elm.Docs.Module -> String
importLines module_ =
    let
        moduleNames =
            Set.singleton module_.name
                |> getModuleNamesFromUnions module_.unions
                |> getModuleNamesFromAliases module_.aliases
                |> getModuleNamesFromValues module_.values
    in
    Set.toList moduleNames
        |> List.map (\name -> "import " ++ name)
        |> String.join "\n"


{-| -}
getModuleNamesFromUnions : List Elm.Docs.Union -> Set String -> Set String
getModuleNamesFromUnions unions accumulated =
    case unions of
        first :: rest ->
            accumulated
                |> getModuleNamesFromTags first.tags
                |> getModuleNamesFromUnions rest

        [] ->
            accumulated


{-| -}
getModuleNamesFromTags : List ( String, List Elm.Type.Type ) -> Set String -> Set String
getModuleNamesFromTags tags accumulated =
    case tags of
        ( _, types ) :: rest ->
            accumulated
                |> getModuleNamesFromTypes types
                |> getModuleNamesFromTags rest

        [] ->
            accumulated


{-| -}
getModuleNamesFromAliases : List Elm.Docs.Alias -> Set String -> Set String
getModuleNamesFromAliases aliases accumulated =
    case aliases of
        first :: rest ->
            accumulated
                |> getModuleNamesFromType first.tipe
                |> getModuleNamesFromAliases rest

        [] ->
            accumulated


{-| -}
getModuleNamesFromValues : List Elm.Docs.Value -> Set String -> Set String
getModuleNamesFromValues values accumulated =
    case values of
        first :: rest ->
            accumulated
                |> getModuleNamesFromType first.tipe
                |> getModuleNamesFromValues rest

        [] ->
            accumulated


{-| -}
getModuleNamesFromTypes : List Elm.Type.Type -> Set String -> Set String
getModuleNamesFromTypes types accumulated =
    List.foldl getModuleNamesFromType accumulated types


getModuleNamesFromType : Elm.Type.Type -> Set String -> Set String
getModuleNamesFromType type_ accumulated =
    case type_ of
        Elm.Type.Var _ ->
            accumulated

        Elm.Type.Lambda argument result ->
            accumulated
                |> getModuleNamesFromType argument
                |> getModuleNamesFromType result

        Elm.Type.Tuple nestedTypes ->
            accumulated
                |> getModuleNamesFromTypes nestedTypes

        Elm.Type.Type typeName arguments ->
            accumulated
                |> addModuleNameFromTypeName typeName
                |> getModuleNamesFromTypes arguments

        Elm.Type.Record fields _ ->
            accumulated
                |> getModuleNamesFromTypes (List.map Tuple.second fields)


{-| -}
addModuleNameFromTypeName : String -> Set String -> Set String
addModuleNameFromTypeName typeName accumulated =
    let
        -- inefficient hackery to get all but the last component of a fully qualified type name
        moduleName =
            typeName
                |> String.split "."
                |> List.reverse
                |> List.drop 1
                |> List.reverse
                |> String.join "."
    in
    if String.isEmpty moduleName || List.member moduleName defaultImportedModuleNames then
        accumulated

    else
        Set.insert moduleName accumulated


{-| -}
defaultImportedModuleNames : List String
defaultImportedModuleNames =
    [ "Basics", "List", "Maybe", "Result", "String", "Char" ]


{-| -}
exposingFromBlocks : List Elm.Docs.Block -> List String
exposingFromBlocks blocks =
    blocks
        |> List.filterMap
            (\block ->
                case block of
                    Elm.Docs.MarkdownBlock _ ->
                        Nothing

                    Elm.Docs.UnionBlock { name } ->
                        Just name

                    Elm.Docs.AliasBlock { name } ->
                        Just name

                    Elm.Docs.ValueBlock { name } ->
                        Just name

                    Elm.Docs.BinopBlock _ ->
                        Nothing

                    Elm.Docs.UnknownBlock _ ->
                        Nothing
            )


{-| -}
bodyFromBlocks : String -> List Elm.Docs.Block -> List String
bodyFromBlocks moduleName blocks =
    blocks
        |> List.filterMap
            (\block ->
                case block of
                    Elm.Docs.MarkdownBlock _ ->
                        Nothing

                    Elm.Docs.UnionBlock { name, comment, args } ->
                        Just <|
                            String.join "\n"
                                [ "{-|" ++ comment ++ "-}"
                                , "type alias " ++ String.join " " (name :: args) ++ " ="
                                , "  " ++ moduleName ++ "." ++ String.join " " (name :: args)
                                ]

                    Elm.Docs.AliasBlock { name, comment, args } ->
                        Just <|
                            String.join "\n"
                                [ "{-|" ++ comment ++ "-}"
                                , "type alias " ++ String.join " " (name :: args) ++ " ="
                                , "  " ++ moduleName ++ "." ++ String.join " " (name :: args)
                                ]

                    Elm.Docs.ValueBlock { name, comment, tipe } ->
                        Just <|
                            String.join "\n"
                                [ "{-|" ++ comment ++ "-}"
                                , name ++ " : " ++ annotationFromType tipe
                                , name ++ " ="
                                , "  " ++ moduleName ++ "." ++ name
                                ]

                    Elm.Docs.BinopBlock _ ->
                        Nothing

                    Elm.Docs.UnknownBlock _ ->
                        Nothing
            )


{-| From default imports listing at <https://package.elm-lang.org/packages/elm/core/latest/>
-}
fixTypeName : String -> String
fixTypeName name =
    if String.startsWith "Basics." name then
        String.dropLeft 7 name

    else if name == "List.List" then
        "List"

    else if name == "Maybe.Maybe" then
        "Maybe"

    else if name == "Result.Result" then
        "Result"

    else if name == "String.String" then
        "String"

    else if name == "Char.Char" then
        "Char"

    else
        name


fixTypeParameter : String -> String
fixTypeParameter name =
    if String.contains " " name && not (isParenthesized name) then
        "(" ++ name ++ ")"

    else
        name


isParenthesized : String -> Bool
isParenthesized name =
    (String.startsWith "(" name && String.endsWith ")" name)
        || (String.startsWith "{" name && String.endsWith "}" name)


{-| -}
annotationFromType : Elm.Type.Type -> String
annotationFromType type_ =
    case type_ of
        Elm.Type.Var name ->
            name

        Elm.Type.Lambda ((Elm.Type.Lambda _ _) as a) b ->
            "(" ++ annotationFromType a ++ ") -> " ++ annotationFromType b

        Elm.Type.Lambda a b ->
            annotationFromType a ++ " -> " ++ annotationFromType b

        Elm.Type.Tuple ts ->
            "( " ++ String.join ", " (List.map annotationFromType ts) ++ " )"

        Elm.Type.Type name ts ->
            String.join " " (fixTypeName name :: List.map (annotationFromType >> fixTypeParameter) ts)

        Elm.Type.Record fields _ ->
            "{ " ++ String.join ", " (List.map (\( name, t ) -> name ++ " : " ++ annotationFromType t) fields) ++ " }"
