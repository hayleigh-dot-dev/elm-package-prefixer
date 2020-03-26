module Generate exposing (moduleFromDocs)

{- Imports ------------------------------------------------------------------ -}
import Elm.Docs
import Elm.Type


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
        , "import " ++ module_.name
        , ""
        ]

    docs = 
      "@docs " ++ String.join ", " (exposingFromBlocks <| Elm.Docs.toBlocks module_)

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
        , docs
        , body
        ]
  }

{-| -}
exposingFromBlocks : List Elm.Docs.Block -> List String
exposingFromBlocks blocks =
  blocks |> List.filterMap (\block ->
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
  blocks |> List.filterMap (\block ->
    case block of
      Elm.Docs.MarkdownBlock _ ->
        Nothing

      Elm.Docs.UnionBlock { name, comment, args } ->
        Just <| String.join "\n"
          [ "{-|" ++ comment ++ "-}"
          , "type alias " ++ name ++ " " ++ String.join " " args ++ " ="
          , "  " ++ moduleName ++ "." ++ name
          ]

      Elm.Docs.AliasBlock { name, comment, args } ->
        Just <| String.join "\n"
          [ "{-|" ++ comment ++ "-}"
          , "type alias " ++ name ++ " " ++ String.join " " args ++ " ="
          , "  " ++ moduleName ++ "." ++ name
          ]

      Elm.Docs.ValueBlock { name, comment, tipe } ->
        Just <| String.join "\n"
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

{-| -}
annotationFromType : Elm.Type.Type -> String
annotationFromType type_ =
  case type_ of
    Elm.Type.Var name ->
      name

    Elm.Type.Lambda a b ->
      annotationFromType a ++ " -> " ++ annotationFromType b

    Elm.Type.Tuple ts ->
      "( " ++ String.join ", " (List.map annotationFromType ts) ++ " )"

    Elm.Type.Type name ts ->
      name ++ " " ++ String.join " " (List.map annotationFromType ts)

    Elm.Type.Record fields _ ->
      "{ " ++ String.join ", " (List.map (\(name, t) -> name ++ " : " ++ annotationFromType t) fields) ++ " }"
