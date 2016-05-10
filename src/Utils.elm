module Utils where

import Html exposing (..)
-- import Html.Attributes exposing (class)

composeTiledHtml : List Html.Attribute -> (a -> List Html) -> Int -> List a -> List Html
composeTiledHtml attributes transform cols list =
    let
        head = List.take cols list
        rest = List.drop cols list

        head' =
            head
                |> List.map transform
                -- |> List.map (\a -> th [class (classname ++ "-item")] a)
                |> List.map (\a -> th attributes a)

    in
        if List.isEmpty head then
            []
        else
            [ (tr [] []) ] ++ head' ++ (composeTiledHtml attributes transform cols rest)
