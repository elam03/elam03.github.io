module Utils where

import Html exposing (..)
import Html.Attributes exposing (class)

composeTiledHtml : String -> (a -> List Html) -> Int -> List a -> List Html
composeTiledHtml classname transform cols list =
    let
        head = List.take cols list
        rest = List.drop cols list

        head' =
            head
                |> List.map transform
                |> List.map (\a -> th [class (classname ++ "-item")] a)
    in
        if List.isEmpty head then
            []
        else
            [ (tr [] []) ] ++ head' ++ (composeTiledHtml classname transform cols rest)
