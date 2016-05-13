module Utils where

import Html exposing (..)
import Random exposing (..)
import Signal exposing (..)
import Time exposing (..)

-- TODO: Use this later...
randomFloats : Signal (Generator (List Float))
randomFloats =
    Random.list 10 (Random.float 0 1)
        |> Signal.constant
        |> Signal.sampleOn (Time.every Time.second)

composeTiledHtml : Int -> List Html.Html -> List Html.Html
composeTiledHtml cols list =
    let
        head = List.take cols list
        rest = List.drop cols list

        head' = head

    in
        if List.isEmpty head then
            []
        else
            [ (tr [] []) ] ++ head' ++ (composeTiledHtml cols rest)
