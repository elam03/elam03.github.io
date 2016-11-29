module Utils exposing (..)

import Html exposing (..)
-- import Random exposing (..)
-- import Time exposing (..)

-- TODO: Use this later...
-- randomFloats : Signal (Generator (List Float))
-- randomFloats =
--     Random.list 10 (Random.float 0 1)
--         |> Signal.constant
--         |> Signal.sampleOn (Time.every Time.second)

composeTiledHtml : Int -> List (Html a) -> List (Html a)
composeTiledHtml cols list =
    let
        head = List.take cols list
        rest = List.drop cols list

        head_ = head

    in
        if List.isEmpty head then
            []
        else
            [ (tr [] []) ] ++ head_ ++ (composeTiledHtml cols rest)
