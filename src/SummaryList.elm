module SummaryList exposing (..)

import Basics.Extra exposing (..)
import Html exposing (..)
import Html.Attributes
import Http
import Json.Decode exposing (..)
import Platform.Cmd
import String exposing (contains)
import Task

import Utils exposing (..)

(=>) : a -> b -> (a, b)
(=>) = (,)

-- MODEL

type alias Summary =
    { title : String
    , contents : List String
    }

type alias SummaryData =
    { summaries : List Summary
    }

type alias Model =
    { file : String
    , summaryData : SummaryData
    , debug : String
    }

errorSummaryData : SummaryData
errorSummaryData =
    { summaries = [errorSummary] }


errorSummary : Summary
errorSummary =
    { title = "error"
    , contents = ["has", "occurred"]
    }

init : String -> (Model, Cmd Msg)
init fileUrl =
  ( Model fileUrl errorSummaryData "debug"
  , getData fileUrl
  )

-- UPDATE

type Msg
    = RequestRefresh
    | Refresh (Maybe SummaryData)


update : Msg -> Model -> (Model, Cmd Msg)
update action model =
    case action of
        RequestRefresh ->
            (model, getData model.file)

        Refresh summaryData ->
            let
                data = summaryData |> Maybe.withDefault errorSummaryData
            in
                ( Model model.file data "Refresh Achieved!"
                , Cmd.none
                )

-- VIEW

viewSummary : Summary -> Html Msg
viewSummary summary =
    let
        items =
            summary.contents
                |> List.map (\s -> li [] [ text s ])

        contents =
            [ h2 [] [ text summary.title ]
            , ul [] items
            ]

        attributes =
            Html.Attributes.classList
                [ ("summarylist", True)
                , ("summarylist-item", True)
                ]
    in
        div [ attributes ] contents

view : Model -> Html Msg
view model =
    let
        items =
            model.summaryData.summaries
                |> List.map (\s -> viewSummary s)

        attributes =
            Html.Attributes.classList
                [ ("summarylist", True)
                , ("summarylist-container", True)
                ]
    in
        div [ attributes ] items

viewFilter : List String -> Model -> Html Msg
viewFilter titleFilters model =
    let
        attributes =
            Html.Attributes.classList
                [ ("summarylist", True)
                , ("summarylist-container", True)
                ]

        getContent titleFilter =
            model.summaryData.summaries
                |> List.filter (\summary -> contains titleFilter summary.title)
                |> List.map (\s -> viewSummary s)

        items =
            titleFilters
                |> List.map (\titleFilter -> getContent titleFilter)
                |> List.concat
    in
        div [ attributes ] items

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

-- CMDS

getData : String -> Cmd Msg
getData location =
    Http.get decodeData location
        |> Task.toMaybe
        |> Task.perform never Refresh

decodeData : Decoder SummaryData
decodeData =
        object1 SummaryData
            ( "summaries" :=
                ( list
                    <| object2 Summary
                        ("title" := string)
                        ("contents" := (list string))
                )
            )
