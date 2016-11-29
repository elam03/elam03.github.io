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
    , contentsType : Maybe String
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
    , contentsType = Just "contents type"
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
            case summary.contentsType of
                Just t ->
                    if t |> String.contains "Sentences" then
                        let
                            paragraph =
                                summary.contents
                                    |> List.intersperse " "
                                    |> List.foldl (++) ""
                        in
                            [ text paragraph ]
                    else
                        summary.contents
                            |> List.map (\s -> li [] [ text s ])

                Nothing ->
                    summary.contents
                        |> List.map (\s -> li [] [ text s ])

        contentsType =
            case summary.contentsType of
                Just t ->
                    text t

                Nothing ->
                    text "No type specified!"

        contents =
            [ h2 [] [ text summary.title ]
            -- , h4 [] [ em [] [ contentsType ] ]
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
    Cmd.none
    -- Http.get decodeData location
    --   |> Task.perform Refresh
      -- |> Task.onError never
        -- |> Task.toMaybe
        -- |> Task.perform never Refresh

decodeData : Decoder SummaryData
decodeData =
        Json.Decode.map SummaryData
            ( field "summaries"
                ( list
                    <| map3 Summary
                        (field "title" string)
                        (maybe (field "type" string))
                        (field "contents" (list string))
                )
            )
