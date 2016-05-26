module SummaryList exposing (..)

import Basics.Extra exposing (..)
import Html exposing (..)
import Html.Attributes exposing (class, href, src, style)
import Http
import Json.Decode exposing (..)
import Platform.Cmd
import Task

import Utils exposing (..)

(=>) : a -> b -> (a, b)
(=>) = (,)

-- MODEL

type alias SummaryData =
    { summaries : List Summary
    }

type alias Summary =
    { title : String
    , contents : List String
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

view : Model -> Html Msg
view model =
    let
        numCols = 2
        attributes = [ classStyle ]

        summaryData =
            model.summaryData.summaries
                |> List.map (\s -> viewSummary s)
                |> composeTiledHtml numCols

        debugContents =
            div []
                [ text "debug: "
                , text model.debug
                ]

        tableContents =
            table attributes summaryData

    in
        div []
            [ tableContents
            -- , debugContents
            ]


viewSummary : Summary -> Html Msg
viewSummary summary =
    let
        contents =
            summary.contents
                |> List.map (\s -> li [] [ text s ])
    in
        th [ classStyle ]
            [ h2 [ style [ ("text-align", "center") ] ] [ text summary.title ]
            , ul [ style [ ("text-align", "left") ] ] contents
            ]

classStyle : Html.Attribute Msg
classStyle = class "summarylist"

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
