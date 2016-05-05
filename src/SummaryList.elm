module SummaryList where

import Effects exposing (Effects, Never)
import Html exposing (..)
import Html.Attributes exposing (class, href, src, style)
import Http
import Json.Decode exposing (..)
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
    }

errorSummaryData : SummaryData
errorSummaryData =
    { summaries = [errorSummary]
    }
    -- { title = "error"
    -- , contents = ["has", "occurred"]
    -- }

errorSummary : Summary
errorSummary =
    { title = "error"
    , contents = ["has", "occurred"]
    }

init : String -> (Model, Effects Action)
init fileUrl =
  ( Model fileUrl errorSummaryData
  , getData fileUrl
  )

-- UPDATE

type Action
    = RequestRefresh
    | Refresh (Maybe (SummaryData))


update : Action -> Model -> (Model, Effects Action)
update action model =
    case action of
        RequestRefresh ->
            ( model
            , getData model.file
            )

        Refresh maybeSummaryData ->
            let
                summaryData =
                    maybeSummaryData
                        |> Maybe.withDefault errorSummaryData
            in
                ( Model model.file summaryData
                , Effects.none
                )

-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
    let
        summaryData =
            model.summaryData.summaries
                |> viewSummaries
    in
        div []
            [ summaryData ]

viewSummaries : List Summary -> Html
viewSummaries summaryData =
    let
        numCols = 2
        classname = "summarylist"

        summaryData' =
            summaryData
                |> composeTiledHtml classname viewSummary numCols
    in
        table [ class classname ]
            summaryData'

viewSummary : Summary -> List Html
viewSummary summary =
    let
        contents =
            summary.contents
                |> List.map (\s -> li [] [ text s ])
        classname = "summarylist-item"
    in
        [ h2 [ style [ ("text-align", "center") ] ] [ text summary.title ]
        , ul [ style [ ("text-align", "left") ] ] contents
        ]

-- EFFECTS

getData : String -> Effects Action
getData location =
    Http.get decodeData location
        |> Task.toMaybe
        |> Task.map Refresh
        |> Effects.task

decodeData : Decoder (SummaryData)
decodeData =
    object1 SummaryData
        ( "summaries" :=
            ( list
                <| object2 Summary
                    ("title" := string)
                    ("contents" := (list string))
            )
        )

-- decodeData : Decoder (List Summary)
-- decodeData =
--     list
--         <| object2 Summary
--             ("title" := string)
--             ("contents" := (list string))
