module SummaryList exposing (..)

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

init : String -> (Model, Cmd Msg)
init fileUrl =
  ( Model fileUrl errorSummaryData
  , getData fileUrl
  )

-- UPDATE

type Msg
    = RequestRefresh
    | Refresh SummaryData
    | FetchFail Http.Error


update : Msg -> Model -> (Model, Cmd Msg)
update action model =
    case action of
        RequestRefresh ->
            (model, getData model.file)

        Refresh summaryData ->
            ( Model model.file summaryData
            , Cmd.none
            )

        FetchFail _ ->
            (model, Cmd.none)

-- VIEW

view : Model -> Html Msg
view model =
    let
        numCols = 2
        attributes = [ class "summarylist-table" ]

        summaryData =
            model.summaryData.summaries
                |> List.map (\s -> viewSummary s)
                |> composeTiledHtml numCols
    in
        table attributes summaryData

viewSummary : Summary -> Html Msg
viewSummary summary =
    let
        contents =
            summary.contents
                |> List.map (\s -> li [] [ text s ])
    in
        th [ class "summarylist-item" ]
            [ h2 [ style [ ("text-align", "center") ] ] [ text summary.title ]
            , ul [ style [ ("text-align", "left") ] ] contents
            ]

-- EFFECTS

getData : String -> Cmd Msg
getData location =
    Http.get decodeData location
        |> Task.perform FetchFail Refresh
        -- |> Task.toMaybe
        -- |> Task.map Refresh
        -- |> Cmd.task

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
