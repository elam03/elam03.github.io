module SummaryList where

import Effects exposing (Effects, Never)
import Html exposing (..)
import Html.Attributes exposing (class, href, src, style)
import Http
import Json.Decode exposing (..)
import Task

(=>) : a -> b -> (a, b)
(=>) = (,)

-- MODEL

type alias Summary =
    { title : String
    , content : List String
    }

type alias Model =
    { file : String
    , summaryData : List Summary
    }

errorSummary : Summary
errorSummary =
    { title = "error"
    , content = ["has", "occurred"]
    }

init : String -> (Model, Effects Action)
init fileUrl =
  ( Model fileUrl []
  , getData fileUrl
  )

-- UPDATE

type Action
    = RequestRefresh
    | Refresh (Maybe (List Summary))


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
                        |> Maybe.withDefault [errorSummary]
            in
                if List.isEmpty summaryData then
                    ( Model model.file summaryData
                    , Effects.none
                    )
                else
                    ( Model model.file summaryData
                    , Effects.none
                    )

-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
    let
        summaryData =
            model.summaryData
                |> viewSummaries
        header = [ text ("fileUrl: " ++ model.file) ]
    in
        div []
            ( header
              ++ [ summaryData ]
            )

viewSummaries : List Summary -> Html
viewSummaries summaryData =
    let
        summaryData' =
            summaryData
                |> List.map viewSummary
    in
        div []
            summaryData'

viewSummary : Summary -> Html
viewSummary summary =
    let
        content =
            summary.content
                |> List.map (\s -> text (s ++ " ! "))
        html =
            [ text summary.title
            , br [] []
            ] ++ content
    in
        div [ class "summary" ]
            html
    -- let
    --     images =
    --         project.previews
    --             |> List.map (\path -> img [ src path, style [ ("width", "32px") ] ] [])
    --
    --     content =
    --         [ text project.title
    --         , br [] []
    --         , text project.description
    --         , br [] []
    --         , a [ href project.content ] [ text "download!" ]
    --         ] ++ images
    -- in
    --     th [ class "summary" ]
    --         content

-- EFFECTS

getData : String -> Effects Action
getData location =
    Http.get decodeData location
        |> Task.toMaybe
        |> Task.map Refresh
        |> Effects.task

decodeData : Decoder (List Summary)
decodeData =
    list
        <| object2 Summary
            ("title" := string)
            ("content" := (list string))
