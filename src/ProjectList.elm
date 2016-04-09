module ProjectList where

import Effects exposing (Effects, Never)
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (..)
import Task

-- MODEL

type alias Project =
    { title : String
    , description : String
    , content : String
    , previews : Maybe (List String)
    }

type alias Model =
    { file : String
    , rawProjects : String
    , projects : List Project
    }

init : String -> (Model, Effects Action)
init file =
  ( Model file "Loading" []
  -- , Effects.none
  -- , getData "http://localhost:8000/1gam_projects/project_list.json"
  , getData "http://elam03.github.io/1gam_projects/project_list.json"

  )

-- UPDATE

type Action
    = RequestRefresh
    | Refresh (Maybe (List Project))


update : Action -> Model -> (Model, Effects Action)
update action model =
    case action of
        RequestRefresh ->
            ( model
            , getData model.file
            )

        Refresh maybeNewFiles ->
            let
                data = Maybe.withDefault [] maybeNewFiles
            in
                if List.isEmpty data then
                    ( Model model.file "failed" data
                    , Effects.none
                    )
                else
                    ( Model model.file ("success " ++ (toString <| List.length data)) data
                    , Effects.none
                    )

-- VIEW

(=>) : a -> b -> (a, b)
(=>) = (,)


view : Signal.Address Action -> Model -> Html
view address model =
    let
        numProjects = toString (List.length model.projects)
    in
        div [ style [ "width" => "600px" ] ]
            [ h2 [ headerStyle ]
                [ text model.file
                , button [onClick address RequestRefresh] [ text "Refresh" ]
                ]
            -- , p [] [ text ("numProjects: " ++ numProjects) ]
            , viewProjects model.projects
            -- , text model.rawProjects
            ]

viewProjects : List Project -> Html
viewProjects projects =
    projects
        |> List.map viewProject
        |> ul [ projectStyle ]

viewProject : Project -> Html
viewProject project =
    div []
        [ text project.title
        -- , span [] project.description
        ]

headerStyle : Attribute
headerStyle =
  style
    [ "width" => "200px"
    , "text-align" => "center"
    ]

imgStyle : String -> Attribute
imgStyle url =
  style
    [ "display" => "inline-block"
    , "width" => "200px"
    , "height" => "200px"
    , "background-position" => "center center"
    , "background-size" => "cover"
    , "background-image" => ("url('" ++ url ++ "')")
    ]

projectStyle : Attribute
projectStyle =
    style []

-- EFFECTS

getData : String -> Effects Action
getData location =
    Http.get decodeUrl location
        |> Task.toMaybe
        |> Task.map Refresh
        |> Effects.task

--     {
--         "title": "1GAM - Rocks",
--         "description": "",
--         "content": "https://drive.google.com/file/d/0BwxvfZmN_TFHT0JsN3RyUmI2d3M/edit?usp=sharing",
--         "previews": ["RocksPreview.png"]
--     },

decodeUrl : Decoder (List Project)
decodeUrl =
    list
        <| object4 Project
            ("title" := string)
            ("description" := string)
            ("content" := string)
            (maybe ("previews" := (list string)))
