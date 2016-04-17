module ProjectList where

import Effects exposing (Effects, Never)
import Html exposing (..)
import Html.Attributes exposing (style, href, src)
-- import Html.Events exposing (onClick)
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
    , assetPath : String
    , projects : List Project
    }

init : String -> String -> (Model, Effects Action)
init projectList assetPath =
  ( Model projectList assetPath []
  , getData projectList
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
        -- project = viewProjects model.projects

        projects =
            model.projects
                |> List.map (\proj -> {proj | previews = proj.previews})
                |> viewProjects
        -- {proj | previews = List.map (\p -> model.assetPath ++ p) proj.previews}
    in
        div [ style [("border-style", "solid")] ]
            [ projects

            ]
            -- [ h2 [ headerStyle ]
            --     [ text model.file
            --     , button [onClick address RequestRefresh] [ text "Refresh" ]
            --     ]
            -- -- , p [] [ text ("numProjects: " ++ numProjects) ]
            -- , viewProjects model.projects
            -- -- , text model.rawProjects
            -- ]

-- <table style="width:100%">
--   <tr>
--     <th>Firstname</th>
--     <th>Lastname</th>
--     <th>Points</th>
--   </tr>
--   <tr>
--     <td>Eve</td>
--     <td>Jackson</td>
--     <td>94</td>
--   </tr>
-- </table>

borderStyle : Attribute
borderStyle =
    style [ ("border-style", "solid"), ("border-width", "1px") ]

composeProjectMap : Int -> List Project -> List Html
composeProjectMap cols projects =
    let
        head = List.take cols projects
        rest = List.drop cols projects
    in
        if List.isEmpty head then
            []
        else
            [ (tr [] []) ] ++ (List.map viewProject head) ++ (composeProjectMap cols rest)

viewProjects : List Project -> Html
viewProjects projects =
    let
        numProjects = List.length projects
        numCols = 3

        projects' = composeProjectMap numCols projects
    in
        table [ style [ ("width", "500px") ] ]
            projects'

viewProject : Project -> Html
viewProject project =
    let
        path = Maybe.withDefault [] (project.previews) |> List.head |> Maybe.withDefault ""

        images = [ img [ src path, style [ ("width", "16px") ] ] [] ]

        content =
            [ text project.title
            , br [] []
            , text project.description
            , br [] []
            , a [ href project.content ] [ text "download!" ]
            ] ++ images
    in
        th [ borderStyle ]
            [ div []
                content
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
