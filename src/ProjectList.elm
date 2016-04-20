module ProjectList where

import Effects exposing (Effects, Never)
import Html exposing (..)
import Html.Attributes exposing (style, href, src)
-- import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (..)
import String exposing (..)
import Task

(=>) : a -> b -> (a, b)
(=>) = (,)

-- MODEL

type alias Project =
    { title : String
    , description : String
    , content : String
    , previews : List String
    }

type alias Model =
    { file : String
    , assetPath : String
    , projects : List Project
    }

errorProject : Project
errorProject =
    { title = "error"
    , description = "error"
    , content = "error"
    , previews = ["error", "error"]
    }

init : String -> String -> (Model, Effects Action)
init projectList assetPath =
  ( Model projectList assetPath []
  , getProjectData projectList
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
            , getProjectData model.file
            )

        Refresh maybeProjectsList ->
            let
                addProjectDescription project =
                    if (project.description |> length) > 0 then
                        project
                    else
                        { project | description = "No Description available!" ++ project.description }

                addProjectAssetPath project =
                    let
                        previews' =
                            project.previews
                                |> List.map (\p -> model.assetPath ++ p)

                    in
                        { project | previews = previews' }

                projects =
                    maybeProjectsList
                        |> Maybe.withDefault [errorProject]
                        |> List.map addProjectDescription
                        |> List.map addProjectAssetPath
            in
                if List.isEmpty projects then
                    ( Model model.file "failed" projects
                    , Effects.none
                    )
                else
                    ( Model model.file "success" projects
                    , Effects.none
                    )

-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
    let
        numProjects = toString (List.length model.projects)

        projects =
            model.projects
                |> viewProjects
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
        images =
            project.previews
                |> List.map (\path -> img [ src path, style [ ("width", "32px") ] ] [])

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

-- EFFECTS

getProjectData : String -> Effects Action
getProjectData location =
    Http.get decodeProjectData location
        |> Task.toMaybe
        |> Task.map Refresh
        |> Effects.task

--     {
--         "title": "1GAM - Rocks",
--         "description": "",
--         "content": "https://drive.google.com/file/d/0BwxvfZmN_TFHT0JsN3RyUmI2d3M/edit?usp=sharing",
--         "previews": ["RocksPreview.png"]
--     },

decodeProjectData : Decoder (List Project)
decodeProjectData =
    list
        <| object4 Project
            ("title" := string)
            ("description" := string)
            ("content" := string)
            ("previews" := (list string))
