module ProjectList where

import Effects exposing (Effects, Never)
import Html exposing (..)
import Html.Attributes exposing (class, href, src, style)
import Http
import Json.Decode exposing (..)
import String exposing (..)
import Task
import Utils exposing (..)

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
        div []
            [ projects ]

viewProjects : List Project -> Html
viewProjects projects =
    let
        numCols = 3
        classname = "projectlist"

        projects' =
            projects
                |> composeTiledHtml classname viewProject numCols
    in
        table [ class classname ]
            projects'

viewProject : Project -> List Html
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
        content

-- EFFECTS

getProjectData : String -> Effects Action
getProjectData location =
    Http.get decodeProjectData location
        |> Task.toMaybe
        |> Task.map Refresh
        |> Effects.task

decodeProjectData : Decoder (List Project)
decodeProjectData =
    list
        <| object4 Project
            ("title" := string)
            ("description" := string)
            ("content" := string)
            ("previews" := (list string))
