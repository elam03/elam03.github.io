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

type alias ProjectList =
    { projects : List Project
    }

type alias Project =
    { title : String
    , category : Maybe String
    , keywords : Maybe (List String)
    , description : Maybe String
    , sourceUrl : Maybe String
    , downloadUrl : Maybe String
    , previews : Maybe (List String)
    }

type alias Model =
    { file : String
    , assetPath : String
    , projects : List Project
    }

errorProjectList : ProjectList
errorProjectList =
    { projects = [ errorProject ]
    }

errorProject : Project
errorProject =
    { title = "error"
    , category = Just "error"
    , keywords = Just ["error"]
    , description = Just "error"
    , sourceUrl = Just "error"
    , downloadUrl = Just "error"
    , previews = Just ["error", "error"]
    }

init : String -> String -> (Model, Effects Action)
init projectList assetPath =
  ( Model projectList assetPath []
  , getProjectData projectList
  )

-- UPDATE

type Action
    = RequestRefresh
    | Refresh (Maybe ProjectList)


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
                    let
                        description = Maybe.withDefault "" project.description
                    in
                        if (description |> length) > 0 then
                            project
                        else
                            { project | description = Just "No Description available!" }

                addProjectAssetPath project =
                    let
                        previews' =
                            project.previews
                                |> Maybe.withDefault []
                                |> List.map (\p -> model.assetPath ++ p)
                    in
                        { project | previews = Just previews' }

                projectList =
                    maybeProjectsList
                        |> Maybe.withDefault errorProjectList

                projects =
                    projectList.projects
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
        attributes = [ class (classname ++ "-item") ]

        projects' =
            projects
                |> composeTiledHtml attributes viewProject numCols
    in
        table [ class classname ]
            projects'

viewProject : Project -> List Html
viewProject project =
    let
        titleContent =
            [ h3 [] [ text project.title ] ]

        categoryContent =
            case project.category of
                Just category -> [ p [] [ text category ] ]
                Nothing -> []

        keywordsContent =
            project.keywords
                |> Maybe.withDefault []
                |> List.map (\keyword -> p [ style [ ("font-weight", "bold") ] ] [ text keyword ])

        descriptionContent =
            case project.description of
                Just description -> [ p [ style [ ("text-align", "left") ] ] [ text description ] ]
                Nothing -> []

        sourceUrlContent =
            case project.sourceUrl of
                Just sourceUrl -> [ a [ href sourceUrl ] [ text "Source Link" ] ]
                Nothing -> []

        downloadUrlContent =
            case project.downloadUrl of
                Just downloadUrl -> [ a [ href downloadUrl ] [ text "Download Link" ] ]
                Nothing -> []

        previewsContent =
            project.previews
                |> Maybe.withDefault []
                |> List.map (\path -> img [ src path, style [ ("width", "32px") ] ] [])

        break = [ br [] [] ]

        content =
            titleContent
            ++ categoryContent
            ++ keywordsContent
            ++ break
            ++ descriptionContent
            ++ break
            ++ sourceUrlContent
            ++ break
            ++ downloadUrlContent
            ++ break
            ++ previewsContent
    in
        content

-- EFFECTS

getProjectData : String -> Effects Action
getProjectData location =
    Http.get decodeData location
        |> Task.toMaybe
        |> Task.map Refresh
        |> Effects.task

decodeData : Decoder (ProjectList)
decodeData =
    object1 ProjectList
        ( "projects" :=
            ( list
                <| object7 Project
                    ("title" := string)
                    (maybe ("category" := string))
                    (maybe ("keywords" := (list string)))
                    (maybe ("description" := string))
                    (maybe ("sourceUrl" := string))
                    (maybe ("downloadUrl" := string))
                    (maybe ("previews" := (list string)))
            )
        )
