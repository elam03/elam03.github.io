module ProjectList exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, href, src, style)
import Http
import Json.Decode exposing (..)
import Platform.Cmd
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

init : String -> String -> (Model, Cmd Msg)
init projectList assetPath =
  ( Model projectList assetPath []
  , getProjectData projectList
  )

-- UPDATE


type Msg
    = Refresh (Result Http.Error ProjectList)


update : Msg -> Model -> (Model, Cmd Msg)
update action model =
    case action of
        Refresh (Ok projectsList) ->
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
                        previews_ =
                            project.previews
                                |> Maybe.withDefault []
                                |> List.map (\p -> model.assetPath ++ p)
                    in
                        { project | previews = Just previews_ }

                projects =
                    projectsList.projects
                        |> List.map addProjectDescription
                        |> List.map addProjectAssetPath
            in
                if List.isEmpty projects then
                    ( Model model.file "failed" projects
                    , Cmd.none
                    )
                else
                    ( Model model.file "success" projects
                    , Cmd.none
                    )

        Refresh (Err _) ->
            (model, Cmd.none)

-- VIEW

view : Model -> Html Msg
view model =
    let
        numCols = 3
        attributes =
            [ Html.Attributes.classList
                [ ("projectlist", True)
                , ("projectlist-container", True)
                ]
            ]

        projects =
            model.projects
                |> List.map (\p -> viewProject p)

    in
        div attributes projects

viewProject : Project -> Html Msg
viewProject project =
    let
        attributes =
            [ Html.Attributes.classList
                [ ("projectlist", True)
                , ("projectlist-item", True)
                ]
            ]

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
        div attributes content

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

classStyle : Html.Attribute Msg
classStyle = class "projectlist"

-- CMDS

getProjectData : String -> Cmd Msg
getProjectData location =
    Http.get location decodeData
        |> Http.send Refresh

decodeData : Decoder (ProjectList)
decodeData =
    Json.Decode.map ProjectList
        ( field "projects"
            ( list
                <| map7 Project
                    (field "title" string)
                    (maybe (field "category" string))
                    (maybe (field "keywords" (list string)))
                    (maybe (field "description" string))
                    (maybe (field "sourceUrl" string))
                    (maybe (field "downloadUrl" string))
                    (maybe (field "previews" (list string)))
            )
        )
