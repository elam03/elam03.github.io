module Main (..) where

import Effects exposing (Effects, Never)
import Html exposing (..)
import Html.Attributes exposing (..)
-- import Html.Events as Events
-- import Http
import Signal exposing (..)
import StartApp
import Task exposing (..)
-- import Time exposing (..)
-- import Json.Decode as Json

import BlogList
import Cityscape
import ProjectList
import SummaryList

(=>) : a -> b -> (a, b)
(=>) = (,)

type Action
    = NoOp
    | CityscapeActions Cityscape.Action
    | ProjectListActions ProjectList.Action
    | SummaryListActions SummaryList.Action
    | BlogListActions BlogList.Action


type alias Model =
    { cityscape : Cityscape.Model
    , projectList : ProjectList.Model
    , summaryList : SummaryList.Model
    , blogList : BlogList.Model
    }

init : String -> String -> String -> String -> (Model, Effects Action)
init projectListFileLocation summaryFileLocation blogFileLocation assetPath =
    let
        (cityscape, cityscapeFx) = Cityscape.init (600, 200)
        (projectList, projectListFx) = ProjectList.init projectListFileLocation assetPath
        (summaryList, summaryListFx) = SummaryList.init summaryFileLocation
        (blogList, blogListFx) = BlogList.init blogFileLocation
    in
        ( { cityscape = cityscape
          , projectList = projectList
          , summaryList = summaryList
          , blogList = blogList
          }
        , Effects.batch
            [ Effects.map CityscapeActions cityscapeFx
            , Effects.map ProjectListActions projectListFx
            , Effects.map SummaryListActions summaryListFx
            , Effects.map BlogListActions blogListFx
            ]
        )

view : Signal.Address Action -> Model -> Html
view address model =
    div []
        [ Cityscape.view (Signal.forwardTo address CityscapeActions) model.cityscape
        , div [ style [ ("display", "flex") ] ]
            [ BlogList.view (Signal.forwardTo address BlogListActions) model.blogList
            ]
        , div [ style [ ("display", "flex") ] ]
            [ SummaryList.view (Signal.forwardTo address SummaryListActions) model.summaryList
            ]
        , div [ style [ ("display", "flex") ] ]
            [ ProjectList.view (Signal.forwardTo address ProjectListActions) model.projectList
            ]
        ]

update : Action -> Model -> (Model, Effects.Effects Action)
update action model =
    case Debug.log "action" action of
        CityscapeActions act ->
            let
                (cityscape, fx) = Cityscape.update act model.cityscape
            in
                ( { model | cityscape = cityscape }
                , Effects.map CityscapeActions fx
                )

        ProjectListActions act ->
            let
                (projectList, fx) = ProjectList.update act model.projectList
            in
                ( { model | projectList = projectList }
                , Effects.map ProjectListActions fx
                )

        SummaryListActions act ->
            let
                (summaryList, fx) = SummaryList.update act model.summaryList
            in
                ( { model | summaryList = summaryList }
                , Effects.map SummaryListActions fx
                )

        BlogListActions act ->
            let
                (blogList, fx) = BlogList.update act model.blogList
            in
                ( { model | blogList = blogList }
                , Effects.map BlogListActions fx
                )

        NoOp ->
            ( model, Effects.none )

app : StartApp.App Model
app =
    StartApp.start
        { init = init "assets/1gam_projects/project_list.json" "assets/summary_data.json" "assets/blogs/blog_content.json" "assets/1gam_projects/"
        , inputs =
            -- [ (Signal.map (\a -> CityscapeActions a) Cityscape.inputs)
            -- , (Signal.map (\a -> BlogListActions a) BlogList.inputs)
            -- ]

            [ (Signal.map (\a -> BlogListActions a) BlogList.inputs)
            , (Signal.map (\a -> CityscapeActions a) Cityscape.inputs)
            ]
            -- [
            --     Signal.mergeMany
            --         [ (Signal.map (\a -> BlogListActions a) BlogList.inputs)
            --         , (Signal.map (\a -> CityscapeActions a) Cityscape.inputs)
            --         ]
            -- ]

        , update = update
        , view = view
        }

main : Signal.Signal Html
main =
    app.html

port runner : Signal (Task.Task Never ())
port runner =
    app.tasks
