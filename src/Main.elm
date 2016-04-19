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

import RandomGif
import ProjectList
import Cityscape exposing (..)

(=>) : a -> b -> (a, b)
(=>) = (,)

type Action
    = NoOp
    | Left RandomGif.Action
    | ProjectListActions ProjectList.Action
    | Message
    | CityscapeActions Cityscape.Action


type alias Model =
    { left : RandomGif.Model
    , projectList : ProjectList.Model
    , cityscape : Cityscape.Model
    }

init : String -> String -> String -> (Model, Effects Action)
init topic fileLocation assetPath =
    let
        (left, leftFx) = RandomGif.init topic
        (projectList, projectListFx) = ProjectList.init fileLocation assetPath
        (cityscape, cityscapeFx) = Cityscape.init (500, 200)
    in
        ( { left = left
          , projectList = projectList
          , cityscape = cityscape
          }
        , Effects.batch
            [ Effects.map Left leftFx
            , Effects.map ProjectListActions projectListFx
            , Effects.map CityscapeActions cityscapeFx
            ]
        )

view : Signal.Address Action -> Model -> Html
view address model =
    div []
        [ Cityscape.view (Signal.forwardTo address CityscapeActions) model.cityscape
        , div [ style [ ("display", "flex") ] ]
            [ ProjectList.view (Signal.forwardTo address ProjectListActions) model.projectList
            ]
        -- , div [ style [ ("display", "flex") ] ]
        --     [ Cityscape.view (Signal.forwardTo address CityscapeActions) model.cityscape
        --     ]
        -- , div [ style [ ("display", "flex") ] ]
        --     [ RandomGif.view (Signal.forwardTo address Left) model.left
        --     ]
        ]

update : Action -> Model -> (Model, Effects.Effects Action)
update action model =
    case Debug.log "action" action of
        Left act ->
            let
                (left, fx) = RandomGif.update act model.left
            in
                ( { model | left = left }
                , Effects.map Left fx
                )
        ProjectListActions act ->
            let
                (projectList, fx) = ProjectList.update act model.projectList
            in
                ( { model | projectList = projectList }
                , Effects.map ProjectListActions fx
                )
        CityscapeActions act ->
            let
                (cityscape, fx) = Cityscape.update act model.cityscape
            in
                ( { model | cityscape = cityscape }
                , Effects.map CityscapeActions fx
                )
        _ ->
            ( model, Effects.none )

app : StartApp.App Model
app =
    StartApp.start
        -- { init = init "funny cats" "http://elam03.github.io/1gam_projects/project_list.json" "http://elam03.github.io/1gam_projects/"
        { init = init "funny cats" "1gam_projects/project_list.json" "1gam_projects/"
        , inputs = [ (Signal.map (\a -> CityscapeActions a) Cityscape.inputs) ]
        , update = update
        , view = view
        }

main : Signal.Signal Html
main =
    app.html

port runner : Signal (Task.Task Never ())
port runner =
    app.tasks
