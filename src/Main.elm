module Main (..) where

import Effects exposing (Effects, Never)
import Html exposing (..)
import Html.Attributes exposing (..)
-- import Html.Events as Events
-- import Http
import StartApp
import Task exposing (..)
-- import Json.Decode as Json

import RandomGif
import ProjectList

(=>) : a -> b -> (a, b)
(=>) = (,)

type Action
    = NoOp
    -- | Refresh
    -- | OnRefresh (Result Http.Error String)
    -- | NewGif (Maybe String)
    | Left RandomGif.Action
    | ProjectListActions ProjectList.Action
    | Message


type alias Model =
    { left : RandomGif.Model
    , projectList : ProjectList.Model
    }

-- initialModel : Model
-- initialModel =
--     {   message = "hello"
--     ,   projectData = getProjectData
--     ,   left = {}
--     -- ,   projectData = "asdfasdf"
--     }

init : String -> String -> (Model, Effects Action)
init topic fileLocation =
    let
        (left, leftFx) = RandomGif.init topic
        (projectList, projectListFx) = ProjectList.init fileLocation
    in
        ( { left = left
          , projectList = projectList
          }
        , Effects.batch
            [ Effects.map Left leftFx
            , Effects.map ProjectListActions projectListFx
            ]
        )

-- get : String -> Task Http.Error (List String)
-- get query =
--     Http.get places ("http://api.zippopotam.us/us/" ++ query)
--
-- places : Json.Decoder (List String)
-- places =
--     let place =
--         Json.object2 (\city state -> city ++ ", " ++ state)
--             ("place name" := Json.string)
--             ("state" := Json.string)
--     in
--         "places" := Json.list place

myStyle : Html.Attribute
myStyle =
    style
        [ ("height", "100px")
        , ("width", "100px")
        , ("border-style", "solid")
        , ("border-color", "black")
        , ("display", "flex")
        -- , ("border-style", "solid")
        ]

view : Signal.Address Action -> Model -> Html
view address model =
    div []
        [ div [ style [ ("display", "flex") ] ]
            [ ProjectList.view (Signal.forwardTo address ProjectListActions) model.projectList
            ]
        , div [ style [ ("display", "flex") ] ]
            [ RandomGif.view (Signal.forwardTo address Left) model.left
            ]
        ]

update : Action -> Model -> ( Model, Effects.Effects Action )
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
        _ ->
            ( model, Effects.none )

app : StartApp.App Model
app =
    StartApp.start
        { init = init "funny cats" "http://elam03.github.io/1gam_projects/project_list.json"
        , inputs = []
        , update = update
        , view = view
        }


main : Signal.Signal Html
main =
    app.html

port runner : Signal (Task.Task Never ())
port runner =
    app.tasks
