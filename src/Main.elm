module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App as Html
import List exposing (map)

import BlogList
import Cityscape
import ProjectList
import SummaryList

(=>) : a -> b -> (a, b)
(=>) = (,)

type Msg
    = NoOp
    | CityscapeMsgs Cityscape.Msg
    | ProjectListMsgs ProjectList.Msg
    | SummaryListMsgs SummaryList.Msg
    | BlogListMsgs BlogList.Msg


type alias Model =
    { cityscape : Cityscape.Model
    , projectList : ProjectList.Model
    , summaryList : SummaryList.Model
    , blogList : BlogList.Model
    }

init : String -> String -> String -> String -> (Model, Cmd Msg)
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
        , Cmd.batch
            [ Cmd.map CityscapeMsgs cityscapeFx
            , Cmd.map ProjectListMsgs projectListFx
            , Cmd.map SummaryListMsgs summaryListFx
            , Cmd.map BlogListMsgs blogListFx
            ]
        )

view : Model -> Html Msg
view model =
    div []
        [ text "blah"
        , Html.map CityscapeMsgs (Cityscape.view model.cityscape)
        , div [ style [ ("display", "flex") ] ]
            [ Html.map BlogListMsgs (BlogList.view model.blogList) ]
        , div [ style [ ("display", "flex") ] ]
            [ Html.map SummaryListMsgs (SummaryList.view model.summaryList) ]
        , div [ style [ ("display", "flex") ] ]
            [ Html.map ProjectListMsgs (ProjectList.view model.projectList) ]
        ]
        -- [ Cityscape.view (Signal.forwardTo address CityscapeMsgs) model.cityscape
        -- , div [ style [ ("display", "flex") ] ]
        --     [ BlogList.view (Signal.forwardTo address BlogListMsgs) model.blogList
        --     ]
        -- , div [ style [ ("display", "flex") ] ]
        --     [ SummaryList.view (Signal.forwardTo address SummaryListMsgs) model.summaryList
        --     ]
        -- , div [ style [ ("display", "flex") ] ]
        --     [ ProjectList.view (Signal.forwardTo address ProjectListMsgs) model.projectList
        --     ]
        -- ]
        -- [ Cityscape.view (Signal.forwardTo address CityscapeMsgs) model.cityscape
        -- , div [ style [ ("display", "flex") ] ]
        --     [ BlogList.view (Signal.forwardTo address BlogListMsgs) model.blogList
        --     ]
        -- , div [ style [ ("display", "flex") ] ]
        --     [ SummaryList.view (Signal.forwardTo address SummaryListMsgs) model.summaryList
        --     ]
        -- , div [ style [ ("display", "flex") ] ]
        --     [ ProjectList.view (Signal.forwardTo address ProjectListMsgs) model.projectList
        --     ]
        -- ]

update : Msg -> Model -> (Model, Cmd.Cmd Msg)
update action model =
    case Debug.log "action" action of
        CityscapeMsgs act ->
            let
                (cityscape, fx) = Cityscape.update act model.cityscape
            in
                ( { model | cityscape = cityscape }
                , Cmd.map CityscapeMsgs fx
                )

        ProjectListMsgs act ->
            let
                (projectList, fx) = ProjectList.update act model.projectList
            in
                ( { model | projectList = projectList }
                , Cmd.map ProjectListMsgs fx
                )

        SummaryListMsgs act ->
            let
                (summaryList, fx) = SummaryList.update act model.summaryList
            in
                ( { model | summaryList = summaryList }
                , Cmd.map SummaryListMsgs fx
                )

        BlogListMsgs act ->
            let
                (blogList, fx) = BlogList.update act model.blogList
            in
                ( { model | blogList = blogList }
                , Cmd.map BlogListMsgs fx
                )

        NoOp ->
            ( model, Cmd.none )

main =
    Html.program
        { init = init "assets/1gam_projects/project_list.json" "assets/summary_data.json" "assets/blogs/blog_content.json" "assets/1gam_projects/"
        -- , inputs =
            -- [ (Signal.map (\a -> CityscapeMsgs a) Cityscape.inputs)
            -- , (Signal.map (\a -> BlogListMsgs a) BlogList.inputs)
            -- ]

            -- [ (Signal.map (\a -> BlogListMsgs a) BlogList.inputs)
            -- , (Signal.map (\a -> CityscapeMsgs a) Cityscape.inputs)
            -- ]
            -- [
            --     Signal.mergeMany
            --         [ (Signal.map (\a -> BlogListMsgs a) BlogList.inputs)
            --         , (Signal.map (\a -> CityscapeMsgs a) Cityscape.inputs)
            --         ]
            -- ]

        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
