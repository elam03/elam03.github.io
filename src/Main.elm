module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App as Html
import List exposing (map)
import Platform.Sub
import Time exposing (..)

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
    | Tick Time


type alias Model =
    { cityscape : Cityscape.Model
    , projectList : ProjectList.Model
    , summaryList : SummaryList.Model
    , blogList : BlogList.Model
    , debug : String
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
          , debug = ""
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
        [ div [ classStyle ]
            [ Html.map CityscapeMsgs (Cityscape.view model.cityscape) ]
        , div [ classStyle ]
            [ Html.map BlogListMsgs (BlogList.view model.blogList) ]
        , div [ classStyle ]
            [ Html.map SummaryListMsgs (SummaryList.view model.summaryList) ]
        , div [ classStyle ]
            [ Html.map ProjectListMsgs (ProjectList.view model.projectList) ]
        ]

        -- [ Html.map CityscapeMsgs (Cityscape.view model.cityscape)
        -- , div [ style [ ("display", "flex") ] ]
        --     [ Html.map BlogListMsgs (BlogList.view model.blogList) ]
        -- , div [ style [ ("display", "flex") ] ]
        --     [ Html.map SummaryListMsgs (SummaryList.view model.summaryList) ]
        -- , div [ style [ ("display", "flex") ] ]
        --     [ Html.map ProjectListMsgs (ProjectList.view model.projectList) ]
        -- -- , div []
        -- --     [ text "debug: "
        -- --     , text model.debug
        -- --     ]
        -- ]

classStyle : Html.Attribute Msg
classStyle = class "main"

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

        Tick time ->
            ( { model | debug = toString time }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ model.cityscape
            |> Cityscape.subscriptions
            |> Sub.map (\a -> CityscapeMsgs a)
        , model.projectList
            |> ProjectList.subscriptions
            |> Sub.map (\a -> ProjectListMsgs a)
        , model.summaryList
            |> SummaryList.subscriptions
            |> Sub.map (\a -> SummaryListMsgs a)
        , model.blogList
            |> BlogList.subscriptions
            |> Sub.map (\a -> BlogListMsgs a)
        ]

main =
    Html.program
        { init = init "assets/1gam_projects/project_list.json" "assets/summary_data.json" "assets/blogs/blog_content.json" "assets/1gam_projects/"
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
