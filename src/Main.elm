module Main exposing (..)

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App as Html
import Http
import Json.Decode as Json exposing ((:=))
import List exposing (map)
import Navigation
import Platform.Sub
import String
import Task
import Time exposing (..)
import UrlParser exposing (Parser, (</>), format, int, oneOf, s, string)

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
    , page : Page
    }

init : Result String Page -> (Model, Cmd Msg)
init result =
    let
        projectListFileLocation = "assets/1gam_projects/project_list.json"
        summaryFileLocation     = "assets/summary_data.json"
        blogFileLocation        = "assets/blogs/blog_content.json"
        assetPath               = "assets/1gam_projects/"

        (cityscape, cityscapeFx)     = Cityscape.init
        (projectList, projectListFx) = ProjectList.init projectListFileLocation assetPath
        (summaryList, summaryListFx) = SummaryList.init summaryFileLocation
        (blogList, blogListFx)       = BlogList.init blogFileLocation

        model =
            { cityscape = cityscape
            , projectList = projectList
            , summaryList = summaryList
            , blogList = blogList
            , debug = ""
            , page = Home
            }

        (model', cmds) = urlUpdate result model

        cmds' =
            Cmd.batch
                [ Cmd.map CityscapeMsgs cityscapeFx
                , Cmd.map ProjectListMsgs projectListFx
                , Cmd.map SummaryListMsgs summaryListFx
                , Cmd.map BlogListMsgs blogListFx
                , cmds
                ]

    in
        ( model', cmds' )
        -- urlUpdate result model

view : Model -> Html Msg
view model =
    div []
        [ div [ class "main" ] [ Html.map CityscapeMsgs (Cityscape.view model.cityscape) ]
        , div [ class "main" ] [ Html.map BlogListMsgs (BlogList.view model.blogList) ]
        , div [ class "main" ] [ Html.map SummaryListMsgs (SummaryList.view model.summaryList) ]
        , div [ class "main" ] [ Html.map ProjectListMsgs (ProjectList.view model.projectList) ]
        ]

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

-------------------------------------------------------------------------------
-- URL PARSERS - check out evancz/url-parser for fancier URL parsing

toHash : Page -> String
toHash page =
    case page of
        Home ->
            "#home"

        Blog id ->
            "#blog/" ++ toString id

hashParser : Navigation.Location -> Result String Page
hashParser location =
    UrlParser.parse identity pageParser (String.dropLeft 1 location.hash)

type Page
    = Home
    | Blog Int

pageParser : Parser (Page -> a) a
pageParser =
    UrlParser.oneOf
        [ UrlParser.format Home (UrlParser.s "home")
        , UrlParser.format Blog (UrlParser.s "blog" </> UrlParser.int)
        ]

-- URL-PARSER
-------------------------------------------------------------------------------

urlUpdate : Result String Page -> Model -> (Model, Cmd Msg)
urlUpdate result model =
    case result of
        Err _ ->
            ( model, Navigation.modifyUrl (toHash model.page) )

        Ok page ->
            { model
            | page = page
            , debug = page |> toHash
            } ! []

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
    Navigation.program (Navigation.makeParser hashParser)
        { init = init
        , view = view
        , update = update
        , urlUpdate = urlUpdate
        , subscriptions = subscriptions
        }
