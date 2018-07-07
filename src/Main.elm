module Main exposing (..)

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
-- import Json.Decode as Json exposing ((:=))
import List exposing (map)
import Navigation
import Platform.Sub
import String
import Task
import Time exposing (..)
import UrlParser exposing (Parser, (</>), int, map, oneOf, s, string)

import BlogList
import Cityscape
import ProjectList
import SummaryList
import RandomQuotes

(=>) : a -> b -> (a, b)
(=>) = (,)

type Msg
    = NoOp
    | CityscapeMsgs Cityscape.Msg
    | ProjectListMsgs ProjectList.Msg
    | SummaryListMsgs SummaryList.Msg
    | BlogListMsgs BlogList.Msg
    | RandomQuotesMsgs RandomQuotes.Msg
    | Tick Time
    | GotoNavBar Page
    | UrlChange Navigation.Location

type alias Model =
    { cityscape : Cityscape.Model
    , projectList : ProjectList.Model
    , summaryList : SummaryList.Model
    , blogList : BlogList.Model
    , randomQuotes : RandomQuotes.Model
    , debug : String
    , page : Page
    }

init : Navigation.Location -> (Model, Cmd Msg)
init location =
    let
        projectListFileLocation = "assets/1gam_projects/project_list.json"
        summaryFileLocation     = "assets/summary_data.json"
        blogFileLocation        = "assets/blogs/blog_content.json"
        assetPath               = "assets/1gam_projects/"

        (cityscape, cityscapeCmds)       = Cityscape.init
        (projectList, projectListCmds)   = ProjectList.init projectListFileLocation assetPath
        (summaryList, summaryListCmds)   = SummaryList.init summaryFileLocation
        (blogList, blogListCmds)         = BlogList.init blogFileLocation
        (randomQuotes, randomQuotesCmds) = RandomQuotes.init 1

        model_ =
            { cityscape = cityscape
            , projectList = projectList
            , summaryList = summaryList
            , blogList = blogList
            , randomQuotes = randomQuotes
            , debug = ""
            , page = Projects
            }

        -- (model_, cmds) = (model, cmds)

        -- (model_, cmds) = urlUpdate result model

        cmds_ =
            Cmd.batch
                [ Cmd.map CityscapeMsgs cityscapeCmds
                , Cmd.map ProjectListMsgs projectListCmds
                , Cmd.map SummaryListMsgs summaryListCmds
                , Cmd.map BlogListMsgs blogListCmds
                , Cmd.map RandomQuotesMsgs randomQuotesCmds
                ]

    in
        ( model_, cmds_ )

viewNavBar : Model -> Html Msg
viewNavBar model =
    let
        tabsInfo =
            [ ("#home", Home, "Home")
            , ("#skills", Skills, "Skills")
            , ("#projects", Projects, "Projects")
            , ("#blog/1", Blog 1, "Blog")
            , ("#quotebanner", QuoteBanner, "QuoteBanner")
            ]

        toMenuItem (linkText, page, displayText) =
            let
                attributes =
                    Html.Attributes.classList
                        [ ("navbar-item", True)
                        , ("active", model.page == page)
                        ]
            in
                a [ attributes, href linkText, onClick (GotoNavBar page) ] [ text displayText ]

        tabs =
            tabsInfo
                |> List.map toMenuItem

        logo =
            [ img [ src "assets/avatars/avatar0.jpg", alt "avatar", class "logo" ] [] ]
    in
        nav [ class "navbar" ]
            (logo ++ tabs)

view : Model -> Html Msg
view model =
    let
        cityscapeContent    = model.cityscape    |> Cityscape.view    |> Html.map CityscapeMsgs
        blogListContent     = model.blogList     |> BlogList.view     |> Html.map BlogListMsgs
        summaryListContent  = model.summaryList  |> SummaryList.view  |> Html.map SummaryListMsgs
        projectListContent  = model.projectList  |> ProjectList.view  |> Html.map ProjectListMsgs
        randomQuotesContent = model.randomQuotes |> RandomQuotes.view |> Html.map RandomQuotesMsgs

        homeContent =
            model.summaryList
                |> SummaryList.viewFilter [ "Motivations", "Interests", "Quotes", "Inspirational People" ]
                |> Html.map SummaryListMsgs

        skillsContent =
            model.summaryList
                |> SummaryList.viewFilter [ "Skills", "Languages", "Experiences" ]
                |> Html.map SummaryListMsgs

        tabsInfo =
            [ (Home, [ cityscapeContent, homeContent ])
            , (Skills, [ cityscapeContent, skillsContent ])
            , (Blog 1, [ cityscapeContent, blogListContent ])
            , (Projects, [ cityscapeContent, projectListContent ])
            , (QuoteBanner, [ cityscapeContent, randomQuotesContent ] )
            ]

        toDiv (page, html) =
            let
                attributes =
                    Html.Attributes.classList
                        [ ("hidden", not (page == model.page))
                        ]

                divs =
                    html
                        |> List.map (\content -> div [ class "content-block" ] [ content ])
            in
                div [ attributes ] divs

        navbar =
            viewNavBar model

        divs =
            tabsInfo
                |> List.map toDiv

        allDivs =
            [ navbar ] ++ divs
    in
        div [] allDivs

update : Msg -> Model -> (Model, Cmd.Cmd Msg)
update action model =
    -- case Debug.log "action" action of
    case action of
        CityscapeMsgs act ->
            let
                (cityscape, cmds) = Cityscape.update act model.cityscape
            in
                ( { model | cityscape = cityscape }
                , Cmd.map CityscapeMsgs cmds
                )

        ProjectListMsgs act ->
            let
                (projectList, cmds) = ProjectList.update act model.projectList
            in
                ( { model | projectList = projectList }
                , Cmd.map ProjectListMsgs cmds
                )

        SummaryListMsgs act ->
            let
                (summaryList, cmds) = SummaryList.update act model.summaryList
            in
                ( { model | summaryList = summaryList }
                , Cmd.map SummaryListMsgs cmds
                )

        BlogListMsgs act ->
            let
                (blogList, cmds) = BlogList.update act model.blogList
            in
                ( { model | blogList = blogList }
                , Cmd.map BlogListMsgs cmds
                )

        RandomQuotesMsgs act ->
            let
                (randomQuotes, cmds) = RandomQuotes.update act model.randomQuotes
            in
                ( { model | randomQuotes = randomQuotes }
                , Cmd.map RandomQuotesMsgs cmds
                )

        Tick time ->
            ( { model | debug = toString time }, Cmd.none )

        GotoNavBar newPage ->
            { model | page = newPage }
                ! [ Navigation.newUrl (toHash newPage) ]

        UrlChange location ->
            ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )

-------------------------------------------------------------------------------
-- URL PARSERS - check out evancz/url-parser for fancier URL parsing

toHash : Page -> String
toHash page =
    case page of
        Home ->
            "#home"

        Skills ->
            "#skills"

        Projects ->
            "#projects"

        Blog id ->
            "#blog/" ++ toString id

        QuoteBanner ->
            "#quotebanner"

-- hashParser : Navigation.Location -> Result String Page
-- hashParser location =
--     UrlParser.parse identity pageParser (String.dropLeft 1 location.hash)

type Page
    = Home
    | Skills
    | Projects
    | Blog Int
    | QuoteBanner

pageParser : Parser (Page -> a) a
pageParser =
    UrlParser.oneOf
        [ UrlParser.map Home (UrlParser.s "home")
        , UrlParser.map Skills (UrlParser.s "skills")
        , UrlParser.map Projects (UrlParser.s "projects")
        , UrlParser.map Blog (UrlParser.s "blog" </> UrlParser.int)
        , UrlParser.map QuoteBanner (UrlParser.s "quotebanner")
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
        , model.randomQuotes
            |> RandomQuotes.subscriptions
            |> Sub.map (\a -> RandomQuotesMsgs a)
        ]

main =
    -- Navigation.program (Navigation.makeParser hashParser)
    Navigation.program UrlChange
        { init = init
        , view = view
        , update = update
        -- , urlUpdate = urlUpdate
        , subscriptions = subscriptions
        }
