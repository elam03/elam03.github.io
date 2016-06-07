module BlogList exposing (..)

import Array
import Html exposing (..)
import Html.Events exposing (onClick, onMouseOver, onMouseOut)
import Html.Attributes exposing (class, href, src, style)
import Http
import Json.Decode exposing (..)
import Markdown
import Platform.Cmd
import Task
import Time exposing (..)
import Utils exposing (..)

(=>) : a -> b -> (a, b)
(=>) = (,)

-- MODEL

type alias ID = Int

type alias BlogList =
    { blogs : List Blog
    }

type alias Blog =
    { title : String
    , id : ID
    , hover : Bool
    , date : Maybe String
    , keywords : Maybe (List String)
    , url : Maybe String
    }

type alias Model =
    { blogListFile : String
    , blogs : List Blog
    , currBlog : Html Msg
    , debug : String
    , currId : ID
    }

errorBlog : Blog
errorBlog =
    { title = "error"
    , id = -1
    , hover = False
    , date = Just ""
    , keywords = Just ["error"]
    , url = Just ""
    }

init : String -> (Model, Cmd Msg)
init blogList =
  ( Model blogList [] (text "") "debug!" -1
  , getBlogList blogList
  )

-- UPDATE

type Msg
    = LoadBlogList BlogList
    | LoadBlogMarkdown String
    | FocusBlog ID
    | HoverBlog ID
    | UnHoverBlog ID
    | FetchFail Http.Error
    | Tick Float

update : Msg -> Model -> (Model, Cmd Msg)
update action model =
    case action of
        LoadBlogList blogsList ->
            let
                populateBlogId b =
                    b
                        |> List.indexedMap (,)
                        |> List.map (\(a, b) -> (a, {b | id = a}))
                        |> List.unzip
                        |> snd

                blogs =
                    blogsList
                        |> .blogs
                        |> populateBlogId
            in
                ( { model | blogs = blogs }
                , Cmd.none
                )

        LoadBlogMarkdown markdownContent ->
            let
                currBlog =
                    markdownContent
                        |> Markdown.toHtml []

                debug =
                    model.debug
            in
                ( Model model.blogListFile model.blogs currBlog debug model.currId
                , Cmd.none
                )

        FocusBlog id ->
            let
                blog =
                    model.blogs
                        |> Array.fromList
                        |> Array.get id
                        |> Maybe.withDefault errorBlog

                blogMarkdownFile =
                    blog.url
                        |> Maybe.withDefault ""

                debug =
                    toString <| List.length model.blogs
            in
                ( Model model.blogListFile model.blogs model.currBlog debug id
                , getContent blogMarkdownFile
                )

        HoverBlog id ->
            let
                blogs =
                    model.blogs
                        |> List.map (\b -> if b.id == id then { b | hover = True } else b)
            in
                ( { model | blogs = blogs }, Cmd.none)

        UnHoverBlog id ->
            let
                blogs =
                    model.blogs
                        |> List.map (\b -> if b.id == id then { b | hover = False } else b)
            in
                ( { model | blogs = blogs }, Cmd.none)

        FetchFail _ ->
            (model, Cmd.none)

        Tick t ->
            (model, Cmd.none)

-- VIEW

classStyle : Html.Attribute Msg
classStyle = class "bloglist"

classContainerStyle : Html.Attribute Msg
classContainerStyle = class "bloglist-container"

classItemStyle : Html.Attribute Msg
classItemStyle = class "bloglist-item"

classHoverStyle : Html.Attribute Msg
classHoverStyle = class "bloglist-item-hover"

classBlogHeader : Html.Attribute Msg
classBlogHeader = class "bloglist-header"

classCurrBlogStyle : Html.Attribute Msg
classCurrBlogStyle = class "bloglist-currblog"

view : Model -> Html Msg
view model =
    let
        currBlogContent =
            let
                currBlogSelected =
                    model.currId >= 0 && model.currId < List.length model.blogs

                currBlog =
                    model.blogs
                        |> Array.fromList
                        |> Array.get model.currId
                        |> Maybe.withDefault errorBlog
            in
                if currBlogSelected then
                    [ h1 [ style [ ("text-align", "center") ] ] [ text currBlog.title ]
                    , div [] [ model.currBlog ]
                    ]
                else
                    []

        blogTileAttributes =
            Html.Attributes.classList
                [ ("bloglist", True)
                , ("bloglist-container", True)
                ]

        blogTiles =
            model.blogs
                |> List.map (\b -> viewBlogTile b)

        blogTilesContainer =
            [ div [ blogTileAttributes ] blogTiles ]

        allTheThings =
            blogTilesContainer
            ++ currBlogContent

    in
        div [] allTheThings

viewBlogTile : Blog -> Html Msg
viewBlogTile blog =
    let
        attribute =
            [ Html.Attributes.classList
                [ ("bloglist", True)
                , ("bloglist-item", True)
                , ("bloglist-item-hover", blog.hover)
                ]
            ]

        attributes =
            [ onClick (FocusBlog blog.id)
            , onMouseOver (HoverBlog blog.id)
            , onMouseOut (UnHoverBlog blog.id)
            ] ++ attribute

        titleContent =
            [ h4 [ style [ ("text-align", "center") ] ] [ text blog.title ] ]

        allContent =
            titleContent
    in
        div attributes allContent

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every minute Tick ]

-- CMDS

getBlogList : String -> Cmd Msg
getBlogList location =
    Http.get decodeBlogList location
        |> Task.perform FetchFail LoadBlogList


getContent : String -> Cmd Msg
getContent location =
    Http.getString location
        |> Task.perform FetchFail LoadBlogMarkdown

decodeBlog : Decoder (Blog)
decodeBlog =
    object6 Blog
        ("title" := string)
        (succeed -1)
        (succeed False)
        (maybe ("date" := string))
        (maybe ("keywords" := (list string)))
        (maybe ("url" := string))

decodeBlogList : Decoder (BlogList)
decodeBlogList =
    object1 BlogList
        <| "blogs" := (list decodeBlog)
