module BlogList where

import Array
import Effects exposing (Effects, Never)
import Html exposing (..)
import Html.Events exposing (onClick, onMouseMove)
import Html.Attributes exposing (class, href, src, style)
import Http
import Json.Decode exposing (..)
import Markdown
import Signal
import Task
import Time
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
    , date : Maybe String
    , keywords : Maybe (List String)
    , url : Maybe String
    }

type alias Model =
    { blogListFile : String
    , blogs : List Blog
    , currBlog : Html
    , debug : String
    , currId : ID
    }

errorBlog : Blog
errorBlog =
    { title = "error"
    , id = 0
    , date = Just ""
    , keywords = Just ["error"]
    , url = Just ""
    }

init : String -> (Model, Effects Action)
init blogList =
  ( Model blogList [] (text "") "debug!" 0
  , getBlogList blogList
  )

-- UPDATE

type Action
    = LoadBlogList (Maybe BlogList)
    | LoadBlogMarkdown (Maybe String)
    | FocusBlog ID
    | HoverBlog ID
    | Tick Float

update : Action -> Model -> (Model, Effects Action)
update action model =
    case action of
        LoadBlogList maybeBlogsList ->
            let
                populateBlogId b =
                    b
                        |> List.indexedMap (,)
                        |> List.map (\(a, b) -> (a, {b | id = a}))
                        |> List.unzip
                        |> snd

                blogs =
                    maybeBlogsList
                        |> Maybe.withDefault (BlogList [])
                        |> .blogs
                        |> populateBlogId
            in
                ( { model | blogs = blogs }
                , Effects.none
                )

        LoadBlogMarkdown maybeMarkdownContent ->
            let
                currBlog =
                    maybeMarkdownContent
                        |> Maybe.withDefault "Failed to load!"
                        |> Markdown.toHtml

                debug =
                    model.debug
            in
                ( Model model.blogListFile model.blogs currBlog debug model.currId
                , Effects.none
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
                ( Model model.blogListFile model.blogs model.currBlog debug model.currId
                , getContent blogMarkdownFile
                )

        HoverBlog id ->
            ( model
            , Effects.none
            )

        Tick t ->
            ( model
            , Effects.none
            )

-- VIEW

classStyle : Html.Attribute
classStyle = class "bloglist"

view : Signal.Address Action -> Model -> Html
view address model =
    let
        numCols = 2

        blogs =
            model.blogs
                |> List.map (\b -> viewBlog address b)
                |> composeTiledHtml numCols

        viewCurrBlog =
            div [ style [ ("border-style", "solid") ] ]
                [ model.currBlog ]
    in
        div []
            [ viewCurrBlog
            , h3 [] [ text ("debug: " ++ model.debug ++ " currId: " ++ toString model.currId) ]
            , table [ classStyle ] blogs
            ]

viewBlog : Signal.Address Action -> Blog -> Html
viewBlog address blog =
    let
        attributes =
            [ classStyle
            , onClick address (FocusBlog blog.id)
            , onMouseMove address (HoverBlog blog.id)
            ]

        titleContent =
            [ h3 [ style [ ("text-align", "center") ] ] [ text blog.title ] ]

        dateContent =
            [ h4 [ style [ ("text-align", "center") ] ] [ text <| Maybe.withDefault "" blog.date ] ]

        keywordContent =
            let
                allKeywords =
                    blog.keywords
                        |> Maybe.withDefault []
                        |> List.intersperse ", "
                        |> List.foldl (++) ""
            in
                [ p [] [text allKeywords] ]

        break = [ br [] [] ]

        allContent =
            titleContent
            ++ dateContent
            ++ keywordContent
    in
        th attributes allContent

inputs : Signal Action
inputs =
    Signal.mergeMany
        [ Signal.map Tick <| Time.every <| Time.minute
        ]

-- EFFECTS

getBlogList : String -> Effects Action
getBlogList location =
    Http.get decodeBlogList location
        |> Task.toMaybe
        |> Task.map LoadBlogList
        |> Effects.task

getContent : String -> Effects Action
getContent location =
    Http.getString location
        |> Task.toMaybe
        |> Task.map LoadBlogMarkdown
        |> Effects.task

decodeBlog : Decoder (Blog)
decodeBlog =
    object5 Blog
        ("title" := string)
        (succeed -1)
        (maybe ("date" := string))
        (maybe ("keywords" := (list string)))
        (maybe ("url" := string))

decodeBlogList : Decoder (BlogList)
decodeBlogList =
    object1 BlogList
        <| "blogs" := (list decodeBlog)
