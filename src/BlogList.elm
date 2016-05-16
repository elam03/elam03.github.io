module BlogList exposing (..)

import Array
import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class, href, src, style)
import Http
import Json.Decode exposing (..)
import Markdown
import Platform.Cmd
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
    , currBlog : Html Msg
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

init : String -> (Model, Cmd Msg)
init blogList =
  ( Model blogList [] (text "") "debug!" 0
  , getBlogList blogList
  )

-- UPDATE

type Msg
    = LoadBlogList BlogList
    | LoadBlogMarkdown String
    | FocusBlog ID
    | HoverBlog ID
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
                        -- |> Maybe.withDefault (BlogList [])
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
                        -- |> Maybe.withDefault "Failed to load!"
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
                ( Model model.blogListFile model.blogs model.currBlog debug model.currId
                , getContent blogMarkdownFile
                )

        HoverBlog id ->
            (model, Cmd.none)

        FetchFail _ ->
            (model, Cmd.none)

        Tick t ->
            (model, Cmd.none)

-- VIEW

classStyle : Html.Attribute Msg
classStyle = class "bloglist"

view : Model -> Html Msg
view model =
    let
        numCols = 2

        blogs =
            model.blogs
                |> List.map (\b -> viewBlog b)
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

viewBlog : Blog -> Html Msg
viewBlog blog =
    let
        attributes =
            [ classStyle
            , onClick (FocusBlog blog.id)
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

-- inputs : Signal Msg
-- inputs =
--     Signal.mergeMany
--         [ Signal.map Tick <| Time.every <| Time.minute
--         ]

-- EFFECTS

getBlogList : String -> Cmd Msg
getBlogList location =
    Http.get decodeBlogList location
        -- |> Task.perform (x -> msg) (a -> msg) Task.Task x a
        -- |> Task.toMaybe
        -- |> Task.map LoadBlogList -- Task never (Maybe LoadBlogList)
        |> Task.perform FetchFail LoadBlogList
        -- |> performSucceed LoadBlogList
        -- |> Task.perform (x -> msg) (a -> msg) Task.Task x a


getContent : String -> Cmd Msg
getContent location =
    Http.getString location
        -- |> Task.toMaybe
        -- |> Task.map LoadBlogMarkdown
        |> Task.perform FetchFail LoadBlogMarkdown
        -- |> performSucceed  LoadBlogMarkdown
        -- |> Cmd.task

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
