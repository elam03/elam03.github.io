module BlogList where

import Array
import Char
import Effects exposing (Effects, Never)
import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class, href, src, style)
import Http
import Json.Decode exposing (..)
import Keyboard
import Markdown
-- import Mouse
import Set
import Signal
import Task
import Utils exposing (..)

-- import Svg exposing (svg, rect, g)
-- import Svg.Attributes
-- import Svg.Events exposing (onClick)

(=>) : a -> b -> (a, b)
(=>) = (,)

-- MODEL

type alias ID = Int
type alias Keys = Set.Set Char.KeyCode

type alias BlogList =
    { blogs : List Blog
    }

type alias Blog =
    { title : String
    , id : ID
    , keywords : Maybe (List String)
    , content : Maybe (List String)
    , url : Maybe String
    }

type alias Model =
    { file : String
    , blogs : List Blog
    , currBlog : List Html
    , debug : String
    , currId : ID
    }

errorBlogList : BlogList
errorBlogList =
    { blogs = [ errorBlog ]
    }

errorBlog : Blog
errorBlog =
    { title = "error"
    , id = 0
    , keywords = Just ["error"]
    , content = Just ["error", "error"]
    , url = Just ""
    }

init : String -> (Model, Effects Action)
init blogList =
  ( Model blogList [] [] "debug!" 0
  , getBlogData blogList
  )

-- UPDATE

type Action
    = RequestRefresh
    | Refresh (Maybe BlogList)
    | KeyDown Keys
    | LoadBlog (Maybe String)
    | FocusBlog ID
    -- | MouseDown (Int, Int)

isDown : Keys -> Char -> Bool
isDown keys keyCode =
    Set.member (Char.toCode keyCode) keys

update : Action -> Model -> (Model, Effects Action)
update action model =
    case action of
        RequestRefresh ->
            ( model
            , getBlogData model.file
            )

        Refresh maybeBlogsList ->
            let
                blogList =
                    maybeBlogsList
                        |> Maybe.withDefault errorBlogList

                blogs =
                    blogList.blogs
            in
                if List.isEmpty blogs then
                    ( Model model.file blogs model.currBlog model.debug model.currId
                    , Effects.none
                    )
                else
                    ( Model model.file blogs model.currBlog model.debug model.currId
                    , Effects.none
                    )

        KeyDown keys ->
            let
                actions =
                    [ getContent "assets/blogs/test_post.md"
                    , getContent "assets/blogs/test_post2.md"
                    , getContent "assets/blogs/test_post3.md"
                    ]
            in
                -- Found my solution to multiple effects here:
                -- https://groups.google.com/forum/#!searchin/elm-discuss/multiple$20effects/elm-discuss/krHtIuW8ub8/HsH0gI8iCgAJ
                -- TLDR; use Effects.batch
                if isDown keys 'T' then
                    ( model
                    , Effects.batch actions
                    )
                else if isDown keys 'Y' then
                    ( model
                    , actions
                        |> List.reverse
                        |> Effects.batch
                    )
                else
                    ( model
                    , Effects.none
                    )

        LoadBlog maybeContent ->
            let
                currBlog =
                    maybeContent
                        |> Maybe.withDefault "Failed to load!"
                        |> Markdown.toHtml

                debug =
                    model.debug
                    -- toString model.currId
            in
                ( Model model.file model.blogs [ currBlog ] debug model.currId
                , Effects.none
                )

        -- MouseDown (x,y) ->
        --     let
        --         s = "(" ++ (toString x) ++ "," ++ (toString y) ++ ")"
        --     in
        --         -- ( Model model.file model.blogs model.currBlog s
        --         ( Model model.file model.blogs model.currBlog model.debug model.currId
        --         , Effects.none
        --         )

        FocusBlog id ->
            let
                index = id

                blog =
                    model.blogs
                        |> Array.fromList
                        |> Array.get index
                        |> Maybe.withDefault errorBlog

                file =
                    blog.url
                        |> Maybe.withDefault ""

                debug =
                    -- model.debug
                    -- file
                    -- toString index
                    toString <| List.length model.blogs
            in
                ( Model model.file model.blogs model.currBlog debug model.currId
                , getContent file
                )


-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
    let
        numBlogs = toString (List.length model.blogs)

        blogs =
            model.blogs
                |> viewBlogs address

        viewCurrBlog =
            div [ style [ ("border-style", "solid") ] ]
                model.currBlog
    in
        div []
            -- [ h3 [] [ text model.debug ]
            [ viewCurrBlog
            , h3 [] [ text ("debug: " ++ model.debug ++ " currId: " ++ toString model.currId) ]
            , button [ onClick address (FocusBlog 48) ] [ text "FocusBlog Test!"]
            , blogs
            ]

viewBlogs : Signal.Address Action -> List Blog -> Html
viewBlogs address blogs =
    let
        numCols = 1
        classname = "bloglist"
        attributes =
            [ class (classname ++ "-item")
            , onClick address (FocusBlog 1)
            ]

        blogs' =
            blogs
                |> composeTiledHtml attributes viewBlog numCols
    in
        table [ class classname ]
            blogs'

viewBlog : Blog -> List Html
viewBlog blog =
    let
        titleContent =
            [ h3 [ style [ ("text-align", "center") ] ] [ text blog.title ] ]

        content =
            blog.content
                |> Maybe.withDefault []
                |> List.map (\c -> p [ style [ ("text-align", "left") ] ] [ text c ])

        url =
            [ p [] [ text (blog.url |> Maybe.withDefault "") ] ]

        break = [ br [] [] ]

        allContent =
            titleContent
            ++ content
            ++ url
    in
        allContent

inputs : Signal Action
inputs =
    Signal.mergeMany
        [ Signal.map KeyDown Keyboard.keysDown
        -- , Signal.map MouseDown Mouse.position
        ]

-- EFFECTS

getBlogData : String -> Effects Action
getBlogData location =
    Http.get decodeData location
        |> Task.toMaybe
        |> Task.map Refresh
        |> Effects.task

decodeData : Decoder (BlogList)
decodeData =
    object1 BlogList
        ( "blogs" :=
            ( list
                <| object5 Blog
                    ("title" := string)
                    (succeed 0)
                    (maybe ("keywords" := (list string)))
                    (maybe ("content" := (list string)))
                    (maybe ("url" := string))
            )
        )

getContent : String -> Effects Action
getContent location =
    Http.getString location
        |> Task.toMaybe
        |> Task.map LoadBlog
        |> Effects.task
