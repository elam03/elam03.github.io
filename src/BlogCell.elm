module BlogCell (Model, init, Action, update, view, Context) where

-- import Char
import Effects exposing (Effects, Never)
import Html exposing (..)
-- import Html.Attributes exposing (class, href, src, style)
import Html.Events exposing (..)
import Http
-- import Json.Decode exposing (..)
-- import Markdown
-- import Set
import Signal
import Task
-- import Utils exposing (..)

-- import Svg exposing (svg, rect, g)
-- import Svg.Attributes
-- import Svg.Events exposing (onClick)

(=>) : a -> b -> (a, b)
(=>) = (,)

-- MODEL

type alias Model =
    { file : String
    , title : String
    , keywords : Maybe (List String)
    , content : Maybe (List String)
    , url : Maybe String
    }

-- errorBlog : BlogCell
-- errorBlog =
--     { file = "error"
--     , title = "error"
--     , keywords = Just ["error"]
--     , content = Just ["error", "error"]
--     , url = Just ""
--     }
init' : String -> Model
init' file =
    { file = file
    , title = ""
    , keywords = Just []
    , content = Just []
    , url = Just ""
    }


init : String -> (Model, Effects Action)
init file =
    let
        model = init' file
    in
        ( model
        , getContent model.file
        )

-- UPDATE

type Action
    = Load (Maybe String)

update : Action -> Model -> (Model, Effects Action)
update action model =
    case action of
        -- RequestRefresh ->
        --     ( model
        --     , getBlogData model.file
        --     )

        Load maybeBlog ->
            ( model
            , Effects.none
            )
            -- let
            --     blogList =
            --         maybeBlogsList
            --             |> Maybe.withDefault errorBlogList
            --
            --     blogs =
            --         blogList.blogs
            -- in
            --     if List.isEmpty blogs then
            --         ( Model model.file blogs model.currBlog model.debug
            --         , Effects.none
            --         )
            --     else
            --         ( Model model.file blogs model.currBlog model.debug
            --         , Effects.none
            --         )
        --
        -- LoadBlog maybeContent ->
        --     let
        --         currBlog =
        --             maybeContent
        --                 |> Maybe.withDefault "Failed to load!"
        --                 |> Markdown.toHtml
        --
        --     in
        --         ( Model model.file model.blogs [ currBlog ] model.debug
        --         , Effects.none
        --         )

-- VIEW

type alias Context =
    { actions : Signal.Address Action
    }

view : Signal.Address Action -> Model -> Html
view address model =
    div []
        [ button [ Html.Events.onClick address (Load (Just model.file)) ] [ text "Load" ]
        , div [] [ text (toString model) ]
        ]
    -- let
    --     numBlogs = toString (List.length model.blogs)
    --
    --     blogs =
    --         model.blogs
    --             |> viewBlogs
    --
    --     viewCurrBlog =
    --         div [ style [ ("border-style", "solid") ] ]
    --             model.currBlog
    -- in
    --     div []
    --         -- [ h3 [] [ text model.debug ]
    --         [ viewCurrBlog
    --         , h3 [] [ text model.debug ]
    --         , blogs
    --         ]

-- viewBlog : BlogCell -> List Html
-- viewBlog blog =
--     let
--         titleContent =
--             [ h3 [ style [ ("text-align", "center") ] ] [ text blog.title ] ]
--
--         content =
--             blog.content
--                 |> Maybe.withDefault []
--                 |> List.map (\c -> p [ style [ ("text-align", "left") ] ] [ text c ])
--
--         url =
--             [ p [] [ text (blog.url |> Maybe.withDefault "") ] ]
--
--         break = [ br [] [] ]
--
--         allContent =
--             titleContent
--             ++ content
--             ++ url
--     in
--         allContent

-- inputs : Signal Action
-- inputs =
--     Signal.mergeMany
--         [ Signal.map KeyDown Keyboard.keysDown
--         , Signal.map MouseDown Mouse.position
--         ]

-- EFFECTS

-- getBlogData : String -> Effects Action
-- getBlogData location =
--     Http.get decodeData location
--         |> Task.toMaybe
--         |> Task.map Load
--         |> Effects.task
--
-- decodeData : Decoder (BlogList)
-- decodeData =
--     object1 Model
--         ( "blogs" :=
--             ( list
--                 <| object4 BlogCell
--                     ("title" := string)
--                     (maybe ("keywords" := (list string)))
--                     (maybe ("content" := (list string)))
--                     (maybe ("url" := string))
--             )
--         )

getContent : String -> Effects Action
getContent location =
    Http.getString location
        |> Task.toMaybe
        |> Task.map Load
        |> Effects.task
