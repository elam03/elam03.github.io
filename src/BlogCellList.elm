module BlogCellList where

import BlogCell

import Effects exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)

-- MODEL
type alias ID = Int

type alias Model =
    { blogCells : List (ID, BlogCell.Model)
    , nextID : ID
    }

init : (Model, Effects Action)
init =
    let
        model =
            { blogCells = []
            , nextID = 0
            }
    in
        ( model, Effects.none )

-- UPDATE

type Action
    = Load
    | Clicked ID

update : Action -> Model -> (Model, Effects Action)
update action model =
    case action of
        Load ->
            ( model, Effects.none )
        Clicked id ->
            ( model, Effects.none )

-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
    let
        a = a
        -- blogCells =
        --     model.blogCells
        --         |> List.map (viewBlogCell address)

    in
        div [ style [("border-style", "solid")] ]
            [ h1 []
                [ text "BlogCellList" ]
            , div [ style [("border-style", "solid")] ]
                [ text "curr" ]
            , div [ style [("border-style", "solid")] ]
                [ text "all cells" ]
            -- blogCells
            ]

-- viewBlogCell : Signal.Address Action -> (ID, BlogCell.Model) -> Html
-- viewBlogCell address (id, model) =
--     let
--         context =
--             BlogCell.Context
--                 (Signal.forwardTo address (Clicked id))
--     in
--         BlogCell.view context model
