module Building exposing (..)

import Color exposing (..)
import Collage exposing (..)
import List exposing (..)

-------------------------------------------------------------------------------
-- Common/Constants
-------------------------------------------------------------------------------

type Layer = Front | Middle | Back | Static
type alias Size = { w : Float, h : Float }

glassWindowSize : Size
glassWindowSize = { w = 5, h = 9 }

glassWindowSpacing : Size
glassWindowSpacing = { w = 3, h = 3 }

buildingWidth : number
buildingWidth = 30

isBack : Building -> Bool
isBack b = b.layer == Back

isMiddle : Building -> Bool
isMiddle b = b.layer == Middle

isFront : Building -> Bool
isFront b = b.layer == Front

isStatic : Building -> Bool
isStatic b = b.layer == Static

-------------------------------------------------------------------------------
-- GlassWindow
-------------------------------------------------------------------------------

type alias GlassWindow =
    { x : Float
    , y : Float
    , w : Float
    , h : Float
    }

newGlassWindow : Float -> Float -> Float -> Float -> GlassWindow
newGlassWindow x_ y_ w_ h_ =
    {   x = x_
    ,   y = y_
    ,   w = w_
    ,   h = h_
    }

generateGlassWindows : Float -> Float -> Float -> Float -> Float -> Float -> List GlassWindow -> List GlassWindow
generateGlassWindows x y w h xSpacing ySpacing windows =
    if (x + glassWindowSize.w + xSpacing) > w then
        windows
    else if (y + glassWindowSize.h + ySpacing) > h then
        generateGlassWindows (x + glassWindowSize.w + xSpacing) ySpacing w h xSpacing ySpacing windows
    else
        generateGlassWindows x (y + glassWindowSize.h + ySpacing) w h xSpacing ySpacing (windows ++ [newGlassWindow x y glassWindowSize.w glassWindowSize.h])

-------------------------------------------------------------------------------
-- Building
-------------------------------------------------------------------------------

type alias Building =
    { x : Float
    , y : Float
    , w : Float
    , h : Float
    , layer : Layer
    , windows : List GlassWindow
    }

newBuilding : Float -> Float -> Float -> Layer -> Building
newBuilding x_ y_ h_ l_ =
    let
        w_ = buildingWidth
    in
        { nullBuilding |
          x = x_
        , y = y_
        , w = w_
        , h = h_
        , layer = l_
        , windows = generateGlassWindows glassWindowSpacing.w glassWindowSpacing.h w_ h_ glassWindowSpacing.w glassWindowSpacing.h []
        }

nullBuilding : Building
nullBuilding =
    { x = 0
    , y = 0
    , w = buildingWidth
    , h = 0
    , layer = Front
    , windows = []
    }


clearGrey : Color
clearGrey =
    let
        c = 90
    in
        rgba c c c 0.8

darkGrey : Color
darkGrey =
    rgba 50 50 50 0.75

glassWindowToForm : GlassWindow -> Form
glassWindowToForm window =
    rect window.w window.h
        |> filled darkGrey
        |> move (window.x + window.w / 2, window.y + window.h / 2)

viewBuilding : Building -> Form
viewBuilding b =
    let windows = List.map (\a -> { a | x = a.x + b.x, y = a.y + b.y } ) b.windows
            |> List.map glassWindowToForm

        allForms =
            [   rect b.w b.h
                    |> filled clearGrey
                    |> move (b.x + b.w / 2, b.y + b.h / 2)
            ] ++ windows
    in
        group allForms
