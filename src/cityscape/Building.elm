module Building where

import Color exposing (..)
import Graphics.Collage exposing (..)
import List exposing (..)

-------------------------------------------------------------------------------
-- Common/Constants
-------------------------------------------------------------------------------

type Layer = Front | Middle | Back | Static
type alias Size = { w : Float, h : Float }

glassWindowSize : Size
glassWindowSize = { w = 10, h = 30 }

glassWindowSpacing : Size
glassWindowSpacing = { w = 6, h = 15 }

buildingWidth : number
buildingWidth = 40

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
newGlassWindow x' y' w' h' =
    {   x = x'
    ,   y = y'
    ,   w = w'
    ,   h = h'
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

newBuilding : Float -> Float -> Layer -> Building
newBuilding x' h' l' =
    let
        w' = buildingWidth
    in
        { nullBuilding |
          x = x'
        , y = 0
        , w = w'
        , h = h'
        , layer = l'
        , windows = generateGlassWindows glassWindowSpacing.w glassWindowSpacing.h w' h' glassWindowSpacing.w glassWindowSpacing.h []
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
    rgba 111 111 111 0.6

darkGrey : Color
darkGrey =
    rgba 50 50 50 0.6

glassWindowToForm : GlassWindow -> Form
glassWindowToForm window =
    rect window.w window.h
        |> filled darkGrey
        |> move (window.x + window.w / 2, window.y + window.h / 2)

displayBuilding : Building -> Form
displayBuilding b =
    let windows = List.map (\a -> { a | x = a.x + b.x, y = a.y + b.y } ) b.windows
            |> List.map glassWindowToForm

        allForms =
            [   rect b.w b.h
                    |> filled clearGrey
                    |> move (b.x + b.w / 2, b.y + b.h / 2)
            ] ++ windows
    in
        group allForms
