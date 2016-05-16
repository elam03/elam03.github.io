module Cityscape exposing (..)

import Array
import Char
import Color exposing (..)
import Collage exposing (..)
import Element exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Random
import Set
import Time exposing (..)

import Building exposing (..)

-- MODEL
type alias Keys = Set.Set Char.KeyCode
type MovementType = TimeMove | MouseMove | StaticMove | NullMove

type alias Sunset =
    { y : Float
    , h : Float
    }

type alias Model =
    {   x : Float
    ,   y : Float
    ,   dx : Float
    ,   dy : Float
    ,   kx : Int
    ,   ky : Int
    ,   keys : List Char.KeyCode
    ,   t : Float
    ,   dt : Float
    ,   seed : Random.Seed
    ,   buildings : List Building
    ,   numBuildingsToAdd : Int
    ,   randomValues : Array.Array Float
    ,   windowWidth : Int
    ,   windowHeight : Int
    ,   movementType : MovementType
    ,   sunset : Sunset
    ,   showInfo : Bool
    }

init : (Int, Int) -> (Model, Cmd Msg)
init (w, h) =
    let model =
        { x = 0
        , y = 0
        , dx = 0
        , dy = 0
        , kx = 0
        , ky = 0
        , keys = []
        , t = 0
        , dt = 0
        , seed = Random.initialSeed 42
        , buildings = []
        , numBuildingsToAdd = 10
        , randomValues = Array.fromList []
        , windowWidth = w
        , windowHeight = h
        , movementType = TimeMove
        , sunset = { y = -(toFloat h / 3), h = (toFloat h / 4) }
        , showInfo = False
        }
    in
        ( model
        , Cmd.none
        )

-- UPDATE

type Msg
    = None
    | TimeDelta Float
    | KeyDown Keys
    | MouseMoved (Int, Int)
    | WindowSizeChange (Int, Int)

update : Msg -> Model -> (Model, Cmd Msg)
update action model =
    case action of
        None ->
            ( model, Cmd.none )
        MouseMoved (mx, my) ->
            ( model |> mouseUpdate (mx, my), Cmd.none )
        KeyDown keys ->
            ( model |> keysUpdate keys, Cmd.none )
        WindowSizeChange (ww, wh) ->
            ( model, Cmd.none )
        TimeDelta dt ->
            ( model |> timeUpdate dt |> randomUpdate |> addBuildingsUpdate |> updateBuildingsInModel |> resetMouseDelta, Cmd.none )

updateBuildings : Float -> Float -> Int -> List Building -> List Building
updateBuildings dx dt windowWidth buildings =
    let
        frontSpeed  = 25 * dx -- px/sec
        middleSpeed = frontSpeed * 2
        backSpeed   = frontSpeed * 3
        staticSpeed = 0

        backBuildings = buildings
                            |> List.filter isBack
                            |> List.map (\b -> { b | x = b.x + backSpeed / dt } )
        middleBuildings = buildings
                            |> List.filter isMiddle
                            |> List.map (\b -> { b | x = b.x + middleSpeed / dt } )
        frontBuildings = buildings
                            |> List.filter isFront
                            |> List.map (\b -> { b | x = b.x + frontSpeed / dt } )

        allBuildings = (backBuildings ++ middleBuildings ++ frontBuildings)

        updatedBuildings = allBuildings
            |> wrapBuildings windowWidth
    in
        updatedBuildings

resetMouseDelta : Model -> Model
resetMouseDelta model =
    { model | dx = 0
    ,         dy = 0
    }

updateBuildingsInModel : Model -> Model
updateBuildingsInModel model =
    case model.movementType of
        MouseMove ->
            let
                updatedBuildings = updateBuildings model.dx model.dt model.windowWidth model.buildings
            in
                { model | buildings = updatedBuildings }
        TimeMove ->
            let
                updatedBuildings = updateBuildings 1 model.dt model.windowWidth model.buildings
            in
                { model | buildings = updatedBuildings }
        StaticMove ->
            model
        _ ->
            model

wrapBuildings : Int -> List Building -> List Building
wrapBuildings widthWrap buildings =
    let
        w = toFloat widthWrap
        checkRightEdge b = if b.x >  (w / 2) then { b | x = b.x - b.w - w } else b
        checkLeftEdge  b = if b.x < -(w / 2 + b.w) then { b | x = b.x + b.w + w } else b
    in
        buildings
            |> List.map checkRightEdge
            |> List.map checkLeftEdge

updateWindowDimensions : (Int, Int) -> Model -> Model
updateWindowDimensions (w, h) model =
    { model | windowWidth = w, windowHeight = h }

randomUpdate : Model -> Model
randomUpdate model =
    model
    -- let generator = Random.list 100 (Random.float 0 1)
    --     tuple = Random.generate generator model.seed
    --     vs = Array.fromList (fst tuple)
    --     newSeed = snd tuple
    --     modifiedModel =
    --         { model | seed = newSeed
    --         ,         randomValues = vs
    --         }
    -- in
    --     modifiedModel

increment : Bool -> Int
increment condition =
    case condition of
        True -> 1
        False -> 0

isDown : Keys -> Char -> Bool
isDown keys keyCode =
    Set.member (Char.toCode keyCode) keys

toMovementType : Char.KeyCode -> MovementType
toMovementType code =
    case (Char.fromCode code) of
        'M' -> MouseMove
        'T' -> TimeMove
        'S' -> StaticMove
        _   -> NullMove

getMovementType : Keys -> List MovementType
getMovementType keys =
    Set.toList keys
        |> List.map toMovementType
        |> List.filter (\a -> a /= NullMove)

keysUpdateMovementType : Keys -> Model -> Model
keysUpdateMovementType keys model =
    let
        processedKeys = getMovementType keys
    in
        if processedKeys |> List.isEmpty then
            model
        else
            { model | movementType = processedKeys |> List.head |> Maybe.withDefault NullMove }

keysUpdateAddBuildings : Keys -> Model -> Model
keysUpdateAddBuildings keys model =
    { model | numBuildingsToAdd = model.numBuildingsToAdd + increment (isDown keys '1') }

keysUpdateModel : Keys -> Model -> Model
keysUpdateModel keys model =
    { model | keys = Set.toList keys }

keysUpdateToggleInfo : Keys -> Model -> Model
keysUpdateToggleInfo keys model =
    if (isDown keys 'I') then
        { model | showInfo = not model.showInfo }
    else
        model

keysUpdate : Keys -> Model -> Model
keysUpdate keys model =
    model
        |> keysUpdateModel keys
        |> keysUpdateAddBuildings keys
        |> keysUpdateMovementType keys
        |> keysUpdateToggleInfo keys

getNumBuildings : Model -> Int
getNumBuildings model =
    List.length model.buildings

getRandomValues : Model -> Int -> Array.Array Float
getRandomValues model numValues =
    Array.slice 0 numValues model.randomValues

popRandomValues : Int -> Model -> Model
popRandomValues numOfValuesToPop model =
    { model | randomValues = Array.slice numOfValuesToPop (Array.length model.randomValues) model.randomValues
    }

toValue : Float -> Float -> Maybe Float -> Float
toValue min max v =
    (Maybe.withDefault 0.5 v) * (max - min) + min

addBuilding : Model -> Building -> Model
addBuilding model building =
    { model | buildings = model.buildings ++ [ building ] }

reduceNewBuildingCount : Model -> Model
reduceNewBuildingCount model =
    { model | numBuildingsToAdd = model.numBuildingsToAdd - 1 }

pickLayer : Float -> Layer
pickLayer value =
    if value >= 66 then
        Front
    else if value >= 33 then
        Middle
    else
        Back

addBuildingsUpdate : Model -> Model
addBuildingsUpdate model =
    if model.numBuildingsToAdd > 0 then
        let
            ww = toFloat (model.windowWidth // 2)
            wh = toFloat (model.windowHeight // 2)
            randomValues = getRandomValues model 3

            x = toValue -ww ww <| Array.get 0 randomValues
            y = model.sunset.y
            h = toValue 25 100 <| Array.get 1 randomValues
            l = pickLayer <| toValue 0 100 <| Array.get 1 randomValues

            modifiedModel = popRandomValues 3 model
        in
            newBuilding x y h l |> addBuilding modifiedModel |> reduceNewBuildingCount
    else
        model

mouseUpdate : (Int, Int) -> Model -> Model
mouseUpdate (mx, my) model =
    let
        (px, py) = (model.x, model.y)
    in
        { model | x  = toFloat mx
        ,         y  = toFloat my
        ,         dx = toFloat mx - px
        ,         dy = toFloat my - py
        }

timeUpdate : Float -> Model -> Model
timeUpdate dt model =
    { model |   t = model.t + dt
    ,           dt = dt
    }

-- inputs : Signal Msg
-- inputs =
--     mergeMany
--         [ Signal.map MouseMoved Mouse.position
--         , Signal.map KeyDown Keyboard.keysDown
--         -- , Signal.map TimeDelta (Signal.map (\t -> t / 20) (fps 30))
--         , Signal.map TimeDelta (fps 30)
--         , Signal.map WindowSizeChange Window.dimensions
--         ]

grad : Float -> Float -> Gradient
grad y h =
    let
        split = 0.75
        center = y - (h * (1.0 - split))
    in
        linear (0, center + h) (0, center)
            [ (0.0,   rgb 0 171 235)  -- blue
            , (split, rgb 255 192 64) -- yellow/orange
            , (split, brown)          -- brown
            , (1.0,   rgb 38 192 0)   -- green
            ]

viewSunset : Model -> Form
viewSunset model =
    let
        w = toFloat model.windowWidth
        h = toFloat model.windowHeight
    in
        gradient (grad model.sunset.y model.sunset.h) (rect w h)

view : Model -> Html Msg
view model =
    let
        (mx, my) = (model.x, -model.y)

        allBuildings = model.buildings |> List.map displayBuilding
        sunset = model |> viewSunset

        things =
            [ sunset ]
            ++ allBuildings
            ++ (displayModelInfo model)
            ++ (displayMouseCursor (mx, my) model)

        finalOutput = collage model.windowWidth model.windowHeight things
            |> Element.toHtml
    in
        Html.div
            [ style [ ("width", (toString model.windowWidth) ++ "px"), ("height", (toString model.windowHeight) ++ "px"), ("border-style", "solid") ] ]
            [ finalOutput ]

displayMouseCursor : (Float, Float) -> Model -> List Form
displayMouseCursor (x, y) model =
    let p = (x - (toFloat model.windowWidth / 2), y + (toFloat model.windowHeight / 2))
        a = model.t
    in
        [ ngon 3 5 |> filled red |> move p |> rotate (degrees a)]

displayModelInfo : Model -> List Form
displayModelInfo model =
    if model.showInfo then
        let m = (model.x, model.y)
            d = (model.dx, model.dy)
            t = round model.t
            dt = round model.dt
            keys = List.map (\key -> Char.fromCode key) model.keys
            (ww, wh) = (toFloat model.windowWidth, toFloat model.windowHeight)
            firstBuilding = List.head model.buildings |> Maybe.withDefault nullBuilding
            movementType = model.movementType

            allInfo =
                    [ show ("(x,y): " ++ toString m)
                    , show ("(dx,dy): " ++ toString d)
                    , show ("t: " ++ toString t)
                    , show ("dt: " ++ toString dt)
                    , show ("keys: " ++ toString keys)
                    , show ("(ww, wh): " ++ toString (ww, wh))
                    , show ("buildings: " ++ toString (List.length model.buildings))
                    , show ("first building: " ++ toString (round firstBuilding.x))
                    , show ("movementType: " ++ toString (movementType))
                    ]

            randomValues = model.randomValues
                                |> Array.toList
                                |> List.map2 (,) [1..(Array.length model.randomValues)]
                                |> List.map displayRandomValue

            formsToDisplay = [ toForm <| flow down allInfo ] ++ randomValues
        in
            formsToDisplay |> List.map (\a -> (a |> move (-ww / 2 + 80, wh / 2 - 100)))
    else
        []

displayRandomValue : (Int, Float) -> Form
displayRandomValue (x', value') =
    let x = toFloat x'
    in
        traced (solid red) (path [ (x, 0), (x, value' * 100) ])
