module Cityscape exposing (..)

import Array
import Char
import Color exposing (..)
import Collage exposing (..)
import Element exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Mouse exposing (Position)
import Keyboard exposing (..)
import Random
import Set
import Time exposing (..)
import Window exposing (Size)

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
    ,   keys : Set.Set KeyCode
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
        , keys = Set.empty
        , t = 0
        , dt = 7
        , seed = Random.initialSeed 42
        , buildings = []
        , numBuildingsToAdd = 10
        , randomValues = Array.fromList []
        , windowWidth = w
        , windowHeight = h
        , movementType = TimeMove
        , sunset = { y = -(toFloat h / 3), h = (toFloat h / 4) }
        , showInfo = True
        }
    in
        ( model
        , Cmd.none
        )

-- UPDATE

type Msg
    = None
    | KeyDown KeyCode
    | KeyUp KeyCode
    | Move Mouse.Position
    | Size Window.Size
    | Tick Time
    | NewRandomValues (List Float)

update : Msg -> Model -> (Model, Cmd Msg)
update action model =
    case action of
        None ->
            ( model, Cmd.none )
        Move xy ->
            ( model
                |> mouseUpdate (xy.x, xy.y)
            , Cmd.none
            )
        Size s ->
            ( model, Cmd.none )
            -- ( { model
            --   | windowWidth = s.width
            --   , windowHeight = s.height
            --   }
            -- , Cmd.none
            -- )

        KeyDown key ->
            let
                model' = { model | keys = Set.insert key model.keys }
            in
                case Char.fromCode key of
                    'B' ->
                        ( { model' | numBuildingsToAdd = 1 }, Cmd.none )
                    'M' ->
                        ( { model' | movementType = MouseMove }, Cmd.none )
                    'T' ->
                        ( { model' | movementType = TimeMove }, Cmd.none )
                    'S' ->
                        ( { model' | movementType = StaticMove }, Cmd.none )
                    'I' ->
                        ( { model' | showInfo = not model'.showInfo }, Cmd.none )
                    _ ->
                        ( model', Cmd.none )

        KeyUp key ->
            let
                model' = { model | keys = Set.remove key model.keys }
            in
                ( model', Cmd.none )

        Tick t ->
            let
                dt = t - model.t
            in
                ( model
                    |> timeUpdate dt t
                    |> addBuildingsUpdate
                    |> updateBuildingsInModel
                    |> resetMouseDelta
                , Random.generate NewRandomValues (Random.list 100 (Random.float 0 1))
                )

        NewRandomValues list ->
            ( { model | randomValues = Array.fromList list }
            , Cmd.none
            )

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

timeUpdate : Float -> Float -> Model -> Model
timeUpdate dt t model =
    { model | dt = dt
    ,         t = t
    }

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every (20 * millisecond) Tick
        , Mouse.moves Move
        , Window.resizes Size
        , Keyboard.downs KeyDown
        , Keyboard.ups KeyUp
        ]

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
            keys =
                model.keys
                    |> Set.toList
                    |> List.map (\key -> Char.fromCode key)
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
