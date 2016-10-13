module Cityscape exposing (..)

import AnimationFrame
import Array
import Char
import Color exposing (..)
import Collage exposing (..)
import Element exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Mouse exposing (Position)
import MouseEvents exposing (MouseEvent, onMouseMove)
import Keyboard exposing (..)
import PageVisibility exposing (..)
import Random
import Set
import Svg
import Svg.Attributes
import Svg.Events
import Task
import Time exposing (Time)
import Window exposing (Size)

import Building exposing (..)

-- MODEL
type MovementType
    = TimeMove
    | MouseMove
    | StaticMove

type alias Sunset =
    { y : Float
    , h : Float
    }

type alias Tree =
    { x : Float
    , y : Float
    , w : Float
    , h : Float
    , layer : Layer
    , trunkWidth : Float
    , color : Color
    , leafEdgeCount : Int

    , currRotationT : Float
    , currRotation : Float
    , angularRotation : Float
    , angularRotationRange : Float
    }

initTree : Tree
initTree =
    let
        model =
            { x = 0
            , y = 0
            , w = 0
            , h = 0
            , layer = Back
            , trunkWidth = 6
            , color = rgb 255 255 255
            , leafEdgeCount = 4
            , currRotationT = 0
            , currRotation = 0
            , angularRotation = 1
            , angularRotationRange = 25
            }
    in
        model

type alias Model =
    {   x : Float
    ,   y : Float
    ,   dx : Float
    ,   dy : Float
    ,   keys : Set.Set KeyCode
    ,   t : Float
    ,   dt : Float
    ,   initialized : Bool
    ,   buildings : List Building
    ,   randomValues : Array.Array Float
    ,   windowWidth : Int
    ,   windowHeight : Int
    ,   movementType : MovementType
    ,   sunset : Sunset
    ,   showInfo : Bool
    ,   trees : List Tree
    ,   paused : Bool
    }

init : (Model, Cmd Msg)
init =
    let
        -- w = Window.width
        w = 300
        h = 200
        model =
        { x = 0
        , y = 0
        , dx = 0
        , dy = 0
        , keys = Set.empty
        , t = 0
        , dt = 0
        , initialized = False
        , buildings = []
        , randomValues = Array.fromList []
        , windowWidth = w
        , windowHeight = h
        , movementType = TimeMove
        , sunset = { y = 0, h = 0 }
        , showInfo = False
        , trees = []
        , paused = False
        }

    in
        ( model, Task.perform Size Size Window.size )

-- UPDATE

type Msg
    = None
    | Move MouseEvent
    | Size Window.Size
    | KeyDown KeyCode
    | KeyUp KeyCode
    | Tick Time
    | NewRandomValues (List Float)
    | VisibilityChange Visibility

update : Msg -> Model -> (Model, Cmd Msg)
update action model =
    case action of
        None ->
            ( model, Cmd.none )
        Move data ->
            let
                x = data.clientPos.x - data.targetPos.x
                y = data.clientPos.y - data.targetPos.y
            in
                ( model |> mouseUpdate (x, y), Cmd.none )
        Size s ->
            let
                w = s.width
                h = 200
                margin = 16 -- This is the margin value for the entire page
            in
                ( { model
                  | windowWidth = w - (margin * 2)
                  , windowHeight = h
                  , sunset = { y = -(toFloat h / 3), h = (toFloat h / 4) }
                  }
                , Cmd.none
                )

        KeyDown key ->
            let
                model' = { model | keys = Set.insert key model.keys }
            in
                case Char.fromCode key of
                    '1' ->
                        ( model' |> addBuildings 10, Cmd.none )
                    '2' ->
                        ( model' |> addTrees 10, Cmd.none )
                    'M' ->
                        ( { model' | movementType = MouseMove }, Cmd.none )
                    'T' ->
                        ( { model' | movementType = TimeMove }, Cmd.none )
                    'S' ->
                        ( { model' | movementType = StaticMove }, Cmd.none )
                    'P' ->
                        ( { model' | paused = not model.paused }, Cmd.none )
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
                if not model.paused then
                    ( model
                        |> timeUpdate dt t
                        |> updateAllEntitiesInModel
                        |> resetMouseDelta
                    , Random.generate NewRandomValues (Random.list 100 (Random.float 0 1))
                    )
                else
                    ( model, Cmd.none )

        NewRandomValues list ->
            let
                setInitialized model =
                    { model | initialized = True }
                model' =
                    { model | randomValues = Array.fromList list }
            in
                if not model'.initialized then
                    let
                        value =
                            model'.windowWidth

                        numInitialBuildings = value // 120
                        numInitialTrees = value // 120
                    in
                        ( model'
                            |> addBuildings numInitialBuildings
                            |> addTrees numInitialTrees
                            |> setInitialized
                        , Cmd.none
                        )
                else
                    ( model', Cmd.none )

        VisibilityChange visible ->
            case visible of
                Visible ->
                    ( { model | paused = False }, Cmd.none )

                Hidden ->
                    ( { model | paused = True }, Cmd.none )

addTrees : Int -> Model -> Model
addTrees numTrees model =
    if numTrees == 0 then
        model
    else
        let
            toValue min max v =
                (Maybe.withDefault 0.5 v) * (max - min) + min

            pickLayer value =
                if value >= 66 then
                    Front
                else if value >= 33 then
                    Middle
                else
                    Back

            popRandomValues numOfValuesToPop model =
                { model | randomValues = Array.slice numOfValuesToPop (Array.length model.randomValues) model.randomValues }

            ww = toFloat model.windowWidth
            x = toValue -ww ww <| Array.get 0 model.randomValues
            y = model.sunset.y
            h = toValue 25 75 <| Array.get 1 model.randomValues
            w = h
            l = pickLayer <| toValue 0 100 <| Array.get 2 model.randomValues

            leafColor =
                let
                    c =
                        model.randomValues
                            |> Array.get 3
                            |> toValue 80 250
                            |> truncate
                in
                    rgba 0 c 0 0.95

            leafEdgeCount =
                model.randomValues
                    |> Array.get 4
                    |> toValue 5 10
                    |> truncate

            tree' =
                { initTree
                | x = x
                , y = y
                , w = w
                , h = h
                , layer = l
                , color = leafColor
                , leafEdgeCount = leafEdgeCount
                }

            model' = popRandomValues 4 model
        in
            { model' | trees = model.trees ++ [ tree' ] }
                |> addTrees (numTrees - 1)

addBuildings : Int -> Model -> Model
addBuildings numBuildings model =
    if numBuildings == 0 then
        model
    else
        let
            getRandomValues model numValues =
                Array.slice 0 numValues model.randomValues

            popRandomValues numOfValuesToPop model =
                { model | randomValues = Array.slice numOfValuesToPop (Array.length model.randomValues) model.randomValues }

            pickLayer value =
                if value >= 66 then
                    Front
                else if value >= 33 then
                    Middle
                else
                    Back

            toValue min max v =
                (Maybe.withDefault 0.5 v) * (max - min) + min

            ww = toFloat (model.windowWidth // 2)
            wh = toFloat (model.windowHeight // 2)
            randomValues = getRandomValues model 3

            x = toValue -ww ww <| Array.get 0 randomValues
            y = model.sunset.y
            h = toValue 25 100 <| Array.get 1 randomValues
            l = pickLayer <| toValue 0 100 <| Array.get 2 randomValues

            model' = popRandomValues 3 model
            building' = newBuilding x y h l
        in
            { model' | buildings = model'.buildings ++ [ building' ] }
                |> addBuildings (numBuildings - 1)

updateAllEntitiesInModel : Model -> Model
updateAllEntitiesInModel model =
    let
        dx =
            case model.movementType of
                MouseMove ->
                    model.dx
                TimeMove ->
                    5
                StaticMove ->
                    0

        toValue layer =
            case layer of
                Front ->
                    0.1
                Middle ->
                    0.2
                Back ->
                    0.3
                Static ->
                    0.0

        wrapThings widthWrap things =
            let
                w = toFloat widthWrap
                checkRightEdge a = if a.x >  (w / 2) then { a | x = a.x - a.w - w } else a
                checkLeftEdge  a = if a.x < -(w / 2 + a.w) then { a | x = a.x + a.w + w } else a
            in
                things
                    |> List.map checkRightEdge
                    |> List.map checkLeftEdge

        moveEntity e =
            { e | x = e.x + dx * (e.layer |> toValue) }

        applySway e =
            let
                keyFrames =
                    [ (0.00, -25)
                    , (0.25, 0)
                    , (0.50, 25)
                    , (0.75, 0)
                    , (1.00, -25)
                    ]

                t =
                    let
                        dt = model.dt / 1000
                        result = e.currRotationT + dt
                    in
                        if result > 1.0 then
                            result - 1.0
                        else
                            result

                lerps keyFrames t =
                    keyFrames
                        |> List.map (\(t', value') -> (abs (t - t')) * value' )
                        |> List.foldl (+) 0

                currRotationT = t
                currRotation = lerps keyFrames currRotationT
                angularRotation = 0
            in
                { e
                | currRotation = currRotation
                , currRotationT = currRotationT
                , angularRotation = angularRotation
                }

        buildings' =
            model.buildings
                |> List.map moveEntity
                |> wrapThings model.windowWidth

        trees' =
            model.trees
                |> List.map moveEntity
                |> List.map applySway
                |> wrapThings model.windowWidth
    in
        { model
        | buildings = buildings'
        , trees = trees'
        }

resetMouseDelta : Model -> Model
resetMouseDelta model =
    { model | dx = 0, dy = 0 }

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
    { model | dt = dt, t = t }

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ PageVisibility.visibilityChanges VisibilityChange
        , AnimationFrame.times Tick
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
        Collage.gradient (grad model.sunset.y model.sunset.h) (Collage.rect w h)

viewTree : Tree -> Form
viewTree t =
    let
        trunk =
            [ Collage.rect t.trunkWidth t.h
                |> filled brown
                |> move (t.x + t.w / 2, t.y + t.h / 2)
            ]

        leaves =
            [ ngon t.leafEdgeCount (t.w / 2)
                |> filled t.color
                |> rotate (degrees t.currRotation)
                |> move (t.x + t.w / 2, t.y + t.h)
            ]
    in
        group <| trunk ++ leaves

view : Model -> Html Msg
view model =
    let
        html = viewWithHtml model
        -- svg = viewWithSvg model
    in
        div []
            [ html
            -- , svg
            ]

viewWithHtml : Model -> Html Msg
viewWithHtml model =
    let
        (mx, my) = (model.x, -model.y)

        allBuildings =
            model.buildings
                |> List.map viewBuilding

        sunset =
            model
                |> viewSunset

        trees =
            model.trees
                |> List.map viewTree

        things =
            [ sunset ]
            ++ trees
            ++ allBuildings
            ++ (displayModelInfo model)
            ++ (displayMouseCursor (mx, my) model)

        finalOutput = collage model.windowWidth model.windowHeight things
            |> Element.toHtml

        attributes =
            [ class "cityscape"
            , onMouseMove Move
            ]
    in
        div attributes [ finalOutput ]

viewWithSvg : Model -> Html Msg
viewWithSvg model =
    Svg.svg [ Svg.Attributes.width "120", Svg.Attributes.height "120", Svg.Attributes.viewBox "0 0 120 120" ]
        [ Svg.rect [ Svg.Attributes.rotate "45", Svg.Attributes.x "10", Svg.Attributes.y "10", Svg.Attributes.width "100", Svg.Attributes.height "100", Svg.Attributes.rx "15", Svg.Attributes.ry "15" ] []
        ]
    -- div [ style [("border-style", "solid")] ] [ Html.text "SVG" ]

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
    let
        x = toFloat x'
    in
        traced (solid red) (path [ (x, 0), (x, value' * 100) ])
