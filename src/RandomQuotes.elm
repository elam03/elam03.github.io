module RandomQuotes exposing (..)

import AnimationFrame exposing (..)
import Html exposing (..)
import Html.Events exposing (onClick)
import HtmlParser exposing (..)
import HtmlParser.Util exposing (textContent)
import Http
import Json.Decode exposing (..)
import Platform.Cmd
import String exposing (..)
import Task
import Time exposing (Time)

import NewsApi exposing (..)

type alias Model =
    { numQuotesToDisplay : Int
    , quotes : List String
    , timeForNextQuote : Float
    }

init : Int -> (Model, Cmd Msg)
init numQuotesToDisplay =
    ( Model 10 [] 0
    , Cmd.none
    )

type Msg
    = GetAnotherRonSwansonQuote Int
    | GetAnotherQuoteOnDesign Int
    | NewQuoteRonSwanson (Result Http.Error (List String))
    | NewQuoteOnDesign (Result Http.Error (List String))
    | NewNewsData (NewsData)
    | Tick Time

update : Msg -> Model -> (Model, Cmd Msg)
update action model =
    case action of
        Tick t ->
            if t > model.timeForNextQuote then
                ( { model | timeForNextQuote = t + 5000 }
                , getAnotherRonSwansonQuote 1
                )
            else
                ( model, Cmd.none )

        GetAnotherRonSwansonQuote numQuotesToGet ->
            ( model, getAnotherRonSwansonQuote numQuotesToGet )

        NewQuoteRonSwanson (Ok newQuotes) ->
            ( { model | quotes = model.quotes ++ newQuotes }
            , Cmd.none
            )

        NewQuoteRonSwanson (Err _) ->
            ( model, Cmd.none )

        GetAnotherQuoteOnDesign numQuotesToGet ->
            ( model, getAnotherQuoteOnDesign numQuotesToGet )

        NewQuoteOnDesign (Ok newQuotes) ->
            let
                newParsedQuotes =
                    newQuotes
                        |> List.map (\quote -> parse quote |> textContent)
            in
                ( { model | quotes = model.quotes ++ newParsedQuotes }
                , Cmd.none
                )

        NewQuoteOnDesign (Err _) ->
            ( model, Cmd.none )

        NewNewsData newData ->
            ( { model | quotes = model.quotes ++ [ newData.title ] }
            , Cmd.none
            )

view : Model -> Html Msg
view model =
    let
        quotes =
            model.quotes
                |> List.map (\quote -> div [] [ text quote ])
                |> List.reverse
                |> List.take model.numQuotesToDisplay

        allThings =
            [ Html.button [ onClick (GetAnotherRonSwansonQuote 1) ] [ text "Manual Quote Add (RS)" ]
            , Html.hr [] []
            , Html.button [ onClick (GetAnotherQuoteOnDesign 1) ] [ text "Manual Quote Add (QoD)" ]
            , Html.hr [] []
            ] ++ [ div [] quotes ]
    in
        div []
            allThings

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ newNewsData NewNewsData ]
        -- [ AnimationFrame.times Tick
        -- , newNewsData NewNewsData
        -- ]

-- HTTP
------------------------------------------------------------

-- Ron Swanson
getAnotherRonSwansonQuote : Int -> Cmd Msg
getAnotherRonSwansonQuote numQuotesToGet =
    let
        url =
            if numQuotesToGet > 1 then
                "http://ron-swanson-quotes.herokuapp.com/v2/quotes/" ++ toString numQuotesToGet
            else
                "http://ron-swanson-quotes.herokuapp.com/v2/quotes"
    in
        Http.send NewQuoteRonSwanson (Http.get url decodeRonSwansonUrl)

decodeRonSwansonUrl : Decoder (List String)
decodeRonSwansonUrl =
    list string

------------------------------------------------------------
-- Quotes on Design

getAnotherQuoteOnDesign : Int -> Cmd Msg
getAnotherQuoteOnDesign numQuotesToGet =
    let
        url = "http://quotesondesign.com/wp-json/posts?filter[orderby]=rand&filter[posts_per_page]=" ++ toString numQuotesToGet
    in
        Http.send NewQuoteOnDesign (Http.get url decodeQuoteOnDesignUrl)

decodeQuoteOnDesignUrl : Decoder (List String)
decodeQuoteOnDesignUrl =
    list
        <| field "content" string
        -- <| map3 { id : String, title : String, context : String }
        --     (field "ID" string)
        --     (field "title" string)
        --     (field "content" string)
