module RandomQuotes exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (..)
import Platform.Cmd
import String exposing (..)
import Task

type alias Model =
    { numQuotesToDisplay : Int
    , quotes : List String
    }

init : Int -> (Model, Cmd Msg)
init numQuotesToDisplay =
    ( Model 5 []
    , Cmd.none
    )

type Msg
    = GetAnotherQuote Int
    | NewQuoteIncoming (Result Http.Error (List String))

update : Msg -> Model -> (Model, Cmd Msg)
update action model =
    case action of
        GetAnotherQuote numQuotesToGet ->
            ( model, getAnotherQuote numQuotesToGet)

        NewQuoteIncoming (Ok newQuotes) ->
            ( Model model.numQuotesToDisplay (model.quotes ++ newQuotes)
            , Cmd.none
            )

        NewQuoteIncoming (Err _) ->
            ( model, Cmd.none )

view : Model -> Html Msg
view model =
    let
        quotes =
            model.quotes
                |> List.map (\quote -> div [] [ text quote ])
                |> List.reverse
                |> List.take model.numQuotesToDisplay

        allThings =
            [ Html.button [ onClick (GetAnotherQuote 1) ] [ text "Manual Quote Add" ]
            ] ++ [ div [] quotes ]
    in
        div []
            allThings

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

-- HTTP

getAnotherQuote : Int -> Cmd Msg
getAnotherQuote numQuotesToGet =
    let
        url =
            if numQuotesToGet > 1 then
                "http://ron-swanson-quotes.herokuapp.com/v2/quotes/" ++ toString numQuotesToGet
            else
                "http://ron-swanson-quotes.herokuapp.com/v2/quotes"
    in
        Http.send NewQuoteIncoming (Http.get url decodeRonSwansonUrl)

decodeRonSwansonUrl : Decoder (List String)
decodeRonSwansonUrl =
    list string
