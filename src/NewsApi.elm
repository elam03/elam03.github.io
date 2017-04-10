port module NewsApi exposing (..)

import Json.Decode exposing (Value)

type alias NewsData =
    { author : Maybe String -- There is a hack over on the JS-side to correctly 'maybe'-fy this.
    , title : String
    , description : String
    , url : String
    , urlToImage : String
    , publishedAt : String
    , channel : String
    }

-- port newNewsData : (NewsData -> msg) -> Sub msg
port newNewsData : (Value -> msg) -> Sub msg
