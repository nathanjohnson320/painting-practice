module Data exposing (Episode, EpisodeType(..), Flags, Series, episodeDecoder, flagDecoder, seasonDecoder, seriesDecoder)

import Json.Decode as Json
import OptimizedDecoder as Decode exposing (Decoder)


type alias Flags =
    { currentYear : Int }


flagDecoder : Json.Decoder Flags
flagDecoder =
    Json.map Flags
        (Json.field "currentYear" Json.int)


type alias Episode =
    { duration : String
    , index : Int
    , painting : Painting
    , premierDate : String
    , summary : String
    , type_ : EpisodeType
    , url : String
    }


type EpisodeType
    = Paid
    | Embedded
    | External


type alias Painting =
    { artist : Artist
    , canvas : String
    , colors : List Color
    , height : Int
    , title : String
    , tools : List Tool
    , width : Int
    }


type alias Artist =
    { name : String
    }


type alias Color =
    { hex : String
    , name : String
    , url : String
    }


type alias Tool =
    { name : String
    , url : String
    }


type alias Season =
    { index : Int
    }


type alias Series =
    { index : Int
    , title : String
    }


episodeDecoder : Decoder Episode
episodeDecoder =
    let
        fieldSet0 =
            Decode.map6 Episode
                (Decode.field "duration" Decode.string)
                (Decode.field "index" Decode.int)
                (Decode.field "painting" postPaintingDecoder)
                (Decode.field "premier_date" Decode.string)
                (Decode.field "summary" Decode.string)
                (Decode.field "type" episodeTypeDecoder)
    in
    Decode.map2 (<|)
        fieldSet0
        (Decode.field "url" Decode.string)


episodeTypeDecoder : Decoder EpisodeType
episodeTypeDecoder =
    Decode.string |> Decode.andThen episodeTypeFromString


episodeTypeFromString : String -> Decoder EpisodeType
episodeTypeFromString string =
    case string of
        "embedded" ->
            Decode.succeed Embedded

        "paid" ->
            Decode.succeed Paid

        "external" ->
            Decode.succeed External

        _ ->
            Decode.fail ("Invalid episode type: " ++ string)


postPaintingDecoder : Decoder Painting
postPaintingDecoder =
    Decode.map7 Painting
        (Decode.field "artist" postArtistDecoder)
        (Decode.field "canvas" Decode.string)
        (Decode.field "colors" <| Decode.list postColorDecoder)
        (Decode.field "height" Decode.int)
        (Decode.field "title" Decode.string)
        (Decode.field "tools" <| Decode.list postToolDecoder)
        (Decode.field "width" Decode.int)


postArtistDecoder : Decoder Artist
postArtistDecoder =
    Decode.map Artist
        (Decode.field "name" Decode.string)


postColorDecoder : Decoder Color
postColorDecoder =
    Decode.map3 Color
        (Decode.field "hex" Decode.string)
        (Decode.field "name" Decode.string)
        (Decode.field "url" Decode.string)


postToolDecoder : Decoder Tool
postToolDecoder =
    Decode.map2 Tool
        (Decode.field "name" Decode.string)
        (Decode.field "url" Decode.string)


seasonDecoder : Decoder Season
seasonDecoder =
    Decode.map Season
        (Decode.field "index" Decode.int)


seriesDecoder : Decoder Series
seriesDecoder =
    Decode.map2 Series
        (Decode.field "index" Decode.int)
        (Decode.field "title" Decode.string)
