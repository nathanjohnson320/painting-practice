module Data exposing (Artist, Color, Episode, EpisodeType(..), Flags, Painting, Series, Tool, encodedEpisode, episodeDecoder, flagDecoder, seriesDecoder)

import Json.Decode as Json
import Json.Encode
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


type alias Series =
    { index : Int
    , title : String
    }


encodedEpisode : Episode -> Json.Encode.Value
encodedEpisode episode =
    let
        type_ =
            case episode.type_ of
                Embedded ->
                    "embedded"

                Paid ->
                    "paid"

                External ->
                    "external"
    in
    Json.Encode.object
        [ ( "duration", Json.Encode.string episode.duration )
        , ( "index", Json.Encode.int episode.index )
        , ( "painting", encodedPainting episode.painting )
        , ( "premier_date", Json.Encode.string episode.premierDate )
        , ( "summary", Json.Encode.string episode.summary )
        , ( "type", Json.Encode.string type_ )
        , ( "url", Json.Encode.string episode.url )
        ]


encodedPainting : Painting -> Json.Encode.Value
encodedPainting painting =
    Json.Encode.object
        [ ( "artist", encodedArtist painting.artist )
        , ( "canvas", Json.Encode.string painting.canvas )
        , ( "colors", Json.Encode.list encodedColor painting.colors )
        , ( "height", Json.Encode.int painting.height )
        , ( "title", Json.Encode.string painting.title )
        , ( "tools", Json.Encode.list encodedTool painting.tools )
        , ( "width", Json.Encode.int painting.width )
        ]


encodedArtist : Artist -> Json.Encode.Value
encodedArtist artist =
    Json.Encode.object
        [ ( "name", Json.Encode.string artist.name )
        ]


encodedColor : Color -> Json.Encode.Value
encodedColor color =
    Json.Encode.object
        [ ( "hex", Json.Encode.string color.hex )
        , ( "name", Json.Encode.string color.name )
        , ( "url", Json.Encode.string color.url )
        ]


encodedTool : Tool -> Json.Encode.Value
encodedTool tool =
    Json.Encode.object
        [ ( "name", Json.Encode.string tool.name )
        , ( "url", Json.Encode.string tool.url )
        ]


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


seriesDecoder : Decoder Series
seriesDecoder =
    Decode.map2 Series
        (Decode.field "index" Decode.int)
        (Decode.field "title" Decode.string)
