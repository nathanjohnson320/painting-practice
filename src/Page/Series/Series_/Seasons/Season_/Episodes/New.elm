port module Page.Series.Series_.Seasons.Season_.Episodes.New exposing (Data, Model, Msg(..), RouteParams, page)

import Browser.Navigation
import Components exposing (input)
import Css
import Data exposing (Color, Episode, EpisodeType(..), Series, Tool, encodedEpisode, episodeDecoder, seriesDecoder)
import DataSource exposing (DataSource)
import DataSource.File as File
import DataSource.Glob as Glob
import Head
import Head.Seo as Seo
import Html.Styled exposing (button, div, h3, label, option, p, select, text, textarea)
import Html.Styled.Attributes as Attr exposing (css, type_, value)
import Html.Styled.Events exposing (onClick, onInput)
import Json.Decode
import Json.Encode
import List.Extra
import Page exposing (StaticPayload)
import Pages.PageUrl exposing (PageUrl)
import Pages.Url
import Path
import Shared
import Tailwind.Breakpoints as Bp
import Tailwind.Utilities as Tw
import View exposing (View)


type alias PaintingForm =
    { title : String
    , artist : String
    , canvas : String
    , width : Int
    , height : Int
    , tools : List Tool
    , colors : List Color
    }


type alias EpisodeForm =
    { duration : String
    , index : Int
    , url : String
    , premierDate : String
    , type_ : EpisodeType
    , summary : String
    }


type alias Model =
    { episode : EpisodeForm
    , painting : PaintingForm
    }


type Msg
    = SelectColors (List ( String, Maybe String ))
    | SelectTools (List ( String, Maybe String ))
    | ChangeTitle String
    | ChangeArtist String
    | ChangeCanvas String
    | ChangeWidth String
    | ChangeHeight String
    | ChangeDuration String
    | ChangeIndex String
    | ChangeUrl String
    | ChangePremierDate String
    | ChangeType String
    | ChangeSummary String
    | DownloadEpisode


type alias RouteParams =
    { series : String, season : String }


init :
    Maybe PageUrl
    -> Shared.Model
    -> StaticPayload Data RouteParams
    -> ( Model, Cmd Msg )
init _ _ _ =
    ( { episode =
            { duration = ""
            , index = 1
            , url = ""
            , premierDate = ""
            , type_ = Embedded
            , summary = ""
            }
      , painting =
            { colors = []
            , tools = []
            , title = ""
            , artist = ""
            , canvas = "white"
            , width = 24
            , height = 18
            }
      }
    , Cmd.none
    )


subscriptions :
    Maybe PageUrl
    -> RouteParams
    -> Path.Path
    -> Model
    -> Sub Msg
subscriptions _ _ _ _ =
    Sub.none


page : Page.PageWithState RouteParams Data Model Msg
page =
    Page.prerender
        { head = head
        , data = data
        , routes = routes
        }
        |> Page.buildWithLocalState
            { view = view
            , init = init
            , subscriptions = subscriptions
            , update = update
            }


update :
    PageUrl
    -> Maybe Browser.Navigation.Key
    -> Shared.Model
    -> StaticPayload Data RouteParams
    -> Msg
    -> Model
    -> ( Model, Cmd Msg )
update _ _ _ static msg ({ episode, painting } as model) =
    case msg of
        SelectColors colors ->
            let
                fullColors =
                    List.concatMap
                        (\( _, maybeColor ) ->
                            case maybeColor of
                                Just color ->
                                    [ List.Extra.find (\c -> c.name == color) static.data.colors |> Maybe.withDefault { name = "", hex = "", url = "" } ]

                                Nothing ->
                                    []
                        )
                        colors
            in
            ( { model | painting = { painting | colors = fullColors } }, Cmd.none )

        SelectTools tools ->
            let
                fullTools =
                    List.concatMap
                        (\( _, maybeTool ) ->
                            case maybeTool of
                                Just tool ->
                                    [ List.Extra.find (\c -> c.name == tool) static.data.tools |> Maybe.withDefault { name = "", url = "" } ]

                                Nothing ->
                                    []
                        )
                        tools
            in
            ( { model | painting = { painting | tools = fullTools } }, Cmd.none )

        ChangeArtist artist ->
            ( { model | painting = { painting | artist = artist } }, Cmd.none )

        ChangeCanvas canvas ->
            ( { model | painting = { painting | canvas = canvas } }, Cmd.none )

        ChangeTitle title ->
            ( { model | painting = { painting | title = title } }, Cmd.none )

        ChangeWidth width ->
            let
                widthInt =
                    Maybe.withDefault 24 (String.toInt width)
            in
            ( { model | painting = { painting | width = widthInt } }, Cmd.none )

        ChangeHeight height ->
            let
                heightInt =
                    Maybe.withDefault 18 (String.toInt height)
            in
            ( { model | painting = { painting | height = heightInt } }, Cmd.none )

        ChangeDuration duration ->
            ( { model | episode = { episode | duration = duration } }, Cmd.none )

        ChangeIndex index ->
            let
                indexInt =
                    Maybe.withDefault 1 (String.toInt index)
            in
            ( { model | episode = { episode | index = indexInt } }, Cmd.none )

        ChangePremierDate premierDate ->
            ( { model | episode = { episode | premierDate = premierDate } }, Cmd.none )

        ChangeSummary summary ->
            ( { model | episode = { episode | summary = summary } }, Cmd.none )

        ChangeType type_ ->
            let
                episodeType =
                    case type_ of
                        "embedded" ->
                            Embedded

                        "paid" ->
                            Paid

                        "external" ->
                            External

                        _ ->
                            Embedded
            in
            ( { model | episode = { episode | type_ = episodeType } }, Cmd.none )

        ChangeUrl url ->
            ( { model | episode = { episode | url = url } }, Cmd.none )

        DownloadEpisode ->
            ( model
            , downloadEpisode
                (encodedEpisode
                    { duration = episode.duration
                    , index = episode.index
                    , painting =
                        { title = painting.title
                        , colors = painting.colors
                        , tools = painting.tools
                        , artist =
                            { name = painting.artist
                            }
                        , canvas = painting.canvas
                        , width = painting.width
                        , height = painting.height
                        }
                    , premierDate = episode.premierDate
                    , summary = episode.summary
                    , type_ = episode.type_
                    , url = episode.url
                    }
                )
            )


routes : DataSource.DataSource (List RouteParams)
routes =
    Glob.succeed RouteParams
        |> Glob.match (Glob.literal "data/series/")
        |> Glob.capture Glob.wildcard
        |> Glob.match (Glob.literal "/seasons/")
        |> Glob.capture Glob.wildcard
        |> Glob.match (Glob.literal ".json")
        |> Glob.toDataSource


data : RouteParams -> DataSource Data
data { series, season } =
    DataSource.map2
        (\episodes s ->
            let
                tools =
                    List.concatMap
                        (\e -> e.painting.tools)
                        episodes
                        |> List.Extra.uniqueBy (\e -> e.name)
                        |> List.sortBy .name

                colors =
                    List.concatMap
                        (\e -> e.painting.colors)
                        episodes
                        |> List.Extra.uniqueBy (\e -> e.name)
                        |> List.sortBy .name
            in
            { episodes = episodes, series = s, tools = tools, colors = colors }
        )
        (Glob.succeed
            identity
            |> Glob.captureFilePath
            |> Glob.match (Glob.literal ("data/series/" ++ series ++ "/seasons/" ++ season ++ "/episodes/"))
            |> Glob.match Glob.wildcard
            |> Glob.match (Glob.literal ".json")
            |> Glob.toDataSource
            |> DataSource.map
                (List.map <| File.jsonFile episodeDecoder)
            |> DataSource.resolve
        )
        (File.jsonFile seriesDecoder <|
            "data/series/"
                ++ series
                ++ ".json"
        )


head :
    StaticPayload Data RouteParams
    -> List Head.Tag
head static =
    Seo.summary
        { canonicalUrlOverride = Nothing
        , siteName = "Paint By Nate"
        , image =
            { url = Pages.Url.external "/img/logo.svg"
            , alt = "Painting pallette"
            , dimensions = Nothing
            , mimeType = Nothing
            }
        , description = "New episode for " ++ static.data.series.title ++ " Season " ++ static.routeParams.season
        , locale = Nothing
        , title = "Paint By Nate - New Episode"
        }
        |> Seo.website


type alias Data =
    { episodes : List Episode
    , series : Series
    , tools : List Tool
    , colors : List Color
    }


view :
    Maybe PageUrl
    -> Shared.Model
    -> Model
    -> StaticPayload Data RouteParams
    -> View Msg
view _ _ model static =
    { title = "Paint By Nate - Add New Episode of " ++ static.data.series.title
    , body =
        [ div []
            [ div
                [ css
                    [ Bp.md
                        [ Tw.grid
                        , Tw.grid_cols_3
                        , Tw.gap_6
                        ]
                    ]
                ]
                [ div
                    [ css
                        [ Bp.md
                            [ Tw.col_span_1
                            ]
                        ]
                    ]
                    [ div
                        [ css
                            [ Tw.px_4
                            , Bp.sm
                                [ Tw.px_0
                                ]
                            ]
                        ]
                        [ h3
                            [ css
                                [ Tw.text_lg
                                , Tw.font_medium
                                , Tw.leading_6
                                , Tw.text_gray_900
                                ]
                            ]
                            [ text "Episode" ]
                        , p
                            [ css
                                [ Tw.mt_1
                                , Tw.text_sm
                                , Tw.text_gray_600
                                ]
                            ]
                            [ text "General information about the episode." ]
                        ]
                    ]
                , div
                    [ css
                        [ Tw.mt_5
                        , Bp.md
                            [ Tw.mt_0
                            , Tw.col_span_2
                            ]
                        ]
                    ]
                    [ div
                        [ css
                            [ Tw.shadow
                            , Bp.sm
                                [ Tw.rounded_md
                                , Tw.overflow_hidden
                                ]
                            ]
                        ]
                        [ div
                            [ css
                                [ Tw.px_4
                                , Tw.py_5
                                , Tw.bg_white
                                , Tw.space_y_6
                                , Bp.sm
                                    [ Tw.p_6
                                    ]
                                ]
                            ]
                            [ input "duration" "time" [ onInput ChangeDuration, value model.episode.duration ]
                            , input "index" "number" [ onInput ChangeIndex, value <| String.fromInt <| model.episode.index ]
                            , input "url" "text" [ onInput ChangeUrl, value model.episode.url ]
                            , input "premier date" "datetime-local" [ onInput ChangePremierDate, value model.episode.premierDate ]
                            , div []
                                [ label
                                    [ Attr.for "Type"
                                    , css
                                        [ Tw.block
                                        , Tw.text_sm
                                        , Tw.font_medium
                                        , Tw.text_gray_700
                                        ]
                                    ]
                                    [ text "Type" ]
                                , select
                                    [ Attr.id "type"
                                    , Attr.name "type"
                                    , onInput ChangeType
                                    , css
                                        [ Tw.mt_1
                                        , Tw.block
                                        , Tw.w_full
                                        , Tw.pl_3
                                        , Tw.pr_10
                                        , Tw.py_2
                                        , Tw.text_base
                                        , Tw.border_gray_300
                                        , Tw.rounded_md
                                        , Css.focus
                                            [ Tw.outline_none
                                            , Tw.ring_indigo_500
                                            , Tw.border_indigo_500
                                            ]
                                        , Bp.sm
                                            [ Tw.text_sm
                                            ]
                                        ]
                                    ]
                                    [ option [ Attr.selected True ]
                                        [ text "Embedded" ]
                                    , option []
                                        [ text "External" ]
                                    , option []
                                        [ text "Paid" ]
                                    ]
                                ]
                            , div []
                                [ label
                                    [ Attr.for "summary"
                                    , css
                                        [ Tw.block
                                        , Tw.text_sm
                                        , Tw.font_medium
                                        , Tw.text_gray_700
                                        ]
                                    ]
                                    [ text "Summary" ]
                                , div
                                    [ css
                                        [ Tw.mt_1
                                        ]
                                    ]
                                    [ textarea
                                        [ Attr.rows 4
                                        , Attr.name "summary"
                                        , Attr.id "summary"
                                        , onInput ChangeSummary
                                        , css
                                            [ Tw.shadow_sm
                                            , Tw.block
                                            , Tw.w_full
                                            , Tw.border_gray_300
                                            , Tw.rounded_md
                                            , Css.focus
                                                [ Tw.ring_indigo_500
                                                , Tw.border_indigo_500
                                                ]
                                            , Bp.sm
                                                [ Tw.text_sm
                                                ]
                                            ]
                                        ]
                                        []
                                    ]
                                ]
                            ]
                        , div
                            [ css
                                [ Tw.px_4
                                , Tw.py_3
                                , Tw.bg_gray_50
                                , Tw.text_right
                                , Bp.sm
                                    [ Tw.px_6
                                    ]
                                ]
                            ]
                            [ button
                                [ Attr.type_ "submit"
                                , onClick DownloadEpisode
                                , css
                                    [ Tw.inline_flex
                                    , Tw.justify_center
                                    , Tw.py_2
                                    , Tw.px_4
                                    , Tw.border
                                    , Tw.border_transparent
                                    , Tw.shadow_sm
                                    , Tw.text_sm
                                    , Tw.font_medium
                                    , Tw.rounded_md
                                    , Tw.text_white
                                    , Tw.bg_indigo_600
                                    , Css.focus
                                        [ Tw.outline_none
                                        , Tw.ring_2
                                        , Tw.ring_offset_2
                                        , Tw.ring_indigo_500
                                        ]
                                    , Css.hover
                                        [ Tw.bg_indigo_700
                                        ]
                                    ]
                                ]
                                [ text "Save" ]
                            ]
                        ]
                    ]
                ]
            ]
        , div
            [ css
                [ Tw.hidden
                , Bp.sm
                    [ Tw.block
                    ]
                ]
            , Attr.attribute "aria-hidden" "true"
            ]
            [ div
                [ css
                    [ Tw.py_5
                    ]
                ]
                [ div
                    [ css
                        [ Tw.border_t
                        , Tw.border_gray_200
                        ]
                    ]
                    []
                ]
            ]
        , div
            [ css
                [ Tw.mt_10
                , Bp.sm
                    [ Tw.mt_0
                    ]
                ]
            ]
            [ div
                [ css
                    [ Bp.md
                        [ Tw.grid
                        , Tw.grid_cols_3
                        , Tw.gap_6
                        ]
                    ]
                ]
                [ div
                    [ css
                        [ Bp.md
                            [ Tw.col_span_1
                            ]
                        ]
                    ]
                    [ div
                        [ css
                            [ Tw.px_4
                            , Bp.sm
                                [ Tw.px_0
                                ]
                            ]
                        ]
                        [ h3
                            [ css
                                [ Tw.text_lg
                                , Tw.font_medium
                                , Tw.leading_6
                                , Tw.text_gray_900
                                ]
                            ]
                            [ text "Painting" ]
                        , p
                            [ css
                                [ Tw.mt_1
                                , Tw.text_sm
                                , Tw.text_gray_600
                                ]
                            ]
                            [ text "Details about the specific painting" ]
                        ]
                    ]
                , div
                    [ css
                        [ Tw.mt_5
                        , Bp.md
                            [ Tw.mt_0
                            , Tw.col_span_2
                            ]
                        ]
                    ]
                    [ div
                        [ css
                            [ Tw.shadow
                            , Tw.overflow_hidden
                            , Bp.sm
                                [ Tw.rounded_md
                                ]
                            ]
                        ]
                        [ div
                            [ css
                                [ Tw.px_4
                                , Tw.py_5
                                , Tw.bg_white
                                , Bp.sm
                                    [ Tw.p_6
                                    ]
                                ]
                            ]
                            [ div
                                [ css
                                    [ Tw.flex
                                    , Tw.flex_col
                                    ]
                                ]
                                [ input "title" "text" [ onInput ChangeTitle, value model.painting.title ]
                                , input "artist" "text" [ onInput ChangeArtist, value model.painting.artist ]
                                , input "canvas" "text" [ onInput ChangeCanvas, value model.painting.canvas ]
                                , input "width" "number" [ onInput ChangeWidth, value <| String.fromInt <| model.painting.width ]
                                , input "height" "number" [ onInput ChangeHeight, value <| String.fromInt <| model.painting.height ]
                                , select
                                    [ Attr.id "colors"
                                    , Attr.name "colors"
                                    , Attr.multiple True
                                    , Html.Styled.Events.on "change"
                                        (Json.Decode.map SelectColors selectDecoder)
                                    , css
                                        [ Tw.mt_1
                                        , Tw.block
                                        , Tw.w_full
                                        , Tw.pl_3
                                        , Tw.pr_10
                                        , Tw.py_2
                                        , Tw.text_base
                                        , Tw.border_gray_300
                                        , Tw.rounded_md
                                        , Css.focus
                                            [ Tw.outline_none
                                            , Tw.ring_indigo_500
                                            , Tw.border_indigo_500
                                            ]
                                        , Bp.sm
                                            [ Tw.text_sm
                                            ]
                                        ]
                                    ]
                                    (List.map
                                        (\color ->
                                            option []
                                                [ text color.name ]
                                        )
                                        static.data.colors
                                    )
                                , select
                                    [ Attr.id "type"
                                    , Attr.name "type"
                                    , Attr.multiple True
                                    , Html.Styled.Events.on "change"
                                        (Json.Decode.map SelectTools selectDecoder)
                                    , css
                                        [ Tw.mt_1
                                        , Tw.block
                                        , Tw.w_full
                                        , Tw.pl_3
                                        , Tw.pr_10
                                        , Tw.py_2
                                        , Tw.text_base
                                        , Tw.border_gray_300
                                        , Tw.rounded_md
                                        , Css.focus
                                            [ Tw.outline_none
                                            , Tw.ring_indigo_500
                                            , Tw.border_indigo_500
                                            ]
                                        , Bp.sm
                                            [ Tw.text_sm
                                            ]
                                        ]
                                    ]
                                    (List.map
                                        (\tool ->
                                            option []
                                                [ text tool.name ]
                                        )
                                        static.data.tools
                                    )
                                ]
                            ]
                        , div
                            [ css
                                [ Tw.px_4
                                , Tw.py_3
                                , Tw.bg_gray_50
                                , Tw.text_right
                                , Bp.sm
                                    [ Tw.px_6
                                    ]
                                ]
                            ]
                            [ button
                                [ Attr.type_ "submit"
                                , onClick DownloadEpisode
                                , css
                                    [ Tw.inline_flex
                                    , Tw.justify_center
                                    , Tw.py_2
                                    , Tw.px_4
                                    , Tw.border
                                    , Tw.border_transparent
                                    , Tw.shadow_sm
                                    , Tw.text_sm
                                    , Tw.font_medium
                                    , Tw.rounded_md
                                    , Tw.text_white
                                    , Tw.bg_indigo_600
                                    , Css.focus
                                        [ Tw.outline_none
                                        , Tw.ring_2
                                        , Tw.ring_offset_2
                                        , Tw.ring_indigo_500
                                        ]
                                    , Css.hover
                                        [ Tw.bg_indigo_700
                                        ]
                                    ]
                                ]
                                [ text "Save" ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
    }


selectDecoder : Json.Decode.Decoder (List ( String, Maybe String ))
selectDecoder =
    Json.Decode.at [ "target", "selectedOptions" ] <|
        Json.Decode.keyValuePairs <|
            Json.Decode.maybe (Json.Decode.at [ "value" ] Json.Decode.string)



-- PORTS


port downloadEpisode : Json.Encode.Value -> Cmd msg
