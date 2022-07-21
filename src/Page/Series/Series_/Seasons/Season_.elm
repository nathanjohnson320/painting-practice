module Page.Series.Series_.Seasons.Season_ exposing (..)

import Browser.Navigation
import Css
import Data exposing (Episode, Series, episodeDecoder, seriesDecoder)
import DataSource exposing (DataSource)
import DataSource.File as File
import DataSource.Glob as Glob
import Head
import Head.Seo as Seo
import Html.Styled exposing (a, button, div, h2, img, option, p, select, text)
import Html.Styled.Attributes as Attr exposing (css)
import Html.Styled.Events exposing (onClick, onInput)
import List.Extra
import Page exposing (StaticPayload)
import Pages.PageUrl exposing (PageUrl)
import Pages.Url
import Path
import Shared
import Tailwind.Breakpoints as Bp
import Tailwind.Utilities as Tw
import View exposing (View)


type alias Model =
    { selectedSeries : String
    , selectedSeason : String
    }


type Msg
    = ChangeSeries String
    | ChangeSeason String
    | Search


type alias RouteParams =
    { series : String, season : String }


init :
    Maybe PageUrl
    -> Shared.Model
    -> StaticPayload Data RouteParams
    -> ( Model, Cmd Msg )
init _ _ static =
    ( { selectedSeries = static.routeParams.series
      , selectedSeason = static.routeParams.season
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
update _ navigationKey _ _ msg model =
    case msg of
        ChangeSeries series ->
            ( { model | selectedSeries = series, selectedSeason = "1" }, Cmd.none )

        ChangeSeason season ->
            ( { model | selectedSeason = season }, Cmd.none )

        Search ->
            case navigationKey of
                Just key ->
                    ( model
                    , Browser.Navigation.pushUrl key <| "/series/" ++ model.selectedSeries ++ "/seasons/" ++ model.selectedSeason
                    )

                Nothing ->
                    ( model, Cmd.none )


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
    DataSource.map3
        (\episodes s options ->
            { episodes = episodes, series = s, options = options }
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
        (Glob.succeed
            identity
            |> Glob.captureFilePath
            |> Glob.match (Glob.literal "data/series/")
            |> Glob.match Glob.wildcard
            |> Glob.match (Glob.literal ".json")
            |> Glob.toDataSource
            |> DataSource.map
                (List.map <| File.jsonFile seriesDecoder)
            |> DataSource.resolve
        )
        (Glob.succeed RouteParams
            |> Glob.match (Glob.literal "data/series/")
            |> Glob.capture Glob.wildcard
            |> Glob.match (Glob.literal "/seasons/")
            |> Glob.capture Glob.wildcard
            |> Glob.match (Glob.literal ".json")
            |> Glob.toDataSource
        )


head :
    StaticPayload Data RouteParams
    -> List Head.Tag
head static =
    let
        series =
            List.Extra.find (\s -> String.fromInt s.index == static.routeParams.series) static.data.series |> Maybe.withDefault { index = 1, title = "" }
    in
    Seo.summary
        { canonicalUrlOverride = Nothing
        , siteName = "Paint By Nate"
        , image =
            { url = Pages.Url.external "/img/logo.svg"
            , alt = "Painting pallette"
            , dimensions = Nothing
            , mimeType = Nothing
            }
        , description = "Videos from " ++ series.title ++ " Season " ++ static.routeParams.season
        , locale = Nothing
        , title = "Paint By Nate - Browse"
        }
        |> Seo.website


type alias Data =
    { episodes : List Episode
    , options : List RouteParams
    , series : List Series
    }


view :
    Maybe PageUrl
    -> Shared.Model
    -> Model
    -> StaticPayload Data RouteParams
    -> View Msg
view _ _ model static =
    let
        seasons =
            List.filter
                (\option ->
                    option.series == model.selectedSeries
                )
                static.data.options
                |> List.sortBy
                    (\o ->
                        String.toInt o.season |> Maybe.withDefault 0
                    )

        episodes =
            List.sortBy .index static.data.episodes
    in
    { title = "Paint By Nate - Browse"
    , body =
        [ div []
            [ div
                [ css
                    [ Tw.relative
                    , Tw.bg_gray_50
                    , Tw.pt_16
                    , Tw.px_4
                    , Bp.lg
                        [ Tw.pt_24
                        , Tw.px_8
                        ]
                    , Bp.sm
                        [ Tw.px_6
                        ]
                    ]
                ]
                [ div
                    [ css
                        [ Tw.absolute
                        , Tw.inset_0
                        ]
                    ]
                    [ div
                        [ css
                            [ Tw.bg_white
                            , Tw.h_1over3
                            , Bp.sm
                                [ Tw.h_2over3
                                ]
                            ]
                        ]
                        []
                    ]
                , div
                    [ css
                        [ Tw.relative
                        , Tw.max_w_7xl
                        , Tw.mx_auto
                        ]
                    ]
                    [ div
                        [ css
                            [ Tw.text_center
                            ]
                        ]
                        [ h2
                            [ css
                                [ Tw.text_3xl
                                , Tw.tracking_tight
                                , Tw.font_extrabold
                                , Tw.text_gray_900
                                , Bp.sm
                                    [ Tw.text_4xl
                                    ]
                                ]
                            ]
                            [ text "Browse Episodes" ]
                        ]
                    , div
                        [ css
                            [ Tw.px_3
                            , Tw.my_5
                            ]
                        ]
                        [ div
                            [ css
                                [ Tw.mt_1
                                , Tw.relative
                                , Tw.rounded_md
                                , Tw.shadow_sm
                                ]
                            ]
                            [ select
                                [ Attr.name "series"
                                , Attr.value model.selectedSeries
                                , onInput ChangeSeries
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
                                    (\o ->
                                        option
                                            [ Attr.value <| String.fromInt o.index
                                            ]
                                            [ text o.title ]
                                    )
                                    static.data.series
                                )
                            , select
                                [ Attr.name "season"
                                , onInput ChangeSeason
                                , Attr.value model.selectedSeason
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
                                    (\o ->
                                        option
                                            [ Attr.value o.season
                                            ]
                                            [ text o.season ]
                                    )
                                    seasons
                                )
                            , button
                                [ Attr.type_ "button"
                                , onClick Search
                                , css
                                    [ Tw.mt_4
                                    , Tw.inline_flex
                                    , Tw.items_center
                                    , Tw.px_2_dot_5
                                    , Tw.py_1_dot_5
                                    , Tw.border
                                    , Tw.border_transparent
                                    , Tw.text_xs
                                    , Tw.font_medium
                                    , Tw.rounded
                                    , Tw.shadow_sm
                                    , Tw.text_white
                                    , Tw.bg_indigo_600
                                    , Tw.w_full
                                    , Tw.justify_center
                                    , Tw.cursor_pointer
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
                                [ text "Search" ]
                            ]
                        ]
                    ]
                ]
            , div
                [ css
                    [ Tw.mt_12
                    , Tw.mx_auto
                    , Tw.grid
                    , Tw.gap_5
                    , Bp.lg
                        [ Tw.grid_cols_3
                        ]
                    ]
                ]
                (List.map (episodeView static.routeParams) episodes)
            ]
        ]
    }


episodeView : RouteParams -> Episode -> Html.Styled.Html Msg
episodeView { series, season } episode =
    a
        [ Attr.href <| "/series/" ++ series ++ "/seasons/" ++ season ++ "/episodes/" ++ String.fromInt episode.index
        , css
            [ Tw.cursor_pointer
            , Tw.flex
            , Tw.flex_col
            , Tw.rounded_lg
            , Tw.shadow_lg
            , Tw.overflow_hidden
            , Tw.no_underline
            ]
        ]
        [ div
            [ css
                [ Tw.flex_shrink_0
                , Tw.flex
                , Tw.justify_center
                ]
            ]
            [ img
                [ Attr.src <|
                    "/img/series/"
                        ++ series
                        ++ "/season/"
                        ++ season
                        ++ "/"
                        ++ String.fromInt episode.index
                        ++ ".jpg"
                , Attr.alt episode.painting.title
                , Attr.attribute "loading" "lazy"
                , css
                    [ Tw.h_72
                    ]
                ]
                []
            ]
        , div
            [ css
                [ Tw.flex_1
                , Tw.bg_white
                , Tw.p_6
                , Tw.flex
                , Tw.flex_col
                , Tw.justify_between
                ]
            ]
            [ div
                [ css
                    [ Tw.flex_1
                    ]
                ]
                [ div
                    [ css
                        [ Tw.text_sm
                        , Tw.font_medium
                        , Tw.text_indigo_600
                        ]
                    ]
                    [ p
                        [ css
                            [ Css.hover
                                [ Tw.underline
                                ]
                            ]
                        ]
                        [ text <| episodeLabel season episode ++ " - " ++ episode.painting.canvas ]
                    ]
                , p
                    [ css
                        [ Tw.text_xl
                        , Tw.font_semibold
                        , Tw.text_gray_900
                        ]
                    ]
                    [ text episode.painting.title ]
                , p
                    [ css
                        [ Tw.mt_3
                        , Tw.text_base
                        , Tw.text_gray_500
                        ]
                    ]
                    [ text episode.summary ]
                ]
            ]
        ]


episodeLabel : String -> Episode -> String
episodeLabel season { index } =
    "S" ++ String.padLeft 2 '0' season ++ "E" ++ String.padLeft 2 '0' (String.fromInt index)
