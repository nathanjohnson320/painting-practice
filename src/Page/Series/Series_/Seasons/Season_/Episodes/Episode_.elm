module Page.Series.Series_.Seasons.Season_.Episodes.Episode_ exposing (Data, Model, Msg, RouteParams, VideoSource(..), page)

import Css
import Data exposing (Episode, EpisodeType(..), episodeDecoder)
import DataSource exposing (DataSource)
import DataSource.File as File
import DataSource.Glob as Glob
import Head
import Head.Seo as Seo
import Html.Styled exposing (a, div, figcaption, figure, h2, h3, iframe, img, li, p, span, text, ul)
import Html.Styled.Attributes as Attr exposing (css)
import Page exposing (Page, StaticPayload)
import Pages.PageUrl exposing (PageUrl)
import Pages.Url
import Shared
import String.Extra
import Svg.Styled as Svg exposing (path, svg)
import Svg.Styled.Attributes as SvgAttr
import Tailwind.Breakpoints as Bp
import Tailwind.Utilities as Tw
import Url
import Url.Parser as Parser exposing ((<?>), s)
import Url.Parser.Query as Query
import View exposing (View)


type alias Model =
    ()


type alias Msg =
    Never


type alias RouteParams =
    { series : String, season : String, episode : String }


page : Page RouteParams Data
page =
    Page.prerender
        { head = head
        , data = data
        , routes = routes
        }
        |> Page.buildNoState { view = view }


routes : DataSource.DataSource (List RouteParams)
routes =
    Glob.succeed RouteParams
        |> Glob.match (Glob.literal "data/series/")
        |> Glob.capture Glob.wildcard
        |> Glob.match (Glob.literal "/seasons/")
        |> Glob.capture Glob.wildcard
        |> Glob.match (Glob.literal "/episodes/")
        |> Glob.capture Glob.wildcard
        |> Glob.match (Glob.literal ".json")
        |> Glob.toDataSource


data : RouteParams -> DataSource Data
data { series, season, episode } =
    DataSource.map
        (\e -> { episode = e })
        (File.jsonFile episodeDecoder <|
            "data/series/"
                ++ series
                ++ "/seasons/"
                ++ season
                ++ "/episodes/"
                ++ episode
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
            , alt = "painting pallette"
            , dimensions = Nothing
            , mimeType = Nothing
            }
        , description = static.data.episode.summary
        , locale = Nothing
        , title = static.data.episode.painting.title
        }
        |> Seo.website


type alias Data =
    { episode : Episode }


type VideoSource
    = Youtube (Maybe String)
    | Invalid


videoSource : Parser.Parser (VideoSource -> a) a
videoSource =
    Parser.oneOf
        [ Parser.map Youtube (s "watch" <?> Query.string "v")
        ]


iframeUrl : String -> VideoSource
iframeUrl frame =
    case Url.fromString frame of
        Nothing ->
            Invalid

        Just url ->
            Maybe.withDefault Invalid (Parser.parse videoSource url)



--   const parsed = new URL(url);
--   const version = parsed.searchParams.get("v") ?? "";
--   return `https://www.youtube.com/embed/${version}`;


view :
    Maybe PageUrl
    -> Shared.Model
    -> StaticPayload Data RouteParams
    -> View Msg
view _ _ static =
    { title = static.data.episode.painting.title
    , body =
        [ div
            [ css
                [ Tw.bg_white
                , Tw.overflow_hidden
                ]
            ]
            [ div
                [ css
                    [ Tw.relative
                    , Tw.max_w_7xl
                    , Tw.mx_auto
                    , Tw.py_16
                    , Tw.px_4
                    , Bp.lg
                        [ Tw.px_8
                        ]
                    , Bp.sm
                        [ Tw.px_6
                        ]
                    ]
                ]
                [ div
                    [ css
                        [ Tw.hidden
                        , Tw.bg_gray_50
                        , Tw.absolute
                        , Tw.top_0
                        , Tw.bottom_0
                        , Tw.left_3over4
                        , Tw.w_screen
                        , Bp.lg
                            [ Tw.block
                            ]
                        ]
                    ]
                    []
                , div
                    [ css
                        [ Tw.mx_auto
                        , Tw.text_base
                        , Tw.max_w_prose
                        , Bp.lg
                            [ Tw.grid
                            , Tw.grid_cols_2
                            , Tw.gap_8
                            , Tw.max_w_none
                            ]
                        ]
                    ]
                    [ div []
                        [ h2
                            [ css
                                [ Tw.text_base
                                , Tw.text_indigo_600
                                , Tw.font_semibold
                                , Tw.tracking_wide
                                , Tw.uppercase
                                ]
                            ]
                            [ text <| static.data.episode.painting.canvas ++ " Canvas" ]
                        , h3
                            [ css
                                [ Tw.mt_2
                                , Tw.text_3xl
                                , Tw.leading_8
                                , Tw.font_extrabold
                                , Tw.tracking_tight
                                , Tw.text_gray_900
                                , Bp.sm
                                    [ Tw.text_4xl
                                    ]
                                ]
                            ]
                            [ text <| static.data.episode.painting.title ]
                        ]
                    ]
                , div
                    [ css
                        [ Tw.mt_8
                        , Bp.lg
                            [ Tw.grid
                            , Tw.grid_cols_2
                            , Tw.gap_8
                            ]
                        ]
                    ]
                    [ div
                        [ css
                            [ Tw.relative
                            , Bp.lg
                                [ Tw.row_start_1
                                , Tw.col_start_2
                                ]
                            ]
                        ]
                        [ div
                            [ css
                                [ Tw.relative
                                , Tw.text_base
                                , Tw.mx_auto
                                , Tw.max_w_prose
                                , Tw.h_full
                                , Bp.lg
                                    [ Tw.max_w_none
                                    ]
                                ]
                            ]
                            [ figure []
                                [ div
                                    []
                                    [ videoView static.routeParams static.data.episode
                                    ]
                                , figcaption
                                    [ css
                                        [ Tw.mt_3
                                        , Tw.flex
                                        , Tw.text_sm
                                        , Tw.text_gray_500
                                        ]
                                    ]
                                    [ svg
                                        [ SvgAttr.css
                                            [ Tw.flex_none
                                            , Tw.w_5
                                            , Tw.h_5
                                            , Tw.text_gray_400
                                            ]
                                        , SvgAttr.viewBox "0 0 20 20"
                                        , SvgAttr.fill "currentColor"
                                        , Attr.attribute "aria-hidden" "true"
                                        ]
                                        [ path
                                            [ SvgAttr.d "M2 6a2 2 0 012-2h6a2 2 0 012 2v8a2 2 0 01-2 2H4a2 2 0 01-2-2V6zM14.553 7.106A1 1 0 0014 8v4a1 1 0 00.553.894l2 1A1 1 0 0018 13V7a1 1 0 00-1.447-.894l-2 1z"
                                            ]
                                            []
                                        ]
                                    , span
                                        [ css
                                            [ Tw.ml_2
                                            ]
                                        ]
                                        [ text <| static.data.episode.painting.artist.name ]
                                    ]
                                ]
                            ]
                        ]
                    , div
                        [ css
                            [ Tw.mt_8
                            , Bp.lg
                                [ Tw.mt_0
                                ]
                            ]
                        ]
                        [ div
                            [ css
                                [ Tw.text_base
                                , Tw.max_w_prose
                                , Tw.mx_auto
                                , Bp.lg
                                    [ Tw.max_w_none
                                    ]
                                ]
                            ]
                            [ p
                                [ css
                                    [ Tw.text_lg
                                    , Tw.text_gray_500
                                    ]
                                ]
                                [ text <| static.data.episode.summary ]
                            ]
                        , div
                            [ css [ Tw.mt_5 ]
                            ]
                            [ p []
                                [ text "Tools" ]
                            , ul [ css [ Tw.list_none ] ] <|
                                List.map
                                    (\tool ->
                                        li
                                            [ css [ Tw.mt_3 ] ]
                                            [ a
                                                [ css [ Tw.text_gray_800, Tw.no_underline ]
                                                , Attr.rel "noopener noreferrer nofollow"
                                                , Attr.target "_blank"
                                                , Attr.href tool.url
                                                ]
                                                [ text <| String.Extra.toTitleCase tool.name ]
                                            ]
                                    )
                                    (List.sortBy .name static.data.episode.painting.tools)
                            ]
                        , div
                            [ css [ Tw.mt_5 ]
                            ]
                            (p []
                                [ text "Colors" ]
                                :: List.map
                                    (\color ->
                                        div
                                            [ css
                                                [ Tw.flex
                                                , Tw.items_center
                                                , Tw.mt_3
                                                ]
                                            ]
                                            [ span
                                                [ css
                                                    [ Tw.w_4
                                                    , Tw.h_4
                                                    , Tw.rounded
                                                    , Tw.mt_0
                                                    , Tw.mb_0
                                                    , Tw.mr_4
                                                    , Tw.border
                                                    ]
                                                , Attr.style "backgroundColor" <| "#" ++ color.hex
                                                ]
                                                []
                                            , a
                                                [ css [ Tw.text_gray_800, Tw.no_underline ]
                                                , Attr.rel "noopener noreferrer nofollow"
                                                , Attr.target "_blank"
                                                , Attr.href color.url
                                                ]
                                                [ text <| String.Extra.toTitleCase color.name ]
                                            ]
                                    )
                                    (List.sortBy .name static.data.episode.painting.colors)
                            )
                        ]
                    ]
                ]
            ]
        ]
    }


videoView : RouteParams -> Episode -> Html.Styled.Html Msg
videoView routeParams episode =
    case episode.type_ of
        Paid ->
            div
                [ css
                    [ Tw.mt_12
                    , Tw.relative
                    , Bp.lg
                        [ Tw.mt_0
                        , Tw.max_w_none
                        , Tw.mx_0
                        , Tw.col_span_6
                        , Tw.flex
                        , Tw.items_center
                        ]
                    , Bp.sm
                        [ Tw.max_w_lg
                        , Tw.mx_auto
                        ]
                    ]
                ]
                [ svg
                    [ SvgAttr.css
                        [ Tw.absolute
                        , Tw.top_0
                        , Tw.left_1over2
                        , Tw.transform
                        , Tw.neg_translate_x_1over2
                        , Tw.neg_translate_y_8
                        , Tw.scale_75
                        , Tw.origin_top
                        , Bp.lg
                            [ Tw.hidden
                            ]
                        , Bp.sm
                            [ Tw.scale_100
                            ]
                        ]
                    , SvgAttr.width "640"
                    , SvgAttr.height "784"
                    , SvgAttr.fill "none"
                    , SvgAttr.viewBox "0 0 640 784"
                    , Attr.attribute "aria-hidden" "true"
                    ]
                    [ Svg.defs []
                        [ Svg.pattern
                            [ SvgAttr.id "4f4f415c-a0e9-44c2-9601-6ded5a34a13e"
                            , SvgAttr.x "118"
                            , SvgAttr.y "0"
                            , SvgAttr.width "20"
                            , SvgAttr.height "20"
                            , SvgAttr.patternUnits "userSpaceOnUse"
                            ]
                            [ Svg.rect
                                [ SvgAttr.x "0"
                                , SvgAttr.y "0"
                                , SvgAttr.width "4"
                                , SvgAttr.height "4"
                                , SvgAttr.css
                                    [ Tw.text_gray_200
                                    ]
                                , SvgAttr.fill "currentColor"
                                ]
                                []
                            ]
                        ]
                    , Svg.rect
                        [ SvgAttr.y "72"
                        , SvgAttr.width "640"
                        , SvgAttr.height "640"
                        , SvgAttr.css
                            [ Tw.text_gray_50
                            ]
                        , SvgAttr.fill "currentColor"
                        ]
                        []
                    , Svg.rect
                        [ SvgAttr.x "118"
                        , SvgAttr.width "404"
                        , SvgAttr.height "784"
                        , SvgAttr.fill "url(#4f4f415c-a0e9-44c2-9601-6ded5a34a13e)"
                        ]
                        []
                    ]
                , div
                    [ css
                        [ Tw.relative
                        , Tw.mx_auto
                        , Tw.w_full
                        , Tw.rounded_lg
                        , Tw.shadow_lg
                        , Bp.lg
                            [ Tw.max_w_md
                            ]
                        ]
                    ]
                    [ a
                        [ Attr.href episode.url
                        , Attr.rel "noopener noreferrer nofollow"
                        , Attr.target "_blank"
                        , css
                            [ Tw.cursor_pointer
                            , Tw.relative
                            , Tw.block
                            , Tw.w_full
                            , Tw.bg_white
                            , Tw.rounded_lg
                            , Tw.overflow_hidden
                            , Css.focus
                                [ Tw.outline_none
                                , Tw.ring_2
                                , Tw.ring_offset_2
                                , Tw.ring_indigo_500
                                ]
                            ]
                        ]
                        [ span
                            [ css
                                [ Tw.sr_only
                                ]
                            ]
                            [ text <| "This is a paid video available at " ++ episode.url ]
                        , img
                            [ css
                                [ Tw.w_full
                                ]
                            , Attr.src <|
                                "/img/series/"
                                    ++ routeParams.series
                                    ++ "/season/"
                                    ++ routeParams.season
                                    ++ "/"
                                    ++ routeParams.episode
                                    ++ ".jpg"
                            , Attr.alt episode.painting.title
                            ]
                            []
                        , div
                            [ css
                                [ Tw.absolute
                                , Tw.inset_0
                                , Tw.w_full
                                , Tw.h_full
                                , Tw.flex
                                , Tw.items_center
                                , Tw.justify_center
                                ]
                            , Attr.attribute "aria-hidden" "true"
                            ]
                            [ div
                                [ css
                                    [ Tw.bg_white
                                    , Tw.rounded_full
                                    , Tw.p_2
                                    ]
                                ]
                                [ svg
                                    [ SvgAttr.css
                                        [ Tw.h_20
                                        , Tw.w_20
                                        , Tw.text_indigo_500
                                        ]
                                    , SvgAttr.viewBox "0 0 20 20"
                                    , SvgAttr.fill "currentColor"
                                    ]
                                    [ path
                                        [ SvgAttr.d "M3 1a1 1 0 000 2h1.22l.305 1.222a.997.997 0 00.01.042l1.358 5.43-.893.892C3.74 11.846 4.632 14 6.414 14H15a1 1 0 000-2H6.414l1-1H14a1 1 0 00.894-.553l3-6A1 1 0 0017 3H6.28l-.31-1.243A1 1 0 005 1H3zM16 16.5a1.5 1.5 0 11-3 0 1.5 1.5 0 013 0zM6.5 18a1.5 1.5 0 100-3 1.5 1.5 0 000 3z"
                                        ]
                                        []
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]

        Embedded ->
            case iframeUrl episode.url of
                Youtube (Just version) ->
                    iframe
                        [ css
                            [ Tw.rounded_lg
                            , Tw.shadow_lg
                            , Tw.object_cover
                            , Tw.object_center
                            , Tw.w_full
                            , Tw.h_80
                            ]
                        , Attr.src <| "https://www.youtube.com/embed/" ++ version
                        , Attr.attribute "frameborder" "0"
                        , Attr.attribute "allow" "accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture"
                        , Attr.attribute "allowfullscreen" ""
                        ]
                        []

                _ ->
                    span [] [ text "Unhandled iframe url" ]

        External ->
            div
                [ css
                    [ Tw.mt_12
                    , Tw.relative
                    , Bp.lg
                        [ Tw.mt_0
                        , Tw.max_w_none
                        , Tw.mx_0
                        , Tw.col_span_6
                        , Tw.flex
                        , Tw.items_center
                        ]
                    , Bp.sm
                        [ Tw.max_w_lg
                        , Tw.mx_auto
                        ]
                    ]
                ]
                [ svg
                    [ SvgAttr.css
                        [ Tw.absolute
                        , Tw.top_0
                        , Tw.left_1over2
                        , Tw.transform
                        , Tw.neg_translate_x_1over2
                        , Tw.neg_translate_y_8
                        , Tw.scale_75
                        , Tw.origin_top
                        , Bp.lg
                            [ Tw.hidden
                            ]
                        , Bp.sm
                            [ Tw.scale_100
                            ]
                        ]
                    , SvgAttr.width "640"
                    , SvgAttr.height "784"
                    , SvgAttr.fill "none"
                    , SvgAttr.viewBox "0 0 640 784"
                    , Attr.attribute "aria-hidden" "true"
                    ]
                    [ Svg.defs []
                        [ Svg.pattern
                            [ SvgAttr.id "4f4f415c-a0e9-44c2-9601-6ded5a34a13e"
                            , SvgAttr.x "118"
                            , SvgAttr.y "0"
                            , SvgAttr.width "20"
                            , SvgAttr.height "20"
                            , SvgAttr.patternUnits "userSpaceOnUse"
                            ]
                            [ Svg.rect
                                [ SvgAttr.x "0"
                                , SvgAttr.y "0"
                                , SvgAttr.width "4}"
                                , SvgAttr.height "4"
                                , SvgAttr.css
                                    [ Tw.text_gray_200
                                    ]
                                , SvgAttr.fill "currentColor"
                                ]
                                []
                            ]
                        ]
                    , Svg.rect
                        [ SvgAttr.y "72"
                        , SvgAttr.width "640"
                        , SvgAttr.height "640"
                        , SvgAttr.css
                            [ Tw.text_gray_50
                            ]
                        , SvgAttr.fill "currentColor"
                        ]
                        []
                    , Svg.rect
                        [ SvgAttr.x "118"
                        , SvgAttr.width "404"
                        , SvgAttr.height "784"
                        , SvgAttr.fill "url(#4f4f415c-a0e9-44c2-9601-6ded5a34a13e)"
                        ]
                        []
                    ]
                , div
                    [ css
                        [ Tw.relative
                        , Tw.mx_auto
                        , Tw.w_full
                        , Tw.rounded_lg
                        , Tw.shadow_lg
                        , Bp.lg
                            [ Tw.max_w_md
                            ]
                        ]
                    ]
                    [ a
                        [ Attr.href episode.url
                        , Attr.rel "noopener noreferrer nofollow"
                        , Attr.target "_blank"
                        , css
                            [ Tw.cursor_pointer
                            , Tw.relative
                            , Tw.block
                            , Tw.w_full
                            , Tw.bg_white
                            , Tw.rounded_lg
                            , Tw.overflow_hidden
                            , Css.focus
                                [ Tw.outline_none
                                , Tw.ring_2
                                , Tw.ring_offset_2
                                , Tw.ring_indigo_500
                                ]
                            ]
                        ]
                        [ span
                            [ css
                                [ Tw.sr_only
                                ]
                            ]
                            [ text "This video is only available on YouTube" ]
                        , img
                            [ css
                                [ Tw.w_full
                                ]
                            , Attr.src <|
                                "/img/series/"
                                    ++ routeParams.series
                                    ++ "/season/"
                                    ++ routeParams.season
                                    ++ "/"
                                    ++ routeParams.episode
                                    ++ ".jpg"
                            , Attr.alt episode.painting.title
                            ]
                            []
                        , div
                            [ css
                                [ Tw.absolute
                                , Tw.inset_0
                                , Tw.w_full
                                , Tw.h_full
                                , Tw.flex
                                , Tw.items_center
                                , Tw.justify_center
                                ]
                            , Attr.attribute "aria-hidden" "true"
                            ]
                            [ svg
                                [ SvgAttr.css
                                    [ Tw.h_20
                                    , Tw.w_20
                                    , Tw.text_indigo_500
                                    ]
                                , SvgAttr.fill "currentColor"
                                , SvgAttr.viewBox "0 0 84 84"
                                ]
                                [ Svg.circle
                                    [ SvgAttr.opacity "0.9"
                                    , SvgAttr.cx "42"
                                    , SvgAttr.cy "42"
                                    , SvgAttr.r "42"
                                    , SvgAttr.fill "white"
                                    ]
                                    []
                                , path
                                    [ SvgAttr.d "M55.5039 40.3359L37.1094 28.0729C35.7803 27.1869 34 28.1396 34 29.737V54.263C34 55.8604 35.7803 56.8131 37.1094 55.9271L55.5038 43.6641C56.6913 42.8725 56.6913 41.1275 55.5039 40.3359Z"
                                    ]
                                    []
                                ]
                            ]
                        ]
                    ]
                ]
