module Page.Index exposing (Data, Model, Msg, page)

import Css
import DataSource exposing (DataSource)
import Head
import Head.Seo as Seo
import Html.Styled exposing (button, div, h1, iframe, p, span, text)
import Html.Styled.Attributes as Attr exposing (css)
import Page exposing (Page, StaticPayload)
import Pages.PageUrl exposing (PageUrl)
import Pages.Url
import Shared
import Svg.Styled as Svg exposing (svg)
import Svg.Styled.Attributes as SvgAttr
import Tailwind.Breakpoints as Bp
import Tailwind.Utilities as Tw
import View exposing (View)


type alias Model =
    ()


type alias Msg =
    Never


type alias RouteParams =
    {}


page : Page RouteParams Data
page =
    Page.single
        { head = head
        , data = data
        }
        |> Page.buildNoState { view = view }


data : DataSource Data
data =
    DataSource.succeed ()


head :
    StaticPayload Data RouteParams
    -> List Head.Tag
head _ =
    Seo.summary
        { canonicalUrlOverride = Nothing
        , siteName = "elm-pages"
        , image =
            { url = Pages.Url.external "TODO"
            , alt = "elm-pages logo"
            , dimensions = Nothing
            , mimeType = Nothing
            }
        , description = "TODO"
        , locale = Nothing
        , title = "TODO title" -- metadata.title -- TODO
        }
        |> Seo.website


type alias Data =
    ()


view :
    Maybe PageUrl
    -> Shared.Model
    -> StaticPayload Data RouteParams
    -> View Msg
view _ _ _ =
    View.placeholder "Practice Painting Videos"
        [ div
            [ css
                [ Tw.mt_16
                , Tw.mx_auto
                , Tw.max_w_7xl
                , Tw.px_4
                , Bp.lg
                    [ Tw.mt_32
                    ]
                , Bp.sm
                    [ Tw.mt_24
                    , Tw.px_6
                    ]
                ]
            ]
            [ div
                [ css
                    [ Bp.lg
                        [ Tw.grid
                        , Tw.grid_cols_12
                        , Tw.gap_8
                        ]
                    ]
                ]
                [ div
                    [ css
                        [ Bp.lg
                            [ Tw.col_span_6
                            , Tw.text_left
                            ]
                        , Bp.md
                            [ Tw.max_w_2xl
                            , Tw.mx_auto
                            ]
                        , Bp.sm
                            [ Tw.text_center
                            ]
                        ]
                    ]
                    [ h1 []
                        [ span
                            [ css
                                [ Tw.mt_1
                                , Tw.block
                                , Tw.text_4xl
                                , Tw.tracking_tight
                                , Tw.font_extrabold
                                , Bp.sm
                                    [ Tw.text_5xl
                                    ]
                                , Bp.xl
                                    [ Tw.text_6xl
                                    ]
                                ]
                            ]
                            [ span
                                [ css
                                    [ Tw.block
                                    , Tw.text_gray_900
                                    ]
                                ]
                                [ text "Free Painting Videos from" ]
                            , span
                                [ css
                                    [ Tw.block
                                    , Tw.text_indigo_600
                                    ]
                                ]
                                [ text "great artists" ]
                            ]
                        ]
                    , p
                        [ css
                            [ Tw.mt_3
                            , Tw.text_base
                            , Tw.text_gray_500
                            , Bp.lg
                                [ Tw.text_lg
                                ]
                            , Bp.sm
                                [ Tw.mt_5
                                , Tw.text_xl
                                ]
                            , Bp.xl
                                [ Tw.text_xl
                                ]
                            ]
                        ]
                        [ text "The tools and colors required to paint each painting along with the associated youtube videos." ]
                    ]
                , div
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
                        [ button
                            [ Attr.type_ "button"
                            , css
                                [ Tw.relative
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
                                [ text "Watch our video to learn more" ]
                            , iframe
                                [ css
                                    [ Tw.w_full
                                    ]
                                , Attr.width 560
                                , Attr.height 315
                                , Attr.src "https://www.youtube.com/embed/lLWEXRAnQd0"
                                , Attr.attribute "frameborder" "0"
                                , Attr.attribute "allow" "accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture"
                                , Attr.attribute "allowfullscreen" ""
                                ]
                                []
                            ]
                        ]
                    ]
                ]
            ]
        ]
