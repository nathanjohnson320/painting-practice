module Shared exposing (Data, Model, Msg(..), SharedMsg(..), template)

import Browser.Navigation
import Css
import Data exposing (flagDecoder)
import DataSource
import Html exposing (Html)
import Html.Styled exposing (a, div, footer, img, main_, nav, p, span, text)
import Html.Styled.Attributes as Attr exposing (css, href)
import Json.Decode as Json
import Pages.Flags
import Pages.PageUrl exposing (PageUrl)
import Path exposing (Path)
import Route exposing (Route)
import SharedTemplate exposing (SharedTemplate)
import Svg.Styled exposing (path, svg)
import Svg.Styled.Attributes as SvgAttr
import Tailwind.Breakpoints as Bp
import Tailwind.Utilities as Tw
import View exposing (View)


template : SharedTemplate Msg Model Data msg
template =
    { init = init
    , update = update
    , view = view
    , data = data
    , subscriptions = subscriptions
    , onPageChange = Just OnPageChange
    }


type Msg
    = OnPageChange
        { path : Path
        , query : Maybe String
        , fragment : Maybe String
        }
    | SharedMsg SharedMsg


type alias Data =
    ()


type SharedMsg
    = NoOp


type alias Model =
    { currentYear : Int
    }


init :
    Maybe Browser.Navigation.Key
    -> Pages.Flags.Flags
    ->
        Maybe
            { path :
                { path : Path
                , query : Maybe String
                , fragment : Maybe String
                }
            , metadata : route
            , pageUrl : Maybe PageUrl
            }
    -> ( Model, Cmd Msg )
init _ flags _ =
    case flags of
        Pages.Flags.BrowserFlags json ->
            case Json.decodeValue flagDecoder json of
                Ok f ->
                    ( { currentYear = f.currentYear }
                    , Cmd.none
                    )

                Err _ ->
                    ( { currentYear = 0 }, Cmd.none )

        _ ->
            ( { currentYear = 0 }
            , Cmd.none
            )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnPageChange _ ->
            ( model, Cmd.none )

        SharedMsg _ ->
            -- SharedMsg globalMsg
            ( model, Cmd.none )


subscriptions : Path -> Model -> Sub Msg
subscriptions _ _ =
    Sub.none


data : DataSource.DataSource Data
data =
    DataSource.succeed ()


view :
    Data
    ->
        { path : Path
        , route : Maybe Route
        }
    -> Model
    -> (Msg -> msg)
    -> View msg
    -> { body : Html msg, title : String }
view _ _ model _ pageView =
    -- view sharedData page model toMsg pageView
    let
        styledBody =
            bodyView model pageView.body
    in
    { body = Html.div [] [ styledBody ]
    , title = pageView.title
    }


bodyView : Model -> List (Html.Styled.Html msg) -> Html msg
bodyView model body =
    Html.Styled.toUnstyled {- This example requires Tailwind CSS v2.0+ -} <|
        div []
            [ nav
                [ css
                    [ Tw.bg_white
                    , Tw.shadow
                    ]
                ]
                [ div
                    [ css
                        [ Tw.max_w_7xl
                        , Tw.mx_auto
                        , Tw.px_2
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
                            [ Tw.relative
                            , Tw.flex
                            , Tw.justify_between
                            , Tw.h_16
                            ]
                        ]
                        [ div
                            [ css
                                [ Tw.flex_1
                                , Tw.flex
                                , Tw.items_center
                                , Tw.justify_center
                                , Bp.sm
                                    [ Tw.items_stretch
                                    , Tw.justify_start
                                    ]
                                ]
                            ]
                            [ div
                                [ css [ Tw.flex_shrink_0, Tw.flex, Tw.items_center ] ]
                                [ a [ href "/" ]
                                    [ img
                                        [ css
                                            [ Tw.h_8, Tw.w_auto ]
                                        , Attr.src "/img/logo.svg"
                                        , Attr.alt "Painting Pallette Logo"
                                        ]
                                        []
                                    ]
                                ]
                            , div
                                [ css [ Tw.ml_6, Tw.flex, Tw.space_x_8 ] ]
                                [ a
                                    [ Attr.href "/series/1/seasons/1"
                                    , css
                                        [ Tw.border_indigo_500
                                        , Tw.text_gray_900
                                        , Tw.inline_flex
                                        , Tw.items_center
                                        , Tw.px_1
                                        , Tw.pt_1
                                        , Tw.border_b_2
                                        , Tw.text_sm
                                        , Tw.font_medium
                                        , Tw.no_underline
                                        ]
                                    ]
                                    [ text "Browse" ]
                                ]
                            ]
                        ]
                    ]
                , main_ [] body
                , footerView model
                ]
            ]


footerView : Model -> Html.Styled.Html msg
footerView model =
    let
        footerItems =
            [ { name = "Email"
              , href = "mailto:admin@paintbynate.art"
              , icon =
                    \props ->
                        svg
                            ([ SvgAttr.css
                                [ Tw.h_6
                                , Tw.w_6
                                ]
                             , SvgAttr.fill "none"
                             , SvgAttr.viewBox "0 0 24 24"
                             , SvgAttr.stroke "currentColor"
                             ]
                                ++ props
                            )
                            [ path
                                [ Attr.attribute "strokelinecap" "round"
                                , Attr.attribute "strokelinejoin" "round"
                                , Attr.attribute "strokewidth" "{2}"
                                , SvgAttr.d "M16 12a4 4 0 10-8 0 4 4 0 008 0zm0 0v1.5a2.5 2.5 0 005 0V12a9 9 0 10-9 9m4.5-1.206a8.959 8.959 0 01-4.5 1.207"
                                ]
                                []
                            ]
              }
            , { name = "LinkTree"
              , href = "https://linktr.ee/paintbynate"
              , icon =
                    \props ->
                        svg
                            ([ SvgAttr.css
                                [ Tw.h_6
                                , Tw.w_6
                                ]
                             , SvgAttr.fill "none"
                             , SvgAttr.viewBox "0 0 24 24"
                             , SvgAttr.stroke "currentColor"
                             ]
                                ++ props
                            )
                            [ path
                                [ Attr.attribute "strokelinecap" "round"
                                , Attr.attribute "strokelinejoin" "round"
                                , Attr.attribute "strokewidth" "{2}"
                                , SvgAttr.d "M3.055 11H5a2 2 0 012 2v1a2 2 0 002 2 2 2 0 012 2v2.945M8 3.935V5.5A2.5 2.5 0 0010.5 8h.5a2 2 0 012 2 2 2 0 104 0 2 2 0 012-2h1.064M15 20.488V18a2 2 0 012-2h3.064M21 12a9 9 0 11-18 0 9 9 0 0118 0z"
                                ]
                                []
                            ]
              }
            ]
    in
    footer
        [ css
            [ Tw.bg_white
            ]
        ]
        [ div
            [ css
                [ Tw.max_w_7xl
                , Tw.mx_auto
                , Tw.py_12
                , Tw.px_4
                , Bp.lg
                    [ Tw.px_8
                    ]
                , Bp.md
                    [ Tw.flex
                    , Tw.items_center
                    , Tw.justify_between
                    ]
                , Bp.sm
                    [ Tw.px_6
                    ]
                ]
            ]
            [ div
                [ css
                    [ Tw.flex
                    , Tw.justify_center
                    , Tw.space_x_6
                    , Bp.md
                        [ Tw.order_2
                        ]
                    ]
                ]
              <|
                List.map
                    (\item ->
                        a
                            [ Attr.href item.href
                            , Attr.rel "noopener noreferrer nofollow"
                            , Attr.target "_blank"
                            , css
                                [ Tw.text_gray_400
                                , Css.hover
                                    [ Tw.text_gray_500
                                    ]
                                ]
                            ]
                            [ span
                                [ css
                                    [ Tw.sr_only
                                    ]
                                ]
                                [ text item.name ]
                            , item.icon
                                [ css
                                    [ Tw.h_6
                                    , Tw.w_6
                                    ]
                                , Attr.attribute "aria-hidden" "true"
                                ]
                            ]
                    )
                    footerItems
            , div
                [ css
                    [ Tw.mt_8
                    , Bp.md
                        [ Tw.mt_0
                        , Tw.order_1
                        ]
                    ]
                ]
                [ p
                    [ css
                        [ Tw.text_center
                        , Tw.text_base
                        , Tw.text_gray_400
                        ]
                    ]
                    [ text <| "© " ++ String.fromInt model.currentYear ++ " Paint By Nate. All rights reserved." ]
                ]
            ]
        ]
