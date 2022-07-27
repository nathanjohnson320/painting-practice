module Components exposing (input)

import Css
import Html.Styled as Styled exposing (Attribute, Html, div, label, text)
import Html.Styled.Attributes as Attr exposing (css)
import String.Extra
import Tailwind.Breakpoints as Bp
import Tailwind.Utilities as Tw


input : String -> String -> List (Attribute msg) -> Html msg
input name type_ attributes =
    div
        [ css
            [ Tw.grid
            , Tw.grid_cols_3
            , Tw.gap_6
            ]
        ]
        [ div
            [ css
                [ Tw.col_span_3
                , Bp.sm
                    [ Tw.col_span_2
                    ]
                ]
            ]
            [ label
                [ Attr.for name
                , css
                    [ Tw.block
                    , Tw.text_sm
                    , Tw.font_medium
                    , Tw.text_gray_700
                    ]
                ]
                [ text <| String.Extra.toTitleCase name ]
            , div
                [ css
                    [ Tw.mt_1
                    , Tw.flex
                    , Tw.rounded_md
                    , Tw.shadow_sm
                    ]
                ]
                [ Styled.input
                    (attributes
                        ++ [ Attr.type_ type_
                           , Attr.name name
                           , Attr.id name
                           , css
                                [ Tw.flex_1
                                , Tw.block
                                , Tw.w_full
                                , Tw.rounded_none
                                , Tw.rounded_r_md
                                , Tw.border_gray_300
                                , Css.focus
                                    [ Tw.ring_indigo_500
                                    , Tw.border_indigo_500
                                    ]
                                , Bp.sm
                                    [ Tw.text_sm
                                    ]
                                ]
                           ]
                    )
                    []
                ]
            ]
        ]
