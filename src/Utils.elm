module Utils exposing (episodeLabel)

import Data exposing (Episode)


episodeLabel : String -> Episode -> String
episodeLabel season { index } =
    "S" ++ String.padLeft 2 '0' season ++ "E" ++ String.padLeft 2 '0' (String.fromInt index)
