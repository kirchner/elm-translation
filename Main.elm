module Main exposing (main)

import Html
import Translation exposing (cldrToArgType, toElm)


main =
    Html.div
        []
        [ Html.pre []
            [ Html.code []
                [ toElm cldrToArgType "greeting" "Hello!"
                    |> Maybe.withDefault "could not parse ICU message"
                    |> Html.text
                ]
            ]
        , Html.pre []
            [ Html.code []
                [ toElm cldrToArgType "greeting" "Hello, {name}!"
                    |> Maybe.withDefault "could not parse ICU message"
                    |> Html.text
                ]
            ]
        , Html.pre []
            [ Html.code []
                [ toElm cldrToArgType "greeting" "You have slept {duration, number} seconds."
                    |> Maybe.withDefault "could not parse ICU message"
                    |> Html.text
                ]
            ]
        , Html.pre []
            [ Html.code []
                [ toElm cldrToArgType "greeting" "You have slept {duration, number, long} seconds."
                    |> Maybe.withDefault "could not parse ICU message"
                    |> Html.text
                ]
            ]
        , Html.pre []
            [ Html.code []
                [ toElm cldrToArgType "greeting" "Then they said: {_, delimited, quote, {Hello!}}"
                    |> Maybe.withDefault "could not parse ICU message"
                    |> Html.text
                ]
            ]
        , Html.pre []
            [ Html.code []
                [ toElm cldrToArgType "greeting" "We have met {_, list, and, {Alice} {Bob} {Cindy}}."
                    |> Maybe.withDefault "could not parse ICU message"
                    |> Html.text
                ]
            ]
        ]
