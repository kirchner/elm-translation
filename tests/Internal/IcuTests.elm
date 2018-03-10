module Internal.IcuTests exposing (..)

import Char
import Dict
import Expect
import Fuzz exposing (Fuzzer)
import Internal.Icu exposing (..)
import Test exposing (Test, describe, fuzz, fuzz2, fuzz3, fuzz4, test)


parseTest : Test
parseTest =
    describe "parse"
        [ test "an empty message" <|
            \_ ->
                ""
                    |> parse
                    |> Expect.equal (Ok [ Text "" ])
        , fuzz textFuzzer "a text" <|
            \text ->
                text
                    |> parse
                    |> Expect.equal (Ok [ Text text ])
        , fuzz nameFuzzer "a simple placeholder" <|
            \placeholder ->
                ("{" ++ placeholder ++ "}")
                    |> parse
                    |> Expect.equal
                        (Ok
                            [ Argument
                                { from = 0
                                , to = 1 + String.length placeholder
                                , placeholder = placeholder
                                , names = []
                                , subMessages = []
                                }
                            ]
                        )
        , fuzz2 nameFuzzer nameFuzzer "a placeholder with one name" <|
            \placeholder name ->
                ("{" ++ placeholder ++ ", " ++ name ++ "}")
                    |> parse
                    |> Expect.equal
                        (Ok
                            [ Argument
                                { from = 0
                                , to = 3 + String.length placeholder + String.length name
                                , placeholder = placeholder
                                , names = [ name ]
                                , subMessages = []
                                }
                            ]
                        )
        , fuzz2 nameFuzzer textFuzzer "a placeholder with one unnamed subPart" <|
            \placeholder text ->
                ("{" ++ placeholder ++ ", {" ++ text ++ "}}")
                    |> parse
                    |> Expect.equal
                        (Ok
                            [ Argument
                                { from = 0
                                , to = 5 + String.length placeholder + String.length text
                                , placeholder = placeholder
                                , names = []
                                , subMessages =
                                    [ Unnamed
                                        { from = 3 + String.length placeholder
                                        , to = 4 + String.length text + String.length placeholder
                                        , message = [ Text text ]
                                        }
                                    ]
                                }
                            ]
                        )
        , fuzz3 nameFuzzer nameFuzzer textFuzzer "a placeholder with on named subpart" <|
            \placeholder name text ->
                ("{" ++ placeholder ++ ", " ++ name ++ "{" ++ text ++ "}}")
                    |> parse
                    |> Expect.equal
                        (Ok
                            [ Argument
                                { from = 0
                                , to =
                                    5
                                        + String.length placeholder
                                        + String.length name
                                        + String.length text
                                , placeholder = placeholder
                                , names = []
                                , subMessages =
                                    [ Named
                                        { name = name
                                        , from =
                                            String.length placeholder
                                                + 3
                                        , to =
                                            String.length placeholder
                                                + String.length name
                                                + String.length text
                                                + 4
                                        , message = [ Text text ]
                                        }
                                    ]
                                }
                            ]
                        )
        , fuzz3 nameFuzzer nameFuzzer textFuzzer "a placeholder with one name and one unnamed subPart" <|
            \placeholder name text ->
                ("{" ++ placeholder ++ ", " ++ name ++ ", {" ++ text ++ "}}")
                    |> parse
                    |> Expect.equal
                        (Ok
                            [ Argument
                                { from = 0
                                , to =
                                    7
                                        + String.length placeholder
                                        + String.length name
                                        + String.length text
                                , placeholder = placeholder
                                , names = [ name ]
                                , subMessages =
                                    [ Unnamed
                                        { from =
                                            String.length placeholder
                                                + String.length name
                                                + 5
                                        , to =
                                            String.length placeholder
                                                + String.length name
                                                + String.length text
                                                + 6
                                        , message = [ Text text ]
                                        }
                                    ]
                                }
                            ]
                        )
        , fuzz4 nameFuzzer nameFuzzer nameFuzzer textFuzzer "a placeholder with one name and one named subPart" <|
            \placeholder name1 name2 text ->
                ("{" ++ placeholder ++ ", " ++ name1 ++ ", " ++ name2 ++ "{" ++ text ++ "}}")
                    |> parse
                    |> Expect.equal
                        (Ok
                            [ Argument
                                { from = 0
                                , to =
                                    7
                                        + String.length placeholder
                                        + String.length name1
                                        + String.length name2
                                        + String.length text
                                , placeholder = placeholder
                                , names = [ name1 ]
                                , subMessages =
                                    [ Named
                                        { from =
                                            String.length placeholder
                                                + String.length name1
                                                + 5
                                        , to =
                                            String.length placeholder
                                                + String.length name1
                                                + 6
                                                + String.length name2
                                                + String.length text
                                        , name = name2
                                        , message = [ Text text ]
                                        }
                                    ]
                                }
                            ]
                        )
        ]


textFuzzer : Fuzzer String
textFuzzer =
    Fuzz.string
        |> Fuzz.map
            (String.filter <|
                \c ->
                    (c /= '{') && (c /= '}') && (c /= '#') && (c /= '\'')
            )
        |> Fuzz.map ((++) "text")


nameFuzzer : Fuzzer String
nameFuzzer =
    Fuzz.list
        (Fuzz.intRange 65 90
            |> Fuzz.map Char.fromCode
        )
        |> Fuzz.map (List.foldr String.cons "name")
