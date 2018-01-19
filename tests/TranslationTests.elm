module TranslationTests exposing (..)

import Expect
import Fuzz exposing (Fuzzer)
import Test exposing (Test, describe, fuzz2, fuzz3, fuzz4)
import Translation exposing (..)


toIcuTest : Test
toIcuTest =
    describe "toIcu"
        [ fuzz2 fuzzName Fuzz.string "a simple text" <|
            \name text ->
                (final name <|
                    s text
                )
                    |> toIcu
                    |> Expect.equal text
        , fuzz3 fuzzName Fuzz.string Fuzz.string "a list of simple texts" <|
            \name text1 text2 ->
                (final name <|
                    concat
                        [ s text1
                        , s text2
                        ]
                )
                    |> toIcu
                    |> Expect.equal (text1 ++ text2)
        , fuzz2 fuzzName fuzzName "a string placeholder" <|
            \name placeholder ->
                (final name <|
                    string .placeholder placeholder
                )
                    |> toIcu
                    |> Expect.equal ("{" ++ placeholder ++ "}")
        , fuzz3 fuzzName fuzzName Fuzz.string "a node" <|
            \name placeholder text ->
                (final name <|
                    node .placeholder placeholder <|
                        s text
                )
                    |> toIcu
                    |> Expect.equal
                        ("{" ++ placeholder ++ ", node, {" ++ text ++ "}}")
        , fuzz2 fuzzName fuzzName "a float with an unnamed printer" <|
            \name placeholder ->
                (final name <|
                    float (printer [] (s << toString)) .placeholder placeholder
                )
                    |> toIcu
                    |> Expect.equal ("{" ++ placeholder ++ ", number}")
        , fuzz4 fuzzName fuzzName fuzzName (Fuzz.list fuzzName) "a float with a named printer" <|
            \name placeholder printerName printerNames ->
                (final name <|
                    float (printer (printerName :: printerNames) (s << toString)) .placeholder placeholder
                )
                    |> toIcu
                    |> Expect.equal
                        ("{"
                            ++ placeholder
                            ++ ", number, "
                            ++ String.join ", " (printerName :: printerNames)
                            ++ "}"
                        )
        , fuzz3 fuzzName fuzzName Fuzz.string "a plural with only one form" <|
            \name placeholder text ->
                (final name <|
                    plural (\_ _ -> Other) floatToString .placeholder placeholder <|
                        { other = s text
                        , zero = Nothing
                        , one = Nothing
                        , two = Nothing
                        , few = Nothing
                        , many = Nothing
                        }
                )
                    |> toIcu
                    |> Expect.equal
                        ("{" ++ placeholder ++ ", plural, other{" ++ text ++ "}}")
        , fuzz3 fuzzName fuzzName fuzzAllPluralForms "a plural with all plural forms" <|
            \name placeholder allPluralForms ->
                let
                    wrapCount text =
                        concat
                            [ s text
                            , count
                            , s text
                            ]
                in
                (final name <|
                    plural (\_ _ -> Other) floatToString .placeholder placeholder <|
                        { other = wrapCount allPluralForms.other
                        , zero = Just (wrapCount allPluralForms.zero)
                        , one = Just (wrapCount allPluralForms.one)
                        , two = Just (wrapCount allPluralForms.two)
                        , few = Just (wrapCount allPluralForms.few)
                        , many = Just (wrapCount allPluralForms.many)
                        }
                )
                    |> toIcu
                    |> Expect.equal
                        ("{"
                            ++ placeholder
                            ++ ", plural, other{"
                            ++ allPluralForms.other
                            ++ "#"
                            ++ allPluralForms.other
                            ++ "} zero{"
                            ++ allPluralForms.zero
                            ++ "#"
                            ++ allPluralForms.zero
                            ++ "} one{"
                            ++ allPluralForms.one
                            ++ "#"
                            ++ allPluralForms.one
                            ++ "} two{"
                            ++ allPluralForms.two
                            ++ "#"
                            ++ allPluralForms.two
                            ++ "} few{"
                            ++ allPluralForms.few
                            ++ "#"
                            ++ allPluralForms.few
                            ++ "} many{"
                            ++ allPluralForms.many
                            ++ "#"
                            ++ allPluralForms.many
                            ++ "}}"
                        )
        ]


floatToString =
    printer [] <|
        \float ->
            s (toString float)



---- FUZZER


fuzzName : Fuzzer String
fuzzName =
    Fuzz.map2
        (\int text ->
            "name" ++ toString int ++ text
        )
        Fuzz.int
        Fuzz.string


fuzzAllPluralForms :
    Fuzzer
        { other : String
        , zero : String
        , one : String
        , two : String
        , few : String
        , many : String
        }
fuzzAllPluralForms =
    Fuzz.map5
        (\text1 text2 text3 text4 text5 ->
            { other = text1
            , zero = text1 ++ text2
            , one = text2 ++ text3
            , two = text3 ++ text4
            , few = text4 ++ text5
            , many = text5 ++ text1
            }
        )
        Fuzz.string
        Fuzz.string
        Fuzz.string
        Fuzz.string
        Fuzz.string
