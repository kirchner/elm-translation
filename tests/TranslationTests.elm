module TranslationTests exposing (..)

import Char
import Date exposing (Date)
import Expect
import Fuzz exposing (Fuzzer)
import Test exposing (Test, describe, fuzz2, fuzz3, fuzz4)
import Time exposing (Time)
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
                    plural floatToString (\_ _ -> Other) .placeholder placeholder <|
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
                    plural floatToString (\_ _ -> Other) .placeholder placeholder <|
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


translateToWithTest : Test
translateToWithTest =
    describe "translateToWith"
        [ fuzz3 nameFuzzer textFuzzer textFuzzer "a simple text " <|
            \name textIn textOut ->
                let
                    targetLocale =
                        locale
                            |> addTranslations [ ( name, textOut ) ]
                in
                final name (s textIn)
                    |> translateToWith targetLocale {}
                    |> Expect.equal textOut
        , fuzz3 nameFuzzer nameFuzzer textFuzzer "a string placeholder" <|
            \name placeholder value ->
                let
                    targetLocale =
                        locale
                            |> addTranslations [ ( name, "{" ++ placeholder ++ "}" ) ]
                in
                final name (string .placeholder placeholder)
                    |> translateToWith targetLocale { placeholder = value }
                    |> Expect.equal value
        , fuzz3 nameFuzzer nameFuzzer Fuzz.float "a number placeholder" <|
            \name placeholder value ->
                let
                    targetLocale =
                        locale
                            |> addTranslations
                                [ ( name, "{" ++ placeholder ++ ", number}" ) ]
                            |> addFloatPrinter floatToString2
                in
                final name (float floatToString .placeholder placeholder)
                    |> translateToWith targetLocale { placeholder = value }
                    |> Expect.equal ("<" ++ toString value ++ ">")
        , fuzz3 nameFuzzer nameFuzzer Fuzz.float "a date placeholder" <|
            \name placeholder int ->
                let
                    value =
                        (int * Time.millisecond)
                            |> Date.fromTime

                    targetLocale =
                        locale
                            |> addTranslations
                                [ ( name, "{" ++ placeholder ++ ", date}" ) ]
                            |> addDatePrinter dateToString2
                in
                final name (date dateToString .placeholder placeholder)
                    |> translateToWith targetLocale { placeholder = value }
                    |> Expect.equal ("<" ++ toString value ++ ">")
        , fuzz3 nameFuzzer nameFuzzer Fuzz.float "a time placeholder" <|
            \name placeholder int ->
                let
                    value =
                        int * Time.millisecond

                    targetLocale =
                        locale
                            |> addTranslations
                                [ ( name, "{" ++ placeholder ++ ", time}" ) ]
                            |> addTimePrinter timeToString2
                in
                final name (time timeToString .placeholder placeholder)
                    |> translateToWith targetLocale { placeholder = value }
                    |> Expect.equal ("<" ++ toString value ++ ">")
        , fuzz3 nameFuzzer textFuzzer textFuzzer "a delimited text" <|
            \name textOld textNew ->
                let
                    targetLocale =
                        locale
                            |> addTranslations
                                [ ( name, "{_, delimited, quote, {" ++ textNew ++ "}}" ) ]
                            |> addDelimitedPrinter quote2
                in
                final name (delimited quote (s textOld))
                    |> translateToWith targetLocale {}
                    |> Expect.equal ("<<" ++ textNew ++ ">>")
        , fuzz3 nameFuzzer (Fuzz.list textFuzzer) (Fuzz.list textFuzzer) "a list" <|
            \name listOld listNew ->
                let
                    targetLocale =
                        locale
                            |> addTranslations
                                [ ( name
                                  , [ "{_, list, and, "
                                    , ("sthNew" :: listNew)
                                        |> List.map (\t -> "{" ++ t ++ "}")
                                        |> String.join " "
                                    , "}"
                                    ]
                                        |> String.concat
                                  )
                                ]
                            |> addListPrinter andList2
                in
                final name (list andList (listOld |> List.map s))
                    |> translateToWith targetLocale {}
                    |> Expect.equal (String.join " <and> " ("sthNew" :: listNew))
        ]


floatToString : Printer Float args msg
floatToString =
    printer [] <|
        \float ->
            s (toString float)


floatToString2 : Printer Float args msg
floatToString2 =
    printer [] <|
        \float ->
            concat
                [ s "<"
                , s (toString float)
                , s ">"
                ]


dateToString : Printer Date args msg
dateToString =
    printer [] <|
        \date ->
            s (toString date)


dateToString2 : Printer Date args msg
dateToString2 =
    printer [] <|
        \date ->
            concat
                [ s "<"
                , s (toString date)
                , s ">"
                ]


timeToString : Printer Time args msg
timeToString =
    printer [] <|
        \time ->
            s (toString time)


timeToString2 : Printer Time args msg
timeToString2 =
    printer [] <|
        \time ->
            concat
                [ s "<"
                , s (toString time)
                , s ">"
                ]


quote : Printer (Text args msg) args msg
quote =
    printer [ "quote" ] <|
        \text ->
            concat
                [ s "'"
                , text
                , s "'"
                ]


quote2 : Printer (Text args msg) args msg
quote2 =
    printer [ "quote" ] <|
        \text ->
            concat
                [ s "<<"
                , text
                , s ">>"
                ]


andList : Printer (List (Text args msg)) args msg
andList =
    printer [ "and" ] <|
        \texts ->
            texts
                |> List.intersperse (s " and ")
                |> concat


andList2 : Printer (List (Text args msg)) args msg
andList2 =
    printer [ "and" ] <|
        \texts ->
            texts
                |> List.intersperse (s " <and> ")
                |> concat



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
