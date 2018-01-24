module TranslationTests exposing (..)

import Char
import Date exposing (Date)
import Expect
import Fuzz exposing (Fuzzer)
import Test exposing (Test, describe, fuzz2, fuzz3, fuzz4)
import Time exposing (Time)
import Translation exposing (..)


asStringWithTest : Test
asStringWithTest =
    describe "asStringWith"
        [ fuzz2 nameFuzzer textFuzzer "a simple text" <|
            \name text ->
                final name (s text)
                    |> asStringWith {}
                    |> Expect.equal text
        , fuzz2 nameFuzzer (Fuzz.list textFuzzer) "a list of simple texts" <|
            \name texts ->
                final name (concat (List.map s texts))
                    |> asStringWith {}
                    |> Expect.equal (String.concat texts)
        , fuzz3 nameFuzzer nameFuzzer textFuzzer "a string placeholder" <|
            \name placeholder text ->
                final name (string .placeholder placeholder)
                    |> asStringWith { placeholder = text }
                    |> Expect.equal text
        , fuzz3 nameFuzzer nameFuzzer Fuzz.float "a float placeholder" <|
            \name placeholder value ->
                final name (float floatToString .placeholder placeholder)
                    |> asStringWith { placeholder = value }
                    |> Expect.equal (toString value)
        , fuzz3 nameFuzzer nameFuzzer dateFuzzer "a date placeholder" <|
            \name placeholder value ->
                final name (date dateToString .placeholder placeholder)
                    |> asStringWith { placeholder = value }
                    |> Expect.equal (toString value)
        , fuzz3 nameFuzzer nameFuzzer timeFuzzer "a time placeholder" <|
            \name placeholder value ->
                final name (time timeToString .placeholder placeholder)
                    |> asStringWith { placeholder = value }
                    |> Expect.equal (toString value)
        , fuzz4 nameFuzzer nameFuzzer textFuzzer Fuzz.float "a plural with one form" <|
            \name placeholder text value ->
                final name
                    (plural floatToString (\_ _ -> Other) .placeholder placeholder <|
                        { other = s text
                        , zero = Nothing
                        , one = Nothing
                        , two = Nothing
                        , few = Nothing
                        , many = Nothing
                        }
                    )
                    |> asStringWith { placeholder = value }
                    |> Expect.equal text
        , [ ( 0, "zero" )
          , ( 1, "one" )
          , ( 2, "two" )
          , ( 3, "few" )
          , ( 4, "many" )
          , ( 5, "other" )
          ]
            |> List.map
                (\( count, text ) ->
                    fuzz2 nameFuzzer
                        nameFuzzer
                        ("count "
                            ++ toString count
                            ++ " selects form "
                            ++ text
                        )
                    <|
                        \name placeholder ->
                            let
                                toPluralForm count _ =
                                    case count of
                                        0 ->
                                            Zero

                                        1 ->
                                            One

                                        2 ->
                                            Two

                                        3 ->
                                            Few

                                        4 ->
                                            Many

                                        _ ->
                                            Other
                            in
                            final name
                                (plural floatToString toPluralForm .placeholder placeholder <|
                                    { other = s "other"
                                    , zero = Just (s "zero")
                                    , one = Just (s "one")
                                    , two = Just (s "two")
                                    , few = Just (s "few")
                                    , many = Just (s "many")
                                    }
                                )
                                |> asStringWith { placeholder = count }
                                |> Expect.equal text
                )
            |> describe "a plural form with all forms"
        , fuzz3 nameFuzzer nameFuzzer Fuzz.float "a plural using count" <|
            \name placeholder value ->
                final name
                    (plural floatToString (\_ _ -> Other) .placeholder placeholder <|
                        { other = count
                        , zero = Nothing
                        , one = Nothing
                        , two = Nothing
                        , few = Nothing
                        , many = Nothing
                        }
                    )
                    |> asStringWith { placeholder = value }
                    |> Expect.equal (toString value)
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
        , fuzz4 nameFuzzer nameFuzzer textFuzzer textFuzzer "a plural with only one form" <|
            \name placeholder textOld textNew ->
                let
                    targetLocale =
                        locale
                            |> addTranslations
                                [ ( name
                                  , [ "{"
                                    , placeholder
                                    , ", plural, other{# "
                                    , textNew
                                    , "}}"
                                    ]
                                        |> String.concat
                                  )
                                ]
                            |> addFloatPrinter floatToString2
                in
                final name
                    (plural floatToString (\_ _ -> Other) .placeholder placeholder <|
                        { other = s textOld
                        , zero = Nothing
                        , one = Nothing
                        , two = Nothing
                        , few = Nothing
                        , many = Nothing
                        }
                    )
                    |> translateToWith targetLocale { placeholder = 0 }
                    |> Expect.equal ("<0> " ++ textNew)
        , fuzz4 nameFuzzer nameFuzzer textFuzzer textFuzzer "a ordinal plural with only one form" <|
            \name placeholder textOld textNew ->
                let
                    targetLocale =
                        locale
                            |> addTranslations
                                [ ( name
                                  , [ "{"
                                    , placeholder
                                    , ", selectordinal, other{# "
                                    , textNew
                                    , "}}"
                                    ]
                                        |> String.concat
                                  )
                                ]
                            |> addFloatPrinter floatToString2
                in
                final name
                    (plural floatToString (\_ _ -> Other) .placeholder placeholder <|
                        { other = s textOld
                        , zero = Nothing
                        , one = Nothing
                        , two = Nothing
                        , few = Nothing
                        , many = Nothing
                        }
                    )
                    |> translateToWith targetLocale { placeholder = 0 }
                    |> Expect.equal ("<0> " ++ textNew)
        ]


toIcuTest : Test
toIcuTest =
    describe "toIcu"
        [ fuzz2 nameFuzzer Fuzz.string "a simple text" <|
            \name text ->
                (final name <|
                    s text
                )
                    |> toIcu
                    |> Expect.equal text
        , fuzz3 nameFuzzer Fuzz.string Fuzz.string "a list of simple texts" <|
            \name text1 text2 ->
                (final name <|
                    concat
                        [ s text1
                        , s text2
                        ]
                )
                    |> toIcu
                    |> Expect.equal (text1 ++ text2)
        , fuzz2 nameFuzzer nameFuzzer "a string placeholder" <|
            \name placeholder ->
                (final name <|
                    string .placeholder placeholder
                )
                    |> toIcu
                    |> Expect.equal ("{" ++ placeholder ++ "}")
        , fuzz3 nameFuzzer nameFuzzer Fuzz.string "a node" <|
            \name placeholder text ->
                (final name <|
                    node .placeholder placeholder <|
                        s text
                )
                    |> toIcu
                    |> Expect.equal
                        ("{" ++ placeholder ++ ", node, {" ++ text ++ "}}")
        , fuzz2 nameFuzzer nameFuzzer "a float with an unnamed printer" <|
            \name placeholder ->
                (final name <|
                    float (printer [] (s << toString)) .placeholder placeholder
                )
                    |> toIcu
                    |> Expect.equal ("{" ++ placeholder ++ ", number}")
        , fuzz4 nameFuzzer nameFuzzer nameFuzzer (Fuzz.list nameFuzzer) "a float with a named printer" <|
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
        , fuzz3 nameFuzzer nameFuzzer Fuzz.string "a plural with only one form" <|
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
        , fuzz3 nameFuzzer nameFuzzer allPluralFormsFuzzer "a plural with all plural forms" <|
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



---- PRINTER


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


dateFuzzer : Fuzzer Date
dateFuzzer =
    Fuzz.float
        |> Fuzz.map
            (\float ->
                (float * Time.millisecond)
                    |> Date.fromTime
            )


timeFuzzer : Fuzzer Time
timeFuzzer =
    Fuzz.float
        |> Fuzz.map
            (\float -> float * Time.millisecond)


allPluralFormsFuzzer :
    Fuzzer
        { other : String
        , zero : String
        , one : String
        , two : String
        , few : String
        , many : String
        }
allPluralFormsFuzzer =
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
        textFuzzer
        textFuzzer
        textFuzzer
        textFuzzer
        textFuzzer
