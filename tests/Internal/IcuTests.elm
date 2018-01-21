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
                    |> Expect.equal (Ok [ Argument placeholder [] [] ])
        , fuzz2 nameFuzzer nameFuzzer "a placeholder with one name" <|
            \placeholder name ->
                ("{" ++ placeholder ++ ", " ++ name ++ "}")
                    |> parse
                    |> Expect.equal (Ok [ Argument placeholder [ name ] [] ])
        , fuzz2 nameFuzzer textFuzzer "a placeholder with one unnamed subPart" <|
            \placeholder text ->
                ("{" ++ placeholder ++ ", {" ++ text ++ "}}")
                    |> parse
                    |> Expect.equal (Ok [ Argument placeholder [] [ Unnamed [ Text text ] ] ])
        , fuzz3 nameFuzzer nameFuzzer textFuzzer "a placeholder with on named subpart" <|
            \placeholder name text ->
                ("{" ++ placeholder ++ ", " ++ name ++ "{" ++ text ++ "}}")
                    |> parse
                    |> Expect.equal (Ok [ Argument placeholder [] [ Named name [ Text text ] ] ])
        , fuzz3 nameFuzzer nameFuzzer textFuzzer "a placeholder with one name and one unnamed subPart" <|
            \placeholder name text ->
                ("{" ++ placeholder ++ ", " ++ name ++ ", {" ++ text ++ "}}")
                    |> parse
                    |> Expect.equal (Ok [ Argument placeholder [ name ] [ Unnamed [ Text text ] ] ])
        , fuzz4 nameFuzzer nameFuzzer nameFuzzer textFuzzer "a placeholder with one name and one named subPart" <|
            \placeholder name1 name2 text ->
                ("{" ++ placeholder ++ ", " ++ name1 ++ ", " ++ name2 ++ "{" ++ text ++ "}}")
                    |> parse
                    |> Expect.equal (Ok [ Argument placeholder [ name1 ] [ Named name2 [ Text text ] ] ])
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
