module Translation
    exposing
        ( AllPluralForms
        , Name
        , PluralForm
            ( Few
            , Many
            , One
            , Other
            , Two
            , Zero
            )
        , Text
        , Translation
        , asNodes
        , asString
        , asStringWith
        , concat
        , count
        , date
        , delimited
        , fallback
        , final
        , float
        , list
        , node
        , plural
        , printer
        , s
        , string
        , time
        , toIcu
        , wrapper
        )

{-|

@docs Translation, Text

@docs final, fallback

@docs Name


# Creating `Text`'s

@docs s, concat, string, node

@docs list, delimited, wrapper

@docs float, date, time, printer

@docs plural, PluralForm, AllPluralForms, count


# Printing `Translation`s

@docs asString, asStringWith, asNodes


# Exporting `Translation`s

@docs toIcu

-}

import Date exposing (Date)
import Dict exposing (Dict)
import Set
import Time exposing (Time)
import VirtualDom exposing (Node)


{-| -}
type alias Name =
    String


{-| A `Translation` is a piece of localized text in a specific language.
You can turn it into a `String` using `asString`. It can also contain
placeholders, for example `Translation { args | name : String } msg`.
You then have to use `asStringWith` and provide values for every
placeholder.

It is also possible to turn a `Translation` into a list of Dom nodes
using `asNodes`. Take a look the package documentation for an example.
It's really convenient!

-}
type Translation args msg
    = Final String (Text args msg)
    | Fallback String (Text args msg)


{-| This is the building block for your translations. `Text`s can
either be just `String`s or placeholders for `String`s, `Float`s,
`Date`s, ..., along with rules for how to print these. Also
pluralization is possible. There are several functions for creating
`Text`s, further down.
-}
type Text args msg
    = Text String
    | Texts (List (Text args msg))
      -- with wrappers
    | List (Wrapper (List (Text args msg)) args msg) (List (Text args msg))
    | Delimited (Wrapper (Text args msg) args msg) (Text args msg)
      -- with placeholders
    | String (Placeholder String args)
    | Node (Placeholder (List (Node msg) -> Node msg) args) (Text args msg)
      -- with printers and placeholders
    | Float (Printer Float) (Placeholder Float args)
    | Date (Printer Date) (Placeholder Date args)
    | Time (Printer Time) (Placeholder Time args)
    | Plural (Float -> String -> PluralForm) (Printer Float) (Placeholder Float args) (AllPluralForms args msg)
      -- misc
    | Count


{-| -}
type Placeholder a args
    = Placeholder (args -> a) Name


{-| When printing placeholders which are not `String`s we need to know
how to turn them into `String`s. We do this by providing a `Printer`.
-}
type Printer a
    = Printer (List Name) (a -> String)


{-| -}
type Wrapper a args msg
    = Wrapper (List Name) (a -> Text args msg)


{-| -}
type PluralForm
    = Other
    | Zero
    | One
    | Two
    | Few
    | Many


{-| -}
type alias AllPluralForms args msg =
    { other : Text args msg
    , zero : Maybe (Text args msg)
    , one : Maybe (Text args msg)
    , two : Maybe (Text args msg)
    , few : Maybe (Text args msg)
    , many : Maybe (Text args msg)
    }


{-| Create a `Printer`. The first argument should be a unique
identifier, the second argument is the actual "printer":

    stringify : Printer a
    stringify =
        printer [ "stringify" ] toString

-}
printer : List Name -> (a -> String) -> Printer a
printer =
    Printer


{-| Create a `Wrapper`. The first argument should be a unique
identifier. For a example, a wrapper could add quotes:

    quote : Wrapper (Text args msg) args msg
    quote =
        wrapper [ "quotes", "outer" ] <|
            \text ->
                concat
                    [ s "\""
                    , text
                    , s "\""
                    ]

Or it could verbalize a list:

    listAnd : Wrapper (List (Text args msg)) args msg
    listAnd =
        wrapper [ "list", "and" ] <|
            \texts ->
                case List.reverse texts of
                    [] ->
                        s ""

                    lastText :: [] ->
                        lastText

                    lastText :: rest ->
                        [ [ lastText
                          , s " and "
                          ]
                        , rest
                            |> List.intersperse (s ", ")
                        ]
                            |> List.concat
                            |> List.reverse
                            |> concat

-}
wrapper : List Name -> (a -> Text args msg) -> Wrapper a args msg
wrapper =
    Wrapper



---- TRANSLATION CONSTRUCTOR


{-| Create a "final" `Translation` out of a `Text` by giving it a name.
Usually the name should be a string representation of the function name
used for the translation.

When using `kirchner/elm-translation-runner`, final translations will be
exported when running

    $ elm-translations generate-json

-}
final : Name -> Text args msg -> Translation args msg
final =
    Final


{-| Create a "fallback" translation. This works as `final`, but when
running

    $ elm-translations generate-json

it will **not** be exported.

-}
fallback : Name -> Text args msg -> Translation args msg
fallback =
    Fallback



---- TEXT CONSTRUCTOR


{-| Create a `Text` which gets replaced by the provided `String`:

    greeting : Translation args msg
    greeting =
        final "greeting" <|
            s "Good morning!"

-}
s : String -> Text args msg
s =
    Text


{-| Join a list of `Text`s:

    longerGreeting : Translation args msg
    longerGreeting =
        final "longerGreeting" <|
            concat
                [ s "Good evening!\n"
                , s "We are happy to have you here!"
                ]

-}
concat : List (Text args msg) -> Text args msg
concat =
    -- TODO: merge lists of lists
    Texts


{-| Create a placeholder for a `String`:

    personalGreeting : Translation { name : String } msg
    personalGreeting =
        final "greeting" <|
            concat
                [ "Hello, "
                , string .name "name"
                , "!"
                ]

-}
string : (args -> String) -> Name -> Text args msg
string accessor name =
    String (Placeholder accessor name)


{-| When using `asNodes` on a `Translation`, this `Text` will become
a node with a `Node.text` subnode containing the provided `Text`:

    question : Translation args msg
    question =
        final "question" <|
            concat
                [ s "What is "
                , node .strong "strong" <|
                    s "your"
                , s " favourite programming language?"
                ]

-}
node : (args -> (List (Node msg) -> Node msg)) -> Name -> Text args msg -> Text args msg
node accessor name =
    Node (Placeholder accessor name)


{-| -}
list : Wrapper (List (Text args msg)) args msg -> List (Text args msg) -> Text args msg
list =
    List


{-| You usually don't want to think about what quoation marks should be
used in the current language. This function let's you define helper wrap
text in the right symbols:

    containingQuotes : Translation args msg
    containingQuotes =
        final "containingQuotes" <|
            concat
                [ s "Then they said: "
                , quotedOuter <|
                    s "What is going on?"
                ]

    quotedOuter : Text args msg -> Text args msg
    quotedOuter =
        delimited <|
            wrapper [ "quotes", "outer" ] <|
                \text ->
                    concat
                        [ s "\""
                        , text
                        , s "\""
                        ]

-}
delimited : Wrapper (Text args msg) args msg -> Text args msg -> Text args msg
delimited =
    Delimited


{-| Create a placeholder for a `Float`. You also need to provide
a `Printer Float`, so we know how to turn the actual value into
a `String`:

    mailboxInfo : Translation { args | mailCount : Float } msg
    mailboxInfo =
        final "mailboxInfo" <|
            concat
                [ "Number of new mails: "
                , float intPrinter .mailCount "mailCount"
                ]

    intPrinter : Printer Float
    intPrinter =
        printer [ "int" ] <|
            \float ->
                float
                    |> floor
                    |> toString

**Note:** The package `kirchner/elm-cldr` exposes several of these
placeholder functions for all the number formats and locales which are
defined in the [CLDR](http://cldr.unicode.org). You most likely want to
use one of those.

-}
float : Printer Float -> (args -> Float) -> Name -> Text args msg
float printer accessor name =
    Float printer (Placeholder accessor name)


{-| -}
date : Printer Date -> (args -> Date) -> Name -> Text args msg
date printer accessor name =
    Date printer (Placeholder accessor name)


{-| -}
time : Printer Time -> (args -> Time) -> Name -> Text args msg
time printer accessor name =
    Time printer (Placeholder accessor name)


{-| -}
plural :
    (Float -> String -> PluralForm)
    -> Printer Float
    -> (args -> Float)
    -> Name
    -> AllPluralForms args msg
    -> Text args msg
plural toPluralForm printer accessor name =
    Plural toPluralForm printer (Placeholder accessor name)


{-| -}
count : Text args msg
count =
    Count



---- PRINT


{-| Turn a `Translation` into a `String` by providing values for all
placeholders.
-}
asStringWith : args -> Translation args msg -> String
asStringWith args translation =
    case translation of
        Final _ text ->
            printText Nothing args text

        Fallback _ text ->
            printText Nothing args text


{-| Turn a `Translation` which does not contain any placeholders into
a `String`.
-}
asString : Translation {} msg -> String
asString translation =
    asStringWith {} translation


{-| Turn a `Translation` into a list of dom nodes. Take a look at the
package documentation for examples.
-}
asNodes : args -> Translation args msg -> List (Node msg)
asNodes args translation =
    case translation of
        Final _ text ->
            textToNodes Nothing args text

        Fallback _ text ->
            textToNodes Nothing args text



-- INTERNAL PRINT HELPER


printText : Maybe String -> args -> Text args msg -> String
printText maybeCount args text =
    case text of
        Text string ->
            string

        Texts subTexts ->
            subTexts
                |> List.map (printText maybeCount args)
                |> String.concat

        String (Placeholder accessor _) ->
            accessor args

        Node (Placeholder accessor _) subText ->
            printText maybeCount args subText

        List (Wrapper _ wrapper) subTexts ->
            subTexts
                |> wrapper
                |> printText maybeCount args

        Delimited (Wrapper _ wrapper) subText ->
            subText
                |> wrapper
                |> printText maybeCount args

        Float (Printer _ print) (Placeholder accessor _) ->
            args
                |> accessor
                |> print

        Date (Printer _ print) (Placeholder accessor _) ->
            args
                |> accessor
                |> print

        Time (Printer _ print) (Placeholder accessor _) ->
            args
                |> accessor
                |> print

        Plural toPluralForm (Printer _ printer) (Placeholder accessor _) allPluralForms ->
            let
                count =
                    args
                        |> accessor
                        |> printer

                pluralForm =
                    toPluralForm (accessor args) count

                printMaybeForm form =
                    form
                        |> Maybe.withDefault allPluralForms.other
                        |> printText (Just count) args
            in
            case pluralForm of
                Other ->
                    allPluralForms.other
                        |> printText (Just count) args

                Zero ->
                    printMaybeForm allPluralForms.zero

                One ->
                    printMaybeForm allPluralForms.one

                Two ->
                    printMaybeForm allPluralForms.two

                Few ->
                    printMaybeForm allPluralForms.few

                Many ->
                    printMaybeForm allPluralForms.many

        Count ->
            case maybeCount of
                Just count ->
                    count

                Nothing ->
                    -- TODO: should we prevent this via types?
                    ""


textToNodes : Maybe String -> args -> Text args msg -> List (Node msg)
textToNodes maybeCount args text =
    case text of
        Text string ->
            [ VirtualDom.text string ]

        Texts subTexts ->
            subTexts
                |> List.map (textToNodes maybeCount args)
                |> List.concat

        String (Placeholder accessor _) ->
            [ args
                |> accessor
                |> VirtualDom.text
            ]

        Node (Placeholder accessor _) subText ->
            [ subText
                |> textToNodes maybeCount args
                |> accessor args
            ]

        List (Wrapper _ wrapper) subTexts ->
            subTexts
                |> wrapper
                |> textToNodes maybeCount args

        Delimited (Wrapper _ wrapper) subText ->
            subText
                |> wrapper
                |> textToNodes maybeCount args

        Float (Printer _ print) (Placeholder accessor _) ->
            [ args
                |> accessor
                |> print
                |> VirtualDom.text
            ]

        Date (Printer _ print) (Placeholder accessor _) ->
            [ args
                |> accessor
                |> print
                |> VirtualDom.text
            ]

        Time (Printer _ print) (Placeholder accessor _) ->
            [ args
                |> accessor
                |> print
                |> VirtualDom.text
            ]

        Plural toPluralForm (Printer _ printer) (Placeholder accessor _) allPluralForms ->
            let
                count =
                    args
                        |> accessor
                        |> printer

                pluralForm =
                    toPluralForm (accessor args) count

                printMaybeForm form =
                    form
                        |> Maybe.withDefault allPluralForms.other
                        |> textToNodes (Just count) args
            in
            case pluralForm of
                Other ->
                    allPluralForms.other
                        |> textToNodes (Just count) args

                Zero ->
                    printMaybeForm allPluralForms.zero

                One ->
                    printMaybeForm allPluralForms.one

                Two ->
                    printMaybeForm allPluralForms.two

                Few ->
                    printMaybeForm allPluralForms.few

                Many ->
                    printMaybeForm allPluralForms.many

        Count ->
            case maybeCount of
                Just count ->
                    [ VirtualDom.text count ]

                Nothing ->
                    -- TODO: should we prevent this via types?
                    []



---- TRANSLATE
--
--
--type Locale
--    = Locale LocaleData
--
--
--type alias LocaleData =
--    { translations : Dict String String
--    , printFloat : List Name -> Float -> String
--    , printInt : List Name -> Int -> String
--    , cardinalForm : Float -> String -> PluralForm
--    , ordinalForm : Float -> String -> PluralForm
--    , allowedCardinalForms : List PluralForm
--    , allowedOrdinalForms : List PluralForm
--    }
--
--
--locale : Locale
--locale =
--    Locale defaultLocaleData
--
--
--defaultLocaleData : LocaleData
--defaultLocaleData =
--    { translations = Dict.empty
--    , printFloat = \_ -> toString
--    , printInt = \_ -> toString
--    , cardinalForm = \_ _ -> Other
--    , ordinalForm = \_ _ -> Other
--    , allowedCardinalForms = [ Other ]
--    , allowedOrdinalForms = [ Other ]
--    }
--
--
--addTranslations : List ( String, String ) -> Locale -> Locale
--addTranslations translationList (Locale localeData) =
--    { localeData
--        | translations =
--            Dict.union
--                (translationList |> Dict.fromList)
--                localeData.translations
--    }
--        |> Locale
--
--
--addAllowedCardinalForms : List PluralForm -> Locale -> Locale
--addAllowedCardinalForms pluralForms (Locale localeData) =
--    { localeData
--        | allowedCardinalForms =
--            pluralForms
--                |> List.append localeData.allowedCardinalForms
--                |> makePluralFormsUnique
--    }
--        |> Locale
--
--
--addAllowedOrdinalForms : List PluralForm -> Locale -> Locale
--addAllowedOrdinalForms pluralForms (Locale localeData) =
--    { localeData
--        | allowedOrdinalForms =
--            pluralForms
--                |> List.append localeData.allowedOrdinalForms
--                |> makePluralFormsUnique
--    }
--        |> Locale
--
--
--makePluralFormsUnique : List PluralForm -> List PluralForm
--makePluralFormsUnique =
--    let
--        fromString form =
--            case form of
--                "Other" ->
--                    Other
--
--                "Zero" ->
--                    Zero
--
--                "One" ->
--                    One
--
--                "Two" ->
--                    Two
--
--                "Few" ->
--                    Few
--
--                "Many" ->
--                    Many
--
--                _ ->
--                    -- this cannot happen
--                    Other
--    in
--    List.map toString
--        >> Set.fromList
--        >> Set.toList
--        >> List.map fromString
--
--
--translateToWith : Locale -> args -> Translation args msg -> String
--translateToWith locale args translation =
--    case translation of
--        Final name text ->
--            translateText Nothing locale args name text
--
--        Fallback name text ->
--            translateText Nothing locale args name text
-- INTERNAL TRANSLATE HELPER
--
--
--translateText : Maybe String -> Locale -> args -> Name -> Text args msg -> String
--translateText maybeCount locale args name text =
--    "TODO: implement"
--
--
--nodeArgs : args -> Text args msg -> Dict Name (List (Node msg) -> Node msg)
--nodeArgs args text =
--    case text of
--        Texts texts ->
--            texts
--                |> List.map (nodeArgs args)
--                |> List.foldl Dict.union Dict.empty
--
--        Node accessor name _ ->
--            Dict.singleton name (args |> accessor)
--
--        _ ->
--            Dict.empty
--
--
--stringArgs : args -> Text args msg -> Dict Name String
--stringArgs args text =
--    case text of
--        Texts texts ->
--            texts
--                |> List.map (stringArgs args)
--                |> List.foldl Dict.union Dict.empty
--
--        String accessor name ->
--            Dict.singleton name (args |> accessor)
--
--        _ ->
--            Dict.empty
--
--
--floatArgs : args -> Text args msg -> Dict Name ( Printer Float, Float )
--floatArgs args text =
--    case text of
--        Texts texts ->
--            texts
--                |> List.map (floatArgs args)
--                |> List.foldl Dict.union Dict.empty
--
--        Float printer accessor name ->
--            Dict.singleton name ( printer, args |> accessor )
--
--        _ ->
--            Dict.empty
---- EXPORT TO ICU MESSAGE FORMAT


{-| -}
toIcu : Translation args msg -> String
toIcu translation =
    case translation of
        Final _ text ->
            textToIcu Nothing text

        Fallback _ text ->
            textToIcu Nothing text


textToIcu : Maybe (Text args msg) -> Text args msg -> String
textToIcu maybeCount text =
    let
        wrap string =
            "{" ++ string ++ "}"
    in
    case text of
        Text string ->
            string

        Texts subTexts ->
            subTexts
                |> List.map (textToIcu maybeCount)
                |> String.concat

        String (Placeholder _ name) ->
            wrap name

        Node (Placeholder _ name) subText ->
            [ name
            , ", node, "
            , subText
                |> textToIcu maybeCount
                |> wrap
            ]
                |> String.concat
                |> wrap

        List (Wrapper wrapperNames _) subTexts ->
            [ "list"
            , wrapperNames
                |> String.join ", "
            , subTexts
                |> List.map (textToIcu maybeCount)
                |> String.join " "
                |> wrap
            ]
                |> String.join ", "
                |> wrap

        Delimited (Wrapper wrapperNames _) subText ->
            [ "delimited"
            , wrapperNames
                |> String.join ", "
            , subText
                |> textToIcu maybeCount
                |> wrap
            ]
                |> String.join ", "
                |> wrap

        Float (Printer printerNames _) (Placeholder _ name) ->
            (name :: "number" :: printerNames)
                |> String.join ", "
                |> wrap

        Date (Printer printerNames _) (Placeholder _ name) ->
            (name :: "date" :: printerNames)
                |> String.join ", "
                |> wrap

        Time (Printer printerNames _) (Placeholder _ name) ->
            (name :: "time" :: printerNames)
                |> String.join ", "
                |> wrap

        Plural _ (Printer printerNames _) (Placeholder _ name) allPluralForms ->
            let
                pluralFormToIcu form text =
                    form ++ wrap (textToIcu Nothing text)
            in
            [ (name :: "plural" :: printerNames)
                |> String.join ", "
            , [ ( "other", Just allPluralForms.other )
              , ( "zero", allPluralForms.zero )
              , ( "one", allPluralForms.one )
              , ( "two", allPluralForms.two )
              , ( "few", allPluralForms.few )
              , ( "many", allPluralForms.many )
              ]
                |> List.filterMap
                    (\( form, maybeText ) ->
                        maybeText
                            |> Maybe.map (pluralFormToIcu form)
                    )
                |> String.join " "
            ]
                |> String.join ", "
                |> wrap

        Count ->
            "#"
