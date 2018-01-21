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
        , Printer
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
        )

{-|

@docs Translation, Text

@docs final, fallback

@docs Name


# Printing `Translation`s

@docs asString, asStringWith, asNodes


# Creating `Text`'s


## Primitives

@docs s, concat, node


## Wrappers

@docs delimited, list


## Placeholders

@docs string, float, date, time

@docs plural, count, PluralForm, AllPluralForms


# Printers

@docs Printer, printer


# Exporting `Translation`s

@docs toIcu

-}

import Date exposing (Date)
import Dict exposing (Dict)
import Internal.Icu as Icu
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
pluralization is possible. There are several functions for creating and
manipulating `Text`s, further down.
-}
type Text args msg
    = Text String
    | Texts (List (Text args msg))
      -- with printers
    | Delimited (Printer (Text args msg) args msg) (Text args msg)
    | List (Printer (List (Text args msg)) args msg) (List (Text args msg))
      -- with placeholders
    | String (Placeholder String args)
    | Node (Placeholder (List (Node msg) -> Node msg) args) (Text args msg)
      -- with printers and placeholders
    | Float (Printer Float args msg) (Placeholder Float args)
    | Date (Printer Date args msg) (Placeholder Date args)
    | Time (Printer Time args msg) (Placeholder Time args)
    | Plural (Printer Float args msg) (Float -> String -> PluralForm) (Placeholder Float args) (AllPluralForms args msg)
      -- misc
    | Count


{-| -}
type Placeholder a args
    = Placeholder (args -> a) Name


{-| Some `Text`s need to know how to convert placeholders into `Text`.
We do this by providing a `Printer`.
-}
type Printer a args msg
    = Printer (List Name) (a -> Text args msg)


{-| This type represents all 6 plural forms defined by the
[CLDR](http://cldr.unicode.org).
-}
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
identifier, the second argument is the actual "printer". For example,
you can lift `toString` to a printer:

    stringify : Printer a args msg
    stringify =
        printer [ "stringify" ] <|
            \a ->
                s (toString a)

-}
printer : List Name -> (a -> Text args msg) -> Printer a args msg
printer =
    Printer



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


{-| Create a "fallback" translation. This works just as `final`, but
when running

    $ elm-translations generate-json

the translation will **not** be exported.

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

    personalGreeting : Translation { args | name : String } msg
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

    question : Translation { args | strong : List (Node msg) -> Node msg) } msg
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


{-| With this function you can create quotation helpers:

    quote : Text args msg -> Text args msg
    quote =
        delimited quotePrinter

    quotePrinter : Printer (Text args msg) args msg
    quotePrinter =
        printer [ "quotes", "outer" ] <|
            \text ->
                concat
                    [ s "‟"
                    , text
                    , s "”"
                    ]

    assumption : Translation args msg
    assumption =
        final "assumption" <|
            concat
                [ s "You may ask yourself: "
                , quote <|
                    s "What is this all for?"
                ]

Take a look at `kirchner/elm-cldr` which defines such helpers for all
languages contained in the [CLDR](http://cldr.unicode.org).

-}
delimited : Printer (Text args msg) args msg -> Text args msg -> Text args msg
delimited =
    Delimited


{-| Use this to build helpers if you want to turn a list like `[
"Alice", "Bob", "Cindy" ]` into `"Alice, Bob and Cindy"`. So basically,
this is like `concat` but you can specify how to actually join the
`Text`s:

    and : List (Text args msg) -> Text args msg
    and =
        list andPrinter

    andPrinter : Printer (List (Text args msg)) args msg
    andPrinter =
        printer [ "list", "and" ] <|
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

    membersInfo : Translation args msg
    membersInfo =
        final "membersInfo" <|
            concat
                [ s "These are all our members: "
                , and
                    [ s "Alice"
                    , s "Bob"
                    , s "Cindy"
                    ]
                ]

Take a look at `kirchner/elm-cldr` which defines such helpers for all
languages contained in the [CLDR](http://cldr.unicode.org).

-}
list : Printer (List (Text args msg)) args msg -> List (Text args msg) -> Text args msg
list =
    List


{-| Create a placeholder for a `Float`. You also need to provide
a `Printer Float`, so we know how to turn the actual value into
a `Text`:

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
                    |> s

**Note:** The package `kirchner/elm-cldr` exposes several of these
placeholder functions for all the number formats and locales which are
defined in the [CLDR](http://cldr.unicode.org). You most likely want to
use one of those.

-}
float : Printer Float args msg -> (args -> Float) -> Name -> Text args msg
float printer accessor name =
    Float printer (Placeholder accessor name)


{-| Like `float` but for `Date` values.
-}
date : Printer Date args msg -> (args -> Date) -> Name -> Text args msg
date printer accessor name =
    Date printer (Placeholder accessor name)


{-| Like `float` but for `Time` values.
-}
time : Printer Time args msg -> (args -> Time) -> Name -> Text args msg
time printer accessor name =
    Time printer (Placeholder accessor name)


{-| This function helps you if you need to choose different variants of
your translation depending on some numeric value. For example `"1
second"` vs. `"2 seconds"` vs. `"1.0 seconds"`. You have to provide
a printer for the number and a function which decides what plural form
should be choosen depending on the numeric value and its printed
representation. You can use `count` within a plural text to insert the
actual printed numerical value.

    newMailsInfo : Translation { args | count : Float } msg
    newMailsInfo =
        final "newMailsInfo" <|
            plural intPrinter toPluralForm .count "count" <|
                { other =
                    concat
                        [ s "You have "
                        , count
                        , s " new emails."
                        ]
                , zero = Nothing
                , one =
                    Just <|
                        concat
                            [ s "You have "
                            , count
                            , s " new email."
                            ]
                , two = Nothing
                , few = Nothing
                , many = Nothing
                }

    toPluralForm : Float -> String -> PluralForm
    toPluralForm float _ =
        if float == 1 then
            One
        else
            Other

Take a look at `kirchner/elm-cldr`. You will find pluralization
functions which are based on the pluralization rules of the
[CLDR](http://cldr.unicode.org). For example, a German version of the
above translation would look like this:

    import Cldr.De exposing (cardinal)

    newMailsInfo : Translation { args | count : Float } msg
    newMailsInfo =
        final "newMailsInfo" <|
            cardinal intPrinter .count "count" <|
                { other =
                    concat
                        [ s "Du hast "
                        , count
                        , s " neue Emails."
                        ]
                , one =
                    concat
                        [ s "Du hast "
                        , count
                        , s " neue Email."
                        ]
                }

So you never have to worry that you missed some pluralization form (or
defined forms which are not necessary)!

-}
plural :
    Printer Float args msg
    -> (Float -> String -> PluralForm)
    -> (args -> Float)
    -> Name
    -> AllPluralForms args msg
    -> Text args msg
plural printer toPluralForm accessor name =
    Plural printer toPluralForm (Placeholder accessor name)


{-| Used within a form of a plural text, this will insert the numerical
value using the printer which was provided to `plural`.
-}
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
package documentation for examples of why this is useful.
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

        Delimited (Printer _ printer) subText ->
            subText
                |> printer
                |> printText maybeCount args

        List (Printer _ printer) subTexts ->
            subTexts
                |> printer
                |> printText maybeCount args

        Float (Printer _ printer) (Placeholder accessor _) ->
            args
                |> accessor
                |> printer
                |> printText maybeCount args

        Date (Printer _ printer) (Placeholder accessor _) ->
            args
                |> accessor
                |> printer
                |> printText maybeCount args

        Time (Printer _ printer) (Placeholder accessor _) ->
            args
                |> accessor
                |> printer
                |> printText maybeCount args

        Plural (Printer _ printer) toPluralForm (Placeholder accessor _) allPluralForms ->
            let
                count =
                    args
                        |> accessor
                        |> printer
                        |> printText Nothing args

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


textToNodes : Maybe (Text args msg) -> args -> Text args msg -> List (Node msg)
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

        Delimited (Printer _ printer) subText ->
            subText
                |> printer
                |> textToNodes maybeCount args

        List (Printer _ printer) subTexts ->
            subTexts
                |> printer
                |> textToNodes maybeCount args

        Float (Printer _ printer) (Placeholder accessor _) ->
            args
                |> accessor
                |> printer
                |> textToNodes maybeCount args

        Date (Printer _ printer) (Placeholder accessor _) ->
            args
                |> accessor
                |> printer
                |> textToNodes maybeCount args

        Time (Printer _ printer) (Placeholder accessor _) ->
            args
                |> accessor
                |> printer
                |> textToNodes maybeCount args

        Plural (Printer _ printer) toPluralForm (Placeholder accessor _) allPluralForms ->
            let
                countAsString =
                    args
                        |> accessor
                        |> printer
                        |> printText Nothing args

                countAsText =
                    args
                        |> accessor
                        |> printer

                pluralForm =
                    toPluralForm (accessor args) countAsString

                printMaybeForm form =
                    form
                        |> Maybe.withDefault allPluralForms.other
                        |> textToNodes (Just countAsText) args
            in
            case pluralForm of
                Other ->
                    allPluralForms.other
                        |> textToNodes (Just countAsText) args

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
                        |> textToNodes Nothing args

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


{-| Turn a `Translation` into the (somewhat extended) [ICU Message
Format](http://icu-project.org/apiref/icu4j/com/ibm/icu/text/MessageFormat.html).
You probably never need this. We use this in
`kirchner/elm-translation-runner` to export `Translation`s to JSON
translation files.

**Note:** This only works if you provide unique `Name`s for the
different placeholders and translations.

-}
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

        Delimited (Printer wrapperNames _) subText ->
            [ "delimited"
            , wrapperNames
                |> String.join ", "
            , subText
                |> textToIcu maybeCount
                |> wrap
            ]
                |> String.join ", "
                |> wrap

        List (Printer wrapperNames _) subTexts ->
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

        Plural (Printer printerNames _) _ (Placeholder _ name) allPluralForms ->
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
