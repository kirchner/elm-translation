module Translation
    exposing
        ( AllPluralForms
        , ArgType
            ( ArgCardinal
            , ArgDate
            , ArgDelimited
            , ArgFloat
            , ArgList
            , ArgNode
            , ArgOrdinal
            , ArgString
            , ArgTime
            )
        , IcuArg
            ( OverwritePlaceholder
            , TakePlaceholder
            )
        , Locale
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
        , addAllowedCardinalForms
        , addAllowedOrdinalForms
        , addDatePrinter
        , addDelimitedPrinter
        , addFloatPrinter
        , addListPrinter
        , addTimePrinter
        , addToCardinalForm
        , addToOrdinalForm
        , addTranslations
        , argTypeToCldr
        , asNodes
        , asString
        , asStringWith
        , cldrToArgType
        , concat
        , count
        , date
        , delimited
        , fallback
        , final
        , float
        , list
        , locale
        , node
        , plural
        , printer
        , s
        , string
        , time
        , toElm
        , toElmType
        , toIcu
        , translateTo
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


# Translating `Translation`s

@docs translateTo

@docs Locale, locale

@docs ArgType, cldrToArgType

@docs addTranslations

@docs addToCardinalForm, addToOrdinalForm, addAllowedCardinalForms, addAllowedOrdinalForms

@docs addDelimitedPrinter, addListPrinter, addFloatPrinter, addDatePrinter, addTimePrinter


# Exporting `Translation`s

@docs toIcu

@docs IcuArg, argTypeToCldr


# Code Generation

@docs toElm, toElmType

-}

import Date exposing (Date)
import Dict exposing (Dict)
import Internal.Icu as Icu
import Set
import String.Extra as String
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
    = TText String
    | TTexts (List (Text args msg))
      -- with printers
    | TDelimited (Printer (Text args msg) args msg) (Text args msg)
    | TList (Printer (List (Text args msg)) args msg) (List (Text args msg))
      -- with placeholders
    | TString (Placeholder String args)
    | TNode (Placeholder (List (Node msg) -> Node msg) args) (Text args msg)
      -- with printers and placeholders
    | TFloat (Printer Float args msg) (Placeholder Float args)
    | TDate (Printer Date args msg) (Placeholder Date args)
    | TTime (Printer Time args msg) (Placeholder Time args)
    | TPlural (Printer Float args msg) (Float -> String -> PluralForm) (Placeholder Float args) (AllPluralForms args msg)
      -- misc
    | TCount


{-| -}
type ArgType
    = ArgString
    | ArgNode
    | ArgDelimited (List Name)
    | ArgList (List Name)
    | ArgFloat (List Name)
    | ArgDate (List Name)
    | ArgTime (List Name)
    | ArgCardinal (List Name)
    | ArgOrdinal (List Name)


{-| -}
type IcuArg
    = TakePlaceholder (List Name)
    | OverwritePlaceholder Name (List Name)


{-| -}
type Placeholder a args
    = Placeholder (args -> a) Name


{-| Some `Text`s need to know how to convert placeholders into `Text`.
We do this by providing a `Printer`.
-}
type Printer a args msg
    = Printer (List Name) (a -> Text args msg)


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


{-| -}
cldrToArgType : List Name -> Maybe ArgType
cldrToArgType names =
    case names of
        "_" :: "delimited" :: otherNames ->
            Just (ArgDelimited otherNames)

        "_" :: "list" :: otherNames ->
            Just (ArgList otherNames)

        _ :: "number" :: otherNames ->
            (Just << ArgFloat) <|
                case otherNames of
                    [] ->
                        [ "decimal", "standard" ]

                    _ ->
                        "decimal" :: otherNames

        _ :: "date" :: otherNames ->
            Just (ArgDate otherNames)

        _ :: "time" :: otherNames ->
            Just (ArgTime otherNames)

        _ :: "plural" :: otherNames ->
            case otherNames of
                [] ->
                    Just (ArgCardinal [ "decimal", "standard" ])

                _ ->
                    Just (ArgCardinal otherNames)

        _ :: "selectordinal" :: otherNames ->
            case otherNames of
                [] ->
                    Just (ArgOrdinal [ "decimal", "standard" ])

                _ ->
                    Just (ArgOrdinal otherNames)

        _ :: "node" :: [] ->
            Just ArgNode

        _ :: [] ->
            Just ArgString

        _ ->
            Nothing


{-| -}
argTypeToCldr : ArgType -> IcuArg
argTypeToCldr argType =
    case argType of
        ArgString ->
            TakePlaceholder []

        ArgNode ->
            TakePlaceholder [ "node" ]

        ArgDelimited names ->
            OverwritePlaceholder "_" ("delimited" :: names)

        ArgList names ->
            OverwritePlaceholder "_" ("list" :: names)

        ArgFloat names ->
            TakePlaceholder ("number" :: names)

        ArgDate names ->
            TakePlaceholder ("date" :: names)

        ArgTime names ->
            TakePlaceholder ("time" :: names)

        ArgCardinal names ->
            TakePlaceholder ("plural" :: names)

        ArgOrdinal names ->
            TakePlaceholder ("selectordinal" :: names)



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
    TText


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
concat texts =
    case texts of
        text :: [] ->
            text

        _ ->
            TTexts texts


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
    TString (Placeholder accessor name)


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
    TNode (Placeholder accessor name)


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
    TDelimited


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
    TList


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
    TFloat printer (Placeholder accessor name)


{-| Like `float` but for `Date` values.
-}
date : Printer Date args msg -> (args -> Date) -> Name -> Text args msg
date printer accessor name =
    TDate printer (Placeholder accessor name)


{-| Like `float` but for `Time` values.
-}
time : Printer Time args msg -> (args -> Time) -> Name -> Text args msg
time printer accessor name =
    TTime printer (Placeholder accessor name)


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
    TPlural printer toPluralForm (Placeholder accessor name)


{-| Used within a form of a plural text, this will insert the numerical
value using the printer which was provided to `plural`.
-}
count : Text args msg
count =
    TCount



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
        TText string ->
            string

        TTexts subTexts ->
            subTexts
                |> List.map (printText maybeCount args)
                |> String.concat

        TString (Placeholder accessor _) ->
            accessor args

        TNode (Placeholder accessor _) subText ->
            printText maybeCount args subText

        TDelimited (Printer _ printer) subText ->
            subText
                |> printer
                |> printText maybeCount args

        TList (Printer _ printer) subTexts ->
            subTexts
                |> printer
                |> printText maybeCount args

        TFloat (Printer _ printer) (Placeholder accessor _) ->
            args
                |> accessor
                |> printer
                |> printText maybeCount args

        TDate (Printer _ printer) (Placeholder accessor _) ->
            args
                |> accessor
                |> printer
                |> printText maybeCount args

        TTime (Printer _ printer) (Placeholder accessor _) ->
            args
                |> accessor
                |> printer
                |> printText maybeCount args

        TPlural (Printer _ printer) toPluralForm (Placeholder accessor _) allPluralForms ->
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

        TCount ->
            case maybeCount of
                Just count ->
                    count

                Nothing ->
                    -- TODO: should we prevent this via types?
                    ""


textToNodes : Maybe (Text args msg) -> args -> Text args msg -> List (Node msg)
textToNodes maybeCount args text =
    case text of
        TText string ->
            [ VirtualDom.text string ]

        TTexts subTexts ->
            subTexts
                |> List.map (textToNodes maybeCount args)
                |> List.concat

        TString (Placeholder accessor _) ->
            [ args
                |> accessor
                |> VirtualDom.text
            ]

        TNode (Placeholder accessor _) subText ->
            [ subText
                |> textToNodes maybeCount args
                |> accessor args
            ]

        TDelimited (Printer _ printer) subText ->
            subText
                |> printer
                |> textToNodes maybeCount args

        TList (Printer _ printer) subTexts ->
            subTexts
                |> printer
                |> textToNodes maybeCount args

        TFloat (Printer _ printer) (Placeholder accessor _) ->
            args
                |> accessor
                |> printer
                |> textToNodes maybeCount args

        TDate (Printer _ printer) (Placeholder accessor _) ->
            args
                |> accessor
                |> printer
                |> textToNodes maybeCount args

        TTime (Printer _ printer) (Placeholder accessor _) ->
            args
                |> accessor
                |> printer
                |> textToNodes maybeCount args

        TPlural (Printer _ printer) toPluralForm (Placeholder accessor _) allPluralForms ->
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

        TCount ->
            case maybeCount of
                Just count ->
                    count
                        |> textToNodes Nothing args

                Nothing ->
                    -- TODO: should we prevent this via types?
                    []



---- TRANSLATE


{-| -}
type Locale args msg
    = Locale (LocaleData args msg)


type alias LocaleData args msg =
    { translations : Dict String String

    -- printer
    , delimitedPrinters : Dict (List Name) (Printer (Text args msg) args msg)
    , listPrinters : Dict (List Name) (Printer (List (Text args msg)) args msg)
    , floatPrinters : Dict (List Name) (Printer Float args msg)
    , datePrinters : Dict (List Name) (Printer Date args msg)
    , timePrinters : Dict (List Name) (Printer Time args msg)

    -- pluralization
    , toCardinalForm : Float -> String -> PluralForm
    , toOrdinalForm : Float -> String -> PluralForm
    , allowedCardinalForms : List PluralForm
    , allowedOrdinalForms : List PluralForm
    }


{-| -}
locale : Locale args msg
locale =
    Locale defaultLocaleData


defaultLocaleData : LocaleData args msg
defaultLocaleData =
    { translations = Dict.empty

    -- printer
    , delimitedPrinters = Dict.empty
    , listPrinters = Dict.empty
    , floatPrinters = Dict.empty
    , datePrinters = Dict.empty
    , timePrinters = Dict.empty

    -- pluralization
    , toCardinalForm = \_ _ -> Other
    , toOrdinalForm = \_ _ -> Other
    , allowedCardinalForms = [ Other ]
    , allowedOrdinalForms = [ Other ]
    }


{-| -}
addTranslations : List ( String, String ) -> Locale args msg -> Locale args msg
addTranslations translationList (Locale localeData) =
    Locale
        { localeData
            | translations =
                Dict.union
                    (translationList |> Dict.fromList)
                    localeData.translations
        }


{-| -}
addDelimitedPrinter : Printer (Text args msg) args msg -> Locale args msg -> Locale args msg
addDelimitedPrinter ((Printer names _) as printer) (Locale localeData) =
    Locale
        { localeData
            | delimitedPrinters =
                Dict.insert names printer localeData.delimitedPrinters
        }


{-| -}
addListPrinter : Printer (List (Text args msg)) args msg -> Locale args msg -> Locale args msg
addListPrinter ((Printer names _) as printer) (Locale localeData) =
    Locale
        { localeData
            | listPrinters =
                Dict.insert names printer localeData.listPrinters
        }


{-| -}
addFloatPrinter : Printer Float args msg -> Locale args msg -> Locale args msg
addFloatPrinter ((Printer names _) as printer) (Locale localeData) =
    Locale
        { localeData
            | floatPrinters =
                Dict.insert names printer localeData.floatPrinters
        }


{-| -}
addDatePrinter : Printer Date args msg -> Locale args msg -> Locale args msg
addDatePrinter ((Printer names _) as printer) (Locale localeData) =
    Locale
        { localeData
            | datePrinters =
                Dict.insert names printer localeData.datePrinters
        }


{-| -}
addTimePrinter : Printer Time args msg -> Locale args msg -> Locale args msg
addTimePrinter ((Printer names _) as printer) (Locale localeData) =
    Locale
        { localeData
            | timePrinters =
                Dict.insert names printer localeData.timePrinters
        }


{-| -}
addToCardinalForm : (Float -> String -> PluralForm) -> Locale args msg -> Locale args msg
addToCardinalForm toCardinalForm (Locale localeData) =
    Locale
        { localeData | toCardinalForm = toCardinalForm }


{-| -}
addToOrdinalForm : (Float -> String -> PluralForm) -> Locale args msg -> Locale args msg
addToOrdinalForm toOrdinalForm (Locale localeData) =
    Locale
        { localeData | toOrdinalForm = toOrdinalForm }


{-| -}
addAllowedCardinalForms : List PluralForm -> Locale args msg -> Locale args msg
addAllowedCardinalForms pluralForms (Locale localeData) =
    { localeData
        | allowedCardinalForms =
            pluralForms
                |> List.append localeData.allowedCardinalForms
                |> makePluralFormsUnique
    }
        |> Locale


{-| -}
addAllowedOrdinalForms : List PluralForm -> Locale args msg -> Locale args msg
addAllowedOrdinalForms pluralForms (Locale localeData) =
    { localeData
        | allowedOrdinalForms =
            pluralForms
                |> List.append localeData.allowedOrdinalForms
                |> makePluralFormsUnique
    }
        |> Locale


makePluralFormsUnique : List PluralForm -> List PluralForm
makePluralFormsUnique =
    let
        fromString form =
            case form of
                "Other" ->
                    Other

                "Zero" ->
                    Zero

                "One" ->
                    One

                "Two" ->
                    Two

                "Few" ->
                    Few

                "Many" ->
                    Many

                _ ->
                    -- this cannot happen
                    Other
    in
    List.map toString
        >> Set.fromList
        >> Set.toList
        >> List.map fromString


{-| -}
translateTo :
    (List Name -> Maybe ArgType)
    -> Locale args msg
    -> Translation args msg
    -> Translation args msg
translateTo toArgType locale translation =
    case translation of
        Final name text ->
            Final name <|
                translateText toArgType Nothing locale name text

        Fallback name text ->
            Fallback name <|
                translateText toArgType Nothing locale name text



--  INTERNAL TRANSLATE HELPER


translateText :
    (List Name -> Maybe ArgType)
    -> Maybe String
    -> Locale args msg
    -> Name
    -> Text args msg
    -> Text args msg
translateText toArgType maybeCount ((Locale localeData) as locale) name text =
    localeData.translations
        |> Dict.get name
        |> Maybe.andThen (Icu.parse >> Result.toMaybe)
        |> Maybe.map
            (icuToText toArgType
                locale
                { node = nodeAccessors text
                , string = stringAccessors text
                , float = floatAccessors text
                , date = dateAccessors text
                , time = timeAccessors text
                }
            )
        |> Maybe.withDefault text


icuToText :
    (List Name -> Maybe ArgType)
    -> Locale args msg
    ->
        { node : Dict Name (args -> (List (Node msg) -> Node msg))
        , string : Dict Name (args -> String)
        , float : Dict Name (args -> Float)
        , date : Dict Name (args -> Date)
        , time : Dict Name (args -> Time)
        }
    -> Icu.Message
    -> Text args msg
icuToText toArgType locale accessors message =
    message
        |> List.map (icuPartToText toArgType locale accessors)
        |> concat


icuPartToText :
    (List Name -> Maybe ArgType)
    -> Locale args msg
    ->
        { node : Dict Name (args -> (List (Node msg) -> Node msg))
        , string : Dict Name (args -> String)
        , float : Dict Name (args -> Float)
        , date : Dict Name (args -> Date)
        , time : Dict Name (args -> Time)
        }
    -> Icu.Part
    -> Text args msg
icuPartToText toArgType ((Locale localeData) as locale) accessors part =
    let
        simplePlaceholder constructor placeholder otherNames printers accessorType =
            Maybe.map2
                (\printer accessor -> constructor printer accessor placeholder)
                (localeData
                    |> printers
                    |> Dict.get otherNames
                )
                (accessors
                    |> accessorType
                    |> Dict.get placeholder
                )
                |> Maybe.withDefault (s "")
    in
    case part of
        Icu.Text text ->
            s text

        Icu.Argument placeholder names subMessages ->
            case toArgType (placeholder :: names) of
                Just ArgString ->
                    accessors.string
                        |> Dict.get placeholder
                        |> Maybe.map
                            (\accessor ->
                                string accessor placeholder
                            )
                        |> Maybe.withDefault (s "")

                Just ArgNode ->
                    accessors.node
                        |> Dict.get placeholder
                        |> Maybe.map
                            (\accessor ->
                                case subMessages of
                                    (Icu.Unnamed subMessage) :: [] ->
                                        node accessor placeholder <|
                                            icuToText toArgType locale accessors subMessage

                                    _ ->
                                        s ""
                            )
                        |> Maybe.withDefault (s "")

                Just (ArgDelimited otherNames) ->
                    case subMessages of
                        (Icu.Unnamed subMessage) :: [] ->
                            localeData.delimitedPrinters
                                |> Dict.get otherNames
                                |> Maybe.map
                                    (\printer ->
                                        delimited printer <|
                                            icuToText toArgType locale accessors subMessage
                                    )
                                |> Maybe.withDefault (s "")

                        _ ->
                            s ""

                Just (ArgList otherNames) ->
                    let
                        toText subMessage =
                            case subMessage of
                                Icu.Unnamed actualMessage ->
                                    icuToText toArgType locale accessors actualMessage

                                Icu.Named _ _ ->
                                    s ""
                    in
                    localeData.listPrinters
                        |> Dict.get otherNames
                        |> Maybe.map
                            (\printer ->
                                list printer <|
                                    List.map toText subMessages
                            )
                        |> Maybe.withDefault (s "")

                Just (ArgFloat otherNames) ->
                    simplePlaceholder float placeholder otherNames .floatPrinters .float

                Just (ArgDate otherNames) ->
                    simplePlaceholder date placeholder otherNames .datePrinters .date

                Just (ArgTime otherNames) ->
                    simplePlaceholder time placeholder otherNames .timePrinters .time

                Just (ArgCardinal otherNames) ->
                    Maybe.map3
                        (\printer accessor allPluralForms ->
                            plural
                                printer
                                localeData.toCardinalForm
                                accessor
                                placeholder
                                allPluralForms
                        )
                        (Dict.get otherNames localeData.floatPrinters)
                        (Dict.get placeholder accessors.float)
                        (allPluralForms (icuToText toArgType locale accessors) subMessages)
                        |> Maybe.withDefault (s "")

                Just (ArgOrdinal otherNames) ->
                    Maybe.map3
                        (\printer accessor allPluralForms ->
                            plural
                                printer
                                localeData.toOrdinalForm
                                accessor
                                placeholder
                                allPluralForms
                        )
                        (Dict.get otherNames localeData.floatPrinters)
                        (Dict.get placeholder accessors.float)
                        (allPluralForms (icuToText toArgType locale accessors) subMessages)
                        |> Maybe.withDefault (s "")

                Nothing ->
                    s ""

        Icu.Hash ->
            count


allPluralForms :
    (Icu.Message -> Text args msg)
    -> List Icu.SubMessage
    -> Maybe (AllPluralForms args msg)
allPluralForms toText subMessages =
    subMessages
        |> List.foldl
            (addPluralForm toText)
            { other = s ""
            , zero = Nothing
            , one = Nothing
            , two = Nothing
            , few = Nothing
            , many = Nothing
            }
        |> Just


addPluralForm :
    (Icu.Message -> Text args msg)
    -> Icu.SubMessage
    -> AllPluralForms args msg
    -> AllPluralForms args msg
addPluralForm toText subMessage allPluralForms =
    case subMessage of
        Icu.Named name message ->
            let
                text =
                    toText message
            in
            case name of
                "other" ->
                    { allPluralForms | other = text }

                "zero" ->
                    { allPluralForms | zero = Just text }

                "one" ->
                    { allPluralForms | one = Just text }

                "two" ->
                    { allPluralForms | two = Just text }

                "few" ->
                    { allPluralForms | few = Just text }

                "many" ->
                    { allPluralForms | many = Just text }

                _ ->
                    allPluralForms

        _ ->
            allPluralForms



-- RETRIVE ARGUMENTS


nodeAccessors : Text args msg -> Dict Name (args -> (List (Node msg) -> Node msg))
nodeAccessors text =
    case text of
        TNode (Placeholder accessor name) _ ->
            Dict.singleton name accessor

        _ ->
            descendWith nodeAccessors text


stringAccessors : Text args msg -> Dict Name (args -> String)
stringAccessors text =
    case text of
        TString (Placeholder accessor name) ->
            Dict.singleton name accessor

        _ ->
            descendWith stringAccessors text


floatAccessors : Text args msg -> Dict Name (args -> Float)
floatAccessors text =
    case text of
        TFloat _ (Placeholder accessor name) ->
            Dict.singleton name accessor

        TPlural _ _ (Placeholder accessor name) _ ->
            Dict.singleton name accessor

        _ ->
            descendWith floatAccessors text


dateAccessors : Text args msg -> Dict Name (args -> Date)
dateAccessors text =
    case text of
        TDate _ (Placeholder accessor name) ->
            Dict.singleton name accessor

        _ ->
            descendWith dateAccessors text


timeAccessors : Text args msg -> Dict Name (args -> Time)
timeAccessors text =
    case text of
        TTime _ (Placeholder accessor name) ->
            Dict.singleton name accessor

        _ ->
            descendWith timeAccessors text


descendWith :
    (Text args msg -> Dict Name a)
    -> Text args msg
    -> Dict Name a
descendWith extractor text =
    case text of
        TTexts subTexts ->
            subTexts
                |> List.map extractor
                |> List.foldl Dict.union Dict.empty

        TDelimited _ subText ->
            -- FIXME: should we extract arguments hidden in printers?
            extractor subText

        TList _ subTexts ->
            -- FIXME: should we extract arguments hidden in printers?
            subTexts
                |> List.map extractor
                |> List.foldl Dict.union Dict.empty

        _ ->
            Dict.empty



---- EXPORT TO ICU MESSAGE FORMAT


{-| Turn a `Translation` into the (somewhat extended) [ICU Message
Format](http://icu-project.org/apiref/icu4j/com/ibm/icu/text/MessageFormat.html).
You probably never need this. We use this in
`kirchner/elm-translation-runner` to export `Translation`s to JSON
translation files.

**Note:** This only works if you provide unique `Name`s for the
different placeholders and translations.

-}
toIcu :
    (ArgType -> IcuArg)
    -> Translation args msg
    -> String
toIcu fromArgType translation =
    case translation of
        Final _ text ->
            textToIcu fromArgType Nothing text

        Fallback _ text ->
            textToIcu fromArgType Nothing text


textToIcu :
    (ArgType -> IcuArg)
    -> Maybe (Text args msg)
    -> Text args msg
    -> String
textToIcu fromArgType maybeCount text =
    let
        wrap string =
            "{" ++ string ++ "}"

        wrapWith argType placeholder =
            case fromArgType argType of
                TakePlaceholder names ->
                    (placeholder :: names)
                        |> String.join ", "

                OverwritePlaceholder newPlaceholder names ->
                    (newPlaceholder :: names)
                        |> String.join ", "
    in
    case text of
        TText string ->
            string

        TTexts subTexts ->
            subTexts
                |> List.map (textToIcu fromArgType maybeCount)
                |> String.concat

        TString (Placeholder _ name) ->
            wrapWith ArgString name
                |> wrap

        TNode (Placeholder _ name) subText ->
            [ wrapWith ArgNode name
            , ", "
            , subText
                |> textToIcu fromArgType maybeCount
                |> wrap
            ]
                |> String.concat
                |> wrap

        TDelimited (Printer wrapperNames _) subText ->
            [ wrapWith (ArgDelimited wrapperNames) "_"
            , subText
                |> textToIcu fromArgType maybeCount
                |> wrap
            ]
                |> String.join ", "
                |> wrap

        TList (Printer wrapperNames _) subTexts ->
            [ wrapWith (ArgList wrapperNames) "_"
            , subTexts
                |> List.map (textToIcu fromArgType maybeCount)
                |> String.join " "
                |> wrap
            ]
                |> String.join ", "
                |> wrap

        TFloat (Printer printerNames _) (Placeholder _ name) ->
            wrapWith (ArgFloat printerNames) name
                |> wrap

        TDate (Printer printerNames _) (Placeholder _ name) ->
            wrapWith (ArgDate printerNames) name
                |> wrap

        TTime (Printer printerNames _) (Placeholder _ name) ->
            wrapWith (ArgTime printerNames) name
                |> wrap

        TPlural (Printer printerNames _) _ (Placeholder _ name) allPluralForms ->
            -- TODO: Ordinal?
            let
                pluralFormToIcu form text =
                    form ++ wrap (textToIcu fromArgType Nothing text)
            in
            [ wrapWith (ArgCardinal printerNames) name
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

        TCount ->
            "#"



---- GENERATING ELM CODE


{-| Given an ICU message, this function produces Elm code for a `Translation`.
For example, `toElm "greeting" "Good morning, {name}!"` will produce the
following function:

    greeting : Translation { args | name : String } msg
    greeting =
        final "greeting" <|
            concat
                [ s "Good morning, "
                , string .name "name"
                , s "!"
                ]

-}
toElm : (List String -> Maybe ArgType) -> String -> String -> Maybe String
toElm toArgType name icuMessage =
    icuMessage
        |> Icu.parse
        |> Result.toMaybe
        |> Maybe.map (icuToElm toArgType name)


{-| Given an ICU message, this function returns the Elm code for the type of
the corresponding `Translation`. So `toElmType "Good morning, {name}!"` will
produce `"Translation { args | name : String } msg`, for example.
-}
toElmType : (List String -> Maybe ArgType) -> String -> Maybe String
toElmType toArgType icuMessage =
    icuMessage
        |> Icu.parse
        |> Result.toMaybe
        |> Maybe.map
            (argsFromMessage toArgType
                >> returnType
            )


icuToElm : (List String -> Maybe ArgType) -> String -> Icu.Message -> String
icuToElm toArgType name icuMessage =
    [ icuMessage
        |> argsFromMessage toArgType
        |> functionDeclaration name
    , functionDefinition toArgType name icuMessage
    ]
        |> String.join "\n"


argsFromMessage : (List String -> Maybe ArgType) -> Icu.Message -> Dict String String
argsFromMessage toArgType message =
    message
        |> List.map (argsFromPart toArgType)
        |> List.foldl Dict.union Dict.empty


argsFromPart : (List String -> Maybe ArgType) -> Icu.Part -> Dict String String
argsFromPart toArgType part =
    case part of
        Icu.Argument name names subMessages ->
            case toArgType (name :: names) of
                Just ArgString ->
                    Dict.singleton name "String"

                Just ArgNode ->
                    case subMessages of
                        (Icu.Unnamed subMessage) :: [] ->
                            Dict.union
                                (Dict.singleton name "List (Node msg) -> Node msg")
                                (argsFromMessage toArgType subMessage)

                        _ ->
                            Dict.empty

                Just (ArgDelimited otherNames) ->
                    case subMessages of
                        (Icu.Unnamed subMessage) :: [] ->
                            argsFromMessage toArgType subMessage

                        _ ->
                            Dict.empty

                Just (ArgList otherNames) ->
                    let
                        argsFromSubMessage subMessage =
                            case subMessage of
                                Icu.Named _ subMessage ->
                                    Just (argsFromMessage toArgType subMessage)

                                _ ->
                                    Nothing
                    in
                    subMessages
                        |> List.filterMap argsFromSubMessage
                        |> List.foldl Dict.union Dict.empty

                Just (ArgFloat otherNames) ->
                    Dict.singleton name "Float"

                Just (ArgDate otherNames) ->
                    Dict.singleton name "Date"

                Just (ArgTime otherNames) ->
                    Dict.singleton name "Time"

                Just (ArgCardinal otherNames) ->
                    Dict.singleton name "Float"

                Just (ArgOrdinal otherNames) ->
                    Dict.singleton name "Float"

                Nothing ->
                    Dict.empty

        _ ->
            Dict.empty


functionDeclaration : String -> Dict String String -> String
functionDeclaration name args =
    [ name
    , ":"
    , returnType args
    ]
        |> String.join " "


returnType : Dict String String -> String
returnType args =
    [ "Translation"
    , arguments args
    , "msg"
    ]
        |> String.join " "


arguments : Dict String String -> String
arguments args =
    let
        printArgument name tvpe =
            [ name
            , ":"
            , tvpe
            ]
                |> String.join " "
    in
    if Dict.isEmpty args then
        "args"
    else
        [ "{ args |"
        , args
            |> Dict.map printArgument
            |> Dict.values
            |> String.join ", "
        , "}"
        ]
            |> String.join " "


functionDefinition : (List String -> Maybe ArgType) -> String -> Icu.Message -> String
functionDefinition toArgType name icuMessage =
    let
        body =
            [ [ "final "
              , quote name
              , " <|"
              ]
                |> String.concat
            , icuMessage
                |> messageToElm toArgType
                |> indent
            ]
                |> String.join "\n"
    in
    [ name ++ " ="
    , body
        |> indent
    ]
        |> String.join "\n"


messageToElm : (List String -> Maybe ArgType) -> Icu.Message -> String
messageToElm toArgType parts =
    case parts of
        part :: [] ->
            part
                |> partToElm toArgType

        _ ->
            [ "concat"
            , parts
                |> List.map (partToElm toArgType)
                |> generateList
                |> indent
            ]
                |> String.join "\n"


partToElm : (List String -> Maybe ArgType) -> Icu.Part -> String
partToElm toArgType part =
    let
        camilize names =
            case names of
                [] ->
                    "default"

                name :: [] ->
                    name

                name :: rest ->
                    name
                        :: List.map String.toSentenceCase rest
                        |> String.concat

        simplePlaceholder tvpe name names =
            [ tvpe
            , camilize names
            , accessor name
            , quote name
            ]
                |> String.join " "
    in
    case part of
        Icu.Text text ->
            [ "s "
            , quote text
            ]
                |> String.concat

        Icu.Argument name names subMessages ->
            case toArgType (name :: names) of
                Just ArgString ->
                    [ "string"
                    , accessor name
                    , quote name
                    ]
                        |> String.join " "

                Just ArgNode ->
                    case subMessages of
                        (Icu.Unnamed subMessage) :: [] ->
                            [ [ "node"
                              , accessor name
                              , quote name
                              , "<|"
                              ]
                                |> String.join " "
                            , subMessage
                                |> messageToElm toArgType
                                |> indent
                            ]
                                |> String.join "\n"

                        _ ->
                            ""

                Just (ArgDelimited otherNames) ->
                    case subMessages of
                        (Icu.Unnamed subMessage) :: [] ->
                            [ [ "delimited"
                              , camilize otherNames
                              , "<|"
                              ]
                                |> String.join " "
                            , subMessage
                                |> messageToElm toArgType
                                |> indent
                            ]
                                |> String.join "\n"

                        _ ->
                            ""

                Just (ArgList otherNames) ->
                    let
                        subMessageToElm subMessage =
                            case subMessage of
                                Icu.Unnamed subMessage ->
                                    messageToElm toArgType subMessage

                                _ ->
                                    ""
                    in
                    [ [ "list"
                      , camilize otherNames
                      ]
                        |> String.join " "
                    , subMessages
                        |> List.map subMessageToElm
                        |> generateList
                        |> indent
                    ]
                        |> String.join "\n"

                Just (ArgFloat otherNames) ->
                    simplePlaceholder "float" name otherNames

                Just (ArgDate otherNames) ->
                    simplePlaceholder "date" name otherNames

                Just (ArgTime otherNames) ->
                    simplePlaceholder "time" name otherNames

                Just (ArgCardinal otherNames) ->
                    let
                        keyValuePairs subMessage =
                            case subMessage of
                                Icu.Unnamed _ ->
                                    Nothing

                                Icu.Named name subMessage ->
                                    Just ( name, messageToElm toArgType subMessage )

                        actualNames =
                            case otherNames of
                                [] ->
                                    [ "decimalStandard" ]

                                _ ->
                                    otherNames
                    in
                    [ [ simplePlaceholder "cardinal" name actualNames
                      , "<|"
                      ]
                        |> String.join " "
                    , subMessages
                        |> List.filterMap keyValuePairs
                        |> generateRecord
                        |> indent
                    ]
                        |> String.join "\n"

                Just (ArgOrdinal otherNames) ->
                    let
                        keyValuePairs subMessage =
                            case subMessage of
                                Icu.Unnamed _ ->
                                    Nothing

                                Icu.Named name subMessage ->
                                    Just ( name, messageToElm toArgType subMessage )

                        actualNames =
                            case otherNames of
                                [] ->
                                    [ "decimalStandard" ]

                                _ ->
                                    otherNames
                    in
                    [ [ simplePlaceholder "ordinal" name otherNames
                      , "<|"
                      ]
                        |> String.join " "
                    , subMessages
                        |> List.filterMap keyValuePairs
                        |> generateRecord
                        |> indent
                    ]
                        |> String.join "\n"

                Nothing ->
                    "s \"\""

        Icu.Hash ->
            "count"



-- HELPER


indent : String -> String
indent text =
    text
        |> String.split "\n"
        |> List.map (\line -> "    " ++ line)
        |> String.join "\n"


quote : String -> String
quote text =
    "\"" ++ text ++ "\""


accessor : String -> String
accessor name =
    "." ++ name


generateList : List String -> String
generateList elements =
    case elements of
        [] ->
            "[]"

        onlyElement :: [] ->
            [ "["
            , onlyElement
            , "]"
            ]
                |> String.join " "

        firstElement :: rest ->
            [ (("[ " ++ firstElement) :: rest)
                |> String.join "\n, "
            , "\n]"
            ]
                |> String.concat


generateRecord : List ( String, String ) -> String
generateRecord elements =
    let
        generateEqual ( key, value ) =
            [ key
            , " =\n"
            , indent value
            ]
                |> String.concat
    in
    case elements of
        [] ->
            "{}"

        firstElement :: rest ->
            [ (("{ " ++ generateEqual firstElement) :: List.map generateEqual rest)
                |> String.join "\n, "
            , "\n}"
            ]
                |> String.concat
