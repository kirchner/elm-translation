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
        , dateArgs
        , delimited
        , float
        , floatArgs
        , list
        , listArgs
        , name
        , node
        , translation
        , nodeArgs
        , plural
        , printer
        , printerNames
        , s
        , staticList
        , string
        , stringArgs
        , time
        , timeArgs
        )

{-| Add typesafe translations to your Elm application. Take a look at the
[README](package.elm-lang.org/packages/kirchner/elm-translation/1.0.0/) for
a variety of examples.

@docs Translation, Text

@docs translation

@docs Name


# Printing `Translation`s

@docs asString, asStringWith, asNodes


# Creating `Text`'s


## Primitives

@docs s, concat, node


## Wrappers

@docs delimited, staticList


## Placeholders

@docs string, list, float, date, time

@docs plural, count, PluralForm, AllPluralForms


# Printers

@docs Printer, printer, printerNames


# Getting Information about a `Translation`

@docs name, nodeArgs, stringArgs, listArgs, floatArgs, dateArgs, timeArgs

-}

import Date exposing (Date)
import Dict exposing (Dict)
import Time exposing (Time)


{-| -}
type alias Name =
    String


{-| A `Translation` is a piece of localized text in a specific language.
You can turn it into a `String` using [`asString`](#asString). It can also
contain placeholders, for example `Translation { args | name : String } node`.
You then have to use [`asStringWith`](#asStringWith) and provide values for
every placeholder.

It is also possible to turn a `Translation` into a list of Dom nodes using
[`asNodes`](#asNodes). Take a look the package documentation for an
example. It's really convenient!

-}
type Translation args node
    = Final (List String) (Text args node)
    | Fallback (List String) (Text args node)


{-| Returns the name of a `Translation`.
-}
name : Translation args node -> List Name
name translation =
    case translation of
        Final name _ ->
            name

        Fallback name _ ->
            name


text : Translation args node -> Text args node
text translation =
    case translation of
        Final _ text ->
            text

        Fallback _ text ->
            text


{-| This is the building block for your translations. `Text`s can
either be just `String`s or placeholders for `String`s, `Float`s,
`Date`s, ..., along with rules for how to print these. Also
pluralization is possible. There are several functions for creating and
manipulating `Text`s, further down.
-}
type Text args node
    = TText String
    | TTexts (List (Text args node))
      -- with printers
    | TDelimited (Printer (Text args node) args node) (Text args node)
    | TStaticList (Printer (List (Text args node)) args node) (List (Text args node))
      -- with placeholders
    | TString (Placeholder String args)
    | TNode (Placeholder (List node -> node) args) (Text args node)
      -- with printers and placeholders
    | TList (Printer (List String) args node) (Placeholder (List String) args)
    | TFloat (Printer Float args node) (Placeholder Float args)
    | TDate (Printer Date args node) (Placeholder Date args)
    | TTime (Printer Time args node) (Placeholder Time args)
    | TPlural (Printer Float args node) (Float -> String -> PluralForm) (Placeholder Float args) (AllPluralForms args node)
      -- misc
    | TCount


{-| -}
type Placeholder a args
    = Placeholder (args -> a) Name


{-| Some `Text`s need to know how to convert placeholders into `Text`.
We do this by providing a `Printer`.
-}
type Printer a args node
    = Printer (List Name) (a -> Text args node)


{-| Create a `Printer`. The first argument should be a unique
identifier, the second argument is the actual "printer". For example,
you can lift `toString` to a printer:

    stringify : Printer a args node
    stringify =
        printer [ "stringify" ] <|
            \a ->
                s (toString a)

-}
printer : List Name -> (a -> Text args node) -> Printer a args node
printer =
    Printer


{-| If you need to get the Names of a `Printer`.
-}
printerNames : Printer a args node -> List Name
printerNames (Printer names _) =
    names


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
type alias AllPluralForms args node =
    { other : Text args node
    , zero : Maybe (Text args node)
    , one : Maybe (Text args node)
    , two : Maybe (Text args node)
    , few : Maybe (Text args node)
    , many : Maybe (Text args node)
    }



---- TRANSLATION CONSTRUCTOR


{-| Create a `Translation` out of a `Text` by giving it a name.
Usually the name should be a string representation of the function name used
for the translation.
-}
translation : List Name -> Text args node -> Translation args node
translation =
    Final



---- TEXT CONSTRUCTOR


{-| Create a `Text` which gets replaced by the provided `String`:

    greeting : Translation args node
    greeting =
        translation "greeting" <|
            s "Good morning!"

-}
s : String -> Text args node
s =
    TText


{-| Join a list of `Text`s:

    longerGreeting : Translation args node
    longerGreeting =
        translation "longerGreeting" <|
            concat
                [ s "Good evening!\n"
                , s "We are happy to have you here!"
                ]

-}
concat : List (Text args node) -> Text args node
concat texts =
    case texts of
        text :: [] ->
            text

        _ ->
            TTexts texts


{-| Create a placeholder for a `String`:

    personalGreeting : Translation { args | name : String } node
    personalGreeting =
        translation "greeting" <|
            concat
                [ "Hello, "
                , string .name "name"
                , "!"
                ]

-}
string : (args -> String) -> Name -> Text args node
string accessor name =
    TString (Placeholder accessor name)


{-| When using `asNodes` on a `Translation`, this `Text` will become
a node with a `Node.text` subnode containing the provided `Text`:

    question : Translation { args | strong : List node -> node) } node
    question =
        translation "question" <|
            concat
                [ s "What is "
                , node .strong "strong" <|
                    s "your"
                , s " favourite programming language?"
                ]

-}
node : (args -> (List node -> node)) -> Name -> Text args node -> Text args node
node accessor name =
    TNode (Placeholder accessor name)


{-| With this function you can create quotation helpers:

    quote : Text args node -> Text args node
    quote =
        delimited quotePrinter

    quotePrinter : Printer (Text args node) args node
    quotePrinter =
        printer [ "quotes", "outer" ] <|
            \text ->
                concat
                    [ s "‟"
                    , text
                    , s "”"
                    ]

    assumption : Translation args node
    assumption =
        translation "assumption" <|
            concat
                [ s "You may ask yourself: "
                , quote <|
                    s "What is this all for?"
                ]

Eventually, their will be helpers for all languages contained in the
[CLDR](http://cldr.unicode.org) at
[`kirchner/elm-cldr`](https://github.com/kirchner/elm-cldr).

-}
delimited : Printer (Text args node) args node -> Text args node -> Text args node
delimited =
    TDelimited


{-| Use this to build helpers if you want to turn a list like `[
"Alice", "Bob", "Cindy" ]` into `"Alice, Bob and Cindy"`. So basically,
this is like `concat` but you can specify how to actually join the
`Text`s:

    and : List (Text args node) -> Text args node
    and =
        staticList andPrinter

    andPrinter : Printer (List (Text args node)) args node
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

    membersInfo : Translation args node
    membersInfo =
        translation "membersInfo" <|
            concat
                [ s "These are all our members: "
                , and
                    [ s "Alice"
                    , s "Bob"
                    , s "Cindy"
                    ]
                ]

If you want to join a dynamic list of strings, take a look at [`list`](#list).

-}
staticList : Printer (List (Text args node)) args node -> List (Text args node) -> Text args node
staticList =
    TStaticList


{-| Create a placeholder for a list of `String`s. You have to
provide a `Printer (List String) args node`:

    fruitList : Translation { args | fruits : List String } node
    fruitList =
        translation "fruitList" <|
            concat
                [ s "Do you really want to by "
                , list andPrinter .fruits "fruits"
                , s "?"
                ]

    andPrinter : Printer (List String) args node
    andPrinter =
        printer [ "list", "and" ] <|
            \strings ->
                case List.reverse strings of
                    [] ->
                        s ""

                    lastString :: [] ->
                        s lastString

                    lastString :: rest ->
                        [ [ s lastString
                          , s " and "
                          ]
                        , rest
                            |> List.intersperse (s ", ")
                        ]
                            |> List.concat
                            |> List.reverse
                            |> concat

-}
list :
    Printer (List String) args node
    -> (args -> List String)
    -> Name
    -> Text args node
list printer accessor name =
    TList printer (Placeholder accessor name)


{-| Create a placeholder for a `Float`. You also need to provide
a `Printer Float`, so we know how to turn the actual value into
a `Text`:

    mailboxInfo : Translation { args | mailCount : Float } node
    mailboxInfo =
        translation "mailboxInfo" <|
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

**Note:** The package
[`kirchner/elm-cldr`](https://github.com/kirchner/elm-cldr) exposes several
of these placeholder functions for all the number formats and locales which
are defined in the [CLDR](http://cldr.unicode.org). You most likely want to
use one of those.

-}
float : Printer Float args node -> (args -> Float) -> Name -> Text args node
float printer accessor name =
    TFloat printer (Placeholder accessor name)


{-| Like [`float`](#float) but for `Date` values.
-}
date : Printer Date args node -> (args -> Date) -> Name -> Text args node
date printer accessor name =
    TDate printer (Placeholder accessor name)


{-| Like [`float`](#float) but for `Time` values.
-}
time : Printer Time args node -> (args -> Time) -> Name -> Text args node
time printer accessor name =
    TTime printer (Placeholder accessor name)


{-| This function helps you if you need to choose different variants of
your translation depending on some numeric value. For example `"1
second"` vs. `"2 seconds"` vs. `"1.0 seconds"`. You have to provide
a printer for the number and a function which decides what plural form
should be choosen depending on the numeric value and its printed
representation. You can use `count` within a plural text to insert the
actual printed numerical value.

    newMailsInfo : Translation { args | count : Float } node
    newMailsInfo =
        translation "newMailsInfo" <|
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

Take a look at [`kirchner/elm-cldr`](https://github.com/kirchner/elm-cldr). You
will find pluralization functions which are based on the pluralization rules of
the [CLDR](http://cldr.unicode.org). For example, a German version of the
above translation would look like this:

    import Cldr.De exposing (cardinal)

    newMailsInfo : Translation { args | count : Float } node
    newMailsInfo =
        translation "newMailsInfo" <|
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

So you never have to worry that you might have missed some pluralization form
(or defined forms which are not necessary)!

-}
plural :
    Printer Float args node
    -> (Float -> String -> PluralForm)
    -> (args -> Float)
    -> Name
    -> AllPluralForms args node
    -> Text args node
plural printer toPluralForm accessor name =
    TPlural printer toPluralForm (Placeholder accessor name)


{-| Used within a form of a plural text, this will insert the numerical
value using the printer which was provided to [`plural`](#plural).
-}
count : Text args node
count =
    TCount



---- PRINT


{-| Turn a `Translation` into a `String` by providing values for all
placeholders.
-}
asStringWith : args -> Translation args node -> String
asStringWith args translation =
    case translation of
        Final _ text ->
            printText Nothing args text

        Fallback _ text ->
            printText Nothing args text


{-| Turn a `Translation` which does not contain any placeholders into
a `String`.
-}
asString : Translation {} node -> String
asString translation =
    asStringWith {} translation


{-| Turn a `Translation` into a list of nodes by providing a way to turn
a `String` into your particular node type. These can be `Html.text`, `Svg.text`
or `Element.text`, for example. Take a look at the
[README](package.elm-lang.org/packages/kirchner/elm-translation/1.0.0/) for
examples of why this is useful.

You probably want to define a helper function for the dom node type you are
using:

    asHtml : args -> Translation args (Html msg) -> List (Html msg)
    asHtml =
        Translation.asNodes Html.text

-}
asNodes : (String -> node) -> args -> Translation args node -> List node
asNodes asTextNode args translation =
    case translation of
        Final _ text ->
            textToNodes asTextNode Nothing args text

        Fallback _ text ->
            textToNodes asTextNode Nothing args text



-- INTERNAL PRINT HELPER


printText : Maybe String -> args -> Text args node -> String
printText maybeCount args text =
    case text of
        TText string ->
            string

        TTexts subTexts ->
            subTexts
                |> List.map (printText maybeCount args)
                |> String.concat

        TDelimited (Printer _ printer) subText ->
            subText
                |> printer
                |> printText maybeCount args

        TStaticList (Printer _ printer) subTexts ->
            subTexts
                |> printer
                |> printText maybeCount args

        TString (Placeholder accessor _) ->
            accessor args

        TNode (Placeholder accessor _) subText ->
            printText maybeCount args subText

        TList (Printer _ printer) (Placeholder accessor _) ->
            args
                |> accessor
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


textToNodes : (String -> node) -> Maybe (Text args node) -> args -> Text args node -> List node
textToNodes asTextNode maybeCount args text =
    case text of
        TText string ->
            [ asTextNode string ]

        TTexts subTexts ->
            subTexts
                |> List.map (textToNodes asTextNode maybeCount args)
                |> List.concat

        TDelimited (Printer _ printer) subText ->
            subText
                |> printer
                |> textToNodes asTextNode maybeCount args

        TStaticList (Printer _ printer) subTexts ->
            subTexts
                |> printer
                |> textToNodes asTextNode maybeCount args

        TString (Placeholder accessor _) ->
            [ args
                |> accessor
                |> asTextNode
            ]

        TNode (Placeholder accessor _) subText ->
            [ subText
                |> textToNodes asTextNode maybeCount args
                |> accessor args
            ]

        TList (Printer _ printer) (Placeholder accessor _) ->
            args
                |> accessor
                |> printer
                |> textToNodes asTextNode maybeCount args

        TFloat (Printer _ printer) (Placeholder accessor _) ->
            args
                |> accessor
                |> printer
                |> textToNodes asTextNode maybeCount args

        TDate (Printer _ printer) (Placeholder accessor _) ->
            args
                |> accessor
                |> printer
                |> textToNodes asTextNode maybeCount args

        TTime (Printer _ printer) (Placeholder accessor _) ->
            args
                |> accessor
                |> printer
                |> textToNodes asTextNode maybeCount args

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
                        |> textToNodes asTextNode (Just countAsText) args
            in
            case pluralForm of
                Other ->
                    allPluralForms.other
                        |> textToNodes asTextNode (Just countAsText) args

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
                        |> textToNodes asTextNode Nothing args

                Nothing ->
                    -- TODO: should we prevent this via types?
                    []



---- RETRIEVE ARGUMENTS


{-| -}
nodeArgs : Translation args node -> Dict Name (args -> (List node -> node))
nodeArgs =
    text >> nodeArgsHelp


nodeArgsHelp : Text args node -> Dict Name (args -> (List node -> node))
nodeArgsHelp text =
    case text of
        TNode (Placeholder accessor name) _ ->
            Dict.singleton name accessor

        _ ->
            descendWith nodeArgsHelp text


{-| -}
stringArgs : Translation args node -> Dict Name (args -> String)
stringArgs =
    text >> stringArgsHelp


stringArgsHelp : Text args node -> Dict Name (args -> String)
stringArgsHelp text =
    case text of
        TString (Placeholder accessor name) ->
            Dict.singleton name accessor

        _ ->
            descendWith stringArgsHelp text


{-| -}
listArgs : Translation args node -> Dict Name (args -> List String)
listArgs =
    text >> listArgsHelp


listArgsHelp : Text args node -> Dict Name (args -> List String)
listArgsHelp text =
    case text of
        TList _ (Placeholder accessor name) ->
            Dict.singleton name accessor

        _ ->
            descendWith listArgsHelp text


{-| -}
floatArgs : Translation args node -> Dict Name (args -> Float)
floatArgs =
    text >> floatArgsHelp


floatArgsHelp : Text args node -> Dict Name (args -> Float)
floatArgsHelp text =
    case text of
        TFloat _ (Placeholder accessor name) ->
            Dict.singleton name accessor

        TPlural _ _ (Placeholder accessor name) _ ->
            Dict.singleton name accessor

        _ ->
            descendWith floatArgsHelp text


{-| -}
dateArgs : Translation args node -> Dict Name (args -> Date)
dateArgs =
    text >> dateArgsHelp


dateArgsHelp : Text args node -> Dict Name (args -> Date)
dateArgsHelp text =
    case text of
        TDate _ (Placeholder accessor name) ->
            Dict.singleton name accessor

        _ ->
            descendWith dateArgsHelp text


{-| -}
timeArgs : Translation args node -> Dict Name (args -> Time)
timeArgs =
    text >> timeArgsHelp


timeArgsHelp : Text args node -> Dict Name (args -> Time)
timeArgsHelp text =
    case text of
        TTime _ (Placeholder accessor name) ->
            Dict.singleton name accessor

        _ ->
            descendWith timeArgsHelp text


descendWith :
    (Text args node -> Dict Name a)
    -> Text args node
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

        TStaticList _ subTexts ->
            -- FIXME: should we extract arguments hidden in printers?
            subTexts
                |> List.map extractor
                |> List.foldl Dict.union Dict.empty

        _ ->
            Dict.empty



------ EXPORT TO ICU MESSAGE FORMAT
--
--
--{-| Turn a `Translation` into the (somewhat extended) [ICU Message
--Format](http://icu-project.org/apiref/icu4j/com/ibm/icu/text/MessageFormat.html).
--You probably never need this. We will eventually use this in
--[`kirchner/elm-translation-runner`](https://github.com/kirchner/elm-translation-runner)
--to export `Translation`s to JSON translation files.
--
--**Note:** This only works if you provide unique `Name`s for the
--different placeholders and translations.
--
---}
--toIcu :
--    (ArgType -> IcuArg)
--    -> Translation args node
--    -> String
--toIcu fromArgType translation =
--    case translation of
--        Final _ text ->
--            textToIcu fromArgType Nothing text
--
--        Fallback _ text ->
--            textToIcu fromArgType Nothing text
--
--
--textToIcu :
--    (ArgType -> IcuArg)
--    -> Maybe (Text args node)
--    -> Text args node
--    -> String
--textToIcu fromArgType maybeCount text =
--    let
--        wrap string =
--            "{" ++ string ++ "}"
--
--        wrapWith argType placeholder =
--            case fromArgType argType of
--                TakePlaceholder names ->
--                    (placeholder :: names)
--                        |> String.join ", "
--
--                OverwritePlaceholder newPlaceholder names ->
--                    (newPlaceholder :: names)
--                        |> String.join ", "
--    in
--    case text of
--        TText string ->
--            string
--
--        TTexts subTexts ->
--            subTexts
--                |> List.map (textToIcu fromArgType maybeCount)
--                |> String.concat
--
--        TDelimited (Printer wrapperNames _) subText ->
--            [ wrapWith (ArgDelimited wrapperNames) "_"
--            , subText
--                |> textToIcu fromArgType maybeCount
--                |> wrap
--            ]
--                |> String.join ", "
--                |> wrap
--
--        TStaticList (Printer wrapperNames _) subTexts ->
--            [ wrapWith (ArgStaticList wrapperNames) "_"
--            , subTexts
--                |> List.map (textToIcu fromArgType maybeCount)
--                |> String.join " "
--                |> wrap
--            ]
--                |> String.join ", "
--                |> wrap
--
--        TString (Placeholder _ name) ->
--            wrapWith ArgString name
--                |> wrap
--
--        TNode (Placeholder _ name) subText ->
--            [ wrapWith ArgNode name
--            , ", "
--            , subText
--                |> textToIcu fromArgType maybeCount
--                |> wrap
--            ]
--                |> String.concat
--                |> wrap
--
--        TList (Printer printerNames _) (Placeholder _ name) ->
--            wrapWith (ArgList printerNames) name
--                |> wrap
--
--        TFloat (Printer printerNames _) (Placeholder _ name) ->
--            wrapWith (ArgFloat printerNames) name
--                |> wrap
--
--        TDate (Printer printerNames _) (Placeholder _ name) ->
--            wrapWith (ArgDate printerNames) name
--                |> wrap
--
--        TTime (Printer printerNames _) (Placeholder _ name) ->
--            wrapWith (ArgTime printerNames) name
--                |> wrap
--
--        TPlural (Printer printerNames _) _ (Placeholder _ name) allPluralForms ->
--            -- TODO: Ordinal?
--            let
--                pluralFormToIcu form text =
--                    form ++ wrap (textToIcu fromArgType Nothing text)
--            in
--            [ wrapWith (ArgCardinal printerNames) name
--            , [ ( "other", Just allPluralForms.other )
--              , ( "zero", allPluralForms.zero )
--              , ( "one", allPluralForms.one )
--              , ( "two", allPluralForms.two )
--              , ( "few", allPluralForms.few )
--              , ( "many", allPluralForms.many )
--              ]
--                |> List.filterMap
--                    (\( form, maybeText ) ->
--                        maybeText
--                            |> Maybe.map (pluralFormToIcu form)
--                    )
--                |> String.join " "
--            ]
--                |> String.join ", "
--                |> wrap
--
--        TCount ->
--            "#"
