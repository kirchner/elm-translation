module Text
    exposing
        ( AllPluralForms
        , Args
        , Dynamic
        , FloatInfo
        , FloatPrinter
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
        , Static
        , Text
        , asNodes
        , asString
        , asStringWith
        , concat
        , count
        , date
        , delimited
        , float
        , floatPrinter
        , floatPrinterNames
        , list
        , name
        , nameArgs
        , nameDate
        , nameFloat
        , nameList
        , nameNode
        , nameString
        , nameTime
        , node
        , plural
        , printer
        , printerNames
        , s
        , scope
        , staticList
        , staticText
        , string
        , time
        , variants
        )

{-| Add typesafe translations to your Elm application. Take a look at the
[README](package.elm-lang.org/packages/kirchner/elm-translation/1.0.0/) for
a variety of examples.

@docs Text, Dynamic, Static

@docs Name


# Printing `Text`s

@docs asString, asStringWith, asNodes


# Creating `Text`'s


## Primitives

@docs s, concat, node


## Wrappers

@docs delimited, staticList


## Placeholders

@docs string, list, float, date, time

@docs plural, count, PluralForm, AllPluralForms


## Variants

@docs variants


# Printers

@docs Printer, printer, printerNames, FloatPrinter, floatPrinter, floatPrinterNames, FloatInfo


# Dynamic `Text`'s

Sometimes you need to be able to dynamically change the contents of your
`Text`'s. To do this, you have to turn a `Text Static` into `Text Dynamic`, in
the following way. Suppose you have the following text:

    question :
        Text Static
            { args
                | name : String
                , email : String
            }
            node
    question =
        concat
            [ s "Hello, "
            , string .name
            , s "! Good to have you back. One question: is "
            , string .email
            , s " still your email address?"
            ]

To make it dynamic, you need to give it and each of its arguments a `String`
name, using `name`, `nameString`, `nameFloat`, etc.:

    questionDynamic :
        Text Dynamic
            { args
                | name : String
                , email : String
            }
            node
    questionDynamic =
        question
            |> name "question"
            |> nameString .name "name"
            |> nameString .email "email"

You then can use `staticText` to build a custom `translateTo` function:

    import Text exposing (Args, staticText)

    translateTo :
        Dict String String
        -> Text Dynamic args node
        -> Text Dynamic args node
    translateTo translations text =
        let
            ( args, _, name, staticText ) =
                staticText text
        in
        translations
            |> Dict.get name
            |> Maybe.andThen yourTranslationParser
            |> Maybe.map (yourTextGenerator args)
            |> Maybe.withDefault text

    yourTranslationParser :
        String
        -> Maybe YourTranslationType

    yourTextGenerator :
        Args args node
        -> YourTranslationType
        -> Text Dynamic args node

You then can use your texts like this:

    print translations name email =
        questionDynamic
            |> translateTo translations
            |> asStringWith
                { name = name
                , email = email
                }

@docs name, scope, nameString, nameNode, nameList, nameFloat, nameDate, nameTime

@docs staticText, Args, nameArgs

-}

import Date exposing (Date)
import Dict exposing (Dict)
import Time exposing (Time)


{-| -}
type alias Name =
    String


{-| A `Text` is a piece of localized text in a specific language.
You can turn it into a `String` using [`asString`](#asString). It can also
contain placeholders, for example `Text Static { args | name : String } node`.
You then have to use [`asStringWith`](#asStringWith) and provide values for
every placeholder.

It is also possible to turn a `Translation` into a list of Dom nodes using
[`asNodes`](#asNodes). You can find examples in the package documentation. It's
really convenient!

`Text`s can either be just `String`s or placeholders for `String`s, `Float`s,
`Date`s, ..., along with rules for how to print these. Also pluralization is
possible. There are several functions for creating and manipulating `Text`s,
further down.

-}
type Text mutability args node
    = Named (Args args node) (List Name) Name (Text Static args node)
    | TVerbatim String
    | TTexts (List (Text Static args node))
      -- with printers
    | TDelimited (Printer (Text Static args node) args node) (Text Static args node)
    | TStaticList (Printer (List (Text Static args node)) args node) (List (Text Static args node))
      -- with placeholders
    | TString (args -> String)
    | TNode (args -> List node -> node) (Text Static args node)
      -- with printers and placeholders
    | TList (args -> List String) (Printer (List String) args node)
    | TFloat (args -> Float) (FloatPrinter args node)
    | TDate (args -> Date) (Printer Date args node)
    | TTime (args -> Time) (Printer Time args node)
    | TPlural (args -> Float) (FloatPrinter args node) (Float -> FloatInfo -> PluralForm) (List ( Float, Text Static args node )) (AllPluralForms args node)
      -- misc
    | TCount
      -- variants
    | TVariant (args -> String) (Text Static args node) (List ( String, Text Static args node ))


{-| -}
type Static
    = Static


{-| -}
type Dynamic
    = Dynamic


{-| In order to make a `Text` dynamic you have to give it a name.
-}
name : Name -> Text Static args node -> Text Dynamic args node
name =
    Named emptyArgs []


{-| -}
type alias Args args node =
    { stringArgs : Dict String (args -> String)
    , nodeArgs : Dict String (args -> List node -> node)
    , listArgs : Dict String (args -> List String)
    , floatArgs : Dict String (args -> Float)
    , dateArgs : Dict String (args -> Date)
    , timeArgs : Dict String (args -> Time)
    }


emptyArgs : Args args node
emptyArgs =
    { stringArgs = Dict.empty
    , nodeArgs = Dict.empty
    , listArgs = Dict.empty
    , floatArgs = Dict.empty
    , dateArgs = Dict.empty
    , timeArgs = Dict.empty
    }


{-| -}
staticText : Text Dynamic args node -> ( Args args node, List Name, Name, Text Static args node )
staticText text =
    case text of
        Named args scope name staticText ->
            ( args, scope, name, staticText )

        _ ->
            -- this part should never be called
            ( emptyArgs, [], "", s "" )


{-| You can additionally give a `Text` some scope. (This should correspond to
the module the text is defined in.)
-}
scope : List Name -> Text Dynamic args node -> Text Dynamic args node
scope newScope text =
    case text of
        Named args _ name actualText ->
            Named args newScope name actualText

        _ ->
            text


{-| -}
nameArgs : Args args node -> Text Dynamic args node -> Text Dynamic args node
nameArgs newArgs text =
    case text of
        Named _ scope name actualText ->
            Named newArgs scope name actualText

        _ ->
            text


{-| -}
nameString : (args -> String) -> Name -> Text Dynamic args node -> Text Dynamic args node
nameString accessor name text =
    case text of
        Named args names name actualText ->
            Named { args | stringArgs = Dict.insert name accessor args.stringArgs }
                names
                name
                actualText

        _ ->
            text


{-| -}
nameNode : (args -> List node -> node) -> Name -> Text Dynamic args node -> Text Dynamic args node
nameNode accessor name text =
    case text of
        Named args names name actualText ->
            Named { args | nodeArgs = Dict.insert name accessor args.nodeArgs }
                names
                name
                actualText

        _ ->
            text


{-| -}
nameList : (args -> List String) -> Name -> Text Dynamic args node -> Text Dynamic args node
nameList accessor name text =
    case text of
        Named args names name actualText ->
            Named { args | listArgs = Dict.insert name accessor args.listArgs }
                names
                name
                actualText

        _ ->
            text


{-| -}
nameFloat : (args -> Float) -> Name -> Text Dynamic args node -> Text Dynamic args node
nameFloat accessor name text =
    case text of
        Named args names name actualText ->
            Named { args | floatArgs = Dict.insert name accessor args.floatArgs }
                names
                name
                actualText

        _ ->
            text


{-| -}
nameDate : (args -> Date) -> Name -> Text Dynamic args node -> Text Dynamic args node
nameDate accessor name text =
    case text of
        Named args names name actualText ->
            Named { args | dateArgs = Dict.insert name accessor args.dateArgs }
                names
                name
                actualText

        _ ->
            text


{-| -}
nameTime : (args -> Time) -> Name -> Text Dynamic args node -> Text Dynamic args node
nameTime accessor name text =
    case text of
        Named args names name actualText ->
            Named { args | timeArgs = Dict.insert name accessor args.timeArgs }
                names
                name
                actualText

        _ ->
            text


{-| Some `Text`s need to know how to convert placeholders into `Text`.
We do this by providing a `Printer`.
-}
type Printer a args node
    = Printer (List Name) (a -> Text Static args node)


{-| -}
type FloatPrinter args node
    = FloatPrinter (List Name) (Float -> Text Static args node) (Float -> FloatInfo)


{-| -}
type alias FloatInfo =
    { absoluteValue : Float
    , integerDigits : List Int
    , fractionDigits : List Int
    }


{-| Create a `FloatPrinter`. The first argument should be a unique
identifier, the second argument is the actual "printer":

    intPrinter : FloatPrinter args node
    intPrinter =
        floatPrinter [ "int" ]
            (floor >> toString >> s)
            (\float ->
                let
                    int =
                        floor float
                in
                { absoluteValue = abs int
                , integerDigits = integerDigits int
                , fractionDigits = []
                }
            )

    integerDigits : Int -> List Int
    integerDigits int =
        integerDigitsHelp [] int
            |> List.reverse

    integerDigitsHelp : List Int -> Int -> List Int
    integerDigitsHelp digits int =
        integerDigitsHelp
            ((int % 10) :: digits)
            (int // 10)

The third argument is used when you have a pluralized text, so we can decide
which plural form should be chosen.

-}
floatPrinter :
    List Name
    -> (Float -> Text Static args node)
    -> (Float -> FloatInfo)
    -> FloatPrinter args node
floatPrinter =
    FloatPrinter


{-| If you need to get the Names of a `FloatPrinter`.
-}
floatPrinterNames : FloatPrinter args node -> List Name
floatPrinterNames (FloatPrinter names _ _) =
    names


{-| Create a `Printer`. The first argument should be a unique
identifier, the second argument is the actual "printer". For example,
you can lift `toString` to a printer:

    stringify : Printer a args node
    stringify =
        printer [ "stringify" ] <|
            \a ->
                s (toString a)

-}
printer : List Name -> (a -> Text Static args node) -> Printer a args node
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
    { other : Text Static args node
    , zero : Maybe (Text Static args node)
    , one : Maybe (Text Static args node)
    , two : Maybe (Text Static args node)
    , few : Maybe (Text Static args node)
    , many : Maybe (Text Static args node)
    }



---- TEXT CONSTRUCTOR


{-| Create a `Text` which gets replaced by the provided `String`:

    greeting : Text Static args node
    greeting =
        s "Good morning!"

-}
s : String -> Text Static args node
s =
    TVerbatim


{-| Join a list of `Text`s:

    longerGreeting : Text Static args node
    longerGreeting =
        translation [ "longerGreeting" ] <|
            concat
                [ s "Good evening!\n"
                , s "We are happy to have you here!"
                ]

-}
concat : List (Text Static args node) -> Text Static args node
concat texts =
    case texts of
        text :: [] ->
            text

        _ ->
            TTexts texts


{-| Create a placeholder for a `String`:

    personalGreeting : Text Static { args | name : String } node
    personalGreeting =
        translation [ "greeting" ] <|
            concat
                [ "Hello, "
                , string .name
                , "!"
                ]

-}
string : (args -> String) -> Text Static args node
string =
    TString


{-| When using `asNodes Html.text` on a `Text`, this `Text` will become
a node with an `Html.text` subnode containing the provided `Text`:

    question : Text Static { args | strong : List node -> node } node
    question =
        concat
            [ s "What is "
            , node .strong <|
                s "your"
            , s " favourite programming language?"
            ]

-}
node : (args -> List node -> node) -> Text Static args node -> Text Static args node
node =
    TNode


{-| With this function you can create quotation helpers:

    quote : Text Static args node -> Text Static args node
    quote =
        delimited quotePrinter

    quotePrinter : Printer (Text Static args node) args node
    quotePrinter =
        printer [ "quotes", "outer" ] <|
            \text ->
                concat
                    [ s "‟"
                    , text
                    , s "”"
                    ]

    assumption : Text Static args node
    assumption =
        concat
            [ s "You may ask yourself: "
            , quote <|
                s "What is this all for?"
            ]

Eventually, their will be helpers for all languages contained in the
[CLDR](http://cldr.unicode.org) at
[`kirchner/elm-cldr`](https://github.com/kirchner/elm-cldr).

-}
delimited :
    Printer (Text Static args node) args node
    -> Text Static args node
    -> Text Static args node
delimited =
    TDelimited


{-| Use this to build helpers if you want to turn a list like `[
"Alice", "Bob", "Cindy" ]` into `"Alice, Bob and Cindy"`. So basically,
this is like `concat` but you can specify how to actually join the
`Text`s:

    and : List (Text Static args node) -> Text Static args node
    and =
        staticList andPrinter

    andPrinter : Printer (List (Text Static args node)) args node
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

    membersInfo : Text Static args node
    membersInfo =
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
staticList :
    Printer (List (Text Static args node)) args node
    -> List (Text Static args node)
    -> Text Static args node
staticList =
    TStaticList


{-| Create a placeholder for a list of `String`s. You have to
provide a `Printer (List String) args node`:

    fruitList : Text Static { args | fruits : List String } node
    fruitList =
        concat
            [ s "Do you really want to by "
            , list .fruits andPrinter
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
    (args -> List String)
    -> Printer (List String) args node
    -> Text Static args node
list =
    TList


{-| Create a placeholder for a `Float`. You also need to provide
a `FloatPrinter`, so we know how to turn the actual value into a `Text`:

    mailboxInfo : Text Static { args | mailCount : Float } node
    mailboxInfo =
        concat
            [ "Number of new mails: "
            , float .mailCount intPrinter
            ]

    intPrinter : FloatPrinter
    intPrinter =
        floatPrinter [ "int" ]
            (floor >> toString >> s)
            (\float ->
                let
                    int =
                        floor float
                in
                { absoluteValue = abs int
                , integerDigits = integerDigits int
                , fractionDigits = []
                }
            )

    integerDigits : Int -> List Int
    integerDigits int =
        integerDigitsHelp [] int
            |> List.reverse

    integerDigitsHelp : List Int -> Int -> List Int
    integerDigitsHelp digits int =
        if int < 10 then
            (int % 10) :: digits
        else
            integerDigitsHelp ((int % 10) :: digits) (int // 10)

**Note:** The package
[`kirchner/elm-cldr`](https://github.com/kirchner/elm-cldr) exposes several
of these placeholder functions for all the number formats and locales which
are defined in the [CLDR](http://cldr.unicode.org). You most likely want to
use one of those.

-}
float : (args -> Float) -> FloatPrinter args node -> Text Static args node
float =
    TFloat


{-| Like [`float`](#float) but for `Date` values.
-}
date : (args -> Date) -> Printer Date args node -> Text Static args node
date =
    TDate


{-| Like [`float`](#float) but for `Time` values.
-}
time : (args -> Time) -> Printer Time args node -> Text Static args node
time =
    TTime


{-| This function helps you if you need to pluralize your text depending on
some numerical value. For example `"1 second"` vs. `"2 seconds"` vs. `"1.0
seconds"`. You have to provide a `FloatPrinter` for the number and a function
which decides what plural form should be choosen depending on the numerical
value and its `FloatInfo`. You can use `count` within a plural text to insert
the actual printed numerical value.

    newMailsInfo : Text Static { args | count : Float } node
    newMailsInfo =
        plural .count
            intPrinter
            toPluralForm
            []
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

    toPluralForm : Float -> FloatInfo -> PluralForm
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

    newMailsInfo : Text Static { args | count : Float } node
    newMailsInfo =
        cardinal .count
            intPrinter
            []
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

If you want to provide special versions for particular numerical values, you
can do this like this:

    import Cldr.En exposing (cardinal, decimalLatnStandard)

    mailInfo : Text Static { args | count : Float } node
    mailInfo =
        concat
            [ s "You have "
            , cardinal .count
                decimalLatnStandard
                [ ( 0, s "no new emails" )
                , ( 1, s "one new email" )
                , ( 12, s "a dozen new emails" )
                ]
                { other =
                    concat
                        [ count
                        , s " new emails"
                        ]
                , one =
                    concat
                        [ count
                        , s " new email"
                        ]
                }
            , s "."
            ]

The first matching value in the list will be taken.

-}
plural :
    (args -> Float)
    -> FloatPrinter args node
    -> (Float -> FloatInfo -> PluralForm)
    -> List ( Float, Text Static args node )
    -> AllPluralForms args node
    -> Text Static args node
plural =
    TPlural


{-| Used within a form of a plural text, this will insert the numerical
value using the printer which was provided to [`plural`](#plural).
-}
count : Text Static args node
count =
    TCount


{-| A placeholder whose content can be chosen from a finite list of variants.
You have to provide a default text which will be displayed when no other
variant matches.

    invitationInfo : Text Static { args | gender : String } node
    invitationInfo =
        variants .gender
            (s "They have invited you.")
            [ ( "male", s "He has invited you." )
            , ( "female", s "She has invited you." )
            ]

-}
variants :
    (args -> String)
    -> Text Static args node
    -> List ( String, Text Static args node )
    -> Text Static args node
variants =
    TVariant



---- PRINT


{-| Turn a `Text` into a `String` by providing values for all
placeholders.
-}
asStringWith : args -> Text mutability args String -> String
asStringWith args text =
    asNodes identity args text
        |> String.concat


{-| Turn a `Text` which does not contain any placeholders into
a `String`.
-}
asString : Text mutability {} String -> String
asString text =
    asStringWith {} text


{-| Turn a `Text` into a list of nodes by providing a way to turn
a `String` into your particular node type. These can be `Html.text`, `Svg.text`
or `Element.text`, for example. Take a look at the
[README](package.elm-lang.org/packages/kirchner/elm-translation/1.0.0/) for
examples of why this is useful.

You probably want to define a helper function for the dom node type you are
using:

    asHtml : args -> Text Static args (Html msg) -> List (Html msg)
    asHtml =
        Translation.asNodes Html.text

-}
asNodes : (String -> node) -> args -> Text mutability args node -> List node
asNodes stringToNode args text =
    asNodesHelp Nothing stringToNode args text



-- INTERNAL PRINT HELPER


asNodesHelp :
    Maybe (List node)
    -> (String -> node)
    -> args
    -> Text mutability args node
    -> List node
asNodesHelp maybeCount stringToNode args text =
    case text of
        Named _ _ _ actualText ->
            asNodesHelp maybeCount stringToNode args actualText

        TVerbatim string ->
            [ stringToNode string ]

        TTexts subTexts ->
            subTexts
                |> List.map (asNodesHelp maybeCount stringToNode args)
                |> List.concat

        TDelimited (Printer _ printer) subText ->
            subText
                |> printer
                |> asNodesHelp maybeCount stringToNode args

        TStaticList (Printer _ printer) subTexts ->
            subTexts
                |> printer
                |> asNodesHelp maybeCount stringToNode args

        TString accessor ->
            [ args
                |> accessor
                |> stringToNode
            ]

        TNode accessor subText ->
            [ subText
                |> asNodesHelp maybeCount stringToNode args
                |> accessor args
            ]

        TList accessor (Printer _ printer) ->
            args
                |> accessor
                |> printer
                |> asNodesHelp maybeCount stringToNode args

        TFloat accessor (FloatPrinter _ printer _) ->
            args
                |> accessor
                |> printer
                |> asNodesHelp maybeCount stringToNode args

        TDate accessor (Printer _ printer) ->
            args
                |> accessor
                |> printer
                |> asNodesHelp maybeCount stringToNode args

        TTime accessor (Printer _ printer) ->
            args
                |> accessor
                |> printer
                |> asNodesHelp maybeCount stringToNode args

        TPlural accessor (FloatPrinter _ printer info) toPluralForm otherTexts allPluralForms ->
            let
                float =
                    accessor args

                count =
                    float
                        |> printer
                        |> asNodesHelp Nothing stringToNode args

                pluralForm =
                    toPluralForm float (info float)

                getPluralForm form =
                    form
                        |> Maybe.withDefault allPluralForms.other
            in
            otherTexts
                |> List.filter (\( otherFloat, _ ) -> otherFloat == float)
                |> List.head
                |> Maybe.map Tuple.second
                |> Maybe.withDefault
                    (case pluralForm of
                        Other ->
                            allPluralForms.other

                        Zero ->
                            getPluralForm allPluralForms.zero

                        One ->
                            getPluralForm allPluralForms.one

                        Two ->
                            getPluralForm allPluralForms.two

                        Few ->
                            getPluralForm allPluralForms.few

                        Many ->
                            getPluralForm allPluralForms.many
                    )
                |> asNodesHelp (Just count) stringToNode args

        TCount ->
            case maybeCount of
                Just count ->
                    count

                Nothing ->
                    []

        TVariant accessor defaultText texts ->
            texts
                |> List.filter (\( variant, _ ) -> variant == accessor args)
                |> List.head
                |> Maybe.map Tuple.second
                |> Maybe.withDefault defaultText
                |> asNodesHelp maybeCount stringToNode args
