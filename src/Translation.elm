module Translation
    exposing
        ( AllPluralForms
        , Arg
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
        , fallback
        , final
        , float
        , node
        , plural
        , printer
        , s
        , string
        , toIcu
        , wrapFloat
        , wrapper
        )

{-|

@docs Translation, Text

@docs final, fallback

@docs Name, Arg


# Creating `Text`'s

@docs s, concat, string, node

@docs float, wrapFloat

@docs plural, PluralForm, AllPluralForms, count


# Printing `Translation`s

@docs asString, asStringWith, asNodes

-}

import Dict exposing (Dict)
import Set
import VirtualDom exposing (Node)


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
    = Texts (List (Text args msg))
    | Verbatim String
    | String (args -> String) Name
    | Node (args -> (List (Node msg) -> Node msg)) Name (Text args msg)
    | Float (Printer Float) (args -> Float) Name
    | WrapFloat (Wrapper Float args msg) (Arg Float args msg) (args -> Float) Name
    | Plural (Float -> String -> PluralForm) (Arg Float args msg) (args -> Float) Name (AllPluralForms args msg)
    | Count


{-| -}
type alias Name =
    String


{-| This is a convenience alias to make the type signatures more
concise. `args` will be a record type specifying which placeholder one
has to provide when printing the translation. This is done by
providing an accessor function `args -> a`. The `Name` should be
a sensefull `String` representation of this accessor.
-}
type alias Arg a args msg =
    (args -> a) -> Name -> Text args msg


{-| When printing placeholders which are not `String`s we need to know
how to turn them into `String`s. We do this by providing a `Printer`.
-}
type Printer a
    = Printer (List Name) (a -> String)


{-| Create a `Printer`. The first argument should be a unique
identifier, the second argument is the actual "printer":

    stringify : Printer a
    stringify =
        printer [ "stringify" ] toString

-}
printer : List Name -> (a -> String) -> Printer a
printer =
    Printer


{-| Sometimes it makes sense to wrap a placeholder in some other `Text`,
for example when defining helpers for printing units like meter,
seconds, ....
-}
type Wrapper a args msg
    = Wrapper (List Name) (Arg a args msg -> Arg a args msg)


{-| Create a `Wrapper`. The first argument should be a unique
identifier, the second argument is a placeholder:

    unitShortLengthMeterWrapper : Wrapper Float
    unitShortLengthMeterWrapper =
        wrapper [ "unit", "short", "length-meter" ] <|
            \wrapped accessor name ->
                concat
                    [ wrapped accessor name
                    , s " m"
                    ]

-}
wrapper : List Name -> (Arg a args msg -> Arg a args msg) -> Wrapper a args msg
wrapper =
    Wrapper


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
    Verbatim


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
node =
    Node


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
string : Arg String args msg
string =
    String


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
float : Printer Float -> Arg Float args msg
float =
    Float


{-| Wrap a `Float` placeholder in some other `Text`. You usually need
these for formatting units:

    distanceInfo : Translation { args | distance : Float } msg
    distanceInfo =
        final "distanceInfo" <|
            concat
                [ "Distance travelled: "
                , wrapFloat unitShortLengthMeterWrapper
                    (float intPrinter)
                    .distance
                    "distance"
                ]

    unitShortLengthMeterWrapper : Wrapper Float
    unitShortLengthMeterWrapper =
        wrapper [ "unit", "short", "length-meter" ] <|
            \wrapped accessor name ->
                concat
                    [ wrapped accessor name
                    , s " m"
                    ]

    intPrinter : Printer Float
    intPrinter =
        printer [ "int" ] <|
            \float ->
                float
                    |> floor
                    |> toString

**Note:** There are also a lot of wrappers in `kirchner/elm-cldr` for
any locale.

-}
wrapFloat : Wrapper Float args msg -> Arg Float args msg -> Arg Float args msg
wrapFloat =
    WrapFloat


{-| -}
plural :
    (Float -> String -> PluralForm)
    -> Arg Float args msg
    -> (args -> Float)
    -> Name
    -> AllPluralForms args msg
    -> Text args msg
plural =
    Plural


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
        Texts texts ->
            texts
                |> List.map (printText maybeCount args)
                |> String.concat

        Verbatim string ->
            string

        Node _ accessor text ->
            printText maybeCount args text

        String accessor _ ->
            accessor args

        Float (Printer _ print) accessor _ ->
            args
                |> accessor
                |> print

        WrapFloat (Wrapper _ wrap) wrapped accessor name ->
            wrap wrapped accessor name
                |> printText maybeCount args

        Plural countToPluralForm wrapped accessor name allPluralForms ->
            let
                countText =
                    wrapped accessor name

                count =
                    countText |> printText Nothing args

                pluralForm =
                    countToPluralForm
                        (args |> accessor)
                        count

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
        Texts texts ->
            texts
                |> List.map (textToNodes maybeCount args)
                |> List.concat

        Verbatim string ->
            [ VirtualDom.text string ]

        Node accessor _ text ->
            [ text
                |> textToNodes maybeCount args
                |> accessor args
            ]

        String accessor _ ->
            [ args
                |> accessor
                |> VirtualDom.text
            ]

        Float (Printer _ print) accessor _ ->
            [ args
                |> accessor
                |> print
                |> VirtualDom.text
            ]

        WrapFloat (Wrapper _ wrap) wrapped accessor name ->
            wrap wrapped accessor name
                |> textToNodes maybeCount args

        Plural countToPluralForm wrapped accessor name allPluralForms ->
            let
                countText =
                    wrapped accessor name

                count =
                    countText |> printText Nothing args

                pluralForm =
                    countToPluralForm
                        (args |> accessor)
                        count

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


type Locale
    = Locale LocaleData


type alias LocaleData =
    { translations : Dict String String
    , printFloat : List Name -> Float -> String
    , printInt : List Name -> Int -> String
    , cardinalForm : Float -> String -> PluralForm
    , ordinalForm : Float -> String -> PluralForm
    , allowedCardinalForms : List PluralForm
    , allowedOrdinalForms : List PluralForm
    }


locale : Locale
locale =
    Locale defaultLocaleData


defaultLocaleData : LocaleData
defaultLocaleData =
    { translations = Dict.empty
    , printFloat = \_ -> toString
    , printInt = \_ -> toString
    , cardinalForm = \_ _ -> Other
    , ordinalForm = \_ _ -> Other
    , allowedCardinalForms = [ Other ]
    , allowedOrdinalForms = [ Other ]
    }


addTranslations : List ( String, String ) -> Locale -> Locale
addTranslations translationList (Locale localeData) =
    { localeData
        | translations =
            Dict.union
                (translationList |> Dict.fromList)
                localeData.translations
    }
        |> Locale


addAllowedCardinalForms : List PluralForm -> Locale -> Locale
addAllowedCardinalForms pluralForms (Locale localeData) =
    { localeData
        | allowedCardinalForms =
            pluralForms
                |> List.append localeData.allowedCardinalForms
                |> makePluralFormsUnique
    }
        |> Locale


addAllowedOrdinalForms : List PluralForm -> Locale -> Locale
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


translateToWith : Locale -> args -> Translation args msg -> String
translateToWith locale args translation =
    case translation of
        Final name text ->
            translateText Nothing locale args name text

        Fallback name text ->
            translateText Nothing locale args name text



-- INTERNAL TRANSLATE HELPER


translateText : Maybe String -> Locale -> args -> Name -> Text args msg -> String
translateText maybeCount locale args name text =
    "TODO: implement"


nodeArgs : args -> Text args msg -> Dict Name (List (Node msg) -> Node msg)
nodeArgs args text =
    case text of
        Texts texts ->
            texts
                |> List.map (nodeArgs args)
                |> List.foldl Dict.union Dict.empty

        Node accessor name _ ->
            Dict.singleton name (args |> accessor)

        _ ->
            Dict.empty


stringArgs : args -> Text args msg -> Dict Name String
stringArgs args text =
    case text of
        Texts texts ->
            texts
                |> List.map (stringArgs args)
                |> List.foldl Dict.union Dict.empty

        String accessor name ->
            Dict.singleton name (args |> accessor)

        _ ->
            Dict.empty


floatArgs : args -> Text args msg -> Dict Name ( Printer Float, Float )
floatArgs args text =
    case text of
        Texts texts ->
            texts
                |> List.map (floatArgs args)
                |> List.foldl Dict.union Dict.empty

        Float printer accessor name ->
            Dict.singleton name ( printer, args |> accessor )

        _ ->
            Dict.empty



---- EXPORT TO ICU MESSAGE FORMAT


toIcu : Translation args msg -> String
toIcu translation =
    case translation of
        Final _ text ->
            textToIcu Nothing text

        Fallback _ text ->
            textToIcu Nothing text


textToIcu : Maybe (Text args msg) -> Text args msg -> String
textToIcu maybeCount text =
    case text of
        Texts texts ->
            texts
                |> List.map (textToIcu maybeCount)
                |> String.concat

        Verbatim string ->
            string

        String _ name ->
            "{" ++ name ++ "}"

        Node _ name nodeText ->
            [ "{"
            , name
            , ", node, {"
            , nodeText |> textToIcu maybeCount
            , "}}"
            ]
                |> String.concat

        Float (Printer printerNames _) _ name ->
            [ "{"
            , [ [ name
                , "number"
                ]
              , printerNames
              ]
                |> List.concat
                |> String.join ", "
            , "}"
            ]
                |> String.concat

        WrapFloat (Wrapper wrapperNames wrapper) arg accessor name ->
            wrapper arg accessor name
                |> textToIcu Nothing

        Plural _ arg _ name allPluralForms ->
            let
                pluralFormToIcu form text =
                    form ++ "{" ++ textToIcu Nothing text ++ "}"
            in
            [ "{"
            , name
            , ", plural, "
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
            , "}"
            ]
                |> String.concat

        Count ->
            "#"
