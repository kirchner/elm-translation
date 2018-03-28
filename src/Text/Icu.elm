module Text.Icu
    exposing
        ( ArgType
            ( ArgCardinal
            , ArgDate
            , ArgDelimited
            , ArgFloat
            , ArgList
            , ArgNode
            , ArgOrdinal
            , ArgStaticList
            , ArgString
            , ArgTime
            )
        , IcuArg
            ( OverwritePlaceholder
            , TakePlaceholder
            )
        , Locale
        , addAllowedCardinalForms
        , addAllowedOrdinalForms
        , addDatePrinter
        , addDelimitedPrinter
        , addFloatPrinter
        , addListPrinter
        , addStaticListPrinter
        , addTexts
        , addTimePrinter
        , addToArgType
        , addToCardinalForm
        , addToOrdinalForm
        , argTypeToCldr
        , cldrToArgType
        , locale
        , translateTo
        )

{-|

@docs translateTo

@docs Locale, locale

@docs addToArgType, ArgType, cldrToArgType

@docs argTypeToCldr, IcuArg

@docs addTexts

@docs addToCardinalForm, addToOrdinalForm, addAllowedCardinalForms, addAllowedOrdinalForms

@docs addDelimitedPrinter, addStaticListPrinter, addListPrinter, addFloatPrinter, addDatePrinter, addTimePrinter

-}

import Date exposing (Date)
import Dict exposing (Dict)
import Internal.Icu as Icu
import Set
import Text
    exposing
        ( AllPluralForms
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
        )
import Time exposing (Time)


{-| -}
type ArgType
    = ArgDelimited (List Name)
    | ArgStaticList (List Name)
    | ArgString
    | ArgNode
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
cldrToArgType : List Name -> Maybe ArgType
cldrToArgType names =
    case names of
        "_" :: "delimited" :: otherNames ->
            Just (ArgDelimited otherNames)

        "_" :: "list" :: otherNames ->
            Just (ArgStaticList otherNames)

        _ :: "list" :: otherNames ->
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


{-| This is the "inverse" function to [`cldrToArgType`](#cldrToArgType). For
example, when exporting a translation with a placeholder `float (printer
[ "decimal", "standard" ] (...)) .duration "duration"` we want to get the
ICU message `{duration, number, decimal, standard}`.
-}
argTypeToCldr : ArgType -> IcuArg
argTypeToCldr argType =
    case argType of
        ArgDelimited names ->
            OverwritePlaceholder "_" ("delimited" :: names)

        ArgStaticList names ->
            OverwritePlaceholder "_" ("list" :: names)

        ArgString ->
            TakePlaceholder []

        ArgNode ->
            TakePlaceholder [ "node" ]

        ArgList names ->
            TakePlaceholder ("list" :: names)

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


{-| Most importantly, you can store texts inside a `Locale` so you can
use [`translateTo`](#translateTo) to dynamically change your translations. It
also holds information on how to parse the ICU formatted translations, printers
for the different types and pluralization rules. You can adjust these to your
needs using the helper functions below.
-}
type Locale args node
    = Locale (LocaleData args node)


type alias LocaleData args node =
    { toArgType : List Name -> Maybe ArgType
    , translations : Dict (List String) String

    -- printer
    , delimitedPrinters : Dict (List Name) (Printer (Text Static args node) args node)
    , staticListPrinters : Dict (List Name) (Printer (List (Text Static args node)) args node)
    , listPrinters : Dict (List Name) (Printer (List String) args node)
    , floatPrinters : Dict (List Name) (FloatPrinter args node)
    , datePrinters : Dict (List Name) (Printer Date args node)
    , timePrinters : Dict (List Name) (Printer Time args node)

    -- pluralization
    , toCardinalForm : Float -> FloatInfo -> PluralForm
    , toOrdinalForm : Float -> FloatInfo -> PluralForm
    , allowedCardinalForms : List PluralForm
    , allowedOrdinalForms : List PluralForm
    }


{-| -}
locale : Locale args node
locale =
    Locale defaultLocaleData


defaultLocaleData : LocaleData args node
defaultLocaleData =
    { toArgType = cldrToArgType
    , translations = Dict.empty

    -- printer
    , delimitedPrinters = Dict.empty
    , staticListPrinters = Dict.empty
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


{-| When parsing an ICU message, we have to decide which keywords correspond to
which placeholders or printers. For example when using the default
[`cldrToArgType`](#cldrToArgType) function, the message `"{duration, number,
decimal, standard}"` should generate a float placeholder named "duration" using
the printer with the name `[ "decimal", "standard" ]`.

You problably only have to change this if you do not want to use
[`kirchner/elm-cldr`](https://github.com/kirchner/elm-cldr) but provide
your own custom printers.

-}
addToArgType : (List Name -> Maybe ArgType) -> Locale args node -> Locale args node
addToArgType toArgType (Locale localeData) =
    Locale
        { localeData
            | toArgType = toArgType
        }


{-| Add texts to your locale. The first element in the tuple is the key
name. These correspond to the first String arguments provided to
[`final`](#final) and [`fallback`](#fallback).
-}
addTexts : List ( List String, String ) -> Locale args node -> Locale args node
addTexts translationList (Locale localeData) =
    Locale
        { localeData
            | translations =
                Dict.union
                    (translationList |> Dict.fromList)
                    localeData.translations
        }


{-| -}
addDelimitedPrinter : Printer (Text Static args node) args node -> Locale args node -> Locale args node
addDelimitedPrinter printer (Locale localeData) =
    Locale
        { localeData
            | delimitedPrinters =
                Dict.insert (Text.printerNames printer) printer localeData.delimitedPrinters
        }


{-| -}
addStaticListPrinter : Printer (List (Text Static args node)) args node -> Locale args node -> Locale args node
addStaticListPrinter printer (Locale localeData) =
    Locale
        { localeData
            | staticListPrinters =
                Dict.insert (Text.printerNames printer) printer localeData.staticListPrinters
        }


{-| -}
addListPrinter : Printer (List String) args node -> Locale args node -> Locale args node
addListPrinter printer (Locale localeData) =
    Locale
        { localeData
            | listPrinters =
                Dict.insert (Text.printerNames printer) printer localeData.listPrinters
        }


{-| -}
addFloatPrinter : FloatPrinter args node -> Locale args node -> Locale args node
addFloatPrinter printer (Locale localeData) =
    Locale
        { localeData
            | floatPrinters =
                Dict.insert (Text.floatPrinterNames printer) printer localeData.floatPrinters
        }


{-| -}
addDatePrinter : Printer Date args node -> Locale args node -> Locale args node
addDatePrinter printer (Locale localeData) =
    Locale
        { localeData
            | datePrinters =
                Dict.insert (Text.printerNames printer) printer localeData.datePrinters
        }


{-| -}
addTimePrinter : Printer Time args node -> Locale args node -> Locale args node
addTimePrinter printer (Locale localeData) =
    Locale
        { localeData
            | timePrinters =
                Dict.insert (Text.printerNames printer) printer localeData.timePrinters
        }


{-| -}
addToCardinalForm : (Float -> FloatInfo -> PluralForm) -> Locale args node -> Locale args node
addToCardinalForm toCardinalForm (Locale localeData) =
    Locale
        { localeData | toCardinalForm = toCardinalForm }


{-| -}
addToOrdinalForm : (Float -> FloatInfo -> PluralForm) -> Locale args node -> Locale args node
addToOrdinalForm toOrdinalForm (Locale localeData) =
    Locale
        { localeData | toOrdinalForm = toOrdinalForm }


{-| -}
addAllowedCardinalForms : List PluralForm -> Locale args node -> Locale args node
addAllowedCardinalForms pluralForms (Locale localeData) =
    { localeData
        | allowedCardinalForms =
            pluralForms
                |> List.append localeData.allowedCardinalForms
                |> makePluralFormsUnique
    }
        |> Locale


{-| -}
addAllowedOrdinalForms : List PluralForm -> Locale args node -> Locale args node
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
    Locale args node
    -> Text Dynamic args node
    -> Text Dynamic args node
translateTo (Locale localeData) text =
    let
        ( args, scope, name, staticText ) =
            Text.staticText text
    in
    localeData.translations
        |> Dict.get (scope ++ [ name ])
        |> Maybe.andThen (Icu.parse >> Result.toMaybe)
        |> Maybe.map
            (\message ->
                icuToText locale
                    { node = args.nodeArgs
                    , string = args.stringArgs
                    , list = args.listArgs
                    , float = args.floatArgs
                    , date = args.dateArgs
                    , time = args.timeArgs
                    }
                    message
                    |> Text.name name
                    |> Text.scope scope
                    |> Text.nameArgs args
            )
        |> Maybe.withDefault text



--  INTERNAL TRANSLATE HELPER


type alias Placeholders args node =
    { node : Dict Name (args -> (List node -> node))
    , string : Dict Name (args -> String)
    , list : Dict Name (args -> List String)
    , float : Dict Name (args -> Float)
    , date : Dict Name (args -> Date)
    , time : Dict Name (args -> Time)
    }


icuToText : Locale args node -> Placeholders args node -> Icu.Message -> Text Static args node
icuToText locale accessors message =
    message
        |> List.map (icuPartToText locale accessors)
        |> Text.concat


icuPartToText : Locale args node -> Placeholders args node -> Icu.Part -> Text Static args node
icuPartToText ((Locale localeData) as locale) accessors part =
    let
        simplePlaceholder constructor placeholder otherNames printers accessorType =
            Maybe.map2
                (\printer accessor -> constructor accessor printer)
                (localeData
                    |> printers
                    |> Dict.get otherNames
                )
                (accessors
                    |> accessorType
                    |> Dict.get placeholder
                )
                |> Maybe.withDefault (Text.s "")
    in
    case part of
        Icu.Text text ->
            Text.s text

        Icu.Argument { placeholder, names, subMessages } ->
            case localeData.toArgType (placeholder :: names) of
                Just (ArgDelimited otherNames) ->
                    case subMessages of
                        (Icu.Unnamed { message }) :: [] ->
                            localeData.delimitedPrinters
                                |> Dict.get otherNames
                                |> Maybe.map
                                    (\printer ->
                                        Text.delimited printer <|
                                            icuToText locale accessors message
                                    )
                                |> Maybe.withDefault (Text.s "")

                        _ ->
                            Text.s ""

                Just (ArgStaticList otherNames) ->
                    let
                        toText subMessage =
                            case subMessage of
                                Icu.Unnamed { message } ->
                                    icuToText locale accessors message

                                Icu.Named _ ->
                                    Text.s ""
                    in
                    localeData.staticListPrinters
                        |> Dict.get otherNames
                        |> Maybe.map
                            (\printer ->
                                Text.staticList printer <|
                                    List.map toText subMessages
                            )
                        |> Maybe.withDefault (Text.s "")

                Just ArgString ->
                    accessors.string
                        |> Dict.get placeholder
                        |> Maybe.map
                            (\accessor ->
                                Text.string accessor
                            )
                        |> Maybe.withDefault (Text.s "")

                Just ArgNode ->
                    accessors.node
                        |> Dict.get placeholder
                        |> Maybe.map
                            (\accessor ->
                                case subMessages of
                                    (Icu.Unnamed { message }) :: [] ->
                                        Text.node accessor <|
                                            icuToText locale accessors message

                                    _ ->
                                        Text.s ""
                            )
                        |> Maybe.withDefault (Text.s "")

                Just (ArgList otherNames) ->
                    simplePlaceholder Text.list placeholder otherNames .listPrinters .list

                Just (ArgFloat otherNames) ->
                    simplePlaceholder Text.float placeholder otherNames .floatPrinters .float

                Just (ArgDate otherNames) ->
                    simplePlaceholder Text.date placeholder otherNames .datePrinters .date

                Just (ArgTime otherNames) ->
                    simplePlaceholder Text.time placeholder otherNames .timePrinters .time

                Just (ArgCardinal otherNames) ->
                    Maybe.map3
                        (\printer accessor allPluralForms ->
                            Text.plural
                                accessor
                                printer
                                localeData.toCardinalForm
                                []
                                allPluralForms
                        )
                        (Dict.get otherNames localeData.floatPrinters)
                        (Dict.get placeholder accessors.float)
                        (allPluralForms (icuToText locale accessors) subMessages)
                        |> Maybe.withDefault (Text.s "")

                Just (ArgOrdinal otherNames) ->
                    Maybe.map3
                        (\printer accessor allPluralForms ->
                            Text.plural
                                accessor
                                printer
                                localeData.toOrdinalForm
                                []
                                allPluralForms
                        )
                        (Dict.get otherNames localeData.floatPrinters)
                        (Dict.get placeholder accessors.float)
                        (allPluralForms (icuToText locale accessors) subMessages)
                        |> Maybe.withDefault (Text.s "")

                Nothing ->
                    Text.s ""

        Icu.Hash ->
            Text.count


allPluralForms :
    (Icu.Message -> Text Static args node)
    -> List Icu.SubMessage
    -> Maybe (AllPluralForms args node)
allPluralForms toText subMessages =
    subMessages
        |> List.foldl
            (addPluralForm toText)
            { other = Text.s ""
            , zero = Nothing
            , one = Nothing
            , two = Nothing
            , few = Nothing
            , many = Nothing
            }
        |> Just


addPluralForm :
    (Icu.Message -> Text Static args node)
    -> Icu.SubMessage
    -> AllPluralForms args node
    -> AllPluralForms args node
addPluralForm toText subMessage allPluralForms =
    case subMessage of
        Icu.Named { name, message } ->
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
