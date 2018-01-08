module Translation
    exposing
        ( ArgName
        , NumberFormat
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
        , concat
        , count
        , float
        , int
        , node
        , nodes
        , plural
        , print
        , printWith
        , s
        , string
        )

import Dict exposing (Dict)
import Set
import VirtualDom exposing (Node)


type Translation args msg
    = Final String (Text args msg)
    | Fallback String (Text args msg)


type Text args msg
    = Texts (List (Text args msg))
    | Text String
    | Node (args -> (List (Node msg) -> Node msg)) ArgName (Text args msg)
    | String (args -> String) ArgName
    | Float (Float -> String) NumberFormat (args -> Float) ArgName
    | Int (Int -> String) NumberFormat (args -> Int) ArgName
    | Plural (Float -> String) (Float -> String -> PluralForm) NumberFormat (args -> Float) ArgName (AllPluralForms args msg)
    | Count


type alias ArgName =
    String


type NumberFormat
    = CustomNumberFormat String
    | DecimalFormat
    | ScientificFormat
    | PercentFormat
    | CurrencyFormat
    | AtLeastFormat


type PluralForm
    = Other
    | Zero
    | One
    | Two
    | Few
    | Many


type alias AllPluralForms args msg =
    { other : Text args msg
    , zero : Maybe (Text args msg)
    , one : Maybe (Text args msg)
    , two : Maybe (Text args msg)
    , few : Maybe (Text args msg)
    , many : Maybe (Text args msg)
    }



---- TRANSLATION CONSTRUCTOR


final : String -> Text args msg -> Translation args msg
final =
    Final


fallback : String -> Text args msg -> Translation args msg
fallback =
    Fallback



---- TEXT CONSTRUCTOR


s : String -> Text args msg
s =
    Text


concat : List (Text args msg) -> Text args msg
concat =
    -- TODO: merge lists of lists
    Texts


node : (args -> (List (Node msg) -> Node msg)) -> String -> Text args msg -> Text args msg
node =
    Node


string : (args -> String) -> String -> Text args msg
string =
    String


float : (Float -> String) -> NumberFormat -> (args -> Float) -> String -> Text args msg
float =
    Float


int : (Int -> String) -> NumberFormat -> (args -> Int) -> String -> Text args msg
int =
    Int


plural :
    (Float -> String)
    -> (Float -> String -> PluralForm)
    -> NumberFormat
    -> (args -> Float)
    -> String
    -> AllPluralForms args msg
    -> Text args msg
plural =
    Plural


count : Text args msg
count =
    Count



---- NUMBER FORMATS


customNumberFormat : String -> NumberFormat
customNumberFormat =
    CustomNumberFormat


decimalFormat : NumberFormat
decimalFormat =
    DecimalFormat


scientificFormat : NumberFormat
scientificFormat =
    ScientificFormat


percentFormat : NumberFormat
percentFormat =
    PercentFormat


currencyFormat : NumberFormat
currencyFormat =
    CurrencyFormat


atLeastFormat : NumberFormat
atLeastFormat =
    AtLeastFormat



---- PRINT


printWith : args -> Translation args msg -> String
printWith args translation =
    case translation of
        Final _ text ->
            printText Nothing args text

        Fallback _ text ->
            printText Nothing args text


print : Translation {} msg -> String
print translation =
    printWith {} translation


nodes : args -> Translation args msg -> List (Node msg)
nodes args translation =
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

        Text string ->
            string

        Node accessor _ text ->
            printText maybeCount args text

        String accessor _ ->
            accessor args

        Float printer _ accessor _ ->
            args
                |> accessor
                |> printer

        Int printer _ accessor _ ->
            args
                |> accessor
                |> printer

        Plural countToString countToPluralForm _ accessor _ allPluralForms ->
            let
                count =
                    args
                        |> accessor
                        |> countToString

                pluralForm =
                    countToPluralForm (args |> accessor) count

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

        Text string ->
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

        Float printer _ accessor _ ->
            [ args
                |> accessor
                |> printer
                |> VirtualDom.text
            ]

        Int printer _ accessor _ ->
            [ args
                |> accessor
                |> printer
                |> VirtualDom.text
            ]

        Plural countToString countToPluralForm _ accessor _ allPluralForms ->
            let
                count =
                    args
                        |> accessor
                        |> countToString

                pluralForm =
                    countToPluralForm (args |> accessor) count

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
    , printFloat : NumberFormat -> Float -> String
    , printInt : NumberFormat -> Int -> String
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


translateText : Maybe String -> Locale -> args -> String -> Text args msg -> String
translateText maybeCount locale args name text =
    "TODO: implement"


nodeArgs : args -> Text args msg -> Dict String (List (Node msg) -> Node msg)
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


stringArgs : args -> Text args msg -> Dict String String
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


floatArgs : args -> Text args msg -> Dict String ( NumberFormat, Float )
floatArgs args text =
    case text of
        Texts texts ->
            texts
                |> List.map (floatArgs args)
                |> List.foldl Dict.union Dict.empty

        Float _ numberFormat accessor name ->
            Dict.singleton name ( numberFormat, args |> accessor )

        Plural _ _ numberFormat accessor name _ ->
            Dict.singleton name ( numberFormat, args |> accessor )

        _ ->
            Dict.empty


intArgs : args -> Text args msg -> Dict String ( NumberFormat, Int )
intArgs args text =
    case text of
        Texts texts ->
            texts
                |> List.map (intArgs args)
                |> List.foldl Dict.union Dict.empty

        Int _ numberFormat accessor name ->
            Dict.singleton name ( numberFormat, args |> accessor )

        _ ->
            Dict.empty
