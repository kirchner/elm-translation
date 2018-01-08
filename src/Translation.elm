module Translation
    exposing
        ( Name
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
    | Node (args -> (List (Node msg) -> Node msg)) Name (Text args msg)
    | String (args -> String) Name
    | Float (Maybe NumberFormat) (Float -> String) (args -> Float) Name
    | Int (Maybe NumberFormat) (Int -> String) (args -> Int) Name
    | Select Name (args -> ( String, Text args msg ))
    | Plural (Maybe NumberFormat) (Float -> String) (Float -> String -> PluralForm) (args -> Float) Name (AllPluralForms args msg)
    | Count


type alias Name =
    String


type NumberFormat
    = CustomNumberFormat String
    | DecimalFormat
    | ScientificFormat
    | PercentFormat
    | CurrencyFormat
    | AtLeastFormat


type Case args msg a
    = Case (a -> Bool) String (Text args msg)


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


float : (Float -> String) -> (args -> Float) -> String -> Text args msg
float =
    Float Nothing


int : (Int -> String) -> (args -> Int) -> String -> Text args msg
int =
    Int Nothing


select : (args -> a) -> String -> Text args msg -> List (Case args msg a) -> Text args msg
select accessor name defaultText cases =
    Select name <|
        \args ->
            args
                |> accessor
                |> pickCase cases
                |> Maybe.withDefault ( "other", defaultText )


pickCase : List (Case args msg a) -> a -> Maybe ( String, Text args msg )
pickCase cases a =
    case cases of
        (Case test name text) :: rest ->
            if test a then
                Just ( name, text )
            else
                pickCase rest a

        [] ->
            Nothing


equals : a -> String -> Text args msg -> Case args msg a
equals a =
    Case ((==) a)


when : (a -> Bool) -> String -> Text args msg -> Case args msg a
when =
    Case


plural :
    (Float -> String)
    -> (Float -> String -> PluralForm)
    -> (args -> Float)
    -> String
    -> AllPluralForms args msg
    -> Text args msg
plural =
    Plural Nothing


count : Text args msg
count =
    Count



---- NUMBER FORMATS


decimal : ((args -> number) -> String -> Text args msg) -> (args -> number) -> String -> Text args msg
decimal textFunc accessor name =
    textFunc accessor name
        |> setNumberFormat DecimalFormat


scientific : ((args -> number) -> String -> Text args msg) -> (args -> number) -> String -> Text args msg
scientific textFunc accessor name =
    textFunc accessor name
        |> setNumberFormat ScientificFormat


percent : ((args -> number) -> String -> Text args msg) -> (args -> number) -> String -> Text args msg
percent textFunc accessor name =
    textFunc accessor name
        |> setNumberFormat PercentFormat


currency : ((args -> number) -> String -> Text args msg) -> (args -> number) -> String -> Text args msg
currency textFunc accessor name =
    textFunc accessor name
        |> setNumberFormat CurrencyFormat


atLeast : ((args -> number) -> String -> Text args msg) -> (args -> number) -> String -> Text args msg
atLeast textFunc accessor name =
    textFunc accessor name
        |> setNumberFormat AtLeastFormat


setNumberFormat : NumberFormat -> Text args msg -> Text args msg
setNumberFormat numberFormat text =
    case text of
        Float _ printer accessor name ->
            Float (Just numberFormat) printer accessor name

        Int _ printer accessor name ->
            Int (Just numberFormat) printer accessor name

        _ ->
            text



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

        Float _ printer accessor _ ->
            args
                |> accessor
                |> printer

        Int _ printer accessor _ ->
            args
                |> accessor
                |> printer

        Select _ textAccessor ->
            args
                |> textAccessor
                |> Tuple.second
                |> printText maybeCount args

        Plural _ countToString countToPluralForm accessor _ allPluralForms ->
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

        Float _ printer accessor _ ->
            [ args
                |> accessor
                |> printer
                |> VirtualDom.text
            ]

        Int _ printer accessor _ ->
            [ args
                |> accessor
                |> printer
                |> VirtualDom.text
            ]

        Select _ textAccessor ->
            args
                |> textAccessor
                |> Tuple.second
                |> textToNodes maybeCount args

        Plural _ countToString countToPluralForm accessor _ allPluralForms ->
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


floatArgs : args -> Text args msg -> Dict String ( Maybe NumberFormat, Float )
floatArgs args text =
    case text of
        Texts texts ->
            texts
                |> List.map (floatArgs args)
                |> List.foldl Dict.union Dict.empty

        Float maybeNumberFormat _ accessor name ->
            Dict.singleton name ( maybeNumberFormat, args |> accessor )

        Plural maybeNumberFormat _ _ accessor name _ ->
            Dict.singleton name ( maybeNumberFormat, args |> accessor )

        _ ->
            Dict.empty


intArgs : args -> Text args msg -> Dict String ( Maybe NumberFormat, Int )
intArgs args text =
    case text of
        Texts texts ->
            texts
                |> List.map (intArgs args)
                |> List.foldl Dict.union Dict.empty

        Int maybeNumberFormat _ accessor name ->
            Dict.singleton name ( maybeNumberFormat, args |> accessor )

        _ ->
            Dict.empty


selectArgs : args -> Text args msg -> Dict String String
selectArgs args text =
    case text of
        Texts texts ->
            texts
                |> List.map (selectArgs args)
                |> List.foldl Dict.union Dict.empty

        Select name accessor ->
            args
                |> accessor
                |> Tuple.first
                |> Dict.singleton name

        _ ->
            Dict.empty
