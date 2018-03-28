module Text.Export
    exposing
        ( Text
        , concat
        , count
        , date
        , delimited
        , float
        , list
        , node
        , plural
        , s
        , staticList
        , string
        , time
        , toIcu
        , variants
        )

import Date exposing (Date)
import Text
import Time exposing (Time)


type alias Name =
    String


type alias Printer a =
    Text.Printer a () ()


type alias FloatPrinter =
    Text.FloatPrinter () ()


type alias ActualText =
    Text.Text Text.Static () ()


type Text
    = TVerbatim String
    | TTexts (List Text)
      -- with printers
    | TDelimited (Printer ActualText) Text
    | TStaticList (Printer (List ActualText)) (List Text)
      -- with placeholders
    | TString Name
    | TNode Name Text
      -- with printers and placeholders
    | TList Name (Printer (List String))
    | TFloat Name FloatPrinter
    | TDate Name (Printer Date)
    | TTime Name (Printer Time)
    | TPlural Name FloatPrinter (List ( Float, Text )) AllPluralForms
      -- misc
    | TCount
      -- variants
    | TVariant Name Text (List ( String, Text ))


type alias AllPluralForms =
    { other : Text
    , zero : Maybe Text
    , one : Maybe Text
    , two : Maybe Text
    , few : Maybe Text
    , many : Maybe Text
    }



---- CONSTRUCTORS


s : String -> Text
s =
    TVerbatim


concat : List Text -> Text
concat =
    TTexts


string : Name -> Text
string =
    TString


node : Name -> Text -> Text
node =
    TNode


delimited : Printer ActualText -> Text -> Text
delimited =
    TDelimited


staticList : Printer (List ActualText) -> List Text -> Text
staticList =
    TStaticList


list : Name -> Printer (List String) -> Text
list =
    TList


float : Name -> FloatPrinter -> Text
float =
    TFloat


date : Name -> Printer Date -> Text
date =
    TDate


time : Name -> Printer Time -> Text
time =
    TTime


plural : Name -> FloatPrinter -> a -> List ( Float, Text ) -> AllPluralForms -> Text
plural name printer _ otherTexts allPluralForms =
    TPlural name printer otherTexts allPluralForms


count : Text
count =
    TCount


variants : Name -> Text -> List ( String, Text ) -> Text
variants =
    TVariant



---- EXPORT


toIcu : Text -> String
toIcu text =
    let
        wrap string =
            "{" ++ string ++ "}"
    in
    case text of
        TVerbatim string ->
            string

        TTexts subTexts ->
            subTexts
                |> List.map toIcu
                |> String.concat

        TDelimited printer subText ->
            [ ("_" :: Text.printerNames printer)
                |> String.join ", "
            , subText
                |> toIcu
                |> wrap
            ]
                |> String.join ", "
                |> wrap

        TStaticList printer subTexts ->
            [ ("_" :: Text.printerNames printer)
                |> String.join ", "
            , subTexts
                |> List.map (toIcu >> wrap)
                |> String.join " "
            ]
                |> String.join ", "
                |> wrap

        TString name ->
            wrap name

        TNode name subText ->
            [ name
            , "node"
            , subText
                |> toIcu
                |> wrap
            ]
                |> String.join ", "
                |> wrap

        TList name printer ->
            (name :: Text.printerNames printer)
                |> String.join ", "
                |> wrap

        TFloat name printer ->
            (name :: Text.floatPrinterNames printer)
                |> String.join ", "
                |> wrap

        TDate name printer ->
            (name :: Text.printerNames printer)
                |> String.join ", "
                |> wrap

        TTime name printer ->
            (name :: Text.printerNames printer)
                |> String.join ", "
                |> wrap

        TPlural name printer otherTexts allPluralForms ->
            let
                toIcuPluralForm ( form, maybeText ) =
                    Maybe.map (\text -> form ++ wrap (toIcu text)) maybeText
            in
            [ (name :: "plural" :: Text.floatPrinterNames printer)
                |> String.join ", "
            , [ ( "other", Just allPluralForms.other )
              , ( "zero", allPluralForms.zero )
              , ( "one", allPluralForms.one )
              , ( "two", allPluralForms.two )
              , ( "few", allPluralForms.few )
              , ( "many", allPluralForms.many )
              ]
                |> List.filterMap toIcuPluralForm
                |> String.join " "
            ]
                |> String.join ", "
                |> wrap

        TCount ->
            "#"

        -- variants
        TVariant name defaultText subTexts ->
            [ name
            , "variants"
            , ((defaultText
                    |> toIcu
                    |> wrap
               )
                :: (subTexts
                        |> List.map
                            (\( variantName, variantText ) ->
                                variantName ++ wrap (toIcu variantText)
                            )
                   )
              )
                |> String.join " "
            ]
                |> String.join ", "
                |> wrap
