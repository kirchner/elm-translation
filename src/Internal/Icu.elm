module Internal.Icu
    exposing
        ( Message
        , Part
            ( Argument
            , Hash
            , Text
            )
        , SubMessage
            ( Named
            , Unnamed
            )
        , parse
        )

{-| Parse ICU messages, i.e. strings of the form

    "some text {placeholder, foo, bar, other{..} one{..}}"

    "some other text {placeholder} text text {placeholder, {..} {..}}"

-}

import Char
import Dict exposing (Dict)
import Parser exposing (..)
import Parser.LanguageKit exposing (..)
import Set


type alias Message =
    List Part


type Part
    = Text String
    | Argument Name (List Name) (List SubMessage)
    | Hash


type SubMessage
    = Unnamed Message
    | Named Name Message


type alias Name =
    String



---- PARSER


parse : String -> Result Parser.Error Message
parse rawMessage =
    Parser.run message rawMessage


message : Parser Message
message =
    inContext "a message" <|
        oneOf
            [ lazy (\_ -> part)
                |> repeat oneOrMore
                |> map joinTextParts
            , succeed [ Text "" ]
                |. end
            ]


part : Parser Part
part =
    inContext "a part" <|
        oneOf
            [ text
            , lazy (\_ -> argument)
            , symbol "#" |> map (always Hash)
            ]


text : Parser Part
text =
    inContext "some text" <|
        succeed Text
            |= oneOf
                [ source <|
                    ignore oneOrMore <|
                        isNotOneOf [ '{', '}', '#', '\'' ]
                , delayedCommit (symbol "'") <|
                    succeed identity
                        |= source (ignore oneOrMore (isNotOneOf [ '\'' ]))
                        |. symbol "'"
                , delayedCommit (symbol "'") <|
                    succeed "'"
                        |. symbol "'"
                , fail "some text"
                ]


argument : Parser Part
argument =
    inContext "an argument" <|
        succeed identity
            |. symbol "{"
            |. spaces
            |= andThen (\placeholder -> namesHelp placeholder []) name
            |. spaces
            |. symbol "}"


namesHelp : String -> List String -> Parser Part
namesHelp placeholder names =
    inContext "a list of names" <|
        oneOf
            [ delayedCommit spaces <|
                succeed identity
                    |. symbol ","
                    |. spaces
                    |= oneOf
                        [ andThen
                            (\subMessage ->
                                subMessagesHelp placeholder (List.reverse names) [ subMessage ]
                            )
                            nextSubMessage
                        , andThen
                            (\name ->
                                namesHelp placeholder (name :: names)
                            )
                            nextName
                        ]
            , succeed (Argument placeholder (List.reverse names) [])
            ]


subMessagesHelp : String -> List String -> List SubMessage -> Parser Part
subMessagesHelp placeholder names subMessages =
    inContext "a list of sub messages" <|
        oneOf
            [ delayedCommit spaces <|
                succeed identity
                    |= andThen
                        (\subMessage -> subMessagesHelp placeholder names (subMessage :: subMessages))
                        nextSubMessage
            , succeed (Argument placeholder names (List.reverse subMessages))
            ]


nextSubMessage : Parser SubMessage
nextSubMessage =
    oneOf
        [ succeed Unnamed
            |. symbol "{"
            |= lazy (\_ -> message)
            |. symbol "}"
        , delayedCommitMap Named name <|
            succeed identity
                |. symbol "{"
                |= lazy (\_ -> message)
                |. symbol "}"
        ]


nextName : Parser String
nextName =
    delayedCommit spaces <|
        succeed identity
            |= name


name : Parser String
name =
    inContext "a name" <|
        variable isFirstVarChar isVarChar Set.empty



---- PARSER HELPER


isFirstVarChar : Char -> Bool
isFirstVarChar char =
    Char.isLower char
        || Char.isUpper char
        || (char == '_')


isVarChar : Char -> Bool
isVarChar char =
    Char.isLower char
        || Char.isUpper char
        || Char.isDigit char
        || (char == '_')


spaces : Parser ()
spaces =
    ignore zeroOrMore <|
        \c ->
            (c == ' ')
                || (c == '\x0D')
                || (c == '\n')
                || (c == '\t')



---- HELPER


joinTextParts : List Part -> List Part
joinTextParts parts =
    case parts of
        [] ->
            []

        (Text first) :: (Text second) :: rest ->
            joinTextParts (Text (first ++ second) :: rest)

        first :: rest ->
            first :: joinTextParts rest


isNotOneOf : List Char -> Char -> Bool
isNotOneOf chars char =
    chars
        |> List.any (\c -> c == char)
        |> not


isPositive : Int -> Parser Int
isPositive int =
    if int < 0 then
        fail "negative integer"
    else
        succeed int
