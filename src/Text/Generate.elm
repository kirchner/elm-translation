module Text.Generate
    exposing
        ( Argument
            ( Cardinal
            , Date
            , Delimited
            , Float
            , List
            , Node
            , Ordinal
            , StaticList
            , String
            , Time
            )
        , Error
            ( ArgumentError
            , ParserError
            )
        , Function
        , Problem
            ( BadArgument
            , ExpectingOnlyNamedSubMessages
            , ExpectingOnlyUnnamedSubMessages
            , ExpectingSingleUnnamedSubMessage
            )
        , function
        , typeSignature
        )

{-|


# Code Generation

The following functions are only needed by
[`kirchner/elm-translation-runner`](https://github.com/kirchner/elm-translation-runner),
so it can generate Elm modules from JSON translation files.

@docs function, typeSignature

@docs Argument, Function

@docs Error, Problem

-}

import Dict exposing (Dict)
import Internal.Icu as Icu
import Parser
import Set exposing (Set)


{-| -}
type Argument
    = String
    | Node
    | Delimited Function
    | StaticList Function
    | List Function
    | Float Function
    | Date Function
    | Time Function
    | Cardinal Function
    | Ordinal Function


{-| -}
type alias Function =
    { name : String
    , moduleName : String
    }


{-| These are the errors you can get when parsing an ICU formatted message.
-}
type Error
    = ParserError Parser.Error
    | ArgumentError
        { source : String
        , from : Int
        , to : Int
        , placeholder : String
        , names : List String
        , namedSubMessages :
            List
                { name : String
                , from : Int
                , to : Int
                }
        , unnamedSubMessages :
            List
                { from : Int
                , to : Int
                }
        , problem : Problem
        }


{-| -}
type Problem
    = BadArgument
    | ExpectingSingleUnnamedSubMessage
    | ExpectingOnlyUnnamedSubMessages
    | ExpectingOnlyNamedSubMessages (List String)


{-| Given an ICU message, this function produces Elm code for a `Translation`.
For example, `function toArgument [ "scope" ] "greeting" "Good morning, {name}!"` will
produce the following function:

    greeting : Translation { args | name : String } node
    greeting =
        translation "scope.greeting" <|
            concat
                [ s "Good morning, "
                , string .name "name"
                , s "!"
                ]

where

    toArgument : List String -> Maybe Argument
    toArgument _ =
        Just String

-}
function :
    (List String -> Maybe Argument)
    -> List String
    -> String
    -> String
    -> Result (List Error) ( String, Set String )
function toArgType scope name icuMessage =
    icuMessage
        |> Icu.parse
        |> Result.mapError (ParserError >> List.singleton)
        |> Result.andThen (icuToElm toArgType icuMessage scope name)


{-| Given an ICU message, this function returns the Elm code for the type of
the corresponding `Translation`. So `typeSignature "Good morning, {name}!"`
will produce `"Translation { args | name : String } node`, for example.
-}
typeSignature : (List String -> Maybe Argument) -> String -> Result (List Error) String
typeSignature toArgType icuMessage =
    icuMessage
        |> Icu.parse
        |> Result.mapError (ParserError >> List.singleton)
        |> Result.andThen
            (argsFromMessage toArgType icuMessage
                >> Result.map returnType
            )



---- IMPLEMENTATION


icuToElm :
    (List String -> Maybe Argument)
    -> String
    -> List String
    -> String
    -> Icu.Message
    -> Result (List Error) ( String, Set String )
icuToElm toArgType source scope name icuMessage =
    Result.map2
        (\declaration definition ->
            ( [ declaration
              , Tuple.first definition
              ]
                |> String.join "\n"
            , Tuple.second definition
            )
        )
        (icuMessage
            |> argsFromMessage toArgType source
            |> Result.map (functionDeclaration name)
        )
        (functionDefinition toArgType source scope name icuMessage)



---- FUNCTION DECLARATION


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
    , "node"
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



---- FUNCTION DEFINITION


functionDefinition :
    (List String -> Maybe Argument)
    -> String
    -> List String
    -> String
    -> Icu.Message
    -> Result (List Error) ( String, Set String )
functionDefinition toArgType source scope name icuMessage =
    messageToElm toArgType source icuMessage
        |> Result.map
            (Tuple.mapFirst <|
                \elm ->
                    let
                        body =
                            [ [ "translation [ "
                              , (scope ++ [ name ])
                                    |> String.join ", "
                              , " ] <|"
                              ]
                                |> String.concat
                            , indent elm
                            ]
                                |> String.join "\n"
                    in
                    [ name ++ " ="
                    , body
                        |> indent
                    ]
                        |> String.join "\n"
            )


messageToElm :
    (List String -> Maybe Argument)
    -> String
    -> Icu.Message
    -> Result (List Error) ( String, Set String )
messageToElm toArgType source parts =
    case parts of
        part :: [] ->
            part
                |> partToElm toArgType source

        _ ->
            parts
                |> collectWithErrors (partToElm toArgType source)
                |> Result.map
                    (\listElm ->
                        ( [ "concat"
                          , listElm
                                |> List.map Tuple.first
                                |> generateList
                                |> indent
                          ]
                            |> String.join "\n"
                        , listElm
                            |> List.map Tuple.second
                            |> List.foldl Set.union Set.empty
                        )
                    )


partToElm :
    (List String -> Maybe Argument)
    -> String
    -> Icu.Part
    -> Result (List Error) ( String, Set String )
partToElm toArgType source part =
    let
        simplePlaceholder tvpe name function =
            [ tvpe
            , function.name
            , accessor name
            , quote name
            ]
                |> String.join " "
    in
    case part of
        Icu.Text text ->
            Ok
                ( [ "s "
                  , quote text
                  ]
                    |> String.concat
                , Set.empty
                )

        Icu.Argument { from, to, placeholder, names, subMessages } ->
            let
                { named, unnamed } =
                    subMessages
                        |> List.foldr
                            (\subMessage sortedSubMessages ->
                                case subMessage of
                                    Icu.Named { name, message } ->
                                        { sortedSubMessages
                                            | named =
                                                ( name, message )
                                                    :: sortedSubMessages.named
                                        }

                                    Icu.Unnamed { message } ->
                                        { sortedSubMessages
                                            | unnamed = message :: sortedSubMessages.unnamed
                                        }
                            )
                            { named = []
                            , unnamed = []
                            }

                argumentError problem =
                    Err
                        [ ArgumentError
                            { source = source
                            , from = from
                            , to = to
                            , placeholder = placeholder
                            , names = names
                            , namedSubMessages =
                                named
                                    |> List.map
                                        (\( name, subMessage ) ->
                                            { name = name
                                            , from = -1
                                            , to = -1
                                            }
                                        )
                            , unnamedSubMessages =
                                unnamed
                                    |> List.map
                                        (\subMessage ->
                                            { from = -1
                                            , to = -1
                                            }
                                        )
                            , problem = problem
                            }
                        ]
            in
            case toArgType names of
                Just (Delimited function) ->
                    case subMessages of
                        (Icu.Unnamed { message }) :: [] ->
                            message
                                |> messageToElm toArgType source
                                |> Result.map
                                    (\( elm, imports ) ->
                                        ( [ [ "delimited"
                                            , function.name
                                            , "<|"
                                            ]
                                                |> String.join " "
                                          , indent elm
                                          ]
                                            |> String.join "\n"
                                        , Set.insert function.moduleName imports
                                        )
                                    )

                        _ ->
                            argumentError ExpectingSingleUnnamedSubMessage

                Just (StaticList function) ->
                    case named of
                        [] ->
                            unnamed
                                |> collectWithErrors (messageToElm toArgType source)
                                |> Result.map
                                    (\listElm ->
                                        ( [ [ "list"
                                            , function.name
                                            ]
                                                |> String.join " "
                                          , listElm
                                                |> List.map Tuple.first
                                                |> generateList
                                                |> indent
                                          ]
                                            |> String.join "\n"
                                        , listElm
                                            |> List.map Tuple.second
                                            |> List.foldl Set.union Set.empty
                                        )
                                    )

                        _ ->
                            argumentError ExpectingOnlyUnnamedSubMessages

                Just String ->
                    Ok
                        ( [ "string"
                          , accessor placeholder
                          , quote placeholder
                          ]
                            |> String.join " "
                        , Set.empty
                        )

                Just Node ->
                    case subMessages of
                        (Icu.Unnamed { message }) :: [] ->
                            messageToElm toArgType source message
                                |> Result.map
                                    (Tuple.mapFirst <|
                                        \elm ->
                                            [ [ "node"
                                              , accessor placeholder
                                              , quote placeholder
                                              , "<|"
                                              ]
                                                |> String.join " "
                                            , indent elm
                                            ]
                                                |> String.join "\n"
                                    )

                        _ ->
                            argumentError ExpectingSingleUnnamedSubMessage

                Just (List function) ->
                    Ok
                        ( simplePlaceholder "list" placeholder function
                        , Set.singleton function.moduleName
                        )

                Just (Float function) ->
                    Ok
                        ( simplePlaceholder "float" placeholder function
                        , Set.singleton function.moduleName
                        )

                Just (Date function) ->
                    Ok
                        ( simplePlaceholder "date" placeholder function
                        , Set.singleton function.moduleName
                        )

                Just (Time function) ->
                    Ok
                        ( simplePlaceholder "time" placeholder function
                        , Set.singleton function.moduleName
                        )

                Just (Cardinal function) ->
                    case unnamed of
                        [] ->
                            named
                                |> List.foldr
                                    (\( name, subMessage ) result ->
                                        case result of
                                            Ok listElm ->
                                                case messageToElm toArgType source subMessage of
                                                    Ok ( elm, imports ) ->
                                                        Ok (( ( name, elm ), imports ) :: listElm)

                                                    Err errors ->
                                                        Err errors

                                            Err errors ->
                                                case messageToElm toArgType source subMessage of
                                                    Ok _ ->
                                                        result

                                                    Err nextErrors ->
                                                        Err (errors ++ nextErrors)
                                    )
                                    (Ok [])
                                |> Result.map
                                    (\listNamedElm ->
                                        ( [ [ simplePlaceholder "cardinal" placeholder function
                                            , "<|"
                                            ]
                                                |> String.join " "
                                          , listNamedElm
                                                |> List.map Tuple.first
                                                |> generateRecord
                                                |> indent
                                          ]
                                            |> String.join "\n"
                                        , listNamedElm
                                            |> List.map Tuple.second
                                            |> List.foldl Set.union
                                                (Set.singleton function.moduleName)
                                        )
                                    )

                        _ ->
                            argumentError <|
                                ExpectingOnlyNamedSubMessages
                                    [ "other"
                                    , "zero"
                                    , "one"
                                    , "two"
                                    , "few"
                                    , "many"
                                    ]

                Just (Ordinal function) ->
                    case unnamed of
                        [] ->
                            named
                                |> List.foldr
                                    (\( name, subMessage ) result ->
                                        case result of
                                            Ok listElm ->
                                                case messageToElm toArgType source subMessage of
                                                    Ok ( elm, imports ) ->
                                                        Ok (( ( name, elm ), imports ) :: listElm)

                                                    Err errors ->
                                                        Err errors

                                            Err errors ->
                                                case messageToElm toArgType source subMessage of
                                                    Ok _ ->
                                                        result

                                                    Err nextErrors ->
                                                        Err (errors ++ nextErrors)
                                    )
                                    (Ok [])
                                |> Result.map
                                    (\listNamedElm ->
                                        ( [ [ simplePlaceholder "cardinal" placeholder function
                                            , "<|"
                                            ]
                                                |> String.join " "
                                          , listNamedElm
                                                |> List.map Tuple.first
                                                |> generateRecord
                                                |> indent
                                          ]
                                            |> String.join "\n"
                                        , listNamedElm
                                            |> List.map Tuple.second
                                            |> List.foldl Set.union
                                                (Set.singleton function.moduleName)
                                        )
                                    )

                        _ ->
                            argumentError <|
                                ExpectingOnlyNamedSubMessages
                                    [ "other"
                                    , "zero"
                                    , "one"
                                    , "two"
                                    , "few"
                                    , "many"
                                    ]

                Nothing ->
                    argumentError BadArgument

        Icu.Hash ->
            Ok
                ( "count"
                , Set.empty
                )



---- ARGS FROM ICU MESSAGE


argsFromMessage :
    (List String -> Maybe Argument)
    -> String
    -> Icu.Message
    -> Result (List Error) (Dict String String)
argsFromMessage toArgType source message =
    message
        |> foldrWithErrors
            (argsFromPart toArgType source)
            Dict.union
            Dict.empty


argsFromPart :
    (List String -> Maybe Argument)
    -> String
    -> Icu.Part
    -> Result (List Error) (Dict String String)
argsFromPart toArgType source part =
    case part of
        Icu.Argument { from, to, placeholder, names, subMessages } ->
            let
                { named, unnamed } =
                    subMessages
                        |> List.foldl
                            (\subMessage sortedSubMessages ->
                                case subMessage of
                                    Icu.Named namedData ->
                                        { sortedSubMessages
                                            | named = namedData :: sortedSubMessages.named
                                        }

                                    Icu.Unnamed unnamedData ->
                                        { sortedSubMessages
                                            | unnamed = unnamedData :: sortedSubMessages.unnamed
                                        }
                            )
                            { named = []
                            , unnamed = []
                            }

                argumentError problem =
                    Err
                        [ ArgumentError
                            { source = source
                            , from = from
                            , to = to
                            , placeholder = placeholder
                            , names = names
                            , namedSubMessages =
                                named
                                    |> List.map
                                        (\{ from, to, name } ->
                                            { name = name
                                            , from = from
                                            , to = to
                                            }
                                        )
                            , unnamedSubMessages =
                                unnamed
                                    |> List.map
                                        (\{ from, to } ->
                                            { from = from
                                            , to = to
                                            }
                                        )
                            , problem = problem
                            }
                        ]
            in
            case toArgType names of
                Just (Delimited _) ->
                    case subMessages of
                        (Icu.Unnamed { message }) :: [] ->
                            argsFromMessage toArgType source message

                        _ ->
                            argumentError ExpectingSingleUnnamedSubMessage

                Just (StaticList _) ->
                    case named of
                        [] ->
                            unnamed
                                |> foldrWithErrors
                                    (.message >> argsFromMessage toArgType source)
                                    Dict.union
                                    Dict.empty

                        _ ->
                            argumentError ExpectingOnlyUnnamedSubMessages

                Just String ->
                    Ok (Dict.singleton placeholder "String")

                Just Node ->
                    case subMessages of
                        (Icu.Unnamed { message }) :: [] ->
                            argsFromMessage toArgType source message
                                |> Result.map
                                    (Dict.union (Dict.singleton placeholder "List node -> node"))

                        _ ->
                            argumentError ExpectingSingleUnnamedSubMessage

                Just (List _) ->
                    Ok (Dict.singleton placeholder "List String")

                Just (Float _) ->
                    Ok (Dict.singleton placeholder "Float")

                Just (Date _) ->
                    Ok (Dict.singleton placeholder "Date")

                Just (Time _) ->
                    Ok (Dict.singleton placeholder "Time")

                Just (Cardinal _) ->
                    case unnamed of
                        [] ->
                            named
                                |> foldrWithErrors
                                    (.message >> argsFromMessage toArgType source)
                                    Dict.union
                                    Dict.empty

                        _ ->
                            argumentError <|
                                ExpectingOnlyNamedSubMessages
                                    [ "other"
                                    , "zero"
                                    , "one"
                                    , "two"
                                    , "few"
                                    , "many"
                                    ]

                Just (Ordinal _) ->
                    case unnamed of
                        [] ->
                            named
                                |> foldrWithErrors
                                    (.message >> argsFromMessage toArgType source)
                                    Dict.union
                                    Dict.empty

                        _ ->
                            argumentError <|
                                ExpectingOnlyNamedSubMessages
                                    [ "other"
                                    , "zero"
                                    , "one"
                                    , "two"
                                    , "few"
                                    , "many"
                                    ]

                Nothing ->
                    argumentError BadArgument

        _ ->
            Ok Dict.empty



---- CODE GENERATION HELPER


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



---- ERROR HELPER


foldrWithErrors :
    (a -> Result (List err) b)
    -> (b -> b -> b)
    -> b
    -> List a
    -> Result (List err) b
foldrWithErrors toResultB mergeB initialB listA =
    listA
        |> List.foldr
            (\a result ->
                case result of
                    Ok b ->
                        case toResultB a of
                            Ok nextB ->
                                Ok (mergeB b nextB)

                            Err errors ->
                                Err errors

                    Err errors ->
                        case toResultB a of
                            Ok _ ->
                                result

                            Err nextErrors ->
                                Err (errors ++ nextErrors)
            )
            (Ok initialB)


collectWithErrors :
    (a -> Result (List err) b)
    -> List a
    -> Result (List err) (List b)
collectWithErrors toResultB listA =
    listA
        |> List.foldr
            (\a result ->
                case result of
                    Ok listB ->
                        case toResultB a of
                            Ok nextB ->
                                Ok (nextB :: listB)

                            Err errors ->
                                Err errors

                    Err errors ->
                        case toResultB a of
                            Ok _ ->
                                result

                            Err nextErrors ->
                                Err (errors ++ nextErrors)
            )
            (Ok [])
