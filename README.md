# Translation

This library helps you to translate and localize your application. It is
meant to be used with `kirchner/elm-cldr`, which exposes all the
localization data found in the [Unicode Common Locale Data
Repository](http://cldr.unicode.org), and with
`kirchner/elm-translation-runner`, a command line tool for converting
JSON files containing translations in the [ICU Message
Format](http://icu-project.org/apiref/icu4j/com/ibm/icu/text/MessageFormat.html)
to Elm packages which expose these translations as data.

## Examples

A very simple translation looks like this:

```elm
module Translations.En exposing (..)

import Translation exposing (Translation, final, s)

greeting : Translation args msg
greeting =
    final "greeting" <|
        s "Good morning!"
```

and to turn it into a `String` you do

```elm
import Translation exposing (asString)
import Translations.En exposing (..)

view =
    greeting
        |> asString
        |> Html.text
```

You can also mix in placeholders into your translations:

```elm
module Translations.En exposing (..)

import Translation exposing (Translation, final, s, string, concat)

question : Translation { args | name : String, email : String } msg
question =
    final "question" <|
        concat
            [ s "Hello, "
            , string .name "name"
            , s "! Good to have you back. One question: is "
            , string .email "email"
            , s " still your email address?"
            ]
```

You then have to provide values for these placeholders when using this
translation:

```elm
import Translation exposing (asStringWith)
import Translations.En exposing (..)

view name email =
    question
        |> asStringWith
            { name = name
            , email = email
            }
        |> Html.text
```

Sometimes part of a translation have to be wrapped in another Html node,
for example when the translation contains
a [link](http://www.sometimesredsometimesblue.com/) or parts of it
should be **bold**. Instead of splitting the translation into parts or
writing plain Html code in it, you can do the following:

```elm
import Html exposing (Html, a, div, strong)
import Html.Attributes exposing (href)
import Translation exposing (Translation, final, s, concat, node, asNodes)

info :
    Translation
        { args
            | docsLink : List (Node msg) -> Node msg
            , contactLink : List (Node msg) -> Node msg
            , strong : List (Node msg) -> Node msg
        }
        msg
info =
    final "info" <|
        concat
            [ s "You will find useful information in our "
            , node .docsLink "docsLink" <|
                s "documentation"
            , s ". But if you have further questions, feel free to "
            , node .contactLink "contactLink" <|
                "contact us"
            , s " at "
            , node .strong "strong" <|
                s "any time"
            , s "!"
            ]


view : Html msg
view =
    info
        |> asNodes
            { docsLink = a [ href ".." ]
            , contactLink = a [ href " .." ]
            , strong = strong []
            }
        |> div []
```

The `view` function is then equivalent to:

```elm
view : Html msg
view =
    div []
        [ text "You will find useful information in our "
        , a [ href ".." ] [ text "documentation" ]
        , text ". But if you have further questions, feel free to "
        , a [ href ".." ] [ text "contact us" ]
        , text " at "
        , strong [] [ text "any time" ]
        , text "!"
        ]
```
