# Translation

This library helps you to translate and localize your application. It is meant
to be used with [`kirchner/elm-cldr`](https://github.com/kirchner/elm-cldr),
which exposes all the localization data found in the [Unicode Common Locale
Data Repository](http://cldr.unicode.org), and with
[`kirchner/elm-translation-runner`](https://github.com/kirchner/elm-translation-runner),
a command line tool for converting JSON files containing translations in the
[ICU Message
Format](http://icu-project.org/apiref/icu4j/com/ibm/icu/text/MessageFormat.html)
to Elm packages which expose these translations as data.


## Examples

A very simple translation looks like this:

```elm
module Translations.En exposing (..)

import Translation exposing (Translation, final, s)

greeting : Translation args node
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

question : Translation { args | name : String, email : String } node
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
import Html exposing (Html, a, div, strong, text)
import Html.Attributes exposing (href)
import Translation exposing (Translation, final, s, concat, node, asNodes)

info :
    Translation
        { args
            | docsLink : List node -> node
            , contactLink : List node -> node
            , strong : List node -> node
        }
        node
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
        |> asNodes text
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



### Numbers, Dates and Times

Apart from simple `string` placeholders, you can use placeholders for values of
type `Float`, `Dates` and `Time`. For each of these placeholders, you either
have to provide a `Printer a`, which encodes how the actual value is turned
into a `String`:

```elm
module Translations.En exposing (..)

import Translation exposing (Translation, Printer, printer, final, s, float, concat)

mailboxInfo : Translation { args | count : Float } node
mailboxInfo =
    final "mailboxInfo" <|
        concat
            [ s "Number of new mails: "
            , float intPrinter .count "count"
            , s "."
            ]

intPrinter : Printer Float args node
intPrinter =
    printer [ "int" ] <|
        \float ->
            float
                |> floor
                |> toString
                |> s
```

Alternatively, you can use the `Printer`'s from
[`kirchner/elm-cldr`](https://github.com/kirchner/elm-cldr), which are
generated from the specifications in the [CLDR](http://cldr.unicode.org) and
cover a wide range of default formattings. A German translation of the previous
example using `elm-cldr` would look like this:

```elm
module Translations.De exposing (..)

import Cldr.De exposing (decimalStandard)
import Translation exposing (Translation, final, s, float, concat)

mailboxInfo : Translation { args | count : Float } node
mailboxInfo =
    final "mailboxInfo" <|
        concat
            [ s "Anzahl neuer Emails: "
            , float decimalStandard .count "count"
            , s "."
            ]
```

**Note:** Right now, `kirchner/elm-cldr` does not expose `Printer`'s for
`Date`'s and `Time`'s, yet.


### Pluralization

In a lot of languages translations change depending on a numerical value, for
example `"1 second"`, `"2 seconds"`, `"1.0 seconds"`. The package
[`kirchner/elm-cldr`](https://github.com/kirchner/elm-cldr) exposes helpers for
these situations. They let you do the following:

```elm
module Translations.De exposing (..)

import Cldr.De exposing (cardinal, decimalStandard)
import Translation exposing (Translation, s, concat)

newMailsInfo : Translation { args | count : Float } node
newMailsInfo =
    final "newMailsInfo" <|
        cardinal decimalStandard .count "count" <|
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
```

Then calling `newMailsInfo |> asStringWith { count = 1 }` is equal to `"Du hast
1 neue Email."`, whereas `newMailsInfo |> asStringWith { count = 42 }` produces
`"Du hast 42 neue Emails."`. There are helpers for cardinal and ordinal
pluralization rules of all locales defined in the
[CLDR](http://cldr.unicode.org).


### Quotations and Lists

You can add quotation marks to your translations:

```elm
module Translations.De exposing (..)

import Translation exposing (Translation, final, s, delimited, concat)

assumption : Translation args node
assumption =
    final "assumption" <|
        concat
            [ s "Ihr fragt euch wahrscheinlich: "
            , delimited quotePrinter <|
                s "Was soll das alles?"
            ]

quotePrinter : Printer (Text args node) args node
quotePrinter =
    printer [ "quotes", "outer" ] <|
        \text ->
            concat
                [ s "„"
                , text
                , s "“"
                ]
```

which will be printed as `"Ihr fragt euch wahrscheinlich: „Was soll das
alles?“"`. Also verbalizing lists is possible:

```elm
module Translations.En exposing (..)

import Translation exposing (Translation, final, s, list, concat)

membersInfo : Translation args node
membersInfo =
    final "membersInfo" <|
        concat
            [ s "These are all our members: "
            , list andPrinter
                [ s "Alice"
                , s "Bob"
                , s "Cindy"
                ]
            ]

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
```

The printed result will be `"These are all our members: Alice, Bob and Cindy"`.

**Note:** `kirchner/elm-cldr` will eventually also expose printers for
delimiters and lists.


## Generating Translations

Usually the actual translations for your application will not be created by
programmers but by translators. One therefore needs a way to generate
`Translation`'s of the above forms from --- for example --- strings in the [ICU
Message
Format](http://icu-project.org/apiref/icu4j/com/ibm/icu/text/MessageFormat.html).
This can be done using the command line tool `elm-translation` from
`kirchner/elm-translation-runner`. It can also generate translation helpers
making it possible to switch between different languages in your application at
runtime.


## Dynamically Replacing Translations

If you have to be able to replace the translations in your application without
a full deploy, we got you covered, as well! Suppose your translations module
looks like this:

```elm
module Translations.En exposing (..)

import Translation exposing (Translation, final, concat, s, string)

greeting : Translation args node
greeting =
    final "greeting" <|
        s "Good morning!"

question : Translation { args | name : String, email : String } node
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

You then can dynamically replace these translations like so:

```elm
import Html
import Translation exposing (translateTo, asString, asStringWith)
import Translations.En exposing (..)

view name email =
    Html.div []
        [ greeting
            |> translateTo dynamicLocale
            |> asString
            |> Html.text
        , question
            |> translateTo dynamicLocale
            |> asStringWith
                { name = name
                , email = email
                }
            |> Html.text
        ]

dynamicLocale : Locale args node
dynamicLocale =
    Translation.locale
        |> addTranslations
            [ ( "greeting", "Guten morgen!" )
            , ( "question"
              , "Hallo, {name}! Schön, dass Du zurück bist."
                  ++ " Eine Frage: ist {email} immer noch deine aktuelle Email-Adresse?"
              )
            ]
```

In a real application you then probably want to load the list of translations
from a server and place `dynamicLocale` inside your model. There are helpers to
add pluralization rules and printers to a `Locale`.
