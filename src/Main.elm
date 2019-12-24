{-
   Copyright (c) 2019 Marek Fajkus

   Permission is hereby granted, free of charge, to any person obtaining
   a copy of this software and associated documentation files (the
   "Software"), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to
   permit persons to whom the Software is furnished to do so, subject to
   the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
   LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
   OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
   WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
-}


module Main exposing (main)

import Browser exposing (Document)
import Css exposing (Style)
import Css.Global as GCss
import Css.Reset
import GitHub exposing (Comment, Issue, IssueState(..), User)
import Html as PlainHtml
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attrs
import Html.Styled.Events as Events
import Markdown
import Prometheus exposing (Alert, Group, Rule, Status(..))
import RemoteData exposing (RemoteData(..), WebData)
import Set exposing (Set)
import Task
import Time exposing (Month(..), Posix, Zone)
import Time.Distance


baseUrl : String
baseUrl =
    "https://status.nixos.org"


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Model


type alias Model =
    { displayIssues : Set Int
    , time : Posix
    , timeZone : Zone
    , openIssues : WebData (List Issue)
    , closedIssues : WebData (List Issue)
    , alerts : WebData (List Group)
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( { displayIssues = Set.empty
      , time = Time.millisToPosix 0
      , timeZone = Time.utc
      , openIssues = Loading
      , closedIssues = Loading
      , alerts = Loading
      }
    , Cmd.batch
        [ GitHub.fetchIssues Open <| OpenIssuesLoaded << RemoteData.fromResult
        , GitHub.fetchIssues Closed <| ClosedIssuesLoaded << RemoteData.fromResult
        , Prometheus.fetchGroups <| AlertsLoaded << RemoteData.fromResult
        , Task.perform TimeTick Time.now
        , Task.perform SetTimeZone Time.here
        ]
    )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 1000 TimeTick



-- Update


type Msg
    = NoOp
    | AlertsLoaded (WebData (List Group))
    | OpenIssuesLoaded (WebData (List Issue))
    | ClosedIssuesLoaded (WebData (List Issue))
    | ToggleIssue Int
    | SetTimeZone Zone
    | TimeTick Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        OpenIssuesLoaded data ->
            ( { model | openIssues = data }
                |> (\m ->
                        RemoteData.map (Set.fromList << List.map .number) data
                            |> RemoteData.map (\set -> { m | displayIssues = set })
                            |> RemoteData.withDefault m
                   )
            , Cmd.none
            )

        ClosedIssuesLoaded data ->
            ( { model | closedIssues = data }, Cmd.none )

        AlertsLoaded data ->
            ( { model | alerts = data }, Cmd.none )

        ToggleIssue num ->
            ( { model
                | displayIssues =
                    if Set.member num model.displayIssues then
                        Set.remove num model.displayIssues

                    else
                        Set.insert num model.displayIssues
              }
            , Cmd.none
            )

        TimeTick time ->
            ( { model | time = time }, Cmd.none )

        SetTimeZone zone ->
            ( { model | timeZone = zone }, Cmd.none )



-- View


colors =
    { blue = Css.hex "4c6eb5"
    , purple = Css.hex "c966fc"
    , red = Css.hex "cc0000"
    , green = Css.hex "5db652"
    , orange = Css.hex "b5a92b"
    }


formatTime : Zone -> Posix -> String
formatTime zone time =
    let
        day =
            Time.toDay zone time

        month =
            case Time.toMonth zone time of
                Jan ->
                    "01"

                Feb ->
                    "02"

                Mar ->
                    "03"

                Apr ->
                    "04"

                May ->
                    "05"

                Jun ->
                    "06"

                Jul ->
                    "07"

                Aug ->
                    "08"

                Sep ->
                    "09"

                Oct ->
                    "10"

                Nov ->
                    "11"

                Dec ->
                    "12"

        year =
            Time.toYear zone time
    in
    String.concat
        [ String.padLeft 2 '0' <| String.fromInt day
        , "/"
        , month
        , "/"
        , String.fromInt <| year - 2000
        ]


{-| Configured toMarkdown function
-}
fromMarkdown : String -> Html msg
fromMarkdown =
    Html.fromUnstyled
        << Markdown.toHtmlWith
            { githubFlavored = Just { tables = True, breaks = True }
            , defaultHighlighting = Just "nix"
            , sanitize = False
            , smartypants = True
            }
            []


centerContainer : Style
centerContainer =
    Css.batch
        [ Css.maxWidth <| Css.px 1200
        , Css.margin2 Css.zero Css.auto
        , Css.paddingLeft <| Css.px 15
        , Css.paddingRight <| Css.px 15
        ]


fancyShadow : Style
fancyShadow =
    Css.boxShadow3 (Css.px 5) (Css.px 5) <| Css.rgba 0 0 0 0.3


userLink : User -> Html msg
userLink user =
    Html.a [ Attrs.href user.url ] [ Html.text <| "@" ++ user.login ]


viewHeader : Html msg
viewHeader =
    let
        link : { url : String, text : String } -> Html msg
        link { url, text } =
            Html.styled Html.a
                [ Css.display Css.inlineBlock
                , Css.marginLeft <| Css.px 12
                , Css.color <| Css.hex "7ebae4"
                , Css.textDecoration Css.none
                , Css.boxSizing Css.contentBox
                , Css.paddingTop <| Css.px 12
                , Css.paddingBottom <| Css.px 2
                , Css.borderBottom3 (Css.px 2) Css.solid <| Css.transparent
                , Css.hover [ Css.borderColor <| Css.hex "7ebae4" ]
                ]
                [ Attrs.href <| baseUrl ++ url ]
                [ Html.text text ]
    in
    Html.styled Html.header
        [ Css.backgroundColor <| Css.hex "1a1a1a"
        , Css.padding2 (Css.px 4) Css.zero
        , Css.color <| Css.hex "ffffff"
        ]
        []
        [ Html.styled Html.div
            [ centerContainer
            , Css.displayFlex
            , Css.flexDirection Css.row
            , Css.justifyContent Css.spaceBetween
            , Css.alignItems Css.center
            ]
            []
            [ Html.h1 []
                [ Html.styled Html.img
                    [ Css.width <| Css.px 150 ]
                    [ Attrs.src "/assets/nixos-logo.svg"
                    ]
                    []
                , Html.styled Html.span
                    [ Css.textIndent <| Css.px -999
                    , Css.display Css.inlineBlock
                    ]
                    []
                    [ Html.text "NixOS" ]
                , Html.styled Html.span
                    [ Css.top <| Css.px -10
                    , Css.display Css.inlineBlock
                    , Css.position Css.relative
                    , Css.fontSize <| Css.px 20
                    , Css.color <| Css.hex "cccccc"
                    ]
                    []
                    [ Html.text "status" ]
                ]
            , Html.nav []
                [ link { url = "/grafana", text = "Grafana" }
                , link { url = "/prometheus", text = "Prometheus" }
                ]
            ]
        ]


viewAlert : Alert -> Html msg
viewAlert alert =
    Html.li []
        [ Html.styled Html.a
            [ Css.color colors.red ]
            [ Attrs.href alert.url ]
            [ Html.text alert.alertName ]
        , Html.text ": "
        , Html.text alert.severity
        , Html.text " "
        , Html.styled Html.span
            [ Css.backgroundColor colors.blue
            , Css.color <| Css.hex "ffffff"
            , Css.padding2 Css.zero <| Css.px 4
            , Css.borderRadius <| Css.px 2
            ]
            []
            [ Html.text alert.state ]
        ]


viewRule : Rule -> Html msg
viewRule rule =
    Html.li []
        [ Html.styled Html.a
            [ Css.color colors.blue
            , Css.textDecoration Css.none
            , Css.hover [ Css.textDecoration Css.underline ]
            ]
            [ Attrs.href rule.url ]
            [ Html.text rule.name ]
        , Html.text ": "
        , Html.text rule.health
        , if List.isEmpty rule.alerts then
            Html.text ""

          else
            Html.styled Html.ul
                [ Css.fontSize <| Css.px 13
                , Css.paddingLeft <| Css.px 15
                ]
                []
            <|
                List.map viewAlert
                    [ { url = "foo"
                      , alertName = "High request latency"
                      , severity = "page"
                      , state = "firing"
                      }
                    , { url = "foo"
                      , alertName = "High request latency"
                      , severity = "page"
                      , state = "firing"
                      }
                    ]
        ]


viewGroup : Group -> Html msg
viewGroup group =
    Html.styled Html.div
        [ Css.width <| Css.pct 33.33
        , Css.lineHeight <| Css.em 1.5
        , Css.padding2 (Css.px 20) <| Css.px 6
        ]
        []
        [ Html.styled Html.h3
            [ Css.textTransform Css.capitalize
            , Css.fontSize <| Css.px 22
            , Css.paddingBottom <| Css.px 12
            , Css.fontWeight Css.bold
            , Css.textAlign Css.center
            ]
            []
            [ Html.text group.name ]
        , Html.styled Html.ul
            [ Css.paddingLeft <| Css.px 50, Css.listStyle Css.disc ]
            []
          <|
            List.map viewRule group.rules
        ]


viewComment : Posix -> Comment -> Html msg
viewComment currentTime comment =
    Html.styled Html.div
        [ Css.margin <| Css.px 15
        , Css.border3 (Css.px 1) Css.solid <| Css.hex "dddddd"
        , Css.padding4 (Css.px 10) (Css.px 15) (Css.px 15) <| Css.px 15
        ]
        []
        [ Html.styled Html.header
            [ Css.displayFlex
            , Css.alignItems <| Css.center
            , Css.marginBottom <| Css.px 10
            ]
            []
            [ Html.styled Html.img
                [ Css.display Css.block
                , Css.borderRadius <| Css.pct 100
                , Css.width <| Css.px 30
                , Css.padding2 (Css.px 6) Css.zero
                ]
                [ Attrs.src comment.author.avatarUrl ]
                []
            , Html.styled Html.div
                [ Css.marginLeft <| Css.px 5 ]
                []
                [ userLink comment.author
                , Html.styled Html.small
                    [ Css.fontSize <| Css.px 13
                    , Css.marginLeft <| Css.px 5
                    ]
                    []
                    [ Html.text <| Time.Distance.inWords comment.createAt currentTime ]
                ]
            ]
        , fromMarkdown comment.text
        ]


viewIssue : Zone -> Posix -> (Issue -> Bool) -> Issue -> Html Msg
viewIssue timeZone time isOpen_ issue =
    let
        isOpen =
            isOpen_ issue
    in
    Html.styled Html.article
        [ Css.margin4 (Css.px 30) (Css.px 50) (Css.px 30) (Css.px 75)
        ]
        []
        [ Html.styled Html.header
            [ Css.position Css.relative
            , Css.displayFlex
            , Css.justifyContent Css.spaceBetween
            ]
            []
            [ Html.div
                []
                [ Html.styled Html.div
                    [ Css.position Css.absolute
                    , Css.top <| Css.px 2
                    , Css.left <| Css.px -62
                    , Css.fontSize <| Css.px 13
                    ]
                    []
                    [ Html.text <| formatTime timeZone issue.createdAt ]
                , Html.styled Html.h4
                    [ Css.display Css.inline
                    , Css.fontWeight Css.bold
                    ]
                    []
                    [ Html.a [ Attrs.href issue.url ]
                        [ Html.text <| "#" ++ String.fromInt issue.number ]
                    , Html.text " "
                    , Html.styled Html.a
                        [ Css.color <| Css.hex "000000"
                        , Css.cursor Css.pointer
                        , Css.hover [ Css.textDecoration Css.underline ]
                        ]
                        [ Events.onClick <| ToggleIssue issue.number ]
                        [ Html.text issue.title ]
                    ]
                , Html.styled Html.div
                    [ Css.display <| Css.inline
                    , Css.marginLeft <| Css.px 6
                    ]
                    []
                    [ Html.text "reported by "
                    , userLink issue.author
                    ]
                ]
            , Html.styled Html.div
                [ Css.width <| Css.px 150
                , Css.textAlign Css.right
                , Css.fontSize <| Css.px 13
                ]
                []
                [ Html.text "comments: "
                , Html.text <| String.fromInt issue.commentsCount
                ]
            ]
        , if isOpen then
            Html.styled Html.div
                [ Css.paddingLeft <| Css.px 15
                , Css.borderLeft3 (Css.px 2) Css.solid <| Css.hex "546fb5"
                , Css.margin4 (Css.px 15) Css.zero Css.zero (Css.px 15)
                ]
                [ Attrs.class "gh-content" ]
                [ Html.styled Html.div
                    [ Css.backgroundColor <| Css.hex "f4f4f4"
                    , Css.padding <| Css.px 15
                    , fancyShadow
                    ]
                    []
                    [ fromMarkdown issue.text ]
                , if issue.commentsCount > 0 then
                    Html.styled Html.h5
                        [ Css.margin2 (Css.px 20) Css.zero, Css.fontWeight Css.bold ]
                        []
                        [ Html.text "Recent Updates:" ]

                  else
                    Html.text ""
                , if issue.commentsCount > 3 then
                    Html.a [ Attrs.href issue.url ]
                        [ Html.text "...see older comments" ]

                  else
                    Html.text ""
                , Html.div [] <| List.map (viewComment time) issue.comments
                ]

          else
            Html.text ""
        ]


viewStatus : WebData (List Group) -> Html msg
viewStatus alerts =
    let
        separator =
            Html.styled Html.div
                [ Css.width <| Css.px 1
                , Css.backgroundColor <| Css.hex "9b9b9b"
                ]
                []
                []

        status =
            RemoteData.map Prometheus.getStatus alerts

        alert xs txt =
            let
                names =
                    List.intersperse ", " xs
                        |> String.concat
            in
            String.concat
                [ names
                , " alerts are "
                , txt
                ]

        color =
            case status of
                Success Operational ->
                    colors.green

                Success (Pending _) ->
                    colors.orange

                Success (Firing _) ->
                    colors.red

                _ ->
                    colors.blue
    in
    Html.section []
        [ Html.styled Html.h3
            [ Css.fontWeight Css.bold
            , Css.fontSize <| Css.px 35
            , Css.textAlign Css.center
            , Css.padding2 (Css.px 50) Css.zero
            , Css.textTransform Css.uppercase
            , Css.backgroundColor color
            , Css.color <| Css.hex "ffffff"
            , Css.marginBottom <| Css.px 20
            ]
            []
            [ Html.text <|
                case status of
                    Success Operational ->
                        "All Systems Operational"

                    Success (Pending xs) ->
                        alert xs "pending."

                    Success (Firing xs) ->
                        alert xs "FIRING!"

                    _ ->
                        "Performing checks..."
            ]
        , Html.styled Html.div
            [ centerContainer
            , Css.displayFlex
            , Css.alignItems Css.stretch
            ]
            []
          <|
            case alerts of
                Success groups ->
                    List.intersperse separator <| List.map viewGroup groups

                Failure err ->
                    [ Html.text "err" ]

                _ ->
                    [ Html.text "Loading" ]
        ]


view : Model -> Document Msg
view model =
    let
        headline =
            Css.batch
                [ Css.fontSize <| Css.px 25
                , Css.marginBottom <| Css.px 30
                ]

        isOpen i =
            Set.member i.number model.displayIssues

        openIssueBtn =
            Html.a [ Attrs.href "https://github.com/NixOS/nixpkgs/issues/new?assignees=&labels=0.kind%3A+bug&template=bug_report.md&title=" ]
                [ Html.text "open new issue" ]
    in
    { title = "NixOS Status"
    , body =
        List.map Html.toUnstyled <|
            [ viewHeader
            , viewStatus model.alerts
            , Html.styled Html.section
                [ centerContainer
                , Css.lineHeight <| Css.em 1.3
                , Css.marginTop <| Css.px 30
                ]
                []
                [ Html.div []
                    [ Html.styled Html.div [ Css.marginBottom <| Css.px 30 ] [] <|
                        case model.openIssues of
                            Success [] ->
                                [ Html.text "There are no open issues at the moment. "
                                , openIssueBtn
                                ]

                            Success issues ->
                                [ Html.styled Html.h3
                                    [ headline ]
                                    []
                                    [ Html.text "Open Issues:" ]
                                , Html.div [] <| List.map (viewIssue model.timeZone model.time isOpen) issues
                                , openIssueBtn
                                ]

                            Failure err ->
                                [ Html.text "err" ]

                            _ ->
                                [ Html.text "Loading..." ]
                    ]
                , Html.styled Html.h3 [ headline ] [] [ Html.text "Past Issues:" ]
                , Html.div [] <|
                    case model.closedIssues of
                        Success issues ->
                            List.map (viewIssue model.timeZone model.time isOpen) issues

                        Failure err ->
                            [ Html.text "err" ]

                        _ ->
                            [ Html.text "Loading..." ]
                , Html.node "link"
                    [ Attrs.href "https://fonts.googleapis.com/css?family=Open+Sans:400,700&display=swap"
                    , Attrs.rel "stylesheet"
                    ]
                    []
                , Css.Reset.meyerV2
                , Css.Reset.borderBoxV201408
                , GCss.global
                    [ GCss.body
                        [ Css.fontFamilies [ "Open Sans", "sans-serif" ]
                        ]
                    , GCss.a [ Css.color colors.blue ]
                    , GCss.class "gh-content"
                        [ GCss.descendants
                            [ GCss.pre
                                [ Css.backgroundColor <| Css.hex "1a1a1a"
                                , Css.color <| Css.hex "ffffff"
                                , Css.padding <| Css.px 12
                                , Css.display Css.block
                                , Css.fontFamily Css.monospace
                                , Css.overflowX Css.auto
                                , Css.margin2 (Css.px 10) Css.zero
                                , fancyShadow
                                ]
                            , GCss.code
                                []
                            , GCss.strong [ Css.fontWeight Css.bold ]
                            , GCss.em [ Css.fontStyle Css.italic ]
                            , GCss.i [ Css.fontStyle Css.italic ]
                            , GCss.blockquote
                                [ Css.marginLeft <| Css.px 15
                                , Css.paddingLeft <| Css.px 15
                                , Css.borderLeft3 (Css.px 2) Css.solid <| Css.hex "dddddd"
                                , Css.backgroundColor <| Css.hex "f4f4f4"
                                , Css.fontStyle Css.italic
                                ]
                            , GCss.p [ Css.padding2 (Css.px 5) Css.zero ]
                            ]
                        ]
                    ]
                ]
            ]
    }
