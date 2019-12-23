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
import Prometheus exposing (Alert, Group, Rule)
import RemoteData exposing (RemoteData(..), WebData)
import Set exposing (Set)


baseUrl : String
baseUrl =
    "https://status.nixos.org"


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- Model


type alias Model =
    { displayIssues : Set Int
    , openIssues : WebData (List Issue)
    , closedIssues : WebData (List Issue)
    , alerts : WebData (List Group)
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( { displayIssues = Set.empty
      , openIssues = Loading
      , closedIssues = Loading
      , alerts = Loading
      }
    , Cmd.batch
        [ GitHub.fetchIssues Open <| OpenIssuesLoaded << RemoteData.fromResult
        , GitHub.fetchIssues Closed <| ClosedIssuesLoaded << RemoteData.fromResult
        , Prometheus.fetchGroups <| AlertsLoaded << RemoteData.fromResult
        ]
    )



-- Update


type Msg
    = NoOp
    | AlertsLoaded (WebData (List Group))
    | OpenIssuesLoaded (WebData (List Issue))
    | ClosedIssuesLoaded (WebData (List Issue))
    | ToggleIssue Int


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
            let
                _ =
                    Debug.log "res" data
            in
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



-- View


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
        ]


userLink : User -> Html msg
userLink user =
    Html.a [ Attrs.href user.url ] [ Html.text <| "@" ++ user.login ]


viewHeader : Html msg
viewHeader =
    let
        link : { url : String, text : String, color : String } -> Html msg
        link { url, text, color } =
            Html.styled Html.a
                [ Css.display Css.inlineBlock
                , Css.marginLeft <| Css.px 12
                , Css.color <| Css.hex color
                , Css.textDecoration Css.none
                , Css.boxSizing Css.contentBox
                , Css.paddingTop <| Css.px 12
                , Css.paddingBottom <| Css.px 2
                , Css.borderBottom3 (Css.px 2) Css.solid <| Css.transparent
                , Css.hover [ Css.borderColor <| Css.hex color ]
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
                [ link { url = "/grafana", text = "Grafana", color = "c966fc" }
                , link { url = "/prometheus", text = "Prometheus", color = "7ebae4" }
                ]
            ]
        ]


viewRule : Rule -> Html msg
viewRule rule =
    Html.li []
        [ Html.styled Html.a
            [ Css.color <| Css.hex "4c6eb5"
            , Css.textDecoration Css.none
            ]
            [ Attrs.href rule.url ]
            [ Html.text rule.name ]
        , Html.text ": "
        , Html.text rule.health
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
        , Html.styled Html.ul [ Css.paddingLeft <| Css.px 50, Css.listStyle Css.disc ] [] <|
            List.map viewRule group.rules
        ]


viewComment : Comment -> Html msg
viewComment comment =
    Html.div []
        [ Html.img
            [ Attrs.src comment.author.avatarUrl ]
            []
        , userLink comment.author
        , fromMarkdown comment.text
        ]


viewIssue : (Issue -> Bool) -> Issue -> Html Msg
viewIssue isOpen_ issue =
    let
        isOpen =
            isOpen_ issue
    in
    Html.styled Html.article
        [ Css.margin2 (Css.px 30) <| Css.px 50
        ]
        []
        [ Html.styled Html.header
            [ Css.displayFlex ]
            []
            [ Html.styled Html.h4
                [ Css.fontWeight Css.bold ]
                []
                [ Html.a [ Attrs.href issue.url ] [ Html.text <| "#" ++ String.fromInt issue.number ]
                , Html.text " "
                , Html.a [ Events.onClick <| ToggleIssue issue.number ]
                    [ Html.text issue.title ]
                ]
            , Html.styled Html.div
                [ Css.marginLeft <| Css.px 6 ]
                []
                [ Html.text "by "
                , userLink issue.author
                ]
            ]
        , if isOpen then
            Html.styled Html.div
                [ Css.paddingLeft <| Css.px 15
                , Css.borderLeft3 (Css.px 2) Css.solid <| Css.hex "546fb5"
                , Css.margin4 (Css.px 15) Css.zero Css.zero (Css.px 15)
                ]
                []
            <|
                fromMarkdown issue.text
                    :: List.map viewComment issue.comments

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
    in
    Html.section []
        [ Html.styled Html.h3
            [ Css.fontWeight Css.bold
            , Css.fontSize <| Css.px 35
            , Css.textAlign Css.center
            , Css.padding2 (Css.px 50) Css.zero
            , Css.textTransform Css.uppercase
            , Css.backgroundColor <| Css.hex "5db652"
            , Css.color <| Css.hex "ffffff"
            , Css.marginBottom <| Css.px 20
            ]
            []
            [ Html.text "All Systems Operational" ]
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
                [ Css.fontSize <| Css.px 30
                , Css.marginBottom <| Css.px 30
                ]

        isOpen i =
            Set.member i.number model.displayIssues
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
                [ Html.styled Html.h3
                    [ headline ]
                    []
                    [ Html.text "Open Issues:" ]
                , Html.div [] <|
                    case model.openIssues of
                        Success issues ->
                            List.map (viewIssue isOpen) issues

                        Failure err ->
                            [ Html.text "err" ]

                        _ ->
                            [ Html.text "xxx" ]
                , Html.styled Html.h3 [ headline ] [] [ Html.text "Past Issues:" ]
                , Html.div [] <|
                    case model.closedIssues of
                        Success issues ->
                            List.map (viewIssue isOpen) issues

                        Failure err ->
                            [ Html.text "err" ]

                        _ ->
                            [ Html.text "xxx" ]
                , Html.node "link" [ Attrs.href "https://fonts.googleapis.com/css?family=Open+Sans:400,700&display=swap", Attrs.rel "stylesheet" ] []
                , Css.Reset.meyerV2
                , Css.Reset.borderBoxV201408
                , GCss.global
                    [ GCss.body
                        [ Css.fontFamilies [ "Open Sans", "sans-serif" ]
                        ]
                    , GCss.a [ Css.color <| Css.hex "4c6eb5" ]
                    , GCss.pre
                        [ Css.backgroundColor <| Css.hex "f6f8fa"
                        , Css.padding <| Css.px 12
                        , Css.display Css.block
                        , Css.fontFamily Css.monospace
                        , Css.overflowX Css.auto
                        , Css.margin2 (Css.px 10) Css.zero
                        ]
                    , GCss.code
                        []
                    , GCss.strong [ Css.fontWeight Css.bold ]
                    , GCss.em [ Css.fontStyle Css.italic ]
                    , GCss.i [ Css.fontStyle Css.italic ]
                    ]
                ]
            ]
    }
