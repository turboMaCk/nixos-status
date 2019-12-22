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
import GitHub exposing (Issue, IssueState(..))
import Html exposing (Html)
import Html.Attributes as Attrs
import Markdown
import Prometheus exposing (Alert, Group, Rule)
import RemoteData exposing (RemoteData(..), WebData)


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
    { openIssues : WebData (List Issue)
    , closedIssues : WebData (List Issue)
    , alerts : WebData (List Group)
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( { openIssues = Loading
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        OpenIssuesLoaded data ->
            ( { model | openIssues = data }, Cmd.none )

        ClosedIssuesLoaded data ->
            ( { model | closedIssues = data }, Cmd.none )

        AlertsLoaded data ->
            let
                _ =
                    Debug.log "res" data
            in
            ( { model | alerts = data }, Cmd.none )



-- View


{-| Configured toMarkdown function
-}
fromMarkdown : String -> Html msg
fromMarkdown =
    Markdown.toHtmlWith
        { githubFlavored = Just { tables = True, breaks = True }
        , defaultHighlighting = Just "nix"
        , sanitize = False
        , smartypants = True
        }
        []


viewGroup : Group -> Html msg
viewGroup group =
    Html.div []
        [ Html.h3 [] [ Html.text group.name ] ]


viewIssue : Bool -> Issue -> Html Msg
viewIssue isOpen issue =
    Html.div []
        [ Html.a [ Attrs.href issue.url ] [ Html.text <| "#" ++ String.fromInt issue.number ]
        , Html.text ": "
        , Html.text issue.title
        , Html.br [] []
        , if isOpen then
            fromMarkdown issue.text

          else
            Html.text ""
        ]


view : Model -> Document Msg
view model =
    { title = "NixOS Status"
    , body =
        [ Html.h2 [] [ Html.text "Status" ]
        , Html.div [] <|
            case model.alerts of
                Success groups ->
                    List.map viewGroup groups

                Failure err ->
                    [ Html.text "err" ]

                _ ->
                    [ Html.text "Loading" ]
        , Html.h2 [] [ Html.text "Open Issues" ]
        , Html.div [] <|
            case model.openIssues of
                Success issues ->
                    List.map (viewIssue True) issues

                Failure err ->
                    [ Html.text "err" ]

                _ ->
                    [ Html.text "xxx" ]
        , Html.h2 [] [ Html.text "Past Issues" ]
        , Html.div [] <|
            case model.closedIssues of
                Success issues ->
                    List.map (viewIssue False) issues

                Failure err ->
                    [ Html.text "err" ]

                _ ->
                    [ Html.text "xxx" ]
        ]
    }
