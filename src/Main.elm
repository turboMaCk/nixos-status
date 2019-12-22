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
import GitHub exposing (Issue)
import Html exposing (Html)
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
    { issues : WebData (List Issue) }


init : () -> ( Model, Cmd Msg )
init () =
    ( { issues = Loading }
    , GitHub.fetchIssues <| IssuesLoaded << RemoteData.fromResult
    )



-- Update


type Msg
    = NoOp
    | IssuesLoaded (WebData (List Issue))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        IssuesLoaded data ->
            ( { model | issues = data }, Cmd.none )



-- View


view : Model -> Document Msg
view model =
    { title = "NixOS Status"
    , body =
        case model.issues of
            Success issues ->
                List.map (\{ number, title } -> Html.div [] [ Html.text <| String.fromInt number, Html.text ":", Html.text title, Html.br [] [] ]) issues

            Failure err ->
                let
                    _ =
                        Debug.log "err" err
                in
                [ Html.text "err" ]

            _ ->
                [ Html.text "xxx" ]
    }
