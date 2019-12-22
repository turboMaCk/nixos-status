module Main exposing (main)

import Browser exposing (Document)
import Html exposing (Html)
import GitHub


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
    {}


init : () -> ( Model, Cmd Msg )
init () =
    ( {}, GitHub.fetchIssues NoOp )



-- Update


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
    ( model, Cmd.none )



-- View


view : Model -> Document Msg
view {} =
    { title = "NixOS Status"
    , body = [ Html.text "hello" ]
    }
