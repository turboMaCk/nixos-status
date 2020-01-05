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


module Prometheus exposing
    ( Alert
    , Group
    , Rule
    , Status(..)
    , getStatus
    , fetchGroups
    )

import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Extra as Decode
import Url.Builder as Url



-- Config


url : String
url =
    "https://status.nixos.org/prometheus"



-- Types


type alias Alert =
    { url : String
    , name : String
    , alertName : String
    , severity : String
    , state : String -- TODO: what values can be there?

    -- can be "pending"
    }


alertDecoder : Decoder Alert
alertDecoder =
    Decode.succeed Alert
        |> Decode.andMap (Decode.at [ "annotations", "summary" ] Decode.string)
        |> Decode.andMap (Decode.at [ "labels", "name" ] Decode.string)
        |> Decode.andMap (Decode.at [ "labels", "alertname" ] Decode.string)
        |> Decode.andMap (Decode.at [ "labels", "severity" ] Decode.string)
        |> Decode.andMap (Decode.field "state" Decode.string)


type alias Rule =
    { url : String
    , health : String -- TODO: what values can be there?
    , name : String
    , alerts : List Alert
    }


ruleDecoder : Decoder Rule
ruleDecoder =
    Decode.succeed Rule
        |> Decode.andMap (Decode.at [ "annotations", "summary" ] Decode.string)
        |> Decode.andMap (Decode.field "health" Decode.string)
        |> Decode.andMap (Decode.field "name" Decode.string)
        |> Decode.andMap (Decode.field "alerts" <| Decode.list alertDecoder)


type alias Group =
    { file : String
    , name : String
    , rules : List Rule
    }


groupDecoder : Decoder Group
groupDecoder =
    Decode.succeed Group
        |> Decode.andMap (Decode.field "file" Decode.string)
        |> Decode.andMap (Decode.field "name" Decode.string)
        |> Decode.andMap (Decode.field "rules" <| Decode.list ruleDecoder)


fetchGroups : (Result Http.Error (List Group) -> msg) -> Cmd msg
fetchGroups msg =
    Http.get
        { url = Url.crossOrigin url [ "api", "v1", "rules" ] []
        , expect =
            Http.expectJson msg <|
                Decode.at [ "data", "groups" ] <|
                    Decode.list groupDecoder
        }


type Status
    = Operational
    | Pending (List String)
    | Firing (List String)


getGroupStatus : Group -> Status
getGroupStatus group =
    let
        states =
            List.concatMap .alerts group.rules
                |> List.map .state
    in
    if List.isEmpty states then
        Operational

    else if List.all ((==) "pending") states then
        Pending [ group.name ]

    else
        Firing [ group.name ]


combineStatus : Status -> Status -> Status
combineStatus s1 s2 =
    case ( s1, s2 ) of
        ( Operational, Operational ) ->
            Operational

        ( Firing xs, Firing ys ) ->
            Firing <| xs ++ ys

        ( Firing xs, _ ) ->
            Firing xs

        ( _, Firing xs ) ->
            Firing xs

        ( Pending xs, Pending ys ) ->
            Pending <| xs ++ ys

        ( Pending xs, _ ) ->
            Pending xs

        ( _, Pending xs ) ->
            Pending xs


getStatus : List Group -> Status
getStatus groups =
    let
        f group acc =
            getGroupStatus group
                |> combineStatus acc
    in
    List.foldr f Operational groups
