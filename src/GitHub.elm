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


module GitHub exposing
    ( Comment
    , Issue
    , IssueState(..)
    , User
    , fetchIssues
    )

{-
   GitHub Data Module

   We're using GitHub's GraphQL API but since we're
   doing only simple queries we not using real GraphQL library
   and instead just simply define string query.

   Nothe that GraphQL API requires authorization and thus
   we need to hardcode unpriviliged token.

-}

import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Extra as Decode
import Json.Encode as Encode



-- Config


{-| Github OAuth Token

Token will be used on client and thus is by definition public.
It's important that this token does not have any priviliges.
All it's suppose to do is to read public activity.

-}
token : String
token =
    "e7f8cdd0e3311d2384929a09612b96d8d7758ccb"


repoOwner : String
repoOwner =
    "NixOS"


repoName : String
repoName =
    "nixpkgs"


labels : List String
labels =
    [ "infrastructure"
    , "1.severity: channel blocker"
    ]



-- Types


type alias User =
    { login : String
    , avatarUrl : String
    , url : String
    }


userDecoder : Decoder User
userDecoder =
    Decode.succeed User
        |> Decode.andMap (Decode.field "login" Decode.string)
        |> Decode.andMap (Decode.field "avatarUrl" Decode.string)
        |> Decode.andMap (Decode.field "url" Decode.string)


type alias Label =
    { name : String
    , color : String
    }


labelDecoder : Decoder Label
labelDecoder =
    Decode.succeed Label
        |> Decode.andMap (Decode.field "name" Decode.string)
        |> Decode.andMap (Decode.field "color" Decode.string)


type alias Comment =
    { text : String
    , author : User
    }


commentDecoder : Decoder Comment
commentDecoder =
    Decode.succeed Comment
        |> Decode.andMap (Decode.field "bodyHTML" Decode.string)
        |> Decode.andMap (Decode.field "author" userDecoder)


type IssueState
    = Open
    | Closed


issueStateDecoder : Decoder IssueState
issueStateDecoder =
    let
        fromString str =
            case String.toLower str of
                "open" ->
                    Decode.succeed Open

                "closed" ->
                    Decode.succeed Closed

                _ ->
                    Decode.fail <| "Unknown issue state " ++ str
    in
    Decode.andThen fromString Decode.string


issueStateToString : IssueState -> String
issueStateToString state =
    case state of
        Open ->
            "OPEN"

        Closed ->
            "CLOSED"


type alias Issue =
    { number : Int
    , title : String
    , createdAt : String -- TODO: date
    , updatedAt : String -- TODO: date
    , state : IssueState
    , url : String
    , text : String
    , labels : List Label
    , author : User
    , commentsCount : Int
    , comments : List Comment
    }


issueDecoder : Decoder Issue
issueDecoder =
    Decode.succeed Issue
        |> Decode.andMap (Decode.field "number" Decode.int)
        |> Decode.andMap (Decode.field "title" Decode.string)
        |> Decode.andMap (Decode.field "createdAt" Decode.string)
        |> Decode.andMap (Decode.field "updatedAt" Decode.string)
        |> Decode.andMap (Decode.field "state" issueStateDecoder)
        |> Decode.andMap (Decode.field "url" Decode.string)
        |> Decode.andMap (Decode.field "bodyHTML" Decode.string)
        |> Decode.andMap (Decode.at [ "labels", "nodes" ] <| Decode.list labelDecoder)
        |> Decode.andMap (Decode.field "author" userDecoder)
        |> Decode.andMap (Decode.at [ "comments", "totalCount" ] Decode.int)
        |> Decode.andMap (Decode.at [ "comments", "nodes" ] <| Decode.list commentDecoder)



-- Query


{-| Sorry for this mess
-}
query : IssueState -> String
query state =
    String.concat
        [ """
            query {
                repository
        """
        , "(owner: "
        , Encode.encode 0 <| Encode.string repoOwner
        , ", name: "
        , Encode.encode 0 <| Encode.string repoName
        , ")"
        , """
            {
                issues(
                last: 30
                labels:
        """
        , Encode.encode 0 <| Encode.list Encode.string labels
        , ", states: ["
        , issueStateToString state
        , "])"
        , """
          {
            nodes {
                number
                title
                createdAt
                updatedAt
                state
                url
                bodyHTML
                labels(first: 10) {
                nodes {
                    name
                    color
                }
                }
                author {
                    login
                    avatarUrl
                    url
                }
                comments(last: 3) {
                    totalCount
                    nodes {
                        author {
                            login
                            avatarUrl
                            url
                        }
                        bodyHTML
                    }
                }
            }
        }
    }
}
"""
        ]


fetchIssues : IssueState -> (Result Http.Error (List Issue) -> msg) -> Cmd msg
fetchIssues state msg =
    Http.request
        { method = "POST"
        , headers = [ Http.header "authorization" <| "Bearer " ++ token ]
        , url = "https://api.github.com/graphql"
        , body =
            Http.jsonBody <|
                Encode.object [ ( "query", Encode.string <| query state ) ]
        , expect =
            Http.expectJson msg <|
                Decode.at [ "data", "repository", "issues", "nodes" ] <|
                    Decode.list issueDecoder
        , timeout = Nothing
        , tracker = Nothing
        }
