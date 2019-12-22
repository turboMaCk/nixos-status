module GitHub exposing (fetchIssues)

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



-- Types


type alias User =
    { login : String
    , avatarUrl : String
    }


userDecoder : Decoder User
userDecoder =
    Decode.succeed User
        |> Decode.andMap (Decode.field "login" Decode.string)
        |> Decode.andMap (Decode.field "avatarUrl" Decode.string)


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
        |> Decode.andMap (Decode.field "bodyText" Decode.string)
        |> Decode.andMap (Decode.field "author" userDecoder)


type IssueState
    = Opened
    | Closed


issueStateDecoder : Decoder IssueState
issueStateDecoder =
    let
        fromString str =
            case str of
                "opened" ->
                    Decode.succeed Opened

                "closed" ->
                    Decode.succeed Closed

                _ ->
                    Decode.fail <| "Unknown issue state " ++ str
    in
    Decode.andThen fromString Decode.string


type alias Issue =
    { number : Int
    , title : String
    , createAt : String -- TODO: date
    , updatedAt : String -- TODO: date
    , state : IssueState
    , url : String
    , text : String
    , labels : List Label
    , author : User
    , comments : List Comment
    }



-- Query


query : String
query =
    """
query {
  repository(owner: "nixos", name: "nixpkgs") {
    issues(
      last: 30
      labels: ["infrastructure", "1.severity: channel blocker"]
    ) {
      nodes {
        number,
        title,
        createdAt,
        updatedAt,
        state,
        url,
        bodyText,
        labels(first: 10) {
        \tnodes {
            name,
      \t\t\tcolor,
          }
        },
        author {
          login,
          avatarUrl,
        },
        comments(last: 10) {
          totalCount,
          nodes {
            author {
              login,
              avatarUrl,
            },
            bodyText,
          }
        },
      }
    }
  }
}
"""


fetchIssues : msg -> Cmd msg
fetchIssues msg =
    Http.request
        { method = "POST"
        , headers = [ Http.header "authorization" <| "Bearer " ++ token ]
        , url = "https://api.github.com/graphql"
        , body = Http.jsonBody <| Encode.object [ ( "query", Encode.string query ) ]
        , expect = Http.expectWhatever (\_ -> msg)
        , timeout = Nothing
        , tracker = Nothing
        }
