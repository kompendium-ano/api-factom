module FactomApi exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import Http
import String


type alias User =
    { userAccessToken : String
    , userName : String
    , userUsage : Float
    , userUsageLimit : Float
    }

type alias Entry =
    { entryStatus : String
    , entryCreatedAt : String
    , entryChainId : String
    , entryEntryHash : String
    , entryContent : String
    , entryExtIds : List (String)
    }

type alias Chain =
    { chainChainEntity : ChainEntity
    }

decodeUser : Decoder User
decodeUser =
    decode User
        |> required "userAccessToken" string
        |> required "userName" string
        |> required "userUsage" float
        |> required "userUsageLimit" float

decodeChain : Decoder Chain
decodeChain =
    decode Chain
        |> required "chainChainEntity" decodeChainEntity

decodeEntry : Decoder Entry
decodeEntry =
    decode Entry
        |> required "entryStatus" string
        |> required "entryCreatedAt" string
        |> required "entryChainId" string
        |> required "entryEntryHash" string
        |> required "entryContent" string
        |> required "entryExtIds" (list string)

getUser : Maybe (String) -> Http.Request (User)
getUser header_Authorization Bearer =
    Http.request
        { method =
            "GET"
        , headers =
            List.filterMap identity
                [ Maybe.map (Http.header "Authorization Bearer") header_Authorization Bearer
                ]
        , url =
            String.join "/"
                [ ""
                , "user"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson decodeUser
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getChains : Maybe (String) -> Maybe (Int) -> Maybe (Int) -> Maybe (String) -> Maybe (String) -> Http.Request (Either)
getChains header_Authorization Bearer query_start query_limit query_status query_sort =
    let
        params =
            List.filter (not << String.isEmpty)
                [ query_start
                    |> Maybe.map (toString >> Http.encodeUri >> (++) "start=")
                    |> Maybe.withDefault ""
                , query_limit
                    |> Maybe.map (toString >> Http.encodeUri >> (++) "limit=")
                    |> Maybe.withDefault ""
                , query_status
                    |> Maybe.map (Http.encodeUri >> (++) "status=")
                    |> Maybe.withDefault ""
                , query_sort
                    |> Maybe.map (Http.encodeUri >> (++) "sort=")
                    |> Maybe.withDefault ""
                ]
    in
        Http.request
            { method =
                "GET"
            , headers =
                List.filterMap identity
                    [ Maybe.map (Http.header "Authorization Bearer") header_Authorization Bearer
                    ]
            , url =
                String.join "/"
                    [ ""
                    , "chains"
                    ]
                ++ if List.isEmpty params then
                       ""
                   else
                       "?" ++ String.join "&" params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson decodeEither
            , timeout =
                Nothing
            , withCredentials =
                False
            }

postChains : Maybe (String) -> Maybe (String) -> List ((String, String)) -> Http.Request (Either)
postChains header_Authorization Bearer query_callback_url body =
    let
        params =
            List.filter (not << String.isEmpty)
                [ query_callback_url
                    |> Maybe.map (Http.encodeUri >> (++) "callback_url=")
                    |> Maybe.withDefault ""
                ]
    in
        Http.request
            { method =
                "POST"
            , headers =
                List.filterMap identity
                    [ Maybe.map (Http.header "Authorization Bearer") header_Authorization Bearer
                    ]
            , url =
                String.join "/"
                    [ ""
                    , "chains"
                    ]
                ++ if List.isEmpty params then
                       ""
                   else
                       "?" ++ String.join "&" params
            , body =
                Http.jsonBody ((Json.Encode.list << List.map (tuple2 Json.Encode.string Json.Encode.string)) body)
            , expect =
                Http.expectJson decodeEither
            , timeout =
                Nothing
            , withCredentials =
                False
            }

postChainsByChainid : Maybe (String) -> String -> Http.Request (Either)
postChainsByChainid header_Authorization Bearer capture_chainid =
    Http.request
        { method =
            "POST"
        , headers =
            List.filterMap identity
                [ Maybe.map (Http.header "Authorization Bearer") header_Authorization Bearer
                ]
        , url =
            String.join "/"
                [ ""
                , "chains"
                , capture_chainid |> Http.encodeUri
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson decodeEither
        , timeout =
            Nothing
        , withCredentials =
            False
        }

postChainsByChainidEntries : Maybe (String) -> String -> Maybe (Int) -> Maybe (Int) -> Maybe (String) -> Maybe (String) -> Http.Request (Either)
postChainsByChainidEntries header_Authorization Bearer capture_chainid query_start query_limit query_status query_sort =
    let
        params =
            List.filter (not << String.isEmpty)
                [ query_start
                    |> Maybe.map (toString >> Http.encodeUri >> (++) "start=")
                    |> Maybe.withDefault ""
                , query_limit
                    |> Maybe.map (toString >> Http.encodeUri >> (++) "limit=")
                    |> Maybe.withDefault ""
                , query_status
                    |> Maybe.map (Http.encodeUri >> (++) "status=")
                    |> Maybe.withDefault ""
                , query_sort
                    |> Maybe.map (Http.encodeUri >> (++) "sort=")
                    |> Maybe.withDefault ""
                ]
    in
        Http.request
            { method =
                "POST"
            , headers =
                List.filterMap identity
                    [ Maybe.map (Http.header "Authorization Bearer") header_Authorization Bearer
                    ]
            , url =
                String.join "/"
                    [ ""
                    , "chains"
                    , capture_chainid |> Http.encodeUri
                    , "entries"
                    ]
                ++ if List.isEmpty params then
                       ""
                   else
                       "?" ++ String.join "&" params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson decodeEither
            , timeout =
                Nothing
            , withCredentials =
                False
            }

getChainsByChainidEntriesFirst : Maybe (String) -> String -> Http.Request (Either)
getChainsByChainidEntriesFirst header_Authorization Bearer capture_chainid =
    Http.request
        { method =
            "GET"
        , headers =
            List.filterMap identity
                [ Maybe.map (Http.header "Authorization Bearer") header_Authorization Bearer
                ]
        , url =
            String.join "/"
                [ ""
                , "chains"
                , capture_chainid |> Http.encodeUri
                , "entries"
                , "first"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson decodeEither
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getChainsByChainidEntriesLast : Maybe (String) -> String -> Http.Request (Either)
getChainsByChainidEntriesLast header_Authorization Bearer capture_chainid =
    Http.request
        { method =
            "GET"
        , headers =
            List.filterMap identity
                [ Maybe.map (Http.header "Authorization Bearer") header_Authorization Bearer
                ]
        , url =
            String.join "/"
                [ ""
                , "chains"
                , capture_chainid |> Http.encodeUri
                , "entries"
                , "last"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson decodeEither
        , timeout =
            Nothing
        , withCredentials =
            False
        }

postChainsSearch : Maybe (String) -> Maybe (Int) -> Maybe (Int) -> Maybe (String) -> Maybe (String) -> List (String) -> Http.Request (Either)
postChainsSearch header_Authorization Bearer query_start query_limit query_status query_sort body =
    let
        params =
            List.filter (not << String.isEmpty)
                [ query_start
                    |> Maybe.map (toString >> Http.encodeUri >> (++) "start=")
                    |> Maybe.withDefault ""
                , query_limit
                    |> Maybe.map (toString >> Http.encodeUri >> (++) "limit=")
                    |> Maybe.withDefault ""
                , query_status
                    |> Maybe.map (Http.encodeUri >> (++) "status=")
                    |> Maybe.withDefault ""
                , query_sort
                    |> Maybe.map (Http.encodeUri >> (++) "sort=")
                    |> Maybe.withDefault ""
                ]
    in
        Http.request
            { method =
                "POST"
            , headers =
                List.filterMap identity
                    [ Maybe.map (Http.header "Authorization Bearer") header_Authorization Bearer
                    ]
            , url =
                String.join "/"
                    [ ""
                    , "chains"
                    , "search"
                    ]
                ++ if List.isEmpty params then
                       ""
                   else
                       "?" ++ String.join "&" params
            , body =
                Http.jsonBody ((Json.Encode.list << List.map Json.Encode.string) body)
            , expect =
                Http.expectJson decodeEither
            , timeout =
                Nothing
            , withCredentials =
                False
            }

postChainsByChainidEntriesSearch : Maybe (String) -> String -> Maybe (Int) -> Maybe (Int) -> Maybe (String) -> Maybe (String) -> List (String) -> Http.Request (Either)
postChainsByChainidEntriesSearch header_Authorization Bearer capture_chainid query_start query_limit query_status query_sort body =
    let
        params =
            List.filter (not << String.isEmpty)
                [ query_start
                    |> Maybe.map (toString >> Http.encodeUri >> (++) "start=")
                    |> Maybe.withDefault ""
                , query_limit
                    |> Maybe.map (toString >> Http.encodeUri >> (++) "limit=")
                    |> Maybe.withDefault ""
                , query_status
                    |> Maybe.map (Http.encodeUri >> (++) "status=")
                    |> Maybe.withDefault ""
                , query_sort
                    |> Maybe.map (Http.encodeUri >> (++) "sort=")
                    |> Maybe.withDefault ""
                ]
    in
        Http.request
            { method =
                "POST"
            , headers =
                List.filterMap identity
                    [ Maybe.map (Http.header "Authorization Bearer") header_Authorization Bearer
                    ]
            , url =
                String.join "/"
                    [ ""
                    , "chains"
                    , capture_chainid |> Http.encodeUri
                    , "entries"
                    , "search"
                    ]
                ++ if List.isEmpty params then
                       ""
                   else
                       "?" ++ String.join "&" params
            , body =
                Http.jsonBody ((Json.Encode.list << List.map Json.Encode.string) body)
            , expect =
                Http.expectJson decodeEither
            , timeout =
                Nothing
            , withCredentials =
                False
            }