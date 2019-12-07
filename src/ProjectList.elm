module ProjectList exposing (..)

import Browser exposing (..)
import Browser.Navigation as Nav
import Dict exposing (Dict, fromList, get)
import Html exposing (..)
import Html.Attributes exposing (class, classList, href, rel, src, style)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode
import String
import Url exposing (Url)
import Url.Parser


type alias Project =
    { id : String
    , name : String
    , url : String
    , description : String
    }


type alias Route =
    String


type alias Flags =
    { projectJson : String }


type alias Model =
    { projects : Dict String Project
    , current : String
    , key : Nav.Key
    , projectOrder : List String
    }


type Msg
    = SelectProject String
    | HandleUrlRequest UrlRequest
    | HandleUrlChange Url


keyFromUrl : Url -> String
keyFromUrl =
    .fragment >> Maybe.withDefault ""


initialModel : Flags -> Nav.Key -> Url -> Model
initialModel flags key url =
    { projects =
        Dict.empty
    , key = key
    , current =
        keyFromUrl url
    , projectOrder = []
    }


viewCurrent : Model -> Html Msg
viewCurrent model =
    case Dict.get model.current model.projects of
        Just current ->
            div
                [ class "content"
                ]
                [ iframe
                    [ current
                        |> .url
                        |> src
                    ]
                    []
                ]

        Nothing ->
            text ""


viewProject : String -> ( String, Project ) -> Html Msg
viewProject current keyval =
    let
        ( key, project ) =
            keyval
    in
    a
        [ href <| "#" ++ key
        , classList
            [ ( "item", True )
            , ( "active", current == key )
            , ( "inactive", not (current == key) )
            ]
        ]
        [ div [ class "project-header" ] [ text project.name ]
        ]


viewProjects : List String -> Dict String Project -> String -> Html Msg
viewProjects order projects current =
    List.map
        (\key ->
            Maybe.withDefault (text "") <|
                Maybe.map (\project -> viewProject current ( key, project )) <|
                    Dict.get key projects
        )
        order
        |> div []


view : Model -> Browser.Document Msg
view model =
    { title = String.join "-" [ "clawtros.com", model.current ]
    , body =
        [ div [ class "container" ]
            [ div [ class "menu" ]
                [ div []
                    [ h1 [] [ text "clawtros.com" ]
                    , viewProjects model.projectOrder model.projects model.current
                    ]
                ]
            , viewCurrent model
            ]
        ]
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectProject index ->
            ( { model | current = index }, Cmd.none )

        HandleUrlRequest (Internal url) ->
            ( model, Nav.pushUrl model.key (Url.toString url) )

        HandleUrlRequest (External stringUrl) ->
            ( model, Cmd.none )

        HandleUrlChange url ->
            update (SelectProject <| keyFromUrl url) model


projectDict : List Project -> Dict String Project
projectDict projects =
    projects
        |> List.map
            (\project -> ( project.id, project ))
        |> Dict.fromList


projectDecoder : Decode.Decoder Project
projectDecoder =
    Decode.map4 Project
        (Decode.field "id" Decode.string)
        (Decode.field "title" Decode.string)
        (Decode.field "url" Decode.string)
        (Decode.field "description" Decode.string)


projectsDecoder : Decode.Decoder (List Project)
projectsDecoder =
    Decode.list projectDecoder


main : Program Flags Model Msg
main =
    Browser.application
        { init =
            \flags url key ->
                let
                    model =
                        initialModel flags key url

                    projects =
                        Decode.decodeString projectsDecoder flags.projectJson
                            |> Result.withDefault []

                    order =
                        List.map .id projects

                    dict =
                        projectDict projects
                in
                ( { model
                    | projects = dict
                    , projectOrder = order
                    , current =
                        if Dict.get (keyFromUrl url) dict /= Nothing then
                            keyFromUrl url

                        else
                            List.head order |> Maybe.withDefault ""
                  }
                , Cmd.none
                )
        , view = view
        , update = update
        , subscriptions = always Sub.none
        , onUrlRequest = HandleUrlRequest
        , onUrlChange = HandleUrlChange
        }
