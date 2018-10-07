module ProjectList exposing (..)

import Browser exposing (..)
import Browser.Navigation as Nav
import String
import Html exposing (..)
import Html.Attributes exposing (class, rel, href, src, classList, style)
import Html.Events exposing (onClick)
import Dict exposing (Dict, fromList, get)
import Url exposing (Url)
import Url.Parser


type alias Project =
    { name : String
    , url : String
    , description : String
    }


type alias Route =
    String


type alias Model =
    { projects : Dict String Project
    , current : String
    , key : Nav.Key
    }


type Msg
    = SelectProject String
    | HandleUrlRequest UrlRequest
    | HandleUrlChange Url


keyFromUrl : Url -> String
keyFromUrl =
    .fragment >> Maybe.withDefault ""


projectList : List ( String, Project )
projectList =
    [ ( "dejong", Project "De Jong Attractor" "http://www.clawtros.com/pdj.html" "Click and drag to alter values" )
    , ( "rd", Project "Reaction Diffusion" "http://www.clawtros.com/rd/" "Reaction / Diffusion as a WebGL Shader" )
    , ( "joy", Project "Interactive Joy Division" "http://www.clawtros.com/joy.html" "Hover to make waves" )
    , ( "avernus", Project "Scary Circle" "http://www.clawtros.com/avernus.html" "" )
    , ( "syn", Project "Cool Synth" "http://www.clawtros.com/synth/" "WebAudio oscillator toy" )
    , ( "txtren", Project "Text Renderer" "http://www.clawtros.com/textrender.html" "Drop-in ASCII THREE.js renderer." )
    , ( "goop", Project "Goop" "http://clawtros.com/goop/" "I have no idea how this works." )
    , ( "glitch", Project "Glitchy Art Maker" "http://clawtros.com/backgrounds/" "Attempting to recreate goop." )
    , ( "mandelbrot", Project "Mandelbrot" "http://mandelbutt.com/" "Raw WebGL Mandelbrot zoomer." )
    , ( "terrain", Project "Terrain" "http://clawtros.com/maze-terrain/" "Accidentally made while experimenting with maze generation, the A* algorithm and THREE.js" )
    , ( "cwp", Project "Crossword Player" "http://clawtros.com/clientcross/" "Reacty Crossword player" )
    , ( "d3", Project "D3 Update/Exit" "http://clawtros.com/d3.html" "Getting D3 enter/update/exit sorted" )
    , ( "wpd", Project "Worst Phonetic Dictionary" "http://phonetic.removablefeast.com/" "Bad phonetic dictionaries" )
    , ( "cwg", Project "Crossword Generator" "http://clawtros.com/recursin-workers/" "Recursive web workers generating crosswords" )
    , ( "float", Project "Floating" "http://clawtros.com/floating.html" "Dots floating on vectors." )
    , ( "marr", Project "Marriage" "http://deeznups.clawtros.com/" "Solving life's greatest problems" )
    , ( "lorenz", Project "Lorenz" "http://clawtros.com/waterwheel/" "Playing with Canvas and D3." )
    , ( "vor", Project "Voronoi" "http://clawtros.com/voronoi/" "Naive Voronoi shades." )
    , ( "minstil5", Project "Minutes til' Five" "http://minutes-til-five.com/" "Countdown Clock" )
    , ( "ulam", Project "Ulam Spirals" "http://removablefeast.com/spiral.html" "I think this was the first thing I made with Canvas?" )
    , ( "cat", Project "Cat Look" "http://removablefeast.com/catlook" "Cats looking" )
    , ( "deal", Project "Deal With Itifier" "http://deal.removablefeast.com/?url=https%3A%2F%2Fcdn-images-1.medium.com%2Fmax%2F1200%2F1*l7zNW_4-afEOfP_mXxs75w.jpeg" "Sunglass Applicator" )
    , ( "drip", Project "Drips" "http://clawtros.com/drips" "Averaging HSV colours with surroundings" )
    , ( "bounce", Project "Bouncing Balls" "http://clawtros.com/google-bouncing-balls/" "Modification of a recreation of a Google Doodle" )
    , ( "alt", Project "Alternate Fingering" "http://fingers.removablefeast.com/" "Alternate uses of knuckle tattoos" )
    , ( "cssp", Project "CSS Parallax" "http://clawtros.com/forest/" "Hover and move the mouse for depthy motion" )
    , ( "names", Project "Name Generators" "http://names.removablefeast.com/" "NLTK" )
    , ( "ttc", Project "TTC Locator" "http://ttc.removablefeast.com/" "All the Toronto Transit Commission vehicles" )
    ]


initialModel : Nav.Key -> Url -> Model
initialModel key url =
    { projects =
        fromList projectList
    , key = key
    , current = keyFromUrl url
    }


getCurrent : Model -> Maybe Project
getCurrent model =
    get model.current model.projects


viewCurrent : Model -> Html Msg
viewCurrent model =
    case getCurrent model of
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
            [ href <|  "#" ++ key
            , classList
                [ ( "item", True )
                , ( "active", current == key )
                , ( "inactive", not (current == key) )
                ]
            ]
            [ div [ class "project-header" ] [ text project.name ]
            ]


viewProjects : Dict String Project -> String -> Html Msg
viewProjects projects current =
    projectList
        |> List.map (viewProject current)
        |> div []


view : Model -> Browser.Document Msg
view model =
    { title = String.join "-" [ "clawtros.com", model.current ]
    , body =
        [ div [ class "container" ]
            [ div [ class "menu" ]
                [ div [  ]
                    [ h1 [] [ text "clawtros.com" ]
                    , viewProjects model.projects model.current
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
            (model, Nav.pushUrl model.key (Url.toString url) )

        HandleUrlRequest (External stringUrl) ->
            ( model, Cmd.none )

        HandleUrlChange url ->
            update (SelectProject <| keyFromUrl url) model


type alias Flags =
    {}


main : Program Flags Model Msg
main =
    Browser.application
        { init =
              (\_ url key ->
                   ( initialModel key url, Cmd.none ))
        , view = view
        , update = update
        , subscriptions = (always Sub.none)
        , onUrlRequest = HandleUrlRequest
        , onUrlChange = HandleUrlChange
        }
