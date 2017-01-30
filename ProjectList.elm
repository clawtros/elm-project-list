module ProjectList exposing (..)

import Navigation
import String
import Html exposing (..)
import Html.Attributes exposing (class, rel, href, src, classList)
import Html.Events exposing (onClick)
import UrlParser exposing ((</>), parseHash)


type alias Project =
    { name : String
    , url : String
    , description : String
    }


type alias Model =
    { projects : List Project
    , current : Int
    }


type Msg
    = SelectProject Int
    | UrlChange Navigation.Location


listGet : List a -> Int -> Maybe a
listGet xs n =
    List.head (List.drop n xs)


initialModel : Model
initialModel =
    { projects =
        [ Project "De Jong Attractor" "http://www.clawtros.com/pdj.html" "Click and drag to alter values"
        , Project "Reaction Diffusion" "http://www.clawtros.com/rd/" "Reaction / Diffusion as a WebGL Shader"
        , Project "Interactive Joy Division" "http://www.clawtros.com/joy.html" "Hover to make waves"
        , Project "Cool Synth" "http://www.clawtros.com/synth/" "WebAudio oscillator toy"
        , Project "Text Renderer" "http://www.clawtros.com/textrender.html" "Drop-in ASCII THREE.js renderer."
        , Project "Goop" "http://clawtros.com/goop/" "I have no idea how this works."
        , Project "Glitchy Art Maker" "http://clawtros.com/backgrounds/" "Attempting to recreate goop."
        , Project "Mandelbrot" "http://mandelbutt.com/" "Raw WebGL Mandelbrot zoomer."
        , Project "Terrain" "http://clawtros.com/maze-terrain/" "Accidentally made while experimenting with maze generation, the A* algorithm and THREE.js"
        , Project "Crossword Player" "http://clawtros.com/clientcross/" "Reacty Crossword player"
        , Project "D3 Update/Exit" "http://clawtros.com/d3.html" "Getting D3 enter/update/exit sorted"
        , Project "Worst Phonetic Dictionary" "http://phonetic.removablefeast.com/" "Bad phonetic dictionaries"
        , Project "Crossword Generator" "http://clawtros.com/recursin-workers/" "Recursive web workers generating crosswords"
        , Project "Floating" "http://clawtros.com/floating.html" "Dots floating on vectors."
        , Project "Marriage" "http://deeznups.clawtros.com/" "Solving life's greatest problems"
        , Project "Lorenz" "http://clawtros.com/waterwheel/" "Playing with Canvas and D3."
        , Project "Voronoi" "http://clawtros.com/voronoi/" "Naive Voronoi shades."
        , Project "Minutes til' Five" "http://minutes-til-five.com/" "Countdown Clock"
        , Project "Ulam Spirals" "http://removablefeast.com/spiral.html" "I think this was the first thing I made with Canvas?"
        , Project "Cat Look" "http://removablefeast.com/catlook" "Cats looking"
        , Project "Deal With Itifier" "http://deal.removablefeast.com/?url=https%3A%2F%2Fcdn-images-1.medium.com%2Fmax%2F1200%2F1*l7zNW_4-afEOfP_mXxs75w.jpeg" "Sunglass Applicator"
        , Project "Drips" "http://clawtros.com/drips" "Averaging HSV colours with surroundings"
        , Project "Bouncing Balls" "http://clawtros.com/google-bouncing-balls/" "Modification of a recreation of a Google Doodle"
        , Project "Alternate Fingering" "http://fingers.removablefeast.com/" "Alternate uses of knuckle tattoos"
        , Project "CSS Parallax" "http://clawtros.com/forest/" "Hover and move the mouse for depthy motion"
        , Project "Name Generators" "http://names.removablefeast.com/" "NLTK"
        , Project "TTC Locator" "http://ttc.removablefeast.com/" "All the Toronto Transit Commission vehicles"
        ]
    , current = 0
    }


getCurrent : Model -> Maybe Project
getCurrent model =
    listGet model.projects model.current


viewCurrent : Model -> Html Msg
viewCurrent model =
    case getCurrent model of
        Just current ->
            div [ class "content" ]
                [ iframe
                    [ current
                        |> .url
                        |> src
                    ]
                    []
                ]

        Nothing ->
            text ""


viewProject : Int -> Int -> Project -> Html Msg
viewProject current index project =
    div
        [ onClick (SelectProject index)
        , classList
            [ ( "item", True )
            , ( "active", current == index )
            , ( "inactive", not (current == index) )
            ]
        ]
        [ div [ class "project-header" ] [ text project.name ]
        , div [ class "project-description" ] [ text project.description ]
        ]


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [ class "menu" ]
            [ h1 [] [ text "clawtros.com" ]
            , div []
                (List.indexedMap
                    (\index project -> (viewProject model.current index project))
                    model.projects
                )
            ]
        , viewCurrent model
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectProject index ->
            { model | current = index } ! [ Navigation.newUrl ("#project/" ++ (toString index)) ]

        UrlChange location ->
            navigateToLocation model location


parseIdFromLocation : Navigation.Location -> Maybe Int
parseIdFromLocation location =
    parseHash (UrlParser.s "project" </> UrlParser.int) location


navigateToLocation : Model -> Navigation.Location -> ( Model, Cmd Msg )
navigateToLocation model location =
    case (parseIdFromLocation location) of
        Just value ->
            { model | current = value } ! []

        Nothing ->
            model ! []


main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { init = navigateToLocation initialModel
        , view = view
        , update = update
        , subscriptions = (always Sub.none)
        }
