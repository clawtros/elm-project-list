module ProjectList exposing (..)

import Navigation
import String
import Html exposing (..)
import Html.Attributes exposing (class, rel, href, src)
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
        [ Project "Cool Synth" "http://www.clawtros.com/synth/" "WebAudio oscillator toy"
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
        , Project "Alternate Fingering" "http://fingers.removablefeast.com/" ""
        , Project "Name Generators" "http://names.removablefeast.com/" "NLTK"
        , Project "TTC Locator" "http://ttc.removablefeast.com/" "All the Toronto Transit Commission vehicles"
        ]
    , current = 1
    }


css : String -> Html Msg
css path =
    node "link" [ rel "stylesheet", href path ] []


getCurrent : Model -> Project
getCurrent model =
    Maybe.withDefault
        { name = ""
        , url = ""
        , description = ""
        }
        (listGet model.projects model.current)


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [ class "menu" ]
            [ h1 [] [ text "clawtros.com" ]
            , div [ class "description" ]
                [ getCurrent model
                    |> .description
                    |> text
                ]
            , div []
                (List.indexedMap
                    (\index ->
                        \project ->
                            div []
                                [ div
                                    [ onClick (SelectProject index)
                                    , class
                                        ((if model.current == index then
                                            "active"
                                          else
                                            "inactive"
                                         )
                                            ++ " item"
                                        )
                                    ]
                                    [ text project.name ]
                                ]
                    )
                    model.projects
                )
            ]
        , div [ class "content" ]
            [ iframe
                [ getCurrent model
                    |> .url
                    |> src
                ]
                []
            ]
        ]



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectProject index ->
            { model | current = index } ! [Navigation.newUrl ("#project/" ++ (toString index))]
        UrlChange location ->
            model ! []


parseIdFromLocation : Navigation.Location -> (Maybe Int)
parseIdFromLocation location =
    parseHash (UrlParser.s "project" </> UrlParser.int) location


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    case (parseIdFromLocation location) of
        Just value ->
            { initialModel | current = value } ! []
        Nothing ->
            initialModel ! []


main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { init = init
        , view = view
        , update = update
        , subscriptions = (always Sub.none)
        }
