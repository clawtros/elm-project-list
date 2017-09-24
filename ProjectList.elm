module ProjectList exposing (..)

import Navigation
import Html exposing (..)
import Html.Attributes exposing (class, classList, rel, href, src)
import Html.Events exposing (onClick)
import Dict exposing (Dict)


type alias Project =
    { name : String
    , url : String
    , description : String
    }


projectList : List ( String, Project )
projectList =
    [ ( "rbtree", Project "Red Black Tree" "http://www.clawtros.com/treeviz/" "" )
    , ( "dejong", Project "De Jong Attractor" "http://www.clawtros.com/pdj.html" "" )
    , ( "reaction-diffusion", Project "Reaction Diffusion" "http://www.clawtros.com/rd/" "" )
    , ( "explodingdots", Project "Exploding Dots" "http://www.clawtros.com/avernus.html" "" )
    , ( "joydivision", Project "Interactive Joy Division" "http://www.clawtros.com/joy.html" "Interactive Joy Division cover using SVG driven by JS" )
    , ( "synth", Project "Synth-ish" "http://www.clawtros.com/synth/" "WebAudio oscillator toy" )
    , ( "threetext", Project "Text Renderer" "http://www.clawtros.com/textrender.html" "Drop-in ASCII THREE.js renderer." )
    , ( "goop", Project "Goop" "http://clawtros.com/goop/" "I have no idea how this works." )
    , ( "textures", Project "Glitchy Art Maker" "http://clawtros.com/backgrounds/" "Made while attempting to recreate goop. Iterates convolutions, kinda?  Frontend using the choo framework." )
    , ( "mandelbrot", Project "Mandelbrot" "http://mandelbutt.com/" "Raw WebGL Mandelbrot zoomer." )
    , ( "terrain", Project "Terrain" "http://clawtros.com/maze-terrain/" "Accidentally made while experimenting with maze generation, the A* algorithm and THREE.js" )
    , ( "reactword", Project "Crossword Player" "http://clawtros.com/clientcross/" "Reacty Crossword playe" )
    , ( "d3", Project "D3 Update/Exit" "http://clawtros.com/d3.html" "Getting D3 enter/update/exit sorted" )
    , ( "worstphonetic", Project "Worst Phonetic Dictionary" "http://phonetic.removablefeast.com/" "Bad phonetic dictionaries" )
    , ( "crossgen", Project "Crossword Generator" "http://clawtros.com/recursin-workers/" "Web workers generating crosswords" )
    , ( "floating", Project "Floating" "http://clawtros.com/floating.html" "Dots floating on vectors." )
    , ( "marriage", Project "Marriage" "http://deeznups.clawtros.com/" "Solving life's greatest problems" )
    , ( "lorenz", Project "Lorenz" "http://clawtros.com/waterwheel/" "Playing with Canvas and D3." )
    , ( "voronoi", Project "Voronoi" "http://clawtros.com/voronoi/" "Naive Voronoi shades." )
    , ( "til5", Project "Minutes til' Five" "http://minutes-til-five.com/" "Countdown Clock" )
    , ( "ulam", Project "Ulam Spirals" "http://removablefeast.com/spiral.html" "I think this was the first thing I made with Canvas?" )
    , ( "catlook", Project "Cat Look" "http://removablefeast.com/catlook" "Cats looking" )
    , ( "deals", Project "Deal With Itifier" "http://deal.removablefeast.com/?url=https%3A%2F%2Fcdn-images-1.medium.com%2Fmax%2F1200%2F1*l7zNW_4-afEOfP_mXxs75w.jpeg" "Sunglass Applicator" )
    , ( "drips", Project "Drips" "http://clawtros.com/drips" "Averaging HSV colours with surroundings" )
    , ( "bouncing", Project "Bouncing Balls" "http://clawtros.com/google-bouncing-balls/" "Modification of a recreation of a Google Doodle" )
    , ( "alternate", Project "Alternate Fingering" "http://fingers.removablefeast.com/" "" )
    , ( "names", Project "Name Generators" "http://names.removablefeast.com/" "NLTK" )
    , ( "parallax", Project "CSS Parallax" "http://clawtros.com/forest/" "Not _that_ parallax!" )
    , ( "ttc", Project "TTC Locator" "http://ttc.removablefeast.com/" "All the Toronto Transit Commission vehicles" )
    ]


projectDict : Dict String Project
projectDict =
    Dict.fromList projectList


type alias Model =
    String


type Msg
    = SelectProject String
    | UrlChanged Navigation.Location


listGet : List a -> Int -> Maybe a
listGet xs n =
    List.head (List.drop n xs)


initialModel : Model
initialModel =
    projectList |> List.head |> Maybe.map (\( key, _ ) -> key) |> Maybe.withDefault ""


getCurrent : String -> Maybe Project
getCurrent key =
    Dict.get key projectDict


viewProject : Project -> String -> Bool -> Html Msg
viewProject project key isActive =
    div
        [ classList
            [ ( "item", True )
            , ( "active", isActive )
            ]
        , onClick <| SelectProject key
        ]
        [ text project.name ]


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [ class "menu" ]
            [ h1 [] [ text "clawtros.com" ]
            , div []
                (List.map
                    (\( key, project ) -> viewProject project key (model == key))
                    projectList
                )
            ]
        , div [ class "content" ]
            [ case getCurrent model of
                Just project ->
                    iframe [ project.url |> src ] []

                Nothing ->
                    text "nothing"
            ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectProject key ->
            key ! [ Navigation.newUrl (toUrl key) ]

        UrlChanged location ->
            model ! []


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    case (Dict.get (toModel location) projectDict) of
        Just _ ->
            (toModel location) ! []

        Nothing ->
            initialModel ! []


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


toUrl : Model -> String
toUrl model =
    case getCurrent model of
        Just project ->
            "#/" ++ model

        Nothing ->
            ""


toModel : Navigation.Location -> Model
toModel location =
    String.dropLeft 2 location.hash


fromUrl : String -> Result String Int
fromUrl url =
    String.toInt (String.dropLeft 2 url)


main : Program Never Model Msg
main =
    Navigation.program
        UrlChanged
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
