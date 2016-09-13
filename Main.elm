module Portfolio exposing (..)

import Navigation
import String

import Html exposing (..)
import Html.App
import Html.Attributes exposing (class, rel, href, src)
import Html.Events exposing (onClick)

type alias Project = { name : String, url : String
                     , description: String
                     }
type alias Model = { projects : List Project
                   , current : Int
                   }
type Msg = SelectProject Int


listGet : List a -> Int -> Maybe a
listGet xs n = List.head (List.drop n xs)


makeProject : String -> String -> String -> Project
makeProject projectName projectURL description =
    { name = projectName
    , url = projectURL
    , description = description
    }

    
initialModel : Model
initialModel =
    { projects = [makeProject "Cool Synth" "http://www.clawtros.com/synth/" "WebAudio oscillator toy"
                 , makeProject "Text Renderer" "http://www.clawtros.com/textrender.html" "Drop-in ASCII THREE.js renderer."
                 , makeProject "Goop""http://clawtros.com/goop/" "I have no idea how this works."
                 , makeProject "Glitchy Art Maker" "http://clawtros.com/backgrounds/" "Attempting to recreate goop."
                 , makeProject "Mandelbrot" "http://mandelbutt.com/" "Raw WebGL Mandelbrot zoomer."
                 , makeProject "Terrain" "http://clawtros.com/maze-terrain/" "Accidentally made while experimenting with maze generation, the A* algorithm and THREE.js"
                 , makeProject "Crossword Player" "http://clawtros.com/clientcross/" "Reacty Crossword player"
                 , makeProject "D3 Update/Exit" "http://clawtros.com/d3.html" "Getting D3 enter/update/exit sorted"
                 , makeProject "Worst Phonetic Dictionary" "http://phonetic.removablefeast.com/" "Bad phonetic dictionaries"
                 , makeProject "Crossword Generator" "http://clawtros.com/recursin-workers/" "Recursive web workers generating crosswords"
                 , makeProject "Floating" "http://clawtros.com/floating.html" "Dots floating on vectors."
                 , makeProject "Marriage" "http://deeznups.clawtros.com/" "Solving life's greatest problems"
                 , makeProject "Lorenz" "http://clawtros.com/waterwheel/" "Playing with Canvas and D3."
                 , makeProject "Voronoi" "http://clawtros.com/voronoi/" "Naive Voronoi shades."
                 , makeProject "Minutes til' Five" "http://minutes-til-five.com/" "Countdown Clock"
                 , makeProject "Ulam Spirals" "http://removablefeast.com/spiral.html" "I think this was the first thing I made with Canvas?"
                 , makeProject "Cat Look" "http://removablefeast.com/catlook" "Cats looking"
                 , makeProject "Deal With Itifier" "http://deal.removablefeast.com/?url=https%3A%2F%2Fcdn-images-1.medium.com%2Fmax%2F1200%2F1*l7zNW_4-afEOfP_mXxs75w.jpeg" "Sunglass Applicator"
                 , makeProject "Drips" "http://clawtros.com/drips" "Averaging HSV colours with surroundings"
                 , makeProject "Bouncing Balls" "http://clawtros.com/google-bouncing-balls/" "Modification of a recreation of a Google Doodle"
                 , makeProject "Alternate Fingering" "http://fingers.removablefeast.com/" ""
                 , makeProject "Name Generators" "http://names.removablefeast.com/" "NLTK"
                 , makeProject "TTC Locator" "http://ttc.removablefeast.com/" "All the Toronto Transit Commission vehicles"
                 ]
    , current = 1
    }


css : String -> Html Msg
css path =
  node "link" [ rel "stylesheet", href path ] []


getCurrent : Model -> Project
getCurrent model = Maybe.withDefault {name=""
                                     , url=""
                                     , description=""}
                   (listGet model.projects model.current)

view : Model -> Html Msg
view model =
    div [class "container"] [
         div [class "menu"] [
              h1 [ ] [text "clawtros.com"]
             , css "styles.css"
             , div [class "description"]
                  [getCurrent model
                  |> .description
                  |> text]
             , div [] (List.indexedMap
                           (\index -> \project -> div [] [
                                       div [onClick (SelectProject index)
                                         , class ((if model.current == index then "active" else "inactive") ++ " item")]
                                           [text project.name]]) model.projects)
             ]
        , div [class "content"] [iframe [getCurrent model
                                        |> .url
                                        |> src] []]
        ]

        
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        newModel = 
            case msg of
                SelectProject index ->
                    {model | current = index}
    in
        (newModel, Navigation.newUrl (toUrl newModel) )

           
init : Result String Int -> ( Model, Cmd Msg )
init result =
    urlUpdate result initialModel


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none 


toUrl : Model -> String
toUrl model =
  "#/" ++ toString (.current model)


fromUrl : String -> Result String Int
fromUrl url =
  String.toInt (String.dropLeft 2 url)


urlParser : Navigation.Parser (Result String Int)
urlParser =
  Navigation.makeParser (fromUrl << .hash)


urlUpdate : Result String Int -> Model -> (Model, Cmd Msg)
urlUpdate result model =
  case result of
    Ok newIndex ->
      ({model | current = newIndex}, Cmd.none)

    Err _ ->
      (model, Cmd.none)

        
main : Program Never
main = Navigation.program urlParser
       { init = init
       , view = view
       , update = update
       , urlUpdate = urlUpdate
       , subscriptions = subscriptions
       }
