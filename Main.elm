module Portfolio exposing (..)


import Html exposing (..)
import Html.App
import Html.Attributes exposing (class, rel, href, src)
import Html.Events exposing (onClick)

type alias Project = { name : String, url : String, description: String }
type alias Model = { projects : List Project, current : Int}
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
                 , makeProject "Crossword Generator" "http://clawtros.com/recursin-workers/" "Recursive crossword solver"
                 , makeProject "Floating" "http://clawtros.com/floating.html" "Dots floating on vectors."
                 , makeProject "Marriage" "http://deeznups.clawtros.com/" "Solving life's greatest problems"
                 , makeProject "Lorenz" "http://clawtros.com/waterwheel/" "Playing with Canvas and D3."
                 , makeProject "Voronoi" "http://clawtros.com/voronoi/" "Naive Voronoi shades."
                 , makeProject "Minutes til' Five" "http://minutes-til-five.com/" ""
                 , makeProject "Ulam Spirals" "http://removablefeast.com/spiral.html" ""
                 , makeProject "Cat Look" "http://removablefeast.com/catlook" ""
                 , makeProject "Deal With Itifier" "http://deal.removablefeast.com/catlook" ""
                 , makeProject "Drips" "http://clawtros.com/drips" ""
                 , makeProject "Bouncing Balls" "http://clawtros.com/google-bouncing-balls/" ""
                 , makeProject "Alternate Fingering" "http://fingers.removablefeast.com/" ""
                 , makeProject "Name Generators" "http://names.removablefeast.com/" ""
                 , makeProject "TTC Locator" "http://ttc.removablefeast.com/" ""
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
    div [] [
         div [class "menu"] [
              h1 [ ] [text "clawtros.com"]
             , css "http://localhost:8080/elport/styles.css"
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

update : Msg -> Model -> Model
update msg model =
    case msg of
        SelectProject index ->
            {model | current = index}
                

main : Program Never
main = Html.App.beginnerProgram
       { model = initialModel
       , view = view
       , update = update
       }
