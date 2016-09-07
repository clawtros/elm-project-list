module Portfolio exposing (..)

import Debug exposing (log)
import Array

import Html.App
import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class, rel, href, src)

type alias Project = { name : String, url : String }
type alias Model = { projects : List Project, current : Int}
type Msg = SelectProject Int


listGet : List a -> Int -> Maybe a
listGet xs n = List.head (List.drop n xs)

    
makeProject : String -> String -> Project
makeProject projectName projectURL =
    { name = projectName
    , url = projectURL
    }

    
initialModel : Model
initialModel =
    { projects = [makeProject "Text Renderer" "http://www.clawtros.com/textrender.html"
                 , makeProject "goop2" "clawtros.com/2"
                 , makeProject "goop3" "clawtros.com/3"]
    , current = 0
    }

    
css : String -> Html Msg
css path =
  node "link" [ rel "stylesheet", href path ] []

      
view : Model -> Html Msg
view model =
    div [] [
         div [class "menu"] [
              h1 [ ] [text "test"]
             , css "http://localhost:8080/elport/styles.css" 
             , div [] (List.indexedMap (\index -> \project -> div [] [
                                                   a [onClick (SelectProject index), class (if model.current == index then "active" else "inactive")] [text (project.name ++ " (" ++ project.url ++ ")")]]) model.projects)
             ]
        , div [class "content"] [iframe [src (.url (Maybe.withDefault {name="", url=""} (listGet model.projects model.current)))] []]
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
