module Portfolio exposing (..)


import Debug exposing (log)

import Html.App

import Html exposing (..)

import Html.Events exposing (onClick)

type alias Project = { name : String, url : String, active : Bool}
type alias Model = List Project
type Msg = SelectProject Project
    
makeProject : String -> String -> Project
makeProject projectName projectURL =
    { name = projectName
    , url = projectURL
    , active = False
    }

initialModel : Model
initialModel = [makeProject "egoop" "clawtros.com"
               , makeProject "goop2" "clawtros.com"
               , makeProject "goop3" "clawtros.com"]

view : Model -> Html Msg
view model =
    ul [] (List.map (\e -> li [] [a [onClick (SelectProject e)] [text e.name]]) model)

update : Msg -> Model -> Model
update msg model =
    case msg of
        SelectProject project ->
            log project.name
            model

main : Program Never
main = Html.App.beginnerProgram
       { model = initialModel
       , view = view
       , update = update
       }
