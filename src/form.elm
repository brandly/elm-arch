module Main exposing (..)

import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)


main =
    Html.beginnerProgram { model = model, view = view, update = update }


type alias Model =
    { name : String
    , age : Int
    , password : String
    , passwordAgain : String
    }


model : Model
model =
    Model "" 0 "" ""


type Msg
    = Name String
    | Age String
    | Password String
    | PasswordAgain String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Name name ->
            { model | name = name }

        Age age ->
            { model | age = Result.withDefault 0 (String.toInt age) }

        Password password ->
            { model | password = password }

        PasswordAgain password ->
            { model | passwordAgain = password }


view : Model -> Html Msg
view model =
    div []
        [ input [ type_ "text", placeholder "name", onInput Name ] []
        , input [ type_ "number", placeholder "age", onInput Age ] []
        , input [ type_ "password", placeholder "password", onInput Password ] []
        , input [ type_ "password", placeholder "password again", onInput PasswordAgain ] []
        , viewValidation model
        ]


viewValidation : Model -> Html Msg
viewValidation model =
    let
        ( color, message ) =
            if model.password /= model.passwordAgain then
                ( "red", "passwords don't match" )
            else if String.length model.password < 8 then
                ( "red", "password too short" )
            else
                ( "green", "OK" )
    in
    div [ style [ ( "color", color ) ] ] [ text message ]
