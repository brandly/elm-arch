module Main exposing (..)

import Dict exposing (Dict)
import Html exposing (Html, button, div, p, text, textarea)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onMouseOver, onMouseOut)
import Round


main =
    Html.beginnerProgram { model = model, view = view, update = update }


type alias Model =
    { content : String
    , selectedLetter : String
    }


model : Model
model =
    { content = ""
    , selectedLetter = ""
    }


type Msg
    = Change String
    | Select String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Change newContent ->
            { model | content = newContent }

        Select letter ->
            { model | selectedLetter = letter }



-- VIEWS


view : Model -> Html Msg
view model =
    let
        clean =
            cleanInput model.content

        letterToCount =
            countLetters clean

        totalChars =
            String.length clean
    in
    div
        [ style
            [ ( "width", "100%" )
            , ( "max-width", "720px" )
            , ( "margin", "0 auto" )
            ]
        ]
        [ textarea
            [ placeholder "type some shit"
            , onInput Change
            , style [ ( "width", "100%" ) ]
            ]
            []
        , div [] [ text ("total letters: " ++ toString totalChars) ]
        , lettersChart letterToCount model.selectedLetter
        ]


lettersChart : Letters -> String -> Html Msg
lettersChart letterToCount selectedLetter =
    let
        letters =
            Dict.keys letterToCount

        letterCountsList =
            List.map (\n -> getCount n letterToCount) letters

        totalChars =
            List.sum letterCountsList

        highestCount =
            let
                highestCount =
                    List.maximum letterCountsList
            in
            case highestCount of
                Nothing ->
                    0

                Just highestCount ->
                    highestCount

        width =
            100 / toFloat (List.length letters)

        column letter =
            let
                count =
                    getCount letter letterToCount

                percentage =
                    toFloat count / toFloat totalChars * 100

                height =
                    toFloat count / toFloat highestCount * 100

                label =
                    if selectedLetter == letter then
                        toString (Round.round 1 percentage) ++ "%"
                    else
                        letter

            in
            div
                [ style
                    [ ( "height", toString height ++ "%" )
                    , ( "width", toString width ++ "%" )
                    , ( "background", "lightblue" )
                    , ( "display", "inline-block" )
                    , ( "vertical-align", "bottom" )
                    ]
                , onMouseOver (Select letter)
                , onMouseOut (Select "")
                ]
                [ p [ style [("text-align", "center")] ] [ text label ]
                ]
    in
    div
        [ style
            [ ( "width", "100%" )
            , ( "height", "100vh" )
            ]
        ]
        (List.map column letters)



--


cleanInput : String -> String
cleanInput input =
    let
        letters =
            String.split "" input

        onlyAlpha =
            List.filter isAlpha letters
    in
    String.join "" onlyAlpha


isAlpha : String -> Bool
isAlpha str =
    if str > "a" && str < "z" then
        True
    else if str > "A" && str < "Z" then
        True
    else
        False


type alias Letters =
    Dict String Int


getCount : String -> Letters -> Int
getCount letter letters =
    let
        count =
            Dict.get letter letters
    in
    case count of
        Nothing ->
            0

        Just count ->
            count


countLetters : String -> Letters
countLetters str =
    let
        charList =
            String.split "" str
    in
    List.foldr addLetter Dict.empty charList


addLetter : String -> Letters -> Letters
addLetter letter counts =
    Dict.update letter incrementLetter counts


incrementLetter : Maybe Int -> Maybe Int
incrementLetter num =
    case num of
        Nothing ->
            Just 1

        Just num ->
            Just (num + 1)
