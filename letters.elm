import Html exposing (Html, button, div, text, input, p)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Dict exposing (Dict)

main =
  Html.beginnerProgram { model = model, view = view, update = update}

type alias Model =
  { content: String
  }

model : Model
model = { content = "" }


type Msg = Change String

update : Msg -> Model -> Model
update msg model =
  case msg of
    Change newContent ->
      { model | content = newContent }

view : Model -> Html Msg
view model =
  let
    letters = countLetters model.content
    totalChars = String.length model.content

    simpleDiv letter =
      let
        count = getCount letter letters
        percentage = toFloat count / toFloat totalChars * 100
      in
        div [ style [ ("width", (toString percentage) ++ "%"), ("background", "lightblue")]]
          [ p [] [text letter]
          , p [] [text (toString percentage ++ "%")]
          ]
  in
    div []
      [ input [ placeholder "type some shit", onInput Change ] []
      , div [] [text ("total letters: " ++ toString totalChars)]
      , div [] (List.map simpleDiv (Dict.keys letters))
      ]


type alias Letters = Dict String Int

getCount : String -> Letters -> Int
getCount letter letters =
  let
    count = Dict.get letter letters
  in
    case count of
      Nothing ->
        0
      Just count ->
        count

countLetters : String -> Letters
countLetters str =
  let
    charList = String.split "" str
  in
    List.foldr addLetter Dict.empty charList

addLetter : String -> Letters -> Letters
addLetter letter counts =
  if Dict.member letter counts then
    Dict.update letter incrementLetter counts
  else
    Dict.insert letter 1 counts

incrementLetter : Maybe Int -> Maybe Int
incrementLetter num =
  case num of
    Nothing -> Just 0
    Just num -> Just (num + 1)
