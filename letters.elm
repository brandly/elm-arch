import Html exposing (Html, button, div, text, input, p)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Dict exposing (Dict)
import Round

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

-- VIEWS

view : Model -> Html Msg
view model =
  let
    clean = cleanInput model.content

    letterToCount = countLetters clean
    totalChars = String.length clean

  in
    div
      [ style
        [ ("width", "100%")
        , ("max-width", "720px")
        , ("margin", "0 auto")
        ]]
      [ input [ placeholder "type some shit", onInput Change, style [("width", "100%")] ] []
      , div [] [text ("total letters: " ++ toString totalChars)]
      , lettersChart letterToCount
      ]

lettersChart : Letters -> Html Msg
lettersChart letterToCount =
  let
    letters = Dict.keys letterToCount

    letterCountsList = List.map (\n -> getCount n letterToCount) letters
    totalChars = List.sum letterCountsList

    highestCount =
      let
        highestCount = List.maximum letterCountsList
      in
        case highestCount of
          Nothing -> 0
          Just highestCount -> highestCount
    width = 100 / toFloat (List.length letters)
    column letter =
      let
        count = getCount letter letterToCount
        percentage = toFloat count / toFloat totalChars * 100
        height = toFloat count / toFloat highestCount * 100
      in
        div
          [ style
            [ ("height", (toString height) ++ "%")
            , ("width", (toString width) ++ "%")
            , ("background", "lightblue")
            , ("display", "inline-block")
            , ("vertical-align", "bottom")
            ]]
          [ p [] [text letter]
          , p [] [text (toString (Round.round 1 percentage) ++ "%")]
          ]

  in
    div
      [ style
        [ ("width", "100%")
        , ("height", "100vh")
      ]]
      (List.map column letters)

--

cleanInput : String -> String
cleanInput input =
  let
    letters = String.split "" input
    onlyAlpha = List.filter isAlpha letters
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
  Dict.update letter incrementLetter counts

incrementLetter : Maybe Int -> Maybe Int
incrementLetter num =
  case num of
    Nothing -> Just 1
    Just num -> Just (num + 1)
