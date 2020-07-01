module Bingo exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

-- MODEL
type alias Model =
    {name : String, gameNumber : Int, entries : List Entry}
type alias Entry =
    {
    id : Int,
    phrase : String,
    points : Int,
    marked : Bool
    }

initialModel : Model
initialModel =
    { name = "Mike"
    , gameNumber = 1
    , entries = initialEntries}

initialEntries : List Entry
initialEntries =
    [
    Entry 1 "Future-Proof" 200 False
    , Entry 2 "Doing Agile" 100 True
    , Entry 3 "In The Cloud" 300 False
    , Entry 4 "Rock-Star Ninja" 400 False
    ]

-- UPDATE


type Msg = NewGame | Mark Int | Sort


update : Msg -> Model -> Model
update msg model =
    case msg of
        NewGame ->
             { model | gameNumber = model.gameNumber + 1, entries = initialEntries }
        Mark id ->
            let
                markEntry e =
                    if e.id == id then
                        {e | marked = (not e.marked)}
                    else
                        e
            in
                { model | entries = List.map markEntry model.entries}
        Sort ->
            { model | entries = List.sortBy .points model.entries}


-- VIEW

playerInfo : String -> Int -> String
playerInfo name gameNumber =
    name ++ " - Game #" ++ (toString gameNumber)


viewPlayer : String -> Int -> Html msg
viewPlayer name gameNumber =
    let
        playerInfoText  =
            playerInfo name gameNumber
                |> String.toUpper
                |> text
    in
        h2 [ id "info", class "classy"]
            [ playerInfoText ]

viewHeader : String -> Html Msg
viewHeader title =
    header []
        [h1 [][text title]]

viewFoother : Html Msg
viewFoother =
    footer []
        [ a [ href "http://elm-lang.org"]
            [ text "Powered by Elm"]
        ]


viewEntryItem : Entry -> Html Msg
viewEntryItem entry =
    li [ classList[ ("marked", entry.marked)] , onClick (Mark entry.id)]
        [ span [ class "phrase" ] [ text entry.phrase ]
        , span [ class "points" ] [ text (toString entry.points) ]
        ]

viewEntryList : List Entry -> Html Msg
viewEntryList entries =
    let
        listOfEntries = List.map viewEntryItem entries
    in
        ul [ ] listOfEntries


sumMarketPoints : List Entry -> Int
sumMarketPoints entries =
    entries
        |> List.filter .marked
        |> List.foldl (\e sum -> sum + e.points) 0
        --|> List.map .points
        --|> List.sum


viewScore : Int -> Html Msg
viewScore sum =
    div
        [ class "score"]
        [ span [ class "label"] [ text "Score"]
        , span [ class "value"] [ text (toString sum)]
        ]


view : Model  -> Html Msg
view model =
    div [ class "content"]
        [ viewHeader (String.toUpper "buzzword")
        , viewPlayer model.name model.gameNumber
        , viewEntryList model.entries
        , viewScore (sumMarketPoints model.entries)
        , div [ class "button-group"] [ button [ onClick NewGame][ text "New Game"], button [onClick Sort][ text "Sort"]]
        , div [ class "debug"] [ text (toString model)]
        , viewFoother
        ]

--main : Html Msg
--main =
--    view initialModel

main : Program Never Model Msg
main =
    Html.beginnerProgram
    {
     model = initialModel
    , view = view
    , update = update
    }