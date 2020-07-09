module Bingo exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Random
import Http
import Json.Decode as Decode exposing (Decoder, field, succeed)
import Json.Encode as Encode

import  ViewHelpers exposing (..)
import Entry exposing (..)


-- MODEL

type GameState = EnteringName | Playing

type alias  Score =
    { id : Int
    , name : String
    , score : Int
    }

type alias Model =
    { name : String
    , gameNumber : Int
    , entries : List Entry.Entry
    , alertMessage : Maybe String
    , nameInput : String
    , gameState : GameState
    }

initialModel : Model
initialModel =
    { name = "Mike"
    , gameNumber = 1
    , entries = []
    , alertMessage = Nothing
    , nameInput = ""
    , gameState = Playing
    }


-- UPDATE


type Msg
    = NewGame
    | Mark Int
    | NewRandom Int
    | SortScore
    | NewEntries (Result Http.Error (List Entry.Entry))
    | CloseAlert
    | ShareScore
    | NewScore (Result Http.Error Score)
    | SetNameMessage String
    | SaveName
    | CancelName
    | ChangeGameState GameState


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeGameState state ->
            ( { model | gameState = state }, Cmd.none)
        SetNameMessage value ->
            ( { model | nameInput = value }, Cmd.none)
        SaveName ->
            if String.isEmpty model.nameInput then
                ( model, Cmd.none)
            else
                ( { model | name = model.nameInput,
                        nameInput = "",
                        gameState = Playing }, Cmd.none)
        CancelName ->
            ( { model | nameInput = "" }, Cmd.none)

        NewRandom randomNumber ->
            ( { model | gameNumber = randomNumber }, Cmd.none )
        SortScore ->
            ( { model | entries = List.sortBy .points model.entries}, Cmd.none )

        ShareScore ->
            ( model, postScore model )

        NewScore (Ok score) ->
            let
                message =
                    "Your score of "
                        ++ (toString score.score)
                        ++ " was successfully shared!"

            in
                ( { model | alertMessage = Just message}, Cmd.none )

        NewScore (Err error) ->
            ( { model | alertMessage = Just(httpErrorToMessage error)}, Cmd.none )

        NewGame ->
            ( { model | alertMessage = Nothing, gameNumber = model.gameNumber + 1 }, getEntries )

        NewEntries (Ok randomEntries) ->
            ( { model | entries = randomEntries }, Cmd.none)

        NewEntries (Err error) ->
            ( {model | alertMessage = Just (httpErrorToMessage error) }, Cmd.none)

        Mark id ->
             ( {model | entries = Entry.markEntryWithId model.entries id }, Cmd.none)
        CloseAlert ->
            ( { model | alertMessage = Nothing }, Cmd.none )

httpErrorToMessage : Http.Error -> String
httpErrorToMessage error =
   case error of
       Http.NetworkError ->
           "Is the server running?"

       Http.BadStatus response ->
           (toString response.status)

       Http.BadPayload message _ ->
           "Decoding Failed: " ++ message

       _ ->
           (toString error)


-- DECODERS/ENCODER

scoreDecoder : Decoder Score
scoreDecoder =
    Decode.map3 Score
        (field "id" Decode.int)
        (field "name" Decode.string)
        (field "score" Decode.int)

encodeScore : Model -> Encode.Value
encodeScore model =
    Encode.object
        [ ("name", Encode.string model.name)
        , ("score", Encode.int (Entry.sumMarkedPoints model.entries))
        ]


-- COMMANDS

getEntries : Cmd Msg
getEntries =
    Entry.getEntries NewEntries entriesUrl

apiUrlPrefix : String
apiUrlPrefix =
    "http://localhost:3000"

entriesUrl : String
entriesUrl =
    apiUrlPrefix
        ++ "/random-entries"


generateRandomNumber : Cmd Msg
generateRandomNumber =
    Random.generate NewRandom (Random.int 1 100)



postScore : Model -> Cmd Msg
postScore model =
    let
        url =
            apiUrlPrefix
                ++ "/scores"
        body =
            encodeScore model
                |> Http.jsonBody

        request =
            Http.post url body scoreDecoder
    in
        Http.send NewScore request

-- VIEW

view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ viewHeader "BUZZWORD BINGO"
        , viewPlayer model.name model.gameNumber
        , alert CloseAlert model.alertMessage
        , viewNameInput model
        , Entry.viewEntryList Mark model.entries
        , viewScore (Entry.sumMarkedPoints model.entries)
        , div [ class "button-group" ]
            [ primaryButton NewGame "New Game"
            , button [ class "primary", onClick ShareScore, disabled (hasZeroScore model)] [ text "Share Score" ]
            , primaryButton SortScore "Sort Score"
            ]
        , div [ class "debug" ] [ text (toString model) ]
        , viewFooter
        ]


viewNameInput : Model -> Html Msg
viewNameInput model =
    case model.gameState of
        EnteringName ->
            div [ class "name-input" ]
                [ input
                    [ type_ "text"
                    , placeholder "Who's playing?"
                    , autofocus True
                    , value model.nameInput
                    , onInput SetNameMessage
                    ]
                    []
                , primaryButton SaveName "Save"
                , primaryButton CancelName "Cancel"
                ]
        Playing ->
            text ""


hasZeroScore : Model -> Bool
hasZeroScore model =
    (sumMarkedPoints model.entries) == 0


viewPlayer : String -> Int -> Html Msg
viewPlayer name gameNumber =
    h2 [ id "info", class "classy" ]
        [ a  [ href "#", onClick (ChangeGameState EnteringName)]
            [text name]
        , text (" - Game #" ++ (toString gameNumber))
        ]


viewHeader : String -> Html Msg
viewHeader title =
    header []
        [ h1 [] [ text title ] ]


viewFooter : Html Msg
viewFooter =
    footer []
        [ a [ href "http://elm-lang.org" ]
            [ text "Powered By Elm" ]
        ]



viewScore : Int -> Html Msg
viewScore sum =
    div
        [ class "score" ]
        [ span [ class "label" ] [ text "Score" ]
        , span [ class "value" ] [ text (toString sum) ]
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, getEntries )
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none )
        }

