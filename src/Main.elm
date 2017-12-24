module Main exposing (..)

import Html exposing (Html, text, div, h1, button, input, span)
import Html.Attributes exposing (class, type_, value)
import Html.Events exposing (onClick, targetValue, onInput)
import Random


---- MODEL ----


type alias Model =
    { dieFaces : List Int, numOfDice : Int, numOfFace : Int, restOfDice : Int }


init : ( Model, Cmd Msg )
init =
    ( { dieFaces = [ 1 ], numOfDice = 1, numOfFace = 6, restOfDice = 1 }, Cmd.none )



---- UPDATE ----


type Msg
    = Roll
    | NewFace Int
    | ChangeNumOfDice String
    | ChangeNumOfFace String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ dieFaces, numOfDice, numOfFace, restOfDice } as model) =
    case msg of
        ChangeNumOfFace str ->
            let
                n =
                    Result.withDefault numOfFace <| String.toInt str
            in
                ( { model | numOfFace = n }, Cmd.none )

        ChangeNumOfDice str ->
            let
                n =
                    Result.withDefault numOfFace <| String.toInt str
            in
                ( { model | numOfDice = n }, Cmd.none )

        Roll ->
            ( { model | dieFaces = [], restOfDice = numOfDice - 1 }, Random.generate NewFace (Random.int 1 numOfFace) )

        NewFace newFace ->
            if restOfDice == 0 then
                ( { model | dieFaces = newFace :: dieFaces }, Cmd.none )
            else if restOfDice >= 0 then
                ( { model | dieFaces = newFace :: dieFaces, restOfDice = restOfDice - 1 }, Random.generate NewFace (Random.int 1 numOfFace) )
            else
                ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view ({ dieFaces, numOfDice, numOfFace } as model) =
    let
        dieFaceResults =
            List.map (\dieFace -> h1 [ class "die-face" ] [ text (toString dieFace) ]) dieFaces
    in
        div []
            [ div [ class "die-faces" ] dieFaceResults
            , div [ class "die-setting" ]
                [ input [ class "die", type_ "number", value <| toString numOfDice, onInput ChangeNumOfDice ] []
                , span [] [ text "d" ]
                , input [ class "die", type_ "number", value <| toString numOfFace, onInput ChangeNumOfFace ] []
                ]
            , button [ onClick Roll ] [ text "Roll" ]
            ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
