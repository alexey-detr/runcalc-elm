module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Attribute, Html, div, h1, input, label, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { seconds : String
    , minutes : String
    , speed : String
    }


init : Model
init =
    { seconds = "0", minutes = "0", speed = "0" }



-- UPDATE


type Msg
    = ChangeMinutes String
    | ChangeSeconds String
    | ChangeSpeed String


parseFloat : String -> Float
parseFloat value =
    case String.toFloat value of
        Just parsed ->
            parsed

        Nothing ->
            0


normalizeInfinity : Float -> Float
normalizeInfinity value =
    case isInfinite value of
        True ->
            0

        False ->
            value


integralFractional : Float -> ( Float, Float )
integralFractional value =
    let
        truncated =
            value |> truncate |> toFloat
    in
    ( value - truncated, truncated )


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeMinutes value ->
            { model
                | minutes = value
                , speed =
                    String.fromFloat <| normalizeInfinity
                        (60
                            / (parseFloat value
                                + parseFloat model.seconds
                                / 60
                              )
                        )
            }

        ChangeSeconds value ->
            { model
                | seconds = value
                , speed =
                    String.fromFloat <| normalizeInfinity
                        (60
                            / (parseFloat model.minutes
                                + parseFloat value
                                / 60
                              )
                        )
            }

        ChangeSpeed value ->
            let
                ( fractional, integral ) =
                    60 / (value |> parseFloat) |> normalizeInfinity |> integralFractional
            in
            { model
                | speed = value
                , minutes = integral |> String.fromFloat
                , seconds = fractional * 60 |> round |> String.fromInt
            }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Tempo" ]
        , div []
            [ label []
                [ text "Minutes"
                , input [ value model.minutes, onInput ChangeMinutes ] []
                ]
            , label []
                [ text "Seconds"
                , input [ value model.seconds, onInput ChangeSeconds ] []
                ]
            ]
        , div []
            [ label []
                [ text "Speed"
                , input [ value model.speed, onInput ChangeSpeed ] []
                ]
            ]
        ]
