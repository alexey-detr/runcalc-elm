module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Attribute, Html, div, h1, input, label, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import List exposing (indexedMap)
import String exposing (join, left, padLeft, split)



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
    { seconds = "00", minutes = "0", speed = "0" }



-- UPDATE


type Msg
    = ChangeMinutes String
    | ChangeSeconds String
    | ChangeSpeed String


parseFloat : String -> Float
parseFloat value =
    Maybe.withDefault 0 (String.toFloat value)


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


formatDecimals : Float -> String
formatDecimals value =
    String.fromFloat value |> split "."
        |> indexedMap
            (\n item ->
                if n == 1 then
                    left 2 item

                else
                    item
            )
        |> join "."


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeMinutes value ->
            { model
                | minutes = value
                , speed =
                    formatDecimals <|
                        normalizeInfinity
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
                    formatDecimals <|
                        normalizeInfinity
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
                , seconds = fractional * 60 |> round |> String.fromInt |> padLeft 2 '0'
            }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Pace" ]
        , div []
            [ div []
                [ label [ for "input_min" ] [ text "Minutes" ]
                , input [ id "input_min", value model.minutes, onInput ChangeMinutes ] []
                ]
            , div []
                [ label [ for "input_sec" ] [ text "Seconds" ]
                , input [ id "input_sec", value model.seconds, onInput ChangeSeconds ] []
                ]
            ]
        , div []
            [ label [ for "input_speed" ] [ text "Speed" ]
            , input [ id "input_speed", value model.speed, onInput ChangeSpeed ] []
            ]
        ]
