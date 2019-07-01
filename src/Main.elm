module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Attribute, Html, div, h1, input, label, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
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
    value
        |> String.toFloat
        |> Maybe.withDefault 0


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
    value
        |> String.fromFloat
        |> split "."
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
                    (60
                        / (parseFloat value
                            + parseFloat model.seconds
                            / 60
                          )
                    )
                        |> normalizeInfinity
                        |> formatDecimals
            }

        ChangeSeconds value ->
            { model
                | seconds = value
                , speed =
                    (60
                        / (parseFloat model.minutes
                            + parseFloat value
                            / 60
                          )
                    )
                        |> normalizeInfinity
                        |> formatDecimals
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
    div [ class "application" ]
        [ h1 [] [ text "Runcalc 3000" ]
        , div [ class "wrapper_pace" ]
            [ div [ class "block_pace" ]
                [ label [ for "input_min" ] [ text "Minutes" ]
                , input [ id "input_min", class "input_minutes", maxlength 2, type_ "tel", pattern "[0-9]*", value model.minutes, onInput ChangeMinutes ] []
                ]
            , div [ class "meta_time-separator" ] [ text ":" ]
            , div [ class "pace-block" ]
                [ label [ for "input_sec" ] [ text "Seconds" ]
                , input [ id "input_sec", class "input_seconds", maxlength 2, type_ "tel", pattern "[0-9]*", value model.seconds, onInput ChangeSeconds ] []
                ]
            ]
        , div [ class "wrapper_speed" ]
            [ div [ class "block_speed" ]
                [ label [ for "input_speed" ] [ text "Speed" ]
                , div [ class "wrapper_speed-input" ]
                    [ input [ id "input_speed", class "input_speed", type_ "tel", pattern "[0-9\\.]*", value model.speed, onInput ChangeSpeed ] []
                    ]
                ]
            , div [ class "meta_speed-description" ] [ text "km/h" ]
            ]
        ]
