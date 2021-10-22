module Swap exposing (..)

import AnalyticsPort exposing (analytics)
import Array exposing (Array)
import Browser
import Core
import Core.Prompt as P
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import List.Extra as LE
import ListView as LV
import QSCore exposing (Selections(..), deselect, select)
import Random
import Random.Array as RA
import Svg 
import Svg.Attributes as SA


main =
    Browser.element
        { init = Core.init identity analytics init
        , view = Core.view view
        , update = Core.update identity analytics update setFresh Nothing Nothing
        , subscriptions = Core.subscriptions subscriptions
        }

type alias Model =
    { numbers : List Int
    , selections : Selections
    , prompt : P.Prompt
    , pivot : Int
    }


type Msg
    = Init (Array Int)
    | Select Int
    | Swap
    | Deselect Int


init : () -> ( Model, Cmd Msg )
init _ =
    ( { numbers = []
      , selections = NoneSelected
      , prompt = ( """Click on any numbered circle to toggle the selection. You can select upto two numbers.""", P.PromptInfo )
      , pivot = 1
      }
    , Cmd.batch
        [ Random.generate
            Init
            (RA.rangeLengthArray 8 10 (Random.int 1 2))
        ]
    )


view : Model -> Html Msg
view model =
    let
        { numbers, selections, prompt, pivot } =
            model
    in
    Html.div
        [ HA.class "experiment-container" ]
        [ Html.div
            [ HA.class "feedback-container" ]
            [ P.show prompt
            ]
        , Html.div
            [ HA.class "observables-container" ]
            [ viewNums numbers selections pivot
            ]
        , Html.div
            [ HA.class "controls-container" ]
            [ Html.button [ HE.onClick Swap, HA.class "button__action--primary" ] [ Html.text "Swap" ] ]
        ]





update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        { numbers, selections } =
            model
    in
    case msg of
        Swap ->
            ( swap model, Cmd.none )

        Select x ->
            let
                ( selections_, pm ) =
                    select x selections
            in
            ( { model
                | selections = selections_
                , prompt = pm
              }
            , Cmd.none
            )

        Deselect i ->
            let
                ( selections_, pm ) =
                    deselect i selections
            in
            ( { model
                | selections = selections_
                , prompt = pm
              }
            , Cmd.none
            )

        Init arr ->
            ( { model
                | numbers = Array.toList arr
              }
            , Cmd.none
            )


swap : Model -> Model
swap model =
    let
        { numbers, selections } =
            model
    in
    case selections of
        NoneSelected ->
            { model
                | prompt = ( "Select 2 numbers to apply this operation", P.PromptDanger )
            }

        OneSelected i ->
            { model
                | prompt = ( "Select One more number to apply this operation", P.PromptDanger )
            }

        BothSelected i j ->
            let
                selections_ =
                    NoneSelected

                swappedNums =
                    LE.swapAt i j numbers
            in
            { model
                | prompt = ( "Select two numbers and then click 'Swap'", P.PromptSuccess )
                , selections = selections_
                , numbers = swappedNums
            }








selectionsListWithLabels : Selections -> List { v : Int, l : Maybe String }
selectionsListWithLabels selections =
    case selections of
        NoneSelected ->
            []

        OneSelected i ->
            [ { v = i, l = Nothing } ]

        BothSelected i j ->
            [ { v = i, l = Nothing }, { v = j, l = Nothing } ]


viewNums : List Int -> Selections -> Int -> Html Msg
viewNums nums selections pivot =
    let
        lwidth =
            1100

        msg =
            case selections of
                NoneSelected ->
                    \ci -> Select ci

                OneSelected i ->
                    \ci ->
                        if ci == i then
                            Deselect ci

                        else
                            Select ci

                BothSelected i j ->
                    \ci ->
                        if ci == i || ci == j then
                            Deselect ci

                        else
                            Select ci
    in
    Html.div
        [ HA.style "display" "flex"
        , HA.style "justify-content" "center"
        , HA.style "flex-grow" "1"
        ]
        [ Svg.svg
            [ SA.viewBox <| "0 0 " ++ String.fromInt lwidth ++ " 300"
            , HA.style "height" "100%"
            , HA.style "width" "100%"
            ]
            [ LV.cells nums (selectionsListWithLabels selections) msg pivot
            ]
        ]
setFresh msg =
    case msg of
        Init _ ->
            True

        _ ->
            False


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
