module P5 exposing (..)

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
import QSCore exposing (Selections(..))
import Random
import Random.Array as RA
import Svg 
import Svg.Attributes as SA
import PartitionAlgorithm exposing (Msg(..))
import PartitionIndicator as PI
import Json.Decode exposing (string)


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
    , i : Int
    , j : Int
    , cur_mode : String
    , prev_mode : String
    }


type Msg
    = Init (Array Int)
    | InitPivot
    | Next
    | NoMsg


init : () -> ( Model, Cmd Msg )
init _ =
    ( { numbers = []
      , selections = NoneSelected
      , prompt = ( """Click on the 'Next' button to start the algorithm.""", P.PromptInfo )
      , pivot = 0
      , i = 0
      , j = 0
      , prev_mode = ""
      , cur_mode = "Incr"
      }
    , Cmd.batch
        [ Random.generate
            Init
            (RA.rangeLengthArray 8 10 (Random.int 1 100))
        ]
    )


view : Model -> Html Msg
view model =
    let
        { numbers, selections, prompt, pivot, prev_mode, cur_mode } =
            model
    in
    Html.div
        [ HA.class "experiment-container" ]
        [ Html.div
            [ HA.class "feedback-container" ]
            [ P.show prompt
            , PI.partitionedIndicator numbers pivot
            ]
        , Html.div
                  [ HA.style "display" "flex"
                  , HA.style "justify-content" "center"
                  , HA.style "flex-grow" "1"
                  , HA.style "align-items" "center"
                  ]
                  [ viewMode prev_mode cur_mode
                  ]
        , Html.div
            [ HA.class "observables-container" ]
            [ viewNums numbers selections pivot
            ]
        , Html.div
            [ HA.class "controls-container" ]
            [ Html.button [ HE.onClick Next, HA.class "button__action--primary" ] [ Html.text "Next" ]
            ]
        ]


next : Model -> Model
next model =
    let
        { numbers, selections, pivot, i, j, prev_mode, cur_mode } =
            model
    in
    case selections of
        BothSelected x y ->
            let
                valI = LE.getAt i numbers |> Maybe.withDefault 0
                valJ = LE.getAt j numbers |> Maybe.withDefault 0
                swappedNums =
                    LE.swapAt i j numbers
            in
            if cur_mode == "Place"
            then
            { model
                | prompt = ( "Algorithm terminates here.", P.PromptSuccess )
                , selections = NoneSelected
                , numbers = LE.swapAt 0 j numbers
                , cur_mode = "Exit"
                , prev_mode = "Place"
            }
            else if valI <= pivot && i < List.length numbers
            then
            { model
                | prompt = ( "Incremented 'i', please click on 'next' for the next operation", P.PromptSuccess )
                , selections = BothSelected (i + 1) j
                , i = i + 1
                , prev_mode = cur_mode
                , cur_mode = "Incr"
            }
            else if valJ >= pivot && j >= 0
            then
            { model
                | prompt = ( "Decremented 'j', please click on 'next' for the next operation", P.PromptSuccess )
                , selections = BothSelected i (j - 1)
                , j = j - 1
                , prev_mode = cur_mode
                , cur_mode = "Decr"
            }
            else if i < j && valI > pivot && valJ < pivot
            then
            { model
                | prompt = ( "Ordered the numbers at indices "++ (String.fromInt i) ++ " and "++ (String.fromInt j) ++ ", please click on 'next' for next operation", P.PromptSuccess )
                , selections = BothSelected i j
                , numbers = swappedNums
                , prev_mode = cur_mode
                , cur_mode = "Order"
            }
            else if i>= j
            then
            { model
                | prompt = ( "Placing the pivot on index "++ (String.fromInt j) ++ ". Please click on 'next' for next operation", P.PromptSuccess )
                , selections = BothSelected i j
                , prev_mode = cur_mode
                , cur_mode = "Place"
            }
            else
                model
        _ ->
            model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        { numbers, selections, i, j } =
            model
    in
    case msg of
        Next ->
            ( next model, Cmd.none )
        
        Init arr ->
            update InitPivot { model
                | numbers = Array.toList arr
                , j = List.length (Array.toList arr) - 1
           }
            
        InitPivot -> 
            (
                { model
                | pivot = LE.getAt 0 numbers |> Maybe.withDefault 0
                , selections = BothSelected i j
            }
            , Cmd.none
            )
        _ -> (model, Cmd.none)

selectionsListWithLabels : Selections -> List { v : Int, l : Maybe String }
selectionsListWithLabels selections =
    case selections of
        NoneSelected ->
            []

        OneSelected i ->
            [ { v = i, l = Just "k" } ]

        BothSelected i j -> 
            if i == j
            then
                [ { v = i, l = Just "i, j" } ]
            else
                [ { v = i, l = Just "i" }, { v = j, l = Just "j" }]

viewNums : List Int -> Selections -> Int -> Html Msg
viewNums nums selections pivot =
    let
        lwidth =
            1100

        msg = 
            (\ci -> NoMsg)

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

viewMode prev_mode cur_mode =
    Html.span 
        [ HA.style "font-size" "x-large"
        , HA.style "background-color" "#D5FAFC"
        , HA.style "padding" "0.5em"
        ] 
        [ Html.text ("Previous Mode : " ++ prev_mode ++ "  , Current Mode : " ++ cur_mode)
        ]
