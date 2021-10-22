module PartitionAlgorithm exposing (..)

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
import Parser exposing (number)
import PartitionIndicator as PI
-- import Parser exposing (number)
-- import Array exposing (length)
-- import Parser.Advanced exposing (number)
-- import Svg.Attributes exposing (mode)


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
    | NoMsg
    | Next
    

init : () -> ( Model, Cmd Msg )
init _ =
    ( { numbers = []
      , selections = BothSelected 0 1
      , prompt = ( """Click on the 'Next' button to step through the algorithm.""", P.PromptInfo )
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
            , PI.partitionedIndicator numbers pivot
            ]
        , Html.div
            [ HA.class "observables-container" ]
            [ viewNums numbers selections pivot
            ]
        , Html.div
            [ HA.class "controls-container" ]
            [ Html.button [ HE.onClick Next, HA.class "button__action--primary" ] [ Html.text "Next" ] ]
        ]





update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        { numbers, selections } =
            model
    in
    case msg of
        Next ->
            ( next model, Cmd.none )

        Init arr ->
            ( { model
                | numbers = Array.toList arr
            }
            , Cmd.none
            )
        _ -> (model, Cmd.none)

next : Model -> Model
next model =
    let
        { numbers, selections, pivot } =
            model
    in
    case selections of
        BothSelected i j ->    
            let
                posi = Tuple.first (selectPos numbers pivot i j)
                posj = Tuple.second (selectPos numbers pivot i j)
                selections_ =
                    BothSelected posi posj 
                    
                swapNums =
                    swapNum numbers pivot i j
            in
            if j == List.length numbers - 1 then
            { model
                | prompt = ( "Partition Algorithm terminates here.  See if the array is partitioned.", P.PromptSuccess )
                , selections = selections_
                , numbers = swapNums
            }
            else
            { model
                | prompt = ( "Click on the 'Next' button to step through the algorithm.", P.PromptSuccess )
                , selections = selections_
                , numbers = swapNums
            }

        _ -> model


selectionsListWithLabels : Selections -> List { v : Int, l : Maybe String }
selectionsListWithLabels selections =
    case selections of
        NoneSelected ->
            []

        OneSelected i ->
            [ { v = i, l = Nothing } ]

        BothSelected i j ->
            [ { v = i, l = Just "i" }, { v = j, l = Just "j" } ]


viewNums : List Int -> Selections -> Int -> Html Msg
viewNums nums selections pivot =
    let
        lwidth =
            1100
        msg = (\ci -> NoMsg)

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



selectPos : List Int -> Int -> Int -> Int ->  (Int, Int) 
selectPos numbers pivot i j =
    let
        len = List.length numbers
        valI = LE.getAt i numbers |> Maybe.withDefault 0
        valJ = LE.getAt j numbers |> Maybe.withDefault 0
    in
    if j < len - 1 then
        if valI > pivot && valJ > pivot then
            (i,j+1)
        else if valI > pivot && valJ <= pivot then 
            (i,j)
        else 
            (i+1,j+1)
    else
        (i,j)
    
    
swapNum : List Int -> Int -> Int -> Int -> List Int
swapNum numbers pivot i j =
    let
        valI = LE.getAt i numbers |> Maybe.withDefault 0
        valJ = LE.getAt j numbers |> Maybe.withDefault 0

    in
    if valI > pivot && pivot >= valJ then
        LE.swapAt i j numbers
    else
        numbers
