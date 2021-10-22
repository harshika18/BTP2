module P4 exposing (..)

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
import QSCore exposing (Selections(..), deselect)
import Random
import Random.Array as RA
import Svg 
import Svg.Attributes as SA
import PartitionAlgorithm exposing (Msg(..))
import PartitionIndicator as PI



main =
    Browser.element
        { init = Core.init identity analytics init
        , view = Core.view view
        , update = Core.update identity analytics update setFresh Nothing Nothing
        , subscriptions = Core.subscriptions subscriptions
        }

select : Int -> Selections -> (Selections, P.Prompt)
select i selections =
    case selections of
        _ ->
            (OneSelected i, ("Click the 'Place Pivot' button to swap the selected number with pivot.",P.PromptSuccess))

type alias Model =
    { numbers : List Int
    , selections : Selections
    , prompt : P.Prompt
    , pivot : Int
    , i : Int
    , j : Int
    }


type Msg
    = Init (Array Int)
    | Select Int
    | PlacePivot
    | IncI
    | DecJ
    | Order
    | Deselect Int
    | InitPivot


init : () -> ( Model, Cmd Msg )
init _ =
    ( { numbers = []
      , selections = NoneSelected
      , prompt = ( """Click on the buttons to start the algorithm.""", P.PromptInfo )
      , pivot = 0
      , i = 0
      , j = 0
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
            [ Html.button [ HE.onClick IncI, HA.class "button__action--primary" ] [ Html.text "Increment i" ]
            , Html.button [ HE.onClick DecJ, HA.class "button__action--primary" ] [ Html.text "Decrement j" ]
            , Html.button [ HE.onClick Order, HA.class "button__action--primary" ] [ Html.text "Order" ]
            , Html.button [ HE.onClick PlacePivot, HA.class "button__action--primary" ] [ Html.text "Place Pivot" ]
            ]
        ]

incI : Model -> Model
incI model =
    let
        { numbers, selections, pivot, i, j } =
            model
    in
    case selections of
        OneSelected x ->
            { model
                | prompt = ( "Click on Place Pivot botton to place the pivot.", P.PromptSuccess )
            }
        BothSelected x y ->
            let
                valI = LE.getAt i numbers |> Maybe.withDefault 0
            in
            if valI <= pivot && i < List.length numbers - 1
            then
            { model
                | prompt = ( "Incremented 'i', please perform the next operation", P.PromptSuccess )
                , selections = BothSelected (i + 1) j
                , i = i + 1
            }
            else if valI > pivot
            then
            { model
                | prompt = ( "Can not increase 'i' because ith element is greater then pivot", P.PromptSuccess )
                , selections = BothSelected i j
            }
            else
            { model
                | prompt = ( "You have been reached the end. Please perform some other operation.", P.PromptSuccess )
                , selections = BothSelected i j
            }
        _ ->
            model

decJ : Model -> Model
decJ model =
    let
        { numbers, selections, pivot, i, j } =
            model
    in
    case selections of
        OneSelected x ->
            { model
                | prompt = ( "Click on Place Pivot botton to place the pivot.", P.PromptSuccess )
            }
        BothSelected x y ->
            let
                valJ = LE.getAt j numbers |> Maybe.withDefault 0
            in
            if valJ >= pivot && j > 0
            then
            { model
                | prompt = ( "Decremented 'j', please perform the next operation", P.PromptSuccess )
                , selections = BothSelected i (j - 1)
                , j = j - 1
            }
            else if valJ < pivot
            then
            { model
                | prompt = ( "Can not decrease 'j' because jth element is less than pivot", P.PromptSuccess )
                , selections = BothSelected i j
            }
            else
            { model
                | prompt = ( "You have been reached the starting point. Please perform some other operation.", P.PromptSuccess )
                , selections = BothSelected i j
            }
        _ ->
            model

order : Model -> Model
order model =
    let
        { numbers, selections, pivot, i, j } =
            model
    in
    case selections of
        OneSelected x ->
            { model
                | prompt = ( "Click on Place Pivot botton to place the pivot.", P.PromptSuccess )
            }
        BothSelected x y ->
            let
                valI = LE.getAt i numbers |> Maybe.withDefault 0
                valJ = LE.getAt j numbers |> Maybe.withDefault 0
                swappedNums =
                    LE.swapAt i j numbers
            in
            if i < j && valI > pivot && valJ < pivot
            then
            { model
                | prompt = ( "Ordered the numbers at indices "++ (String.fromInt i) ++ " and "++ (String.fromInt j), P.PromptSuccess )
                , selections = BothSelected i j
                , numbers = swappedNums
            }
            else
            { model
                | prompt = ( "Can not order the elements, as either ith element < pivot or jth element > pivot", P.PromptSuccess )
                , selections = BothSelected i j
            }
        _ ->
            model

placepivot : Model -> Model
placepivot model =
    let
        { numbers, selections, pivot, i, j } =
            model
    in
    case selections of
        BothSelected x y ->
            { model
                | prompt = ( "Select one element in order to place the pivot", P.PromptSuccess )
            }
        OneSelected x ->
            let
                selections_ =
                    BothSelected i j
                valI = LE.getAt x numbers |> Maybe.withDefault 0
                swappedNums =
                    LE.swapAt x 0 numbers
            in
            if valI <= pivot
            then
            { model
                | prompt = ( "Placed the pivot on the selected place.", P.PromptSuccess )
                , selections = selections_
                , numbers = swappedNums
            }
            else
            { model
                | prompt = ( "Can't perform the operation. Value of selected element should be less than or equal to pivot", P.PromptSuccess )
                , selections = BothSelected i j
            }
        _ ->
            model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        { numbers, selections, i, j } =
            model
    in
    case msg of
        PlacePivot ->
            ( placepivot model, Cmd.none )
        
        IncI ->
            ( incI model, Cmd.none )

        DecJ ->
            ( decJ model, Cmd.none )
        
        Order ->
            ( order model, Cmd.none )

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

        Deselect x ->
            let
                ( selections_, pm ) =
                    deselect x selections
            in
            ( { model
                | selections = selections_
                , prompt = pm
              }
            , Cmd.none
            )
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
            case selections of
                _ ->
                    \ci -> Select ci

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
