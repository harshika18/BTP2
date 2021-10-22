module PartitionIndicator exposing (..)

import Html exposing (Html)
import Html.Attributes as HA
import List.Extra as LE
-- import Html exposing (i)
-- import Css exposing (num)

partitionedIndicator : List Int -> Int -> Html msg
partitionedIndicator nums pivot =
    let 
        isPartitioned =isPartition nums 0 0 pivot
        partitionedBgColor = "#388E3C"
        bgColor = "#F57C00"
        styles = 
            [ HA.style "background" <| if isPartitioned then partitionedBgColor else bgColor
            , HA.style "color" "#FFFFFF"
            , HA.style "padding" "10px"
            , HA.style "margin" "0 10px"
            ]
        text = if isPartitioned then "PARTITIONED" else "NOT PARTITIONED"
    in
        Html.div
            styles
            [ Html.b [] [ Html.text text]
            ]

isPartition : List Int -> Int -> Int -> Int -> Bool
isPartition nums flag i pivot =
    let
        valI = LE.getAt i nums |> Maybe.withDefault 0
    in
        if flag >= 1 && i == List.length nums then
            True
        else if flag == 0 && i == List.length nums then
            False
        else if valI == pivot && flag == 2 then
            isPartition nums 2 (i+1) pivot
        else if valI == pivot && flag < 2 then
            isPartition nums 1 (i+1) pivot
        else if valI < pivot && flag < 2 then
            isPartition nums 0 (i+1) pivot
        else if valI < pivot && flag == 2 then
            False
        else if valI > pivot && flag == 0 then
            False
        else
            isPartition nums 2 (i+1) pivot
