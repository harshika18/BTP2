module ListView exposing (cell, cells, cell_radius, cell_gap
    , drawcells, no_options)

import Svg exposing (Svg)
import Svg.Attributes as SA
import Svg.Events as SE
import Html.Attributes as HA 
import List.Extra as LE



cell_radius = 40
cell_gap = 20


strokeColor = "#F44336"
regularFill = "#FFF59D"
inplaceFill = "#AED581"


crc_ col strokeCol td r x y = 
    Svg.circle
        [ SA.cx (String.fromFloat x)
        , SA.cy (String.fromFloat y)
        , SA.r (String.fromFloat r)
        , SA.fill col
        , SA.stroke strokeCol
        , SA.strokeWidth "4"
        , HA.style "transition" <| String.replace "{n}" (String.fromFloat td) "all {n}s ease-in-out"
        ]
        []


txt : Float -> Float -> Int -> Svg msg
txt x y v = 
    Svg.text_
        [ SA.x (String.fromFloat x)
        , SA.y (String.fromFloat y)
        , SA.fontSize "40"
        , SA.fill "#112233"
        , SA.textAnchor "middle"
        , SA.dominantBaseline "middle" 
        ]
        [v |> String.fromInt |> Svg.text]


txt_ : Float -> Float -> Float -> String -> Svg msg
txt_ x y fsize v = 
    Svg.text_
        [ SA.x (String.fromFloat x)
        , SA.y (String.fromFloat y)
        , SA.fontSize (String.fromFloat fsize)
        , SA.fill "#112233"
        , SA.textAnchor "middle"
        , SA.dominantBaseline "middle" 
        ]
        [Svg.text v]



cell : Float -> Float -> Int -> Int -> Maybe String -> Bool -> Bool -> msg -> Svg msg
cell x y v idx label selected inplace m = 
    let cfn = 
            case (selected, inplace) of
                (True, True) ->  
                    crc_ "#AED581" strokeColor 0.5 cell_radius
                (True, False) ->
                    crc_ regularFill strokeColor 0.5 cell_radius
                (False, True) ->
                    crc_ "#AED581" "#fff"  1 cell_radius
                (False, False) ->
                    crc_ regularFill "#fff" 1 cell_radius

        idxCrc = 
            if selected then 
                crc_ "#fff" "#F44336" 0.5 (cell_radius / 2) 0 (cell_radius * 2)
            else
                crc_ "#fff" "#fff" 1 (cell_radius / 2) 0 (cell_radius * 2)

    in
        Svg.g
            [ SA.transform 
                ( String.concat
                    [ "translate(" 
                    , (String.fromFloat (x * (2 * cell_radius + cell_gap)))
                    , ", "
                    , (String.fromFloat y)
                    , ")"
                    ]
                )
            , SA.cursor "pointer"
            , SE.onClick m
            ]
            [ cfn 0 0
            , idxCrc 
            , txt 0 0 v
            , txt_ 0 (cell_radius * 2) 25 (String.fromInt idx)
            , case label of
                Nothing -> txt_ 0 (cell_radius * 3) 25 ""
                Just l -> txt_ 0 (cell_radius * 3) 25 l
            ]
        

cells : List Int -> List {v: Int, l: Maybe String} -> (Int -> msg) -> Int -> Svg msg
cells nums labelledSelections m pivot =
    let sortedNums = List.sort nums
        selections = labelledSelections |> List.map (\ls -> ls.v)
        labels = labelledSelections |> List.map (\ls -> ls.l)
    in
        Svg.g
            [ SA.transform 
                ( String.concat
                    [ "translate(" 
                    , (String.fromFloat <| 
                        toFloat (1200 - (((2 * cell_radius + cell_gap) * (List.length nums)) - cell_gap)) / 2)
                    , ", "
                    , (String.fromFloat 100)
                    , ")"
                    ]
                )
            ]
            (List.indexedMap 
                (\ p n -> cell (toFloat p) 0 n p 
                    (labelledSelections 
                    |> (List.filter (\ls -> ls.v == p))
                    |> List.map (\ls -> ls.l)
                    |> List.head
                    |> Maybe.withDefault Nothing)
                    (List.member p selections) 
                    (n <= pivot) 
                    (m p))
                nums)


-- choices for each cell
-- color of cell
-- color of text
-- selected?
-- label?

type alias Options = 
    { cellColor : Maybe String
    , textColor : Maybe String
    , label : Maybe String
    , labelStrokeColor : Maybe String
    }


no_options = 
    { cellColor = Nothing
    , textColor = Nothing
    , label = Nothing
    , labelStrokeColor = Nothing
    }

drawCell : Options -> Bool -> Bool -> Int -> Int -> msg -> Float -> Float -> Svg msg
drawCell opts selected inplace idx v m x y =
    let cellColor = Maybe.withDefault 
                        (if inplace then inplaceFill else regularFill) 
                        opts.cellColor
        textColor = Maybe.withDefault "#000" opts.cellColor
        label = Maybe.withDefault "" opts.label
        labelStrokeColor = Maybe.withDefault 
                            (if selected then strokeColor else "#fff")
                            opts.labelStrokeColor

        cfn = 
            case (selected, inplace) of
                (True, True) ->  
                    crc_ cellColor strokeColor 0.5 cell_radius
                (True, False) ->
                    crc_ cellColor strokeColor 0.5 cell_radius
                (False, True) ->
                    crc_ cellColor "#fff"  1 cell_radius
                (False, False) ->
                    crc_ cellColor "#fff" 1 cell_radius

        idxCrc = 
            if selected then
                crc_ "#fff" labelStrokeColor 0.5 (cell_radius / 2) 0 (cell_radius * 2)
            else
                crc_ "#fff" labelStrokeColor 1 (cell_radius / 2) 0 (cell_radius * 2)

    in
        Svg.g
            [ SA.transform 
                ( String.concat
                    [ "translate(" 
                    , (String.fromFloat (x * (2 * cell_radius + cell_gap)))
                    , ", "
                    , (String.fromFloat y)
                    , ")"
                    ]
                )
            , SA.cursor "pointer"
            , SE.onClick m
            ]
            [ cfn 0 0
            , idxCrc 
            , txt_ 0 0 40 <| String.fromInt v
            , txt_ 0 (cell_radius * 2) 25 (String.fromInt idx)
            , txt_ 0 (cell_radius * 3) 25 label
            ]



drawcells : List (Int, Maybe Options) -> List Int -> (Int -> msg) -> Svg msg
drawcells nums selected m =
    let sortedValues = List.map (\(n, os) -> n) nums |> List.sort
    in
        Svg.g
            [ SA.transform 
                ( String.concat
                    [ "translate(" 
                    , (String.fromFloat <| 
                        toFloat (1200 - 
                            (((2 * cell_radius + cell_gap) * (List.length nums)) - cell_gap)) / 2)
                    , ", "
                    , (String.fromFloat 100)
                    , ")"
                    ]
                )
            ]
            (List.indexedMap 
                (\ p (num, opts) ->
                    case opts of
                        Just opts_ ->
                            drawCell opts_ 
                                (List.member p selected)
                                ((sortedValues |> LE.getAt p |> Maybe.withDefault -1) == num)
                                p
                                num
                                (m p)
                                (toFloat p) 
                                0
                        Nothing ->
                            drawCell no_options
                                (List.member p selected)
                                ((sortedValues |> LE.getAt p |> Maybe.withDefault -1) == num)
                                p
                                num
                                (m p)
                                (toFloat p) 
                                0 
                            )
                nums)