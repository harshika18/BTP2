module QSCore exposing (..)
import List.Extra as LE
import Core.Prompt as P


type Selections
    = NoneSelected
    | OneSelected Int
    | BothSelected Int Int

select : Int -> Selections -> (Selections, P.Prompt)
select i selections =
    case selections of
        NoneSelected ->
            (OneSelected i, ("Select one more number and then click the 'Swap' button",P.PromptSuccess))
        OneSelected j ->
            (BothSelected j i, ("Click the 'Swap' button to swap the selected numbers.",P.PromptSuccess))
        BothSelected j k ->
            (BothSelected j k, ("Two numbers are already selected.",P.PromptInfo))




validIdx : Int -> List a -> Bool
validIdx i lst =
    (i < List.length lst) && (i >= 0)


validSelections : Selections -> List Int -> Bool
validSelections selections nums = 
    case selections of
        NoneSelected -> True
        OneSelected i -> validIdx i nums
        BothSelected i j -> (validIdx i nums) && (validIdx j nums)


deselect : Int -> Selections -> (Selections, P.Prompt)
deselect i selections =
    case selections of
        NoneSelected ->
            (NoneSelected, ("",P.PromptInfo))
        OneSelected j ->
            if (i == j) then 
                (NoneSelected, ("Select two numbers to swap.",P.PromptInfo))
            else
                (OneSelected j, ("",P.PromptInfo))
        BothSelected j k ->
            if (i == j) then
                (OneSelected k, ("Select one more number.",P.PromptInfo))
            else
                if (i == k) then
                    (OneSelected j, ("Select one more number.",P.PromptInfo))
                else (BothSelected j k, ("Click the 'Swap' button to swap the selected numbers.",P.PromptInfo))


isSelected : Int -> Selections -> Bool
isSelected index selections =
    case selections of
        NoneSelected ->
            False
        OneSelected j ->
            if j == index then True else False
        BothSelected j k ->
            if (j == index || k == index) then True else False


selectionsList : Selections -> List Int
selectionsList selections =
    case selections of
       NoneSelected -> []
       OneSelected i -> [i]
       BothSelected i j -> [i, j]



swap : List Int -> Selections -> List Int
swap array selections =
    case selections of
        NoneSelected -> array
        OneSelected i -> array
        BothSelected i j -> LE.swapAt i j array
