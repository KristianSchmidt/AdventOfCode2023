#load "Helpers.fsx"

open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let isDigit c =
    c = '0' || c = '1' || c = '2' ||
    c = '3' || c = '4' || c = '5' ||
    c = '6' || c = '7' || c = '8' ||
    c = '9'

let data = Helpers.Web.getInput 1
    
let ans1 =
    data
    |> Array.map (fun s -> s.ToCharArray() |> Array.filter isDigit |> Array.map (string>>int))
    |> Array.sumBy (fun arr -> arr[0]*10 + arr[arr.Length-1])

ans1

/// Part 2

let ans2 = data

ans2