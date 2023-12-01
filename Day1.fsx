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

let valid = [|
    "1";"2";"3";"4";"5"
    "6";"7";"8";"9"
    "one";"two";"three";"four";"five"
    "six";"seven";"eight";"nine"
    |]

let toNum =
    function
    | "1" | "one" -> 1
    | "2" | "two" -> 2
    | "3" | "three" -> 3
    | "4" | "four" -> 4
    | "5" | "five" -> 5
    | "6" | "six" -> 6
    | "7" | "seven" -> 7
    | "8" | "eight" -> 8
    | "9" | "nine" -> 9

let getNumber (s : string) =
    let arr =
        [|0..s.Length-1|]
        |> Array.collect (fun i -> valid |> Array.where (fun s' -> s.Substring(i).StartsWith(s')))
    (toNum arr[0])*10 + (toNum arr[arr.Length-1])



let ans2 = data |> Array.sumBy getNumber

ans2