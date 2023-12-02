#load "Helpers.fsx"

open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data = Helpers.Web.getInput 2
    
let redLimit, greenLimit, blueLimit = 12, 13, 14

let isValid (_, arr : string array) =
    let isColorInvalid (s : string) =
        let sp = s.Split(' ')
        let l = sp[0] |> int
        let color = sp[1]
        match color, l with
        | "red", _ when l > redLimit -> true
        | "green", _ when l > greenLimit -> true
        | "blue", _ when l > blueLimit -> true
        | _ -> false

    arr |> Array.exists isColorInvalid |> not

let ans1 =
    data
    |> Array.map (fun s -> s.Split(':')[0], s.Split(':').[1].Substring(1).Split(';'))
    |> Array.map (fun (id, arr) -> id, arr |> Array.collect (fun s -> s.Split(',')) |> Array.map (fun s -> s.Trim()))
    |> Array.where isValid
    |> Array.map fst
    |> Array.sumBy (fun s -> s.Split(' ')[1] |> int)

ans1

/// Part 2

let setPower ([|(_,arr1);(_,arr2);(_,arr3)|]) =
    let m1 = arr1 |> Array.map (fun (s : string) -> s.Split(' ')[0] |> int) |> Array.max
    let m2 = arr2 |> Array.map (fun (s : string) -> s.Split(' ')[0] |> int) |> Array.max
    let m3 = arr3 |> Array.map (fun (s : string) -> s.Split(' ')[0] |> int) |> Array.max
    m1*m2*m3

let ans2 =
    data
    |> Array.map (fun s -> s.Split(':').[1].Substring(1).Split(';'))
    |> Array.map (fun arr -> arr |> Array.collect (fun s -> s.Split(',')) |> Array.map (fun s -> s.Trim()))
    |> Array.map (fun arr -> arr |> Array.groupBy (fun s -> s.Split(' ')[1]))
    |> Array.sumBy setPower

ans2