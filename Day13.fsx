#load "Helpers.fsx"

#time "on"

open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data =
    String.Join('\n', Helpers.Web.getInput 13).Split("\n\n")
    |> Array.map (fun s -> s.Split('\n') |> Array.map (fun s -> s.ToCharArray()))
    |> Array.map array2D

let areMirrored (arr1 : char array) (arr2 : char array) =
    [0..(min arr1.Length arr2.Length)]
    |> Seq.forall (fun i -> arr1[arr1.Length-1-i] = arr2[i])
    
let mirroredInRow (arr : char array2d) (row : int) =
    [0..arr.GetLength(1)-1]
    |> Seq.forall (fun i -> areMirrored arr[0..row-1,i] arr[row..,i])

let mirroredInCol (arr : char array2d) (col : int) =
    [0..arr.GetLength(0)-1]
    |> Seq.forall (fun j -> areMirrored arr[j,0..col-1] arr[j,col..])

type Hit = RowHit of int | ColHit of int

let findMirror (arr : char array2d) =
    let rowSearch = 
        [|1..arr.GetLength(0)-1|]
        |> Array.filter (fun i -> mirroredInRow arr i)
        |> Array.map RowHit
    
    let colSearch =
        [|1..arr.GetLength(1)-1|]
        |> Array.filter (fun j -> mirroredInCol arr j)
        |> Array.map ColHit

    Array.collect id [|rowSearch; colSearch|]
    |> Array.distinct

let score =
    function
    | RowHit i -> 100*i
    | ColHit i -> i

let ans1 = data |> Array.sumBy (findMirror >> Array.exactlyOne >> score)

ans1

/// Part 2

let smudgeExpand (arr : char array2d) =
    let oldHit = findMirror arr |> Array.exactlyOne
    seq {
        for r in 0 .. arr.GetLength(0)-1 do
            for c in 0 .. arr.GetLength(1)-1 do
                let old = arr[r,c]
                let newVal = if (old = '#') then '.' else '#'
                arr[r,c] <- newVal
                yield! (findMirror arr |> Array.filter ((<>)oldHit))
                arr[r,c] <- old
    }
    |> Seq.distinct
    |> Seq.exactlyOne

let ans2 = data |> Array.sumBy (smudgeExpand >> score)

ans2