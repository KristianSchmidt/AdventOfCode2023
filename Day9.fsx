#load "Helpers.fsx"

open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data =
    Helpers.Web.getInput 9
    |> Array.map (fun s -> s.Split(' ') |> Array.map int64)

let findLevel (arr : int64 array) =
    let rec f i (curr : int64 array) =
        if (curr |> Array.forall ((=)0L)) then
            i
        else
            let next = curr |> Seq.pairwise |> Seq.map (fun (a,b) -> b - a) |> Array.ofSeq
            f (i+1) next

    f 0 arr

let findNext (arr : int64 array) =
    let maxLevel = findLevel arr

    let rec f row col =
        match row with
        | 0 when col <= 0 -> arr[arr.Length - 1 + col]
        | r when r = maxLevel -> 0L
        | _ when col > 0 -> f row (col-1) + f (row+1) col
        | _ -> f (row-1) col - f (row-1) (col - 1)

    f 0 1

let ans1 = Array.sumBy findNext data

ans1

/// Part 2

let findPrev (arr : int64 array) =
    let maxLevel = findLevel arr

    let rec f row col =
        match row with
        | 0 when col >= 0 -> arr[col]
        | r when r = maxLevel -> 0L
        | _ when col < 0 -> f (row) (col+1) - f (row+1) col
        | _ -> f (row-1) (col+1) - f (row-1) (col)

    f 0 -1

let ans2 = Array.sumBy findPrev data

ans2