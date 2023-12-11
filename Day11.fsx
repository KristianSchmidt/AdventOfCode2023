#load "Helpers.fsx"

open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data = array2D <| Helpers.Web.getInput 11

let rows =
    [| 0 .. data.GetLength(0)-1 |]
    |> Array.filter (fun i -> data[i,*] |> Array.forall ((=)'.'))
    |> set
        
let cols =
    [| 0 .. data.GetLength(1)-1 |]
    |> Array.filter (fun i -> data[*,i] |> Array.forall ((=)'.'))
    |> set

let pairs =
    let galaxies =
        seq {
            for r in 0 .. data.GetLength(0) - 1 do
                for c in 0 .. data.GetLength(1) - 1 do
                    if data[r,c] = '#' then
                        yield (r,c)
        } |> Array.ofSeq
    seq {
        for i in 0 .. galaxies.Length - 1 do
            for j in i+1 .. galaxies.Length - 1 do
                yield (galaxies[i],galaxies[j])
    }
    |> Array.ofSeq

let colsBetween c1 c2 =
    cols
    |> Set.filter (fun c -> c > (min c1 c2) && c < (max c1 c2))
    |> Set.count

let rowsBetween r1 r2 =
    rows
    |> Set.filter (fun c -> c > (min r1 r2) && c < (max r1 r2))
    |> Set.count

let findDist penalty ((r1,c1),(r2,c2)) =
    let cBet = colsBetween c1 c2 |> int64
    let rBet = rowsBetween r1 r2 |> int64
    let extra = penalty - 1 |> int64
    (int64 (abs (r1-r2) + abs (c1-c2))) + (extra * (cBet + rBet))

let ans1 = pairs |> Array.sumBy (findDist 2)
let ans2 = pairs |> Array.sumBy (findDist 1_000_000)

ans1
ans2