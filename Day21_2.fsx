#load "Helpers.fsx"

open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data = Helpers.Web.getInput 21 |> Array.map (fun s -> s.ToCharArray())
(*
let data = """...........
.....###.#.
.###.##..#.
..#.#...#..
....#.#....
.##..S####.
.##..#...#.
.......##..
.##.#.####.
.##..##.##.
...........""".Split('\n') |> Array.map (fun s -> s.ToCharArray())
*)

let maxR, maxC = data.Length-1, data.[0].Length-1

let isValid (r,c) = r >= 0 && r <= maxR && c >= 0 && c <= maxC

let getNeighbors (r,c) =
    [| (r+1,c); (r-1,c); (r,c+1); (r,c-1) |]
    |> Array.filter isValid
    |> Array.filter (fun (r,c) -> data.[r % maxR].[c % maxC] <> '#')

let start =
    let row = data |> Array.findIndex (fun arr -> arr |> Array.contains 'S')
    let col = data.[row] |> Array.findIndex ((=)'S')
    row, col

let simulate start endStep =
    let rec f current step =
        if step = endStep then
            current
        else
            let next = current |> Array.collect getNeighbors |> Array.distinct
            f next (step + 1)

    f [|start|] 0

let ans1 = simulate start 64 |> Array.length

ans1

/// Part 2

let steps = 26501365L
//let steps = 65L

let unreachables =
    Array.allPairs [|1L..129L|] [|1L..129L|]
    |> Array.filter (fun (r,c) ->
            let r' = int r
            let c' = int c
            data[r'-1][c'] = '#' && data[r'+1][c'] = '#' && data[r'][c'-1] = '#' && data[r'][c'+1] = '#'
        )
    |> Set.ofArray

let wholeRow rowMod =
    [|0L..130L|]
    |> Seq.filter (fun c -> (rowMod+c) % 2L = (steps % 2L) && data.[int rowMod].[int c] <> '#')
    |> Seq.filter (fun c -> Set.contains (rowMod,c) unreachables |> not)
    |> Seq.length
    |> int64

let wr = Helpers.memoize wholeRow

let wholeSquare = [|0L..130L|] |> Array.sumBy wr

let hops = (steps - 65L) / 131L

let diamond =
    let r,c = 0,65
    let r2,c2 = 130,65
    seq {
        for i in 0..65 do
            yield (r+i,c-i)
            yield (r+i,c+i)
            yield (r2-i,c2-i)
            yield (r2-i,c2+i)
    }
    |> Seq.distinct
    |> Seq.filter (fun (r,c) -> (r+c) % 2 = int (steps % 2L) && data.[r].[c] <> '#')
    |> Seq.filter (fun (r,c) -> Set.contains (int64 r, int64 c) unreachables |> not)
    |> Seq.toArray


diamond.Length

diamond
|> Array.sortBy fst
|> Array.iter (printfn "%A")

let nonMiddle =
    [|0L..hops-1L|]
    |> Array.map (fun i -> ((i * 2L) + 1L)*2L*wholeSquare-(int64 diamond.Length))
    |> Array.sum

let middle =
    2L * hops * wholeSquare + (simulate start 65 |> Array.length |> int64)
    // 2x hops - 1 + 1,5
    // 2x hops + simulate 65

nonMiddle + middle






















let cr (dist, rowMod) =

    let arr = Array.append [|131L - dist..130L|] [|0..dist-1L|]

    let res =
        arr
        |> Seq.filter (fun c -> (rowMod+c) % 2L = (steps % 2L) && data.[int rowMod].[int c] <> '#')
        |> Seq.filter (fun c -> Set.contains (rowMod,c) unreachables |> not)
        //|> (fun arr -> printfn "%A" (Array.ofSeq arr); arr)
        |> Seq.length
        |> int64

    res

let cr' = Helpers.memoize cr

let cr2 (rowMod, minc, maxc) =
    [|minc..maxc|]
    |> Seq.filter (fun c -> (rowMod+c) % 2L = (steps % 2L) && data.[int rowMod].[int c] <> '#')
    |> Seq.filter (fun c -> Set.contains (rowMod,c) unreachables |> not)
    //|> (fun arr -> printfn "%A" (Array.ofSeq arr); arr)
    |> Seq.length
    |> int64

let cr2' = Helpers.memoize cr2


let countRow r =
    let rowMod = r % 131L

    let width = if r > steps then 2L*steps-r else r

    let minc = 65L - width
    let maxc = 65L + width
    let dist = maxc % 130L

    if (minc >= 0 && maxc <= 130) then
        //printfn "%A" (rowMod, minc, maxc)
        cr2' (rowMod, minc, maxc)
    else
        let wholes = (maxc - minc + 1L) / 131L
        let leftover = cr' (dist, rowMod)
        wholes * (wr rowMod) + leftover
    
    //let wholeRow = (cr' (0L, rowMod, 0L, 130L))
    
    //printfn "Wholes: %A * %A" wholes wholeRow
    //printfn "Leftover %i = %A" dist leftover

    
    
data.[14][57]
let r = steps*2L-1L

[|0L..130L|]
|> Array.iter (fun i -> printfn "%i = %i" i (wr i))

[|0L..130L|] |> Array.sumBy wr

countRow 14L

[|0L .. 2L*steps|]
|> Array.sumBy countRow

//    (fun i ->
//        let res = countRow i
//        printfn "%i = %A" i res
//        res
//    )

// 626322974728898 too high
// 626325272102317 not correct
// 626079694744771 not correct
// 626075086420000 not correct
// 626325268810898
// 626325296985245
// 626078128611296
// 626078181201647 not correct
// 626078181209224 not correct
// 626323734163026 not correct
// 313163415083096 not correct
// 313163415075444 too low

