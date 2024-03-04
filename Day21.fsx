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

let getNeighbors2 (r,c) =
    [| (r+1,c); (r-1,c); (r,c+1); (r,c-1) |]
    |> Array.filter (fun (r,c) ->
        let r' = if (r % data.Length < 0) then data.Length + (r % data.Length) else r % data.Length
        let c' = if (c % data.Length < 0) then data.Length + (c % data.Length) else c % data.Length
        //printfn "r: %i r': %i c': %i" r r' c'
        data.[r'].[c'] <> '#'
    )

let gn = Helpers.memoize getNeighbors2

let simulate2 start endStep =
    let rec f (current : (int*int) array) step (res : int list) =
        //printfn "Step %i: %i" step current.Length
        if step = endStep then
            current
        else
            let next = current |> Array.collect gn |> Array.distinct
            f next (step + 1) (next.Length :: res)

    f [|start|] 0 [1]

let south =
    simulate2 (130,65) 131
    |> Array.filter (fun (r,c) -> r > 130 && c >= 0 && c <= 130)
    |> Array.length |> int64

let north =
    simulate2 (0,65) 131
    |> Array.filter (fun (r,c) -> r < 0 && c >= 0 && c <= 130)
    |> Array.length |> int64

let east =
    simulate2 (65,130) 131
    |> Array.filter (fun (r,c) -> r >= 0 && r <= 130 && c > 130)
    |> Array.length |> int64

let west =
    simulate2 (65,0) 131
    |> Array.filter (fun (r,c) -> r >= 0 && r <= 130 && c < 0)
    |> Array.length |> int64

let [|northwestSmall;northwestBig|] =
    [|simulate2 (130,131) 66; simulate2 (130,131) 131|]
    |> Array.map (Array.filter (fun (r,c) -> r >= 0 && r <= 130 && c >= 0 && c <= 130))
    |> Array.map (Array.length >> int64)

let [|northeastSmall;northeastBig|] =
    [|simulate2 (130,-1) 66; simulate2 (130,-1) 131|]
    |> Array.map (Array.filter (fun (r,c) -> r >= 0 && r <= 130 && c >= 0 && c <= 130))
    |> Array.map (Array.length >> int64)

let [|southwestSmall;southwestBig|] =
    [|simulate2 (0,131) 66; simulate2 (0,131) 131|]
    |> Array.map (Array.filter (fun (r,c) -> r >= 0 && r <= 130 && c >= 0 && c <= 130))
    |> Array.map (Array.length >> int64)

let [|southeastSmall;southeastBig|] =
    [|simulate2 (0,-1) 66; simulate2 (0,-1) 131|]
    |> Array.map (Array.filter (fun (r,c) -> r >= 0 && r <= 130 && c >= 0 && c <= 130))
    |> Array.map (Array.length >> int64)

let whole =
    simulate2 start 131
    |> Array.filter (fun (r,c) -> r >= 0 && r <= 130 && c >= 0 && c <= 130)
    |> Array.length
    |> int64

let f (r : int64) = 2L * r * (r + 1L)

let steps = 65L + 2L*131L

let hops = (steps - 65L) / 131L

let bigSmalls = (hops - 1L) * 2L + 1L

let bigs = (hops - 1L)
let smalls = hops

let real = simulate2 start (int steps) |> Array.length


// 
// 187745

let res =
    (f (hops - 1L) + 1L) * whole
        + north + south + east + west
        + bigs * northwestBig + smalls * northwestSmall
        + bigs * northeastBig + smalls * northeastSmall
        + bigs * southwestBig + smalls * southwestSmall
        + bigs * southeastBig + smalls * southeastSmall

// 95900

let res =
    [|0L..3L|]
    |> Array.map (fun i -> i * 131L + 65L)
    |> Array.map (int >> (simulate2 start) >> Array.length)

let steps = 26501365L
let steps = 65L + 2L*131L

let upperBound = steps * steps + (steps + 1L) * (steps + 1L)

upperBound % 626322974728898L

let f (r : int64) = 2L * r * (r + 1L)

let g i = f i - f (i-1L)

f 2 - f 1

f 3 - f 2

g 1

let unreachables =
    Array.allPairs [|1L..129L|] [|1L..129L|]
    |> Array.filter (fun (r,c) ->
            let r' = int r
            let c' = int c
            data[r'-1][c'] = '#' && data[r'+1][c'] = '#' && data[r'][c'-1] = '#' && data[r'][c'+1] = '#'
        )
    |> Set.ofArray

let reachables =
    Array.allPairs [|0L..130L|] [|0L..130L|]
    |> Array.filter (fun p -> Set.contains p unreachables |> not)
    |> Array.filter (fun (r,c) -> (r + c) % 2L = 1L && data.[int r][int c] <> '#')

let reachable = reachables |> Array.length

let bfs =
    Helpers.BFS.bfs getNeighbors start

bfs
|> Map.filter (fun (r,c) v -> (abs (65 - r) + abs (65 - c)) <> v)
|> Map.iter (fun (r,c) v -> printfn "%A: Expected %i got %i " (r,c) (abs (65 - r) + abs (65 - c)) v)
    
(131*131) / 2

let wholes = (f steps) / (131L*131L)
let rest = (f steps) % (131L*131L)

wholes * (int64 reachable) + (simulate2 start 65 |> Array.length |> int64)

let hops = (steps - 65L) / 131L

(f hops + 1L)*(int64 reachable) + (simulate2 start 65 |> Array.length |> int64)

simulate2 start (int steps)

let totalCandidates =
    [|1L .. 2L .. steps|]
    |> Array.sumBy g

printfn "%.2f" <| float totalCandidates * ratio


(25. / 9.) * 2.

187745. / 34617.

simulate2 start (131)
|> Array.filter (fun (x,y) -> x >= 0 && x <= 130 && y >= 0 && y <= 130)
|> Array.length
|> Array.groupBy fst
|> Array.sortBy fst
|> Array.iter (fun (k,arr) -> printfn "%i = %A" k (Array.length arr))
//|> Array.map (snd >> (Array.sortBy snd))
//|> Array.filter (fst >> ((=)14))

let ans2 = data.Length

let steps = 26501365L
//let steps = 65L + 131L*2L

let area = ((2L*steps+1L)*(2L*steps+1L)) / 2L

let areaSmall = 131L*131L

let hops = (steps-65L) / 131L


let wholes =
    [1L..hops-1L]
    |> List.sumBy (fun i -> (i*2L)+1L)
    
let oneWhole = 7649L

oneWhole * wholes + oneWhole * (2L*hops + 1L) + (simulate2 start 65 |> Array.length |> int64)

area

let countRow' (minc, maxc, r) =
    //let min = if minc < 0L then 131L + minc else minc
    
    let arr = if minc < 0L then Array.append [|131L + minc..130L|] [|0..maxc|] else [|minc..maxc|]

    let res =
        arr
        |> Array.filter (fun c -> (r+c) % 2L = (steps % 2L) && data.[int r].[int c] <> '#')
        //|> (fun arr -> printfn "%A" arr; arr)
        |> Array.length
        |> int64
    printfn "Computing %A = %i" (minc, maxc, r) res
    res

let cr = Helpers.memoize countRow'

let countRow (r : int64) =
    let stepsFrom = 65L - r |> abs
    let width = steps - stepsFrom

    let minc = 65L - width
    let maxc = 65L + width
    let wholeRows = (maxc - minc) / 131L

    let rowMod = r % 131L
    let row = if rowMod >= 0 then rowMod else 131L + rowMod
    //printfn "Row: %i, Width: %i MinC: %i, MaxC: %i, WholeRows: %i" row width minc maxc wholeRows

    wholeRows * (cr (0L, 130L, row)) + (cr (minc % 131L, maxc % 131L, row))

let resNew =
    [65L-steps..65L+steps] |> List.sumBy countRow
    |> List.map (fun i -> i, countRow i)
    |> Map.ofList

[0L..130L] |> List.sumBy countRow

countRow' (39L,84L,76L)

simulate2 start (int steps)

let resOld =
    simulate start 30
    |> Array.groupBy fst
    |> Array.map (fun (k,arr) -> k, arr.Length)
    |> Map.ofArray

resOld[39]

[36..95]
|> List.map (fun i -> i, resOld[i], resNew[i])
|> List.filter (fun (i,o,n) -> int64 o <> n)

|> Array.sortBy fst
|> Array.iter (printfn "%A")

let oneWhole = [0L..130L] |> List.sumBy (fun i -> cr (0L,130L,i))

let top = (65L - steps, 65L)
let left = (65L, 65L - steps)

let area = 131L*131L

area - (steps * steps) % area

((steps * steps) / area) * 7652L

let h = steps*2L+1L
313163415083096L*2L
let areaWhole = h*h / 2L

h * h % area

areaWhole % area

(areaWhole / area) * oneWhole

let hops = (steps - 65L) / 131L

let hInWholes = 2L*hops+1L

(hInWholes*hInWholes*oneWhole) / 2L

(hInWholes*hInWholes*7649L) / 2L


let wholes =
    [|0L..hops-1L|]
    |> Array.sumBy (fun i -> i*2L+1L)
    |> (fun x -> x * 2L)


wholes * oneWhole + (2L*hops - 1L) * oneWhole + (simulate2 start 65 |> Array.length |> int64)
// middle row
// diamond + 2 * hops - 1

// 626322974728898 too high
// 626078181216945 not correct
// 626078181213049 not correct
// 626078181209296 not correct
// 626078181201647 not correct
// 626078181209224 not correct
// 626323734163026 not correct
// 625382471438242 not correct
// 313163415083096 not correct
// 313163415075444 too low

ans2