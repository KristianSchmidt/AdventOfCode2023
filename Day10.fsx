#load "Helpers.fsx"

#time "on"

open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data =
    Helpers.Web.getInput 10
    |> Array.map (fun s -> s.ToCharArray())

let data2 =
    [|
       ".........."
       ".S------7."
       ".|F----7|."
       ".||....||."
       ".||....||."
       ".|L-7F-J|."
       ".|..||..|."
       ".L--JL--J."
       ".........."
    |] |> Array.map (fun s -> s.ToCharArray())

let parseMap (arr : char array array) =
    let filter ((a,b),(c,d)) =
        a >= 0 && a < arr.Length && c >= 0 && c < arr.Length &&
        b >= 0 && b < arr[0].Length && d >= 0 && d < arr[0].Length

    let parseChar row col char =
        let N = (row-1,col)
        let S = (row+1,col)
        let W = (row,col-1)
        let E = (row,col+1)
        let neighbors =
            match char with
            | '.' -> [||]
            | 'S' -> [|(-1,-1)|]
            | '7' -> [|S;W|]
            | 'J' -> [|N;W|]
            | 'F' -> [|S;E|]
            | 'L' -> [|N;E|]
            | '|' -> [|N;S|]
            | '-' -> [|W;E|]
        neighbors |> Array.map (fun e -> ((row,col),e))
        
    let baseMap =
        arr
        |> Array.mapi (fun row arr' ->
            arr' |> Array.mapi (fun col c -> parseChar row col c) |> Array.collect id)
        |> Array.collect id
    let sPos = baseMap |> Array.filter (snd >> ((=)(-1,-1))) |> Array.exactlyOne |> fst
    let sNeighbors =
        baseMap
        |> Array.filter (snd >> (=)sPos)
        |> Array.map (fun (a,b) -> (b,a))

    sPos, baseMap
          |> Array.append sNeighbors
          |> Array.filter filter
          |> Array.groupBy fst
          |> Array.map (fun (k, arr) -> (k,arr |> Array.map snd))
          |> Map.ofArray

let sPos, map = parseMap data

let ans1 =
    Helpers.BFS.bfs (fun a -> map[a]) sPos
    |> Map.toArray
    |> Array.map snd
    |> Array.max

ans1

/// Part 2

#time "on"

let isEdge (a,b) = a = 0 || b = 0 || a = data.Length-1 || b = data[0].Length-1

let loopPoints = Helpers.BFS.bfs (fun a -> map[a]) sPos |> Map.keys |> set

loopPoints.Count

let maxR = data.Length - 1
let maxC = data[0].Length - 1

let newData =
    seq {
        for r in 0 .. maxR do
            yield
                seq {
                    for c in 0 .. maxC do
                        if loopPoints.Contains (r,c) then
                            yield data[r][c]
                        else
                            yield '.'
                } |> Array.ofSeq
    }
    |> Array.ofSeq

newData
|> Array.iter (fun arr -> printfn "%s" <| new String(arr))

type Pos =
    | OnSquare of (int*int)
    | BetweenSquares of ((int*int)*(int*int))

// Potential betweens from OnSquare
// Count: 8
//
// Two towards each side:
//
// Can squeeze between if the two nodes are not neighbors
//
// Potential neighbors from BetweenSquares

let isValidP (r,c) =
    r >= 0 && r <= maxR && c >= 0 && c <= maxC 

let isValid p =
    match p with
    | OnSquare p -> isValidP p && not <| loopPoints.Contains(p)
    | BetweenSquares (p1,p2) ->
        if isValidP p1 && isValidP p2 && map.ContainsKey(p1) && map.ContainsKey(p2) then
            map[p1] |> Array.contains p2 |> not
        else
            false
        
let neighbors =
    function
    | OnSquare (r,c) ->
        let regular =
            [| (r-1,c); (r+1,c)
               (r,c-1); (r,c+1) |]
            |> Array.map OnSquare
        let between =
            let below = [|(r+1,c-1),(r+1,c);(r+1,c),(r+1,c+1)|]
            let above = [|(r-1,c-1),(r-1,c);(r-1,c),(r-1,c+1)|]
            let west  = [|(r-1,c-1),(r,c-1);(r,c-1),(r+1,c-1)|]
            let east  = [|(r+1,c+1),(r,c+1);(r,c+1),(r-1,c+1)|]
            Array.concat [|below; above; west; east|] |> Array.map BetweenSquares
        
        Array.append regular between
        |> Array.filter isValid
    | BetweenSquares ((r1,c1),(r2,c2)) ->
        let between =
            if (r1 = r2) then
                let w, e = min c1 c2, max c1 c2
                let up   = [|(r1-1,w),(r1,w);(r1-1,c1),(r2-1,c2);(r1-1,e),(r1,e)|]
                let down = [|(r1+1,w),(r1,w);(r1+1,c1),(r2+1,c2);(r1+1,e),(r1,e)|]
                Array.append up down |> Array.map BetweenSquares
            else // c1 = c2
                let u, d = min r1 r2, max r1 r2
                let west = [|(d,c1-1),(d,c1);(r1,c1-1),(r2,c2-1);(u,c1-1),(u,c1)|]
                let east = [|(d,c1+1),(d,c1);(r1,c1+1),(r2,c2+1);(u,c1+1),(u,c1)|]
                Array.append east west |> Array.map BetweenSquares
        let onSquare =
            if (r1 = r2) then
                let up   = [|(r1-1,c1);(r1-1,c2)|]
                let down = [|(r1+1,c1);(r1+1,c2)|]
                Array.append up down |> Array.map OnSquare
            else // c1 = c2
                let west = [|(r1,c1-1);(r2,c1-1)|]
                let east = [|(r1,c1+1);(r2,c2+1)|]
                Array.append west east |> Array.map OnSquare
        Array.append onSquare between
        |> Array.filter isValid

let searchPoints =
    seq {
        for r in 0..maxR do
            for c in 0..maxC do
                if isEdge (r,c) && isValid (OnSquare (r,c)) then
                    yield (r,c)
    }
    |> Array.ofSeq

let reachable =
    searchPoints
    |> Array.map OnSquare
    |> Array.fold
        (fun (s : Set<Pos>) p ->
            if (s.Contains(p)) then
                s
            else
                let newPoints = Helpers.BFS.bfs neighbors p |> Map.keys |> set
                Set.union s newPoints
            ) Set.empty
    |> Set.filter (function | OnSquare _ -> true | _ -> false)
    |> Set.count

let all = data.Length * data[0].Length

let ans2 = all - loopPoints.Count - reachable

ans2