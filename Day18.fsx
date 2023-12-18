#load "Helpers.fsx"

open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let example = """R 6 (#70c710)
D 5 (#0dc571)
L 2 (#5713f0)
D 2 (#d2c081)
R 2 (#59c680)
D 2 (#411b91)
L 5 (#8ceee2)
U 2 (#caa173)
L 1 (#1b58a2)
U 2 (#caa171)
R 2 (#7807d2)
U 3 (#a77fa3)
L 2 (#015232)
U 2 (#7a21e3)""".Split('\n')

let data =
    Helpers.Web.getInput 18
    //example
    |> Array.map (fun s -> s.Split(' '))
    |> Array.map (fun [|l;c;color|] -> (l,int64 c, color))

let nextPoint (x,y) (dir,count) =
    match dir with
    | "U" -> (x-count,y)
    | "D" -> (x+count,y)
    | "R" -> (x,y+count)
    | "L" -> (x,y-count)

let area (arr : (int64*int64) array) =
    let mutable a = 0L
    for i in 0 .. arr.Length-2 do
        let (x,y) = arr[i]
        let (x',y') = arr[i+1]
        a <- a + (y+y')*(x-x')
    let (xn,yn) = Array.last arr
    let (x0,y0) = Array.head arr
    a <- a + (yn+y0)*(xn-x0)
    abs a / 2L

let expandPointsCount ((x : int64,y : int64),(x',y')) =
    let minX,maxX = min x x', max x x'
    let minY,maxY = min y y', max y y'
    if (maxX > minX) then maxX - minX else maxY - minY

let b =
    data
    |> Array.map (fun (l,c,_) -> (l,c))
    |> Array.scan nextPoint (0L,0L)
    |> Array.pairwise
    |> Array.sumBy expandPointsCount

let A = 
    data
    |> Array.map (fun (l,c,_) -> (l,c))
    |> Array.scan nextPoint (0,0)
    |> area

let ans1 = A + b / 2L + 1L

ans1

/// Part 2

let toHex s = Int64.Parse(s, System.Globalization.NumberStyles.HexNumber)

let instructions =
    data
    |> Array.map (fun (_,_,color) -> toHex <| color.Substring(2,5), color.Substring(7,1))

let nextPoint2 (x,y) (count : int64,dir) =
    match dir with
    | "3" -> (x-count,y)
    | "1" -> (x+count,y)
    | "0" -> (x,y+count)
    | "2" -> (x,y-count)

let A2 = 
    instructions
    |> Array.scan nextPoint2 (0L,0L)
    |> area

let b2 =
    instructions
    |> Array.scan nextPoint2 (0L,0L)
    |> Array.pairwise
    |> Array.sumBy expandPointsCount

let ans2 = A2 + b2 / 2L + 1L

ans2