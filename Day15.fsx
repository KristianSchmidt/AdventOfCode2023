#load "Helpers.fsx"

open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data =
    Helpers.Web.getInput 15
    |> Array.head
    |> fun s -> s.Split(',')

let example = "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7".Split(',')

let score (s : string) =
    s.ToCharArray()
    |> Array.fold (fun s c -> (((int c) + s) * 17) % 256) 0

let ans1 = data |> Array.sumBy score

ans1

/// Part 2

type Op =
    | Dash of string
    | Equals of string * int

let label = function | Dash l -> l | Equals (l,_) -> l

let stringToOp (s: string) =
    match s.EndsWith('-') with
    | true -> Dash (s.Substring(0,s.Length-1))
    | false -> Equals (s.Split('=')[0],int <| s.Split('=')[1])

let ops = data |> Array.map stringToOp

let updateBoxes (boxes : Map<int, (string*int) list>) (op : Op) =
    let boxNum = op |> label |> score
    let box = boxes |> Map.tryFind boxNum |> Option.defaultValue []
    match op with
    | Dash l -> Map.add boxNum (List.filter (fun (l',i) -> l' <> l) box) boxes
    | Equals (l,i) ->
        match box |> List.exists (fun (l',_) -> l' = l) with
        | true ->
            let newBox = box |> List.map (fun (l',i') -> if (l = l') then (l,i) else (l',i'))
            boxes |> Map.add boxNum newBox
        | false -> Map.add boxNum (List.append box [(l,i)]) boxes

let boxScore k (l : (string*int) list) =
    l
    |> List.mapi (fun i (_,focal) -> (k+1)*(i+1)*focal)
    |> List.sum

let ans2 =
    ops
    |> Array.fold updateBoxes Map.empty
    |> Map.map (fun k v -> boxScore k v)
    |> Map.values
    |> Seq.sum
    
ans2