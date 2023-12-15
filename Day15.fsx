#load "Helpers.fsx"

open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data =
    Helpers.Web.getInput 15
    |> Array.head
    |> fun s -> s.Split(',')

let data2 = "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7".Split(',')

let score (s : string) =
    s.ToCharArray()
    |> Array.fold (fun s c -> (((int c) + s) * 17) % 256) 0


let ans1 = data |> Array.sumBy score

ans1

/// Part 2

type Op =
    | Dash of label : string
    | Equals of label : string * focalLength : int

    with
        member this.label() =
            match this with
            | Dash l -> l
            | Equals (l,_) -> l

let ops =
    data
    |> Array.map (fun s ->
        if (s.EndsWith('-')) then
            Dash (s.Substring(0,s.Length-1))
        else
            let arr = s.Split('=')
            Equals (arr[0],int arr[1])
      )

let updateBoxes (boxes : Map<int, (string*int) list>) (op : Op) =
    let boxNum = score <| op.label()
    let box = boxes |> Map.tryFind boxNum |> Option.defaultValue []
    match op with
    | Dash l ->
        let newBox = box |> List.filter (fun (l',i) -> l' <> l)
        boxes |> Map.add boxNum newBox
    | Equals (l,i) ->
        match box |> List.exists (fun (l',_) -> l' = l) with
        | true ->
            let newBox =
                box |> List.map (fun (l',i') -> if (l = l') then (l,i) else (l',i'))
            boxes |> Map.add boxNum newBox
        | false ->
            let newBox = List.append box [(l,i)]
            boxes |> Map.add boxNum newBox

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
    

// 1630 not correct

ans2