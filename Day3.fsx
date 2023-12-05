#load "Helpers.fsx"

open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data = Helpers.Web.getInput 3

let dataArr = data |> Array.map (fun s -> s.ToCharArray())

let adj (i,j) =
    let getElem (i,j) =
        if (i < 0 || i >= dataArr.Length || j < 0 || j >= dataArr[0].Length) then
            None
        else
            Some (dataArr[i][j])

    [|(i-1,j-1); (i-1,j); (i-1,j+1)
      (i  ,j-1);          (i,  j+1)
      (i+1,j-1); (i+1,j); (i+1,j+1)
    |] |> Array.choose getElem

let isDigit (c : char) =
    c = '0' || c = '1' || c = '2' || c = '3' || c = '4' || 
    c = '5' || c = '6' || c = '7' || c = '8' || c = '9'

let symbols =
    data
    |> Array.collect (fun s -> s.ToCharArray() |> Array.filter (isDigit >> not))
    |> Set.ofArray

let hasSymbol (arr : char array) = 
    arr |> Array.exists (fun c -> Set.contains c symbols && c <> '.')

let numbersOnLine i =
    let d = data[i].Split(Set.toArray symbols)
    let is = d |> Array.scan (fun s t -> s + t.Length + 1) -1 |> Array.tail

    Array.zip d is
    |> Array.filter (fst >> (fun s -> String.IsNullOrEmpty(s)) >> not)
    |> Array.map (fun (s,i') -> int s, [|i'-s.Length+1 .. i'|] |> Array.map (fun i' -> (i,i'-1)))
    |> Array.map (fun (num, arr) -> num, arr |> Array.collect adj |> hasSymbol)

let ans1 =
    [|0..data.Length-1|]
    |> Array.collect numbersOnLine
    |> Array.filter snd
    |> Array.sumBy fst

ans1

/// Part 2

let numberSetsOnLine i =
    let d = data[i].Split(Set.toArray symbols)
    let is = d |> Array.scan (fun s t -> s + t.Length + 1) -1 |> Array.tail

    Array.zip d is
    |> Array.filter (fst >> (fun s -> String.IsNullOrEmpty(s)) >> not)
    |> Array.map (fun (s,i') -> [|i'-s.Length+1 .. i'|] |> Array.map (fun i' -> (i,i'-1)))

let numberMap =
    [|0..data.Length-1|]
    |> Array.collect numberSetsOnLine
    |> Array.mapi (fun i arr -> arr |> Array.map (fun c -> (c,i)))
    |> Array.collect id
    |> Map.ofArray

let numbers =
    [|0..data.Length-1|]
    |> Array.collect numberSetsOnLine
    |> Array.map (fun arr -> arr |> Array.map (fun (i,j) -> dataArr[i][j]) |> (fun arr -> String(arr) |> int))
    |> Array.mapi (fun i x -> (i,x))
    |> Map.ofArray

let adj2 (i,j) =
    let getElem (i,j) =
        if (i < 0 || i >= dataArr.Length || j < 0 || j >= dataArr[0].Length) then
            None
        else
            Some (i,j)

    [|(i-1,j-1); (i-1,j); (i-1,j+1)
      (i  ,j-1);          (i,  j+1)
      (i+1,j-1); (i+1,j); (i+1,j+1) |]
    |> Array.choose getElem

let isAdjToTwoSets (i,j) =
    adj2 (i,j)
    |> Array.choose (fun c -> Map.tryFind c numberMap)
    |> Set.ofArray
    |> (fun s -> Set.count s = 2)

let gears =
    seq {
        for i in [0..dataArr.Length-1] do
            for j in [0..dataArr[0].Length-1] do
                if dataArr[i][j] = '*' then
                    yield (i,j)
    }
    |> Seq.toArray
    |> Array.filter isAdjToTwoSets

let ratio c =
    adj2 c
    |> Array.choose (fun c -> Map.tryFind c numberMap)
    |> Array.distinct
    |> Array.map (fun c -> numbers[c])
    |> Array.fold (*) 1

let ans2 = gears |> Array.sumBy ratio

ans2