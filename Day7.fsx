#load "Helpers.fsx"

open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data =
    Helpers.Web.getInput 7
    |> Array.map (fun s -> s.Split(' '))
    |> Array.map (fun [|c;bid|] -> c.ToCharArray(), int bid)

let cardScore =
    function
    | 'A' -> 14
    | 'K' -> 13
    | 'Q' -> 12
    | 'J' -> 11
    | 'T' -> 10
    | c -> int (string c)

let test = [|'A'; 'A'; 'K'; '5'; '9'|]

Array.countBy id test |> Array.sortByDescending snd

let iskind count (arr : char array) =
    Array.countBy id arr
    |> Array.sortByDescending snd
    |> Array.head
    |> (snd >> ((=)count))

let is5kind = iskind 5
let is4kind = iskind 4
let is3kind = iskind 3

let isFullHouse (arr : char array) =
    Array.countBy id arr
    |> Array.length
    |> ((=)2)

let isTwoPair (arr : char array) =
    let sorted =
        Array.countBy id arr
        |> Array.sortByDescending snd
    match sorted with
    | [|(_,2); (_, 2); (_, 1)|] -> true
    | _ -> false

let isOnePair (arr : char array) =
    let sorted =
        Array.countBy id arr
        |> Array.sortByDescending snd
    match sorted with
    | [|(_,2); (_, 1); (_, 1); (_, 1)|] -> true
    | _ -> false

let handRank arr =
    match arr with
    | arr when is5kind arr -> 6
    | arr when is4kind arr -> 5
    | arr when isFullHouse arr -> 4
    | arr when is3kind arr -> 3
    | arr when isTwoPair arr -> 2
    | arr when isOnePair arr -> 1
    | _ -> 0

let compareHands hand1 hand2 =
    let rank1, rank2 = handRank hand1, handRank hand2
    if (rank1 > rank2) then
        1
    else if (rank2 > rank1) then
        -1
    else
        let diff =
            Array.zip hand1 hand2
            |> Array.map (fun (c1,c2) -> cardScore c1 - cardScore c2)
            |> Array.tryFind ((<>)0)
        match diff with
        | None -> 0
        | Some x -> Math.Sign(x)

[<CustomComparison; CustomEquality>]
type Hand =
    { Cards : char array }
    interface IComparable with
        member this.CompareTo other =
            match other with
            | :? Hand as h -> (this :> IComparable<_>).CompareTo h
            | _ -> -1

    interface IComparable<Hand> with
        member this.CompareTo other = compareHands this.Cards other.Cards

    interface IEquatable<Hand> with
        member this.Equals other = other.Cards.Equals this.Cards

    override this.Equals other =
        match other with
        | :? Hand as p -> (this :> IEquatable<_>).Equals p
        | _ -> false
    override this.GetHashCode () = this.Cards.GetHashCode()
    
let ans1 =
    data
    |> Array.map (fun (c,r) -> { Cards = c }, r)
    |> Array.sortBy fst
    |> Array.mapi (fun i (_,r) -> (i+1)*r)
    |> Array.sum

ans1

/// Part 2

let ans2 = data

ans2