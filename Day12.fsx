#load "Helpers.fsx"

#time "on"
open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data =
    Helpers.Web.getInput 12
    |> Array.map (fun s -> (s.Split(' ')[0]).ToCharArray() |> List.ofArray, (s.Split(' ')[1]).Split(',') |> Array.map int |> List.ofArray)

let arrangements (chars' : char list) (groups' : int list) =
    // Number of valid arrangements for each choice made
    //
    // On every point, has to choose to start group here or not
    //
    // If one starts, then it has to fit everything

    let mutable cache : Map<char list * int list, int64> = Map.empty

    let rec f chars groups =
        let res =
            if cache.ContainsKey((chars,groups)) then
                cache[chars,groups]
            else
                //printfn "Chars: %A - Groups: %A" chars groups
                match chars, groups with
                | [], [] -> 1L // No chars left, no groups left, completed successfully
                | [], _ :: _ -> 0L // No chars left but groups left, no success
                | cs, [] when cs |> List.forall (fun c -> c = '.' || c = '?') -> 1L // no groups left and the rest don't have to be filled
                | cs, [] when cs |> List.exists (fun c -> c = '#') -> 0L // no groups left but there's still a group defined
                | '#' :: cs, g :: gs -> // Must place next group here
                    //printfn "%A - %i" cs g
                    // check if size of next group is valid for all tiles
                    let tilesValid =
                        cs.Length >= (g-1) &&
                        cs |> Seq.take (g-1) |> Seq.forall (fun c -> c = '#' || c = '?')
                    // should either run out of tiles or have the next tile be either '.' or '?'
                    if tilesValid then
                        if cs.Length = g-1 then // No more tiles left, so if there are also no more groups left, then it's valid
                            if gs.IsEmpty then 1 else 0
                        else
                            // Theres tiles left - next tile cannot be # because then it would have been in the group
                            if (cs[g-1] = '.' || cs[g-1] = '?') then
                                // Skipping extra tile to account for next . or ?
                                f (List.skip g cs) gs
                            else
                                0L
                    else
                        0L
                | '.' :: cs, gs -> f cs gs
                | '?' :: cs, gs ->
                    let placement = f ('#' :: cs) gs
                    let noPlacement = f ('.' :: cs) gs
                    placement + noPlacement
        
        cache <- cache |> Map.add (chars, groups) res
        res

    f chars' groups'

let ans1 = data |> Array.sumBy (fun d -> d ||> arrangements)

ans1

/// Part 2

let unfold (chars : char list) (groups : int list) =
    let newChars  = List.collect id [ chars; ['?']; chars; ['?']; chars; ['?']; chars; ['?']; chars ]
    let newGroups = List.collect id [ groups; groups; groups; groups; groups ]
    newChars, newGroups

let ans2 =
    data
    |> Array.Parallel.mapi (fun i d -> d ||> unfold ||> arrangements)
    |> Array.sum

ans2