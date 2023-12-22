#load "Helpers.fsx"

open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data = Helpers.Web.getInput 20

//data |> Array.iter (printfn "%A")

type Pulse = | High | Low

type FlipFlopState = | On | Off

type Module = | FlipFlop of FlipFlopState
              | Conjunction of Map<string,Pulse>
              | Broadcast

let parseModule (s : string) =
    match s with
    | Helpers.Regex "broadcaster -> (.+)" [dest] -> "broadcaster", Broadcast, dest.Split(", ")
    | Helpers.Regex "%(\w+) -> (.+)" [name; dest] -> name, FlipFlop Off, dest.Split(", ")
    | Helpers.Regex "&(\w+) -> (.+)" [name; dest] -> name, Conjunction Map.empty, dest.Split(", ")

let modules =
    let raw = data |> Array.map parseModule
    let grouped = raw |> Array.groupBy (fun (_,t,_) -> match t with | Conjunction _ -> true | _ -> false)
    let nonConjunction = grouped |> Array.head |> snd
    let conjunction = grouped |> Array.tail |> Array.head |> snd
    let findInputMap (name : string) =
        Array.append nonConjunction conjunction
        |> Array.filter (fun (_,_,dest) -> dest |> Array.contains name)
        |> Array.map (fun (n,_,_) -> n, Low)
        |> Map.ofArray
    
    let conjunctions =
        conjunction
        |> Array.map (fun (n,_,dest) -> n, (Conjunction (findInputMap n), dest))
    let nonconjs =
        nonConjunction
        |> Array.map (fun (n,m,dest) -> n, (m,dest))
    Array.append conjunctions nonconjs
    |> Map.ofArray

let receivePulse (name : string) (p : Pulse) (sender : string) (m : Module, dest : string array) =
    match m, p with
    | Broadcast, _ -> m, dest |> Array.map (fun d -> name, p, d)
    | FlipFlop On, Low -> FlipFlop Off, dest |> Array.map (fun d -> name, Low, d)
    | FlipFlop Off, Low -> FlipFlop On, dest |> Array.map (fun d -> name, High, d)
    | FlipFlop _, High -> m, Array.empty
    | Conjunction map, p ->
        let newMap = map |> Map.add sender p
        let allHigh = newMap |> Map.toSeq |> Seq.map snd |> Seq.forall ((=)High)
        match allHigh with
        | true  -> Conjunction newMap, dest |> Array.map (fun d -> name, Low, d)
        | false -> Conjunction newMap, dest |> Array.map (fun d -> name, High, d)

let simulate startModules =
    let rec f (modules : Map<string, Module*string array>) queue (lowPulses : int) (highPulses : int) =
        if (queue |> Array.isEmpty) then
            lowPulses, highPulses
        else
            let sender, pulse, dest = Array.head queue
            //printfn "Processing: %A" (Array.head queue)
            if modules.ContainsKey(dest) then
                let newModule, newPulses = receivePulse dest pulse sender modules.[dest]
                let newLow = newPulses |> Array.filter (fun (_,p,_) -> p = Low) |> Array.length
                let newHigh = newPulses |> Array.filter (fun (_,p,_) -> p = High) |> Array.length
                let newQueue = Array.append (Array.tail queue) newPulses
                let newModules = modules |> Map.add dest (newModule, snd modules.[dest])
                f newModules newQueue (lowPulses + newLow) (highPulses + newHigh)
            else
                f modules (Array.tail queue) lowPulses highPulses

    let startPulses = Array.replicate 1000 ("button", Low, "broadcaster") 
    f startModules startPulses 1000 0

let (low, high) = simulate modules

let ans1 = low * high

ans1

/// Part 2

let simulate2 startModules =
    let mutable buttonPresses = 1
    let rec f (modules : Map<string, Module*string array>) queue =
        let sender, pulse, dest =
            if (queue |> Array.isEmpty) then
               buttonPresses <- buttonPresses + 1
               //modules |> Map.toSeq |> Seq.map (snd >> fst) |> Seq.iter (printfn "%A")
               if buttonPresses % 10_000 = 0 then printfn "Buttonpresses: %i" buttonPresses
               ("button", Low, "broadcaster")
            else
                Array.head queue
            
        //printfn "Processing: %A" (Array.head queue)
        if modules.ContainsKey(dest) then
            let newModule, newPulses = receivePulse dest pulse sender modules.[dest]
            if dest = "ns" then
                match newModule with
                | Conjunction m when m |> Map.exists (fun k v -> v = High) ->
                    printfn "%i: %A" buttonPresses m
                | _ -> ()
            let newQueue = Array.append (if queue.Length > 0 then Array.tail queue else Array.empty) newPulses
            let newModules = modules |> Map.add dest (newModule, snd modules.[dest])
            f newModules newQueue
        else // this is the rx module
            //printfn "Rx: %A" (sender, pulse, dest)
            if pulse = Low then
                buttonPresses
            else
                f modules (Array.tail queue)

    f startModules [|("button", Low, "broadcaster")|]

let dc = 3797L
let vp = 3847L
let cq = 3877L
let rv = 4051L

let ans2 = [dc; vp; cq; rv] |> List.fold Helpers.lcm 1L

ans2