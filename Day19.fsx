#load "Helpers.fsx"

open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let workflows,parts =
    let data = Helpers.Web.getInput 19
    let idx = data |> Array.findIndex ((=)"")
    data[0..idx-1], data[idx+1..]

workflows |> Array.iter (printfn "%A")

type Descision = | Accepted | Rejected
type Var = | X | M | A | S

let parseVar = function | "x" -> X |  "m" -> M | "a" -> A | "s" -> S

type Part = { X : int; M : int; A : int; S : int }

let parsePart =
    function
    | Helpers.Regex "{x=(\d+),m=(\d+),a=(\d+),s=(\d+)}" [x;m;a;s] -> { X = int x; M = int m; A = int a; S = int s }

type Destination = string

type Rule =
    | Unconditional of Destination
    | GreaterThan of Var * int * Destination
    | LessThan of Var * int * Destination

type RuleEval =
    | Goto of Destination
    | Finished of string
    | NoMatch

let evalRule (r : Rule) (p : Part) =
    match r with
    | Unconditional "A" -> Finished "A"
    | Unconditional "R" -> Finished "R"
    | Unconditional dest -> Goto dest
    | GreaterThan (X, num, dest) -> if p.X > num then Goto dest else NoMatch
    | GreaterThan (M, num, dest) -> if p.M > num then Goto dest else NoMatch
    | GreaterThan (A, num, dest) -> if p.A > num then Goto dest else NoMatch
    | GreaterThan (S, num, dest) -> if p.S > num then Goto dest else NoMatch
    | LessThan (X, num, dest) -> if p.X < num then Goto dest else NoMatch
    | LessThan (M, num, dest) -> if p.M < num then Goto dest else NoMatch
    | LessThan (A, num, dest) -> if p.A < num then Goto dest else NoMatch
    | LessThan (S, num, dest) -> if p.S < num then Goto dest else NoMatch

type Workflow = Rule list

type WorkflowEval =
    | GotoWorkflow of Destination
    | FinishedWorkflow of string

let evalWorkflow (workflow : Workflow) p =
    let rec f w =
        match w with
        | r :: rs ->
            match evalRule r p with
            | Finished x -> FinishedWorkflow x
            | Goto "A" -> FinishedWorkflow "A"
            | Goto "R" -> FinishedWorkflow "R"
            | Goto x -> GotoWorkflow x
            | NoMatch -> f rs
        | [] -> failwithf "."

    f workflow

let parseRule (s : string) =
    match s with
    | Helpers.Regex "(\w+)>(\d+):(\w+)" [v;num;dest] -> GreaterThan (parseVar v, int num, dest)
    | Helpers.Regex "(\w+)<(\d+):(\w+)" [v;num;dest] -> LessThan (parseVar v, int num, dest)
    | Helpers.Regex "(\w+)" [dest] -> Unconditional dest

let parseWorkflow (s : string) =
    let [|name;ruleString|] = s.Substring(0, s.IndexOf("}")).Split('{')
    name, ruleString.Split(',') |> Array.map parseRule |> List.ofArray

let wmap = workflows |> Array.map parseWorkflow |> Map.ofArray

let parts' = parts |> Array.map parsePart

let evaluatePart (p : Part) =
    let rec f w =
        match evalWorkflow w p with
        | GotoWorkflow flow ->
            //printfn "GOTO: %s" flow
            f wmap[flow]
        | FinishedWorkflow s -> s

    f wmap["in"]

let score (p : Part) = p.X + p.M + p.A + p.S

let ans1 = parts' |> Array.filter (evaluatePart >> (=)"A") |> Array.sumBy score

ans1

/// Part 2

type PartSlice = { Xmin : int64; Xmax : int64; Mmin : int64; Mmax : int64; Amin : int64; Amax : int64; Smin : int64; Smax : int64 }

let valid (p : PartSlice) = p.Xmin <= p.Xmax && p.Mmin <= p.Mmax && p.Amin <= p.Amax && p.Smin <= p.Smax

type RuleEval2 =
    | Goto of PartSlice * Destination
    | Finished of PartSlice * string
    | NoMatch of PartSlice

let evalRule2 (r : Rule) (p : PartSlice) =
    match r with
    | Unconditional "A" -> [Finished (p, "A")]
    | Unconditional "R" -> [Finished (p, "R")]
    | Unconditional dest -> [Goto (p, dest)]
    | GreaterThan (X, num, dest) -> [Goto({p with Xmin = (int64 num)+1L}, dest); NoMatch {p with Xmax = num}]
    | GreaterThan (M, num, dest) -> [Goto({p with Mmin = (int64 num)+1L}, dest); NoMatch {p with Mmax = num}]
    | GreaterThan (A, num, dest) -> [Goto({p with Amin = (int64 num)+1L}, dest); NoMatch {p with Amax = num}]
    | GreaterThan (S, num, dest) -> [Goto({p with Smin = (int64 num)+1L}, dest); NoMatch {p with Smax = num}]
    | LessThan (X, num, dest)    -> [Goto({p with Xmax = (int64 num)-1L}, dest); NoMatch {p with Xmin = num}]
    | LessThan (M, num, dest)    -> [Goto({p with Mmax = (int64 num)-1L}, dest); NoMatch {p with Mmin = num}]
    | LessThan (A, num, dest)    -> [Goto({p with Amax = (int64 num)-1L}, dest); NoMatch {p with Amin = num}]
    | LessThan (S, num, dest)    -> [Goto({p with Smax = (int64 num)-1L}, dest); NoMatch {p with Smin = num}]

let evalWorkflow2 (workflow : Workflow) (p : PartSlice) =
    let rec f (w : Workflow) (ps : PartSlice list) =
        match w with
        | r :: rs -> 
            let slices = ps |> List.collect (evalRule2 r)
            let accepted =
                slices
                |> List.choose (function | Finished (ps, "A") -> Some ps | Goto(ps,"A") -> Some ps | _ -> None)
                |> List.filter valid
            
            let gotos =
                slices
                |> List.filter (function | Goto(ps,dest) when dest <> "A" && dest <> "R" -> true | _ -> false)
                |> List.collect (function | Goto(ps,dest) -> f wmap[dest] [ps])
                |> List.filter valid
                
            let nomatches =
                slices
                |> List.filter (function | NoMatch ps -> true | _ -> false)
                |> List.map (function | NoMatch ps -> ps)
                |> List.filter valid
                |> List.collect (fun ps -> f rs [ps])

            List.collect id [accepted; gotos; nomatches]

        | _ -> failwithf "."

    f workflow [p]

let baseSlice = { Xmin = 1L; Xmax = 4000L; Mmin = 1L; Mmax = 4000L; Amin = 1L; Amax = 4000L; Smin = 1L; Smax = 4000L }

let sliceSize (ps : PartSlice) =
    (ps.Xmax - ps.Xmin + 1L) * (ps.Mmax - ps.Mmin + 1L) * (ps.Amax - ps.Amin + 1L) * (ps.Smax - ps.Smin + 1L)

let ans2 = evalWorkflow2 wmap["in"] baseSlice |> List.sumBy sliceSize

ans2