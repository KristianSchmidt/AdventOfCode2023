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

let parts2 = parts |> Array.map parsePart

let evaluatePart (p : Part) =
    let rec f w =
        match evalWorkflow w p with
        | GotoWorkflow flow ->
            //printfn "GOTO: %s" flow
            f wmap[flow]
        | FinishedWorkflow s -> s

    f wmap["in"]

let score (p : Part) = p.X + p.M + p.A + p.S

let ans1 = parts2 |> Array.filter (evaluatePart >> (=)"A") |> Array.sumBy score

ans1

/// Part 2

let ans2 = 0

ans2