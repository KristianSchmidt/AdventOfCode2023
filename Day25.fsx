#load "Helpers.fsx"
#r "nuget: Microsoft.Z3"

open System
open Microsoft.Z3

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data =
    Helpers.Web.getInput 25
//    """jqt: rhn xhk nvd
//rsh: frs pzl lsr
//xhk: hfx
//cmg: qnr nvd lhk bvb
//rhn: xhk bvb hfx
//bvb: xhk hfx
//pzl: lsr hfx nvd
//qnr: nvd
//ntq: jqt hfx bvb xhk
//nvd: lhk
//lsr: lhk
//rzs: qnr cmg lsr rsh
//frs: qnr lhk lsr""".Split('\n')
    |> Array.map (fun s -> s.Split(": ")[0], s.Split(": ").[1].Split(' '))
    |> Array.collect (fun (x,arr) -> arr |> Array.collect (fun y -> [|(x,y);(y,x)|]))

let nodes = data |> Array.map fst |> Array.distinct

let s, t = nodes[Helpers.random 0 (nodes.Length-1)], nodes[Helpers.random 0 (nodes.Length-1)]

let edges =
    data
    |> Array.distinct
    |> Array.append [|("start", s); (t, "end")|]

let ctx = new Context()

let solver = ctx.MkOptimize()

let d_vars =
    edges
    |> Array.map (fun (x,y) -> (x,y), ctx.MkIntConst(sprintf "d_%s_%s" x y))
    |> Map.ofArray

let z_vars =
    nodes
    |> Array.map (fun n -> n, ctx.MkIntConst(sprintf "z_%s" n))
    |> Map.ofArray

let zero = ctx.MkInt(0)
let one = ctx.MkInt(1)

edges
|> Array.map (fun (x,y) ->
    match x,y with
    | _,_ when x = "start" -> ctx.MkGe(ctx.MkAdd(d_vars[(x,y)], z_vars[y]), one)
    | _,_ when y = "end" -> ctx.MkGe(ctx.MkSub(d_vars[(x,y)], z_vars[x]), zero)
    | _,_ -> ctx.MkGe(ctx.MkAdd(ctx.MkSub(d_vars[(x,y)], z_vars[x]), z_vars[y]), zero)
    )
|> Array.iter (fun c -> solver.Add(c))

d_vars
|> Map.toArray
|> Array.map snd
|> Array.collect (fun var -> [|ctx.MkGe(var, zero); ctx.MkLe(var, one)|])
|> Array.iter (fun c -> solver.Add(c))

z_vars
|> Map.toArray
|> Array.map snd
|> Array.collect (fun var -> [|ctx.MkGe(var, zero); ctx.MkLe(var, one)|])
|> Array.iter (fun c -> solver.Add(c))

d_vars
|> Map.toArray
|> Array.map fst
|> Array.distinctBy (fun (x,y) -> Array.sort [|x;y|])
|> Array.filter (fun (x,y) -> x <> "start" && y <> "end")
|> Array.map (fun (x,y) -> ctx.MkEq(d_vars[(x,y)], d_vars[(y,x)]))
|> Array.iter (fun c -> solver.Add(c))

solver.Add(ctx.MkEq(d_vars[("start", s)], zero))
solver.Add(ctx.MkEq(d_vars[(t, "end")], zero))
(*
solver.Add(ctx.MkEq(d_vars[("hfx", "pzl")], one))
solver.Add(ctx.MkEq(d_vars[("pzl", "hfx")], one))
solver.Add(ctx.MkEq(d_vars[("bvb", "cmg")], one))
solver.Add(ctx.MkEq(d_vars[("cmg", "bvb")], one))
solver.Add(ctx.MkEq(d_vars[("nvd", "jqt")], one))
solver.Add(ctx.MkEq(d_vars[("jqt", "nvd")], one))
*)
let sum =
    d_vars
    |> Map.values
    |> Seq.fold (fun state t -> ctx.MkAdd(state, t)) zero

solver.Add(ctx.MkLe(sum, ctx.MkInt(7)))

solver.MkMinimize(sum)


solver.Check()

d_vars
|> Map.toSeq
|> Seq.iter (fun (e, expr) ->
    let i = solver.Model.Evaluate(expr).SExpr()
    if i <> "0" then
        printfn "%A = %s" e i)

z_vars
|> Map.toSeq
|> Seq.iter (fun (e, expr) ->
    let i = solver.Model.Evaluate(expr).SExpr()
    //if i <> "0.0" then
    printfn "%A = %s" e i)

let ans1 = z_vars
           |> Map.toArray
           |> Array.groupBy (fun (e,expr) -> solver.Model.Evaluate(expr).SExpr())
           |> Array.map (fun (k,arr) -> Array.length arr)
           |> Array.fold (*) 1


ans1

/// Part 2

let ans2 = data

ans2