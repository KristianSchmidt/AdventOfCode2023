#load "Helpers.fsx"
#r "nuget: Microsoft.Z3"

open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

type Hail =
    { px : decimal; py : decimal; pz : decimal
      vx : decimal; vy : decimal; vz : decimal }

let data =
    Helpers.Web.getInput 24
    |> Array.map (
        function
        | Helpers.Regex "(\d+), (\d+), (\d+) @ (.+), (.+), (.+)" [px;py;pz;vx;vy;vz]
            -> { px = decimal px; py = decimal py; pz = decimal pz
                 vx = decimal vx; vy = decimal vy; vz = decimal vz }
       )

let min = 200000000000000M
let max = 400000000000000M

let mutable count = 0
for i in 0 .. data.Length - 2 do
    for j in i .. data.Length - 1 do
        let h1 = data[i]
        let h2 = data[j]
        let (a,b,c,d,e,f) = h2.vx, -h1.vx, h2.vy, -h1.vy, (h1.px-h2.px), (h1.py-h2.py)
        let det = a*d - b*c
        if (det <> 0m) then
          let t = (e*d - b*f)/det
          let s = (a*f - e*c)/det;
          let x1, y1 = h1.px + h1.vx * s, h1.py + h1.vy * s
          let x2, y2 = h2.px + h2.vx * t, h2.py + h2.vy * t
          if (x1 >= min && x1 <= max && y1 >= min && y1 <= max &&
              x2 >= min && x2 <= max && y2 >= min && y2 <= max &&
              t >= 0M && s >= 0M) then
              count <- count + 1
              printfn "(x1,y1) = (%A,%A)" x1 y1
              printfn "(x2,y2) = (%A,%A)" x2 y2

let ans1 = data

ans1

/// Part 2

open Microsoft.Z3

let ctx = new Context()

let solver = ctx.MkSolver()

let px_sol, py_sol, pz_sol = ctx.MkIntConst("px_sol"), ctx.MkIntConst("py_sol"), ctx.MkIntConst("pz_sol")
let vx_sol, vy_sol, vz_sol = ctx.MkIntConst("vx_sol"), ctx.MkIntConst("vy_sol"), ctx.MkIntConst("vz_sol")

let zero = ctx.MkInt(0)

let mkConstraint (h : Hail) (p : int64) (v : int64) pSolVar vSolVar timeDim =
    ctx.MkEq(
      ctx.MkAdd(pSolVar, ctx.MkMul(vSolVar, timeDim)),
      ctx.MkAdd(ctx.MkInt(p), ctx.MkMul(ctx.MkInt(v), timeDim))
    )
    
let addForHail (i : int) (h : Hail) =
    let timeVar = ctx.MkIntConst(sprintf "time_%i" i)
    solver.Add(ctx.MkGt(timeVar, zero))
    solver.Add(mkConstraint h (int64 h.px) (int64 h.vx) px_sol vx_sol timeVar)
    solver.Add(mkConstraint h (int64 h.py) (int64 h.vy) py_sol vy_sol timeVar)
    solver.Add(mkConstraint h (int64 h.pz) (int64 h.vz) pz_sol vz_sol timeVar)

// Add data for all constraints, but we
// actually only need 3 of them, since that would create
// 9 equations with 9 unknowns
data |> Array.iteri addForHail

solver.Check()

let ans2 =
    [ int64 <| solver.Model.Evaluate(px_sol).SExpr()
      int64 <| solver.Model.Evaluate(py_sol).SExpr()
      int64 <| solver.Model.Evaluate(pz_sol).SExpr() ]
    |> List.sum

ans2