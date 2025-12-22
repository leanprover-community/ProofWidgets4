import ProofWidgets.Component.RefreshComponent
import ProofWidgets.Component.HtmlDisplay
import ProofWidgets.Component.OfRpcMethod
import ProofWidgets.Component.Panel.SelectionPanel

/-!
This file showcases the `RefreshComponent` using some basic examples.

We use `dbg_trace` to help see which Lean processes are active at any time.
-/

open Lean.Widget ProofWidgets RefreshComponent Jsx Lean Server


/-! Example 1: Count from 1 to 10, taking one second for each count. -/

partial def countToTen : CoreM Html := do
  mkRefreshComponent (.text "Let's count to ten!!!") <| runRefreshStepM (count 0)
where
  count (n : Nat) : CoreM (RefreshStep CoreM) := do
    IO.sleep 1000
    dbg_trace "counted {n}"
    Core.checkSystem "count to ten"
    if n = 10 then
      return .last <| .text s!"Wie niet weg is, is gezien, ik kom!!!"
    else
      return .cont (.text s!"{n + 1}!!!") (count (n + 1))

-- If you close and reopen the infoview, the counting continues as if it was open the whole time.
#html countToTen

-- Here, we duplicate the widget using the same underlying computation
-- #html (do let x ← countToTen; return <span>{x}<br/>{x}</span>)

-- Here, we duplicate the widget and duplicate the underlying computation
-- #html (return <span>{← countToTen}<br/>{← countToTen}</span>)



/-! Example 2: Print the selected expressions one by one in an infinite loop. -/

@[server_rpc_method]
partial def cycleSelections (props : CancelPanelWidgetProps) : RequestM (RequestTask Html) :=
  RequestM.asTask do
    let some goal := props.goals[0]? | return .text "there are no goals"
    goal.ctx.val.runMetaM {} do
    mkCancelRefreshComponent props.cancelTkRef.val (.text "loading...") <| runRefreshStepM do
      let args ← props.selectedLocations.mapM (·.saveExprWithCtx)
      if h : args.size ≠ 0 then
        have : NeZero args.size := ⟨h⟩
        loop args 0
      else
        return .last <| .text "please select some expression"
where
  loop (args : Array ExprWithCtx) (i : Fin args.size) : MetaM (RefreshStep MetaM) := do
    return .cont <InteractiveCode fmt={← args[i].runMetaM Widget.ppExprTagged}/> do
      dbg_trace "cycled through expression {i}"
      IO.sleep 1000
      Core.checkSystem "cycleSelections"
      have : NeZero args.size := ⟨by cases i; grind⟩
      loop args (i + 1)

@[widget_module]
def cycleComponent : Component CancelPanelWidgetProps :=
  mk_rpc_widget% cycleSelections

elab stx:"cycleSelections" : tactic => do
  let ref ← WithRpcRef.mk (← IO.mkRef (← IO.CancelToken.new))
  Widget.savePanelWidgetInfo (hash cycleComponent.javascript)
    (return json% { cancelTkRef : $(← rpcEncode ref)}) stx

-- run_meta showSelectRefreshWidget cycleComponent

example : 1 + 2 + 3 = 6 ^ 1 ∧ True := by
  constructor
  cycleSelections -- place your cursor here and select some expressions in the goals
  all_goals trivial
-- If you activate the widget multiple times, then thanks to the global cancel token ref,
-- all but one of the widget computations will say `This component was cancelled`.



/-! Example 3: Compute the fibonacci numbers from 400000 to 400040, in parallel,
and show the results as they come up. -/

def fibo (n : Nat) : Nat := Id.run do
  let mut (a, b) := (0, 1)
  for _ in 0...n do
    (a, b) := (b, a + b)
  return a

structure FiboState where
  pending : Array (Nat × Task Html)
  results : Array (Nat × Html) := #[]

def FiboState.update (s : FiboState) : BaseIO FiboState := do
  let mut results := s.results
  let mut pending := #[]
  for (n, t) in s.pending do
    if ← IO.hasFinished t then
      results := results.push (n, t.get)
    else
      pending := pending.push (n, t)
  results := results.insertionSort  (·.1 < ·.1)
  return { results, pending }

def FiboState.render (s : FiboState) (t : Option Nat := none) : Html :=
  let header := match t with
    | some t => s!"Finished in {t}ms"
    | none => "Computing Fibonacci numbers ⏳";
  <details «open»={true}>
    <summary className="mv2 pointer">
      {.text header}
    </summary>
    {Html.element "ul" #[("style", json% { "padding-left" : "30px"})] (s.results.map (·.2))}
  </details>

def generateFibo (n : Nat) : Html :=
  <li>{.text s!"the {n}-th Fibonacci number has {(fibo n).log2} binary digits."}</li>

partial def FiboWidget : CoreM Html := do
  mkRefreshComponent (.text "loading...") <| runRefreshStepM do
    IO.sleep 1 -- If we don't wait 1ms first, the infoview lags too much.
    let pending := (400000...=400040).toArray.map fun n => (n, Task.spawn fun _ => generateFibo n)
    let t0 ← IO.monoMsNow
    loop t0 { pending }
where
  loop (t0 : Nat) (s : FiboState) : CoreM (RefreshStep CoreM) := do
    Core.checkSystem "FiboWidget"
    while !(← s.pending.anyM (IO.hasFinished ·.2)) do
      IO.sleep 10
      Core.checkSystem "FiboWidget"
    let s ← s.update
    if s.pending.isEmpty then
      return .last <| s.render ((← IO.monoMsNow) - t0)
    else
      return .cont s.render (loop t0 s)

-- #html FiboWidget
