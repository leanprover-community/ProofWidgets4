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
  mkRefreshComponentM (.text "Let's count to ten!!!") fun token ↦ do
    let mut n := 0
    repeat do
      IO.sleep 1000
      dbg_trace "counted {n}"
      Core.checkSystem "count to ten"
      if n = 10 then
        token.refresh <| .text s!"Wie niet weg is, is gezien, ik kom!!!"
        break
      token.refresh <| .text s!"{n + 1}!!!"
      n := n + 1
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
    mkCancelRefreshComponent props.cancelTkRef.val (.text "loading...") fun token ↦ do
      let args ← props.selectedLocations.mapM (·.saveExprWithCtx)
      if h : args.size ≠ 0 then
        have : NeZero args.size := ⟨h⟩
        let mut i : Fin args.size := 0
        repeat do
          token.refresh <InteractiveCode fmt={← args[i].runMetaM Widget.ppExprTagged}/>
          dbg_trace "cycled through expression {i}"
          IO.sleep 1000
          Core.checkSystem "cycleSelections"
          have : NeZero args.size := ⟨by cases i; grind⟩
          i := i + 1
      else
        token.refresh <| .text "please select some expression"

@[widget_module]
def cycleComponent : Component CancelPanelWidgetProps :=
  mk_rpc_widget% cycleSelections

elab stx:"cycleSelections" : tactic => do
  let ref ← WithRpcRef.mk (← IO.mkRef (← IO.CancelToken.new))
  Widget.savePanelWidgetInfo (hash cycleComponent.javascript)
    (return json% { cancelTkRef : $(← rpcEncode ref)}) stx

-- run_meta do addPanelWidgetLocal <| ← mkCancelPanelWidget cycleComponent

example : 1 + 2 + 3 = 6 ^ 1 ∧ True := by
  constructor
  cycleSelections -- place your cursor here and select some expressions in the goals
  all_goals trivial
-- If you activate the widget multiple times, they all appear next to eachother as duplicates.



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
  mkRefreshComponentM (.text "loading...") fun token ↦ do
    IO.sleep 1 -- If we don't wait 1ms first, the infoview lags too much.
    let pending := (400000...=400040).toArray.map fun n => (n, Task.spawn fun _ => generateFibo n)
    let t0 ← IO.monoMsNow
    let mut s : FiboState := { pending }
    repeat do
      Core.checkSystem "FiboWidget"
      while !(← s.pending.anyM (IO.hasFinished ·.2)) do
        IO.sleep 10
        Core.checkSystem "FiboWidget"
      s ← s.update
      if s.pending.isEmpty then
        token.refresh (s.render ((← IO.monoMsNow) - t0))
        break
      token.refresh s.render

-- #html FiboWidget

/-! Example 4 -/

/-
This is a demonstration of a bug with widgets.
Whenever the orange bar progresses to the next declaration, the widgets in the infoview reload.
This is undesirable, because it will restart the computation that these widgets may be doing.
-/

@[server_rpc_method]
partial def countToTen' (_ : PanelWidgetProps) : RequestM (RequestTask Html) :=
  RequestM.asTask do
    mkRefreshComponentM (m := EIO Exception) (.text "loading...") fun token ↦ do
      for i in (0 : Nat)...10 do
        token.refresh (.text s!"{i}")
        IO.sleep 1000

@[widget_module]
def countToTen'Component : Component PanelWidgetProps :=
  mk_rpc_widget% countToTen'

elab stx:"#countToTen" : command => Elab.Command.liftCoreM do
  Widget.savePanelWidgetInfo (hash countToTen'Component.javascript)
    (pure (json% {})) stx

elab "#wait" n:num : command => do
  IO.sleep (1000 * .ofNat n.getNat)

/-
Place your cursor on the `#countToTen` command, and observe that it starts counting
0, 1, 2, 3, 4, and then, it goes back to 0, because the orange bar has moved to
the next `#wait 5` command
-/
#countToTen

#wait 5
#wait 5
#wait 5
#wait 5
#wait 5
#wait 5
