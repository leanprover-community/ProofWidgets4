/-
Copyright (c) 2025 Jovan Gerbscheid. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Jovan Gerbscheid
-/
import ProofWidgets.Data.Html

/-!
## `RefreshComponent`

This file defines `RefreshComponent`, which allows you to have an HTML widget that updates
incrementally as more results are computed by a Lean computation.

To make this interaction work, we use an `IO.Ref` that the Lean and JavaScript both have access to.
It stores the HTML that should currently be on display, and a task that computes the next HTML.

When the widget (re)loads, it first needs to query the current HTML, before awaiting the next HTML.
-/

class AsTask (m : Type → Type) (n : outParam <| Type → Type) where
  asTask {α} (act : m α) (prio : Task.Priority := Task.Priority.default) : m (Task (n α))

export AsTask (asTask)

instance : AsTask BaseIO (·) where
  asTask act prio := BaseIO.asTask act prio

instance {ε} : AsTask (EIO ε) (Except ε) where
  asTask act prio := EIO.asTask act prio

instance {ρ m n} [AsTask m n] : AsTask (ReaderT ρ m) n where
  asTask act prio := fun ctx => asTask (act ctx) prio

instance {σ m n} [AsTask m n] [Monad m] : AsTask (StateT σ m) n where
  asTask act prio := do StateT.lift <| asTask (act.run' (← get)) prio

instance {ω σ : Type} {m n} [AsTask m n] [Monad m] [STWorld ω m] [MonadLiftT (ST ω) m] :
    AsTask (StateRefT σ m) n where
  asTask act prio := do StateRefT'.lift <| asTask (act.run' (← get)) prio

open Lean in
instance (priority := high) : AsTask CoreM (Except Exception) where
  asTask act prio := asTask (Core.withCurrHeartbeats act) prio

namespace ProofWidgets
open Lean Server Widget Jsx
namespace RefreshComponent

-- /-- The result that is sent to the `RefreshComponent` after each query. -/
-- structure RefreshProps where
--   /-- The new HTML that will replace the current HTML. -/
--   html : Html
--   /-- Whether the `RefreshComponent` should continue to refresh. -/
--   refresh : Bool := false
--   deriving RpcEncodable--, Inhabited

structure ResultProps where
  html : Html
  idx : Nat
  deriving RpcEncodable, Inhabited

/-- The `RefreshState` stores the incremental result of the HTML computation. -/
structure RefreshState where
  curr : ResultProps
  next : Task (Option ResultProps)
  -- /-- The state that the widget should currently be in.
  -- If `new := true`, then this result is currenlty not yet shown in the widget. -/
  -- | result (new : Bool) (result : RefreshProps)
  -- /-- The widget is awaiting a refresh. To pass it to the widget, `promise` should be resolved.
  -- If the widget is loaded, then `curr` is the HTML that is currently shown. -/
  -- | awaiting (promise : IO.Promise RefreshProps) (curr : Html)
  -- deriving Inhabited

/-- A reference to a `RefreshState`. This is used to keep track of the refresh state. -/
structure RefreshRef where
  ref : IO.Ref RefreshState
  deriving TypeName

structure RequestProps where
  state : WithRpcRef RefreshRef
  oldIdx : Nat
  deriving RpcEncodable


/-- `awaitRefresh` is called through RPC to obtain the next HTML to display.
If the result is not yet available, it creates an `IO.Promise` and uses `ServerTask.mapCheap`,
so that this function returns directly when the promise is resolved. -/
@[server_rpc_method]
def awaitRefresh (props : RequestProps) : RequestM (RequestTask (Option ResultProps)) := do
  let { oldIdx, state } := props
  let { curr, next } ← state.val.ref.get
  match compare oldIdx curr.idx with
  | .lt => return .pure curr
  | .eq => return .mapCheap .ok ⟨next⟩
  | .gt => panic! "how is the index bigger in JavaScript than in Lean??"

/-- `getCurrState` is called through RPC whenever the widget reloads.
This can be because the infoview was closed and reopened.
But it can also be because a differen expression was selected in the goal. -/
@[server_rpc_method]
def getCurrState (ref : WithRpcRef RefreshRef) : RequestM (RequestTask ResultProps) := do
  return .pure (← ref.val.ref.get).curr


deriving instance TypeName for IO.CancelToken

/-- `cancelRefresh` is called through RPC by `RefreshComponent` upon cancellation.
It sets the cancel token for the task(s) on the Lean side. -/
@[server_rpc_method]
def cancelRefresh (cancelTk : WithRpcRef IO.CancelToken) : RequestM (RequestTask String) :=
  RequestM.asTask do cancelTk.val.set; return "ok"


/-- The arguments passed to `RefreshComponent`. -/
structure RefreshComponentProps where
  /-- The initial HTML that is displayed. Usually this is a "loading..." kind of message. -/
  initial : Html
  /-- The refresh state that is queried for updating the display. -/
  state : WithRpcRef RefreshRef
  /-- The cancel token that will be set when the component is unloaded/reloaded. -/
  cancelTk : Option (WithRpcRef IO.CancelToken)
  deriving RpcEncodable

/-- Display an inital HTML, and repeatedly update the display with new HTML objects
as they appear in `state`. A dedicated thread should be spawned in order to modify `state`. -/
@[widget_module]
def RefreshComponent : Component RefreshComponentProps where
  javascript := include_str ".." /  ".." / ".lake" / "build" / "js" / "RefreshComponent.js"


/-! ## API for creating `RefreshComponent`s -/

/-- A monad transformer for keeping track of a `RefreshRef`. -/
abbrev RefreshT (m : Type → Type) [STWorld IO.RealWorld m] :=
  ReaderT (IO.Promise ResultProps) StateRefT RefreshState m

variable {m : Type → Type} [Monad m] [STWorld IO.RealWorld m] [MonadLiftT BaseIO m]
  [MonadLiftT (ST IO.RealWorld) m]

def RefreshT.run {α} (initial : Html) (x : RefreshT m α) : m α := do
  let promise ← IO.Promise.new
  let ref ← IO.mkRef { curr := { html := initial, idx := 0 }, next := promise.result? }
  x promise ref

inductive RefreshResult (m : Type → Type) [STWorld IO.RealWorld m] where
  | none
  | last (html : Html)
  | cont (html : Html) (cont : RefreshT m Unit)
  deriving Inhabited

/-- Update the `RefreshState` from the context, using `k`. -/
def refresh (k : RefreshResult m) : RefreshT m Unit := do
  let idx := (← get).curr.idx + 1
  match k with
  | .none =>
    -- we drop the reference to the promise, so the corresponding task will return `none`.
    modify fun { curr, .. } => { curr, next := .pure none }
  | .last html =>
    (← read).resolve { html, idx }
    MonadState.set { curr := { html, idx }, next := .pure none }
  | .cont html cont =>
    (← read).resolve { html, idx }
    let newPromise ← IO.Promise.new
    MonadState.set { curr := { html, idx }, next := newPromise.result? }
    withReader (fun _ => newPromise) cont

def refreshM [i : MonadAlwaysExcept Exception m] (k : m (RefreshResult m)) : RefreshT m Unit := do
  have := i.except
  refresh <| ←
    try k
    catch e =>
      if let .internal id _ := e then
        if id == interruptExceptionId then
          return .last <| .text "This component was cancelled"
      return .last
        <span>
          Error refreshing this component: <InteractiveMessage msg={← WithRpcRef.mk e.toMessageData}/>
        </span>

end RefreshComponent

open RefreshComponent

/--
Create a `RefreshComponent` in the context of a `Widget.InteractiveGoal`.
This should be used when the refreshing widget depends on shift-clicked expressions in the goal.

Warning: the thread that is updating `state` has started running on the Lean server before the
widget is activated. The advantage is that there is no delay before the computation starts.
The disadvantage is that if a `RefreshComponent` si reloaded/unloaded too quickly,
it doesn't call `cancelRefresh`, and the thread will continue running unneccessarily.
So, it is recommended to not rely purely on `cancelRefresh` for cancelling outdated refresh tasks.

As a solution, the cancel token can be stored in a global ref, which should be reset at each call.
This has the side effect that there can be at most one instance of that particular widget
running at any time, and any other instances will say `This component was cancelled`.
This is usually not an issue because there is no reason to have the widget duplicated.
-/
def mkGoalRefreshComponent (goal : Widget.InteractiveGoal) (cancelTk : IO.CancelToken)
    (initial : Html) (k : RefreshT MetaM Unit) :
    BaseIO Html := do
  let promise ← IO.Promise.new
  let ref ← IO.mkRef { curr := { html := initial, idx := 0 }, next := promise.result? }
  discard <| IO.asTask (prio := .dedicated) do
    goal.ctx.val.runMetaM {} do withTheReader Core.Context ({ · with cancelTk? := cancelTk }) do
      let decl ← goal.mvarId.getDecl
      let lctx := decl.lctx |>.sanitizeNames.run' {options := (← getOptions)}
      Meta.withLCtx lctx decl.localInstances <| k promise ref
  return <RefreshComponent
    initial={initial}
    state={← WithRpcRef.mk { ref }}
    cancelTk={← WithRpcRef.mk cancelTk}/>

def mkRefreshComponentM {m n} [Monad m] [AsTask m n] [STWorld IO.RealWorld m] [MonadLiftT BaseIO m]
    (initial : Html) (k : RefreshT m Unit) : m Html := do
  let promise ← IO.Promise.new
  let ref ← IO.mkRef { curr := { html := initial, idx := 0 }, next := promise.result? }
  discard <| asTask (prio := .dedicated) <| k promise ref
  return <RefreshComponent
    initial={initial}
    state={← WithRpcRef.mk { ref }}
    cancelTk={none}/>

end ProofWidgets
