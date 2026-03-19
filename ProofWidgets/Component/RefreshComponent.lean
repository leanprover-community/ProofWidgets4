/-
Copyright (c) 2025 Jovan Gerbscheid. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Jovan Gerbscheid
-/
module

public import ProofWidgets.Component.Panel.Basic
public import ProofWidgets.Data.Html
public import ProofWidgets.Util

/-!
# The `RefreshComponent` widget

This file defines `RefreshComponent`, which allows you to have an HTML widget that updates
incrementally as more results are computed by a Lean computation.

## Known limitations

Cancellation is a bit hard to get right, and there is one limitation.

1. When using a `RefreshComponent` that comes directly from a command/tactic (no shift-clicking),
then we pass the cancel token used by elaboration to the refresh component computation.
Unfortunately there is an edge case where the cancel token gets set while the corresponding
command does not get re-elaborated. In particular, if you have three command in a row, e.g.
```
#html countToTen
#html countToTen
#html countToTen
```
Then commenting out the third command will cancel the widget of the first command.
It refreshes the second command, and doesn't affect commands before the first command - only
the first command gets badly affected.
-/

public meta section

/-- An effectful wrapper around `Thunk.get` analogous to `IO.wait`.
Unlike `Thunk.get`, the compiler should never move this out of a `do` block. -/
@[noinline]
private opaque IO.forceThunk (t : Thunk α) : BaseIO α :=
  return t.get

local instance [Inhabited α] : Inhabited (Thunk α) :=
  ⟨Thunk.mk fun _ => default⟩

namespace ProofWidgets
open Lean Server Widget Jsx
namespace RefreshComponent

/-- A HTML tree together with a version number.
We use this to check whether the tree currently on display is up to date. -/
structure VersionedHtml where
  html : Html
  idx : Nat
  deriving RpcEncodable

/-- The server-side state of a `RefreshComponent`. -/
structure RefreshState where
  /-- The HTML tree to display, stored as a delayed computation.

  We only force the thunk when the client requests it.
  This means that if several updates arrive in quick succession,
  only the ones that the client actually sees will be computed. -/
  curr : Thunk Html
  /-- Version of the current HTML tree.
  It should increase by `1` with each state update. -/
  idx : Nat
  /-- A task that resolves when this state has become out-of-date.
  It resolves with `some ()` if a new state is available,
  and with `none` if the display will never be updated again.

  It is implemented using `RefreshToken.promise.result?`,
  but we don't store the `Promise` here
  in order to make sure it can reach a refcount of 0. -/
  next : Task (Option Unit)
  deriving Inhabited

/-- A reference to a `RefreshState`.
Only exists because `TypeName` is not derivable for compound types. -/
structure RefreshRef where
  ref : IO.Ref RefreshState
  deriving TypeName

/-- The data used to call `awaitRefresh`, for updating the HTML display. -/
structure AwaitRefreshParams where
  state : WithRpcRef RefreshRef
  /-- The index of the HTML tree that is currently on display. -/
  oldIdx : Nat
  deriving RpcEncodable

/-- If any updates are available (i.e., the server-side state is more recent than the client's),
immediately return the most recent version of the HTML tree.
Otherwise await the next update.
Returns `none` if the client's state is the last one that should be shown. -/
@[server_rpc_method]
def awaitRefresh (ps : AwaitRefreshParams) : RequestM (RequestTask (Option VersionedHtml)) := do
  let { curr, idx, next } ← ps.state.val.ref.get
  if ps.oldIdx < idx then
    -- We have a more recent state than the client. Send the current state immediately.
    RequestM.asTask do
      let html ← IO.forceThunk curr
      return some { html, idx }
  else
    -- We have the same state as the client. Wait for an update.
    RequestM.mapTaskCostly ⟨next⟩ fun
      | some () => do
        let { curr, idx, .. } ← ps.state.val.ref.get
        let html ← IO.forceThunk curr
        return some { html, idx }
      | none => return none

deriving instance TypeName for IO.CancelToken

structure Props where
  /-- Initial state of the component. -/
  state : WithRpcRef RefreshRef
  /-- Will be cancelled whenever the widget is rendered with a new `state`, or unmounted,
  or when the whole infoview is closed. -/
  cancelTk : WithRpcRef IO.CancelToken
  deriving RpcEncodable

/-- HACK: We cancel background threads driving `RefreshComponent`s when they are no longer needed.
However, there is currently no good way to do this when the infoview closes,
as in that case React effect cleanup functions are never executed.
Instead, `RefreshComponent` calls this persistent monitor task during creation.
The call is auto-cancelled when the infoview closes, setting `cancelTk`. -/
@[server_rpc_method]
partial def monitor (p : Props) : RequestM (RequestTask Unit) :=
  RequestM.asTask do
    repeat do
      IO.sleep 1000
      if ← (← read).cancelTk.wasCancelledByCancelRequest then
        p.cancelTk.val.set
        return

end RefreshComponent

open RefreshComponent

/-- Displays a HTML tree, refreshing the display when the tree is updated.

Use `mkRefreshComponentM` to conveniently spawn a dedicated thread
from which you can update the display.

The component resets its state when rendered with a new `props.state`.
Conversely, reusing `props.state` preserves the client-side state.  -/
@[widget_module]
def RefreshComponent : Component Props where
  javascript := include_str ".." /  ".." / ".lake" / "build" / "js" / "RefreshComponent.js"

/-! ## API for creating a `RefreshComponent` -/

/-- A `RefreshToken` allows you to manage a `RefreshComponent` instance.

Use `RefreshToken.refresh` to update the HTML currently on display.
Use `RefreshToken.cancelTk` to check if the instance has been discarded. -/
structure RefreshToken where
  state : IO.Ref RefreshState
  /-- If set, the `RefreshComponent` is no longer displayed in the UI.
  The `RefreshToken` should be discarded, and any associated thread should exit.

  For early cooperative cancellation of `CoreM` computations,
  we recommend passing this to `Core.Context`. -/
  cancelTk : IO.CancelToken
  /-- The promise that `RefreshState.next` waits for.

  If we drop the `RefreshToken`, and hence this promise,
  `RefreshState.next` will resolve to `none`.
  This ensures that no `awaitRefresh` call is left hanging forever
  when all threads holding onto this `RefreshToken` have exited. -/
  private promise : IO.Ref (IO.Promise Unit)

/-- Create a `RefreshToken` with an initial HTML tree. -/
private def RefreshToken.new (initial : Thunk Html) : BaseIO RefreshToken := do
  let promise ← IO.Promise.new
  let state := {
    curr := initial
    -- Client's initial version is `0`; this state is an update on that.
    idx := 1
    next := promise.result?
  }
  return {
    state := ← IO.mkRef state
    cancelTk := ← IO.CancelToken.new
    promise := ← IO.mkRef promise
  }

/-- Update the current HTML tree to be `html`.

The `html` thunk is only forced when the client requests it;
if another update is made before that, it will never be forced.

This function is thread-safe: each update is made atomically. -/
def RefreshToken.refreshLazy (token : RefreshToken) (html : Thunk Html) : BaseIO Unit := do
  let { state, promise, .. } := token
  let newPromise ← IO.Promise.new
  -- `state` is the ref that guards the critical region.
  let st ← unsafe state.take
  let oldPromise ← promise.swap newPromise
  state.set {
    curr := html
    idx := st.idx + 1
    next := newPromise.result?
  }
  oldPromise.resolve ()

/-- Update the current HTML tree to be `html`. See also `refreshLazy`. -/
def RefreshToken.refresh (token : RefreshToken) (html : Html) : BaseIO Unit :=
  token.refreshLazy (.pure html)

/-- Create a `RefreshComponent` instance together with a token to manage it. -/
def mkRefreshComponent (initial : Html := .text "") : BaseIO (Html × RefreshToken) := do
  let token ← RefreshToken.new initial
  return (<RefreshComponent state={← WithRpcRef.mk ⟨token.state⟩}
      cancelTk={← WithRpcRef.mk token.cancelTk} />, token)

variable {m} [Monad m] [MonadSaveCtx m (EIO Exception)] [MonadLiftT BaseIO m]

/-- Create a `RefreshComponent` together with a dedicated thread that drives it by running `k`. -/
def mkRefreshComponentM (initial : Html) (k : RefreshToken → m Unit) : m Html := do
  let (html, token) ← mkRefreshComponent initial
  discard <| BaseIO.asTask (prio := .dedicated) <|
    (← saveCtxM <| k token).catchExceptions fun ex => do
      if let .internal id _ := ex then
        -- TODO: This should never be shown once we fix cancellation in all situations.
        if id == interruptExceptionId then
          token.refresh <| .text "This component was cancelled"
      else
        token.refresh <span>
            An error occurred while refreshing this component:
            <InteractiveMessage msg={← WithRpcRef.mk ex.toMessageData}/>
          </span>
  return html

end ProofWidgets
