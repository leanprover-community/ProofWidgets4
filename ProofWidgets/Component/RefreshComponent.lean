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

namespace ProofWidgets
open Lean Server Widget Jsx
namespace RefreshComponent

/-- An HTML tree together with a task that may yield a new tree in the future.
This is the server-side state of a `RefreshComponent` instance. -/
structure UpdatableHtml where
  /-- The HTML tree that the component should currently be displaying. -/
  curr : Html
  /-- A task that produces either the next HTML tree to display,
  or `none` if the display will never be updated again.
  It is implemented using `RefreshToken.promise.result?`,
  but we don't store the `Promise` here
  in order to make sure it can reach a refcount of 0. -/
  next : Task (Option UpdatableHtml)
  deriving TypeName, Nonempty

/-- Wait for `state.next` and return the result.
In case this result is `some nextState`, also return `nextState.curr`. -/
@[server_rpc_method]
def awaitNextHtml (state : WithRpcRef UpdatableHtml) :
    RequestM (RequestTask (Option (Html × WithRpcRef UpdatableHtml))) :=
  let { next, .. } := state.val
  RequestM.mapTaskCheap ⟨next⟩ fun nextState? =>
    nextState?.mapM fun nextState =>
      return (nextState.curr, ← WithRpcRef.mk nextState)

/-- Return `state.curr`. -/
@[server_rpc_method]
def getCurrHtml (state : WithRpcRef UpdatableHtml) : RequestM (RequestTask Html) :=
  return .pure state.val.curr

deriving instance TypeName for IO.CancelToken

structure Props where
  /-- Initial state of the component. -/
  state : WithRpcRef UpdatableHtml
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
        return ()

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
  /-- If this token is set, the `RefreshToken` is no longer displayed in the UI.
  Subsequent `refresh` calls will have no effect.
  We recommend passing this token to `Core.Context` if running `CoreM` computations. -/
  cancelTk : IO.CancelToken
  /-- The promise that `UpdatableHtml.next` waits for.
  If we drop the `RefreshToken`, and hence this promise,
  `UpdatableHtml.next` will resolve to `none`.
  This ensures that no `awaitNextHtml` call is left hanging forever
  when all threads holding onto this `RefreshToken` have exited. -/
  private promise : IO.Ref (IO.Promise UpdatableHtml)

/-- Create a fresh `RefreshToken`. -/
private def RefreshToken.new : BaseIO RefreshToken := do
  let promise ← IO.Promise.new
  return { cancelTk := ← IO.CancelToken.new, promise := ← IO.mkRef promise }

/-- Update the current HTML tree to be `html`.

This function makes use of `ST.Ref.take` in order to be thread safe.
That is, if multiple threads call `refresh` with the same token,
each thread will make its update atomically. -/
def RefreshToken.refresh (token : RefreshToken) (html : Html) : BaseIO Unit := unsafe do
  let { promise, .. } := token
  let newPromise ← IO.Promise.new
  let oldPromise ← promise.take
  oldPromise.resolve { curr := html, next := newPromise.result? }
  promise.set newPromise

/-- Create a `RefreshComponent` instance together with a token to manage it. -/
def mkRefreshComponent (initial : Html := .text "") : BaseIO (Html × RefreshToken) := do
  let token ← RefreshToken.new
  let html := { curr := initial, next := (← token.promise.get).result? }
  return (<RefreshComponent state={← WithRpcRef.mk html}
      cancelTk={← WithRpcRef.mk token.cancelTk} />, token)

variable {m} [Monad m] [MonadSaveCtx m (EIO Exception)] [MonadLiftT BaseIO m]

/-- Create a `RefreshComponent` together with a dedicated thread that drives it by running `k`. -/
def mkRefreshComponentM (initial : Html) (k : RefreshToken → m Unit) : m Html := do
  let (html, token) ← mkRefreshComponent initial
  discard <| BaseIO.asTask (prio := .dedicated) <|
    (← saveCtxM <| k token).catchExceptions fun ex => do
      -- TODO: This should never be shown once we fix cancellation in all situations.
      if let .internal id _ := ex then
        if id == interruptExceptionId then
          token.refresh <| .text "This component was cancelled"
      else
        token.refresh <span>
            An error occurred while refreshing this component:
            <InteractiveMessage msg={← WithRpcRef.mk ex.toMessageData}/>
          </span>
  return html

end ProofWidgets
