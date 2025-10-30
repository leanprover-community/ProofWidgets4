import ProofWidgets.Data.Html

namespace ProofWidgets
open Lean Server
open scoped Jsx

/-- The result returned by one step of a refresh task. -/
inductive RefreshResult where
  | none
  | last (html : Html)
  | cont (html : Html) (task : Task RefreshResult)

/-- A task that can be queried by a `RefreshComponent` to refresh the display. -/
def RefreshTask := Task RefreshResult

/-- Create a `RefreshTask` from an `IO` computation. -/
def RefreshTask.ofIO (k : IO RefreshResult) : BaseIO RefreshTask :=
  BaseIO.asTask do
    return match ← k.toBaseIO with
    | .error e => .last <| .text s!"Error refreshing this component: {e.toString}"
    | .ok result => result

/-- Create a `RefreshTask` from a `CoreM` computation. -/
def RefreshTask.ofCoreM (k : CoreM RefreshResult) : CoreM RefreshTask := do
  let k := k.run' (← read) (← get)
  BaseIO.asTask do
    match ← k.toBaseIO with
    | .ok result => return result
    | .error e =>
      if let .internal id _ := e then
        if id == interruptExceptionId then
          return .last <| .text "This component was cancelled"
      return .last <|
        <span>
        Error refreshing this component: <InteractiveMessage msg={← WithRpcRef.mk e.toMessageData}/>
        </span>

/-- Create a `RefreshTask` from a `MetaM` computation. -/
def RefreshTask.ofMetaM (k : MetaM RefreshResult) : MetaM RefreshTask := do
  RefreshTask.ofCoreM <| k.run' (← read) (← get)


instance : TypeName RefreshTask := unsafe .mk RefreshTask ``RefreshTask

/-- The result that is sent to a `RefreshComponent` after each query. -/
structure RefreshResultProps where
  /-- The new HTML that will replace the current HTML.
  If it is `none`, then the value of `refresh` is ignored,
  and the `RefreshComponent` stops refreshing. -/
  html : Option Html := none
  /-- The computation that computes the next refresh.
  If it is `none`, then the `RefreshComponent` stops refreshing. -/
  refresh : Option (WithRpcRef RefreshTask) := none
  deriving RpcEncodable, Inhabited

/-- `awaitRefresh` is called through RPC by a `RefreshComponent` to obtain the next HTML to display.
It uses `Task.get` to await the result, blocking the thread until the result is ready. -/
@[server_rpc_method]
partial def awaitRefresh (task : WithRpcRef RefreshTask) :
    RequestM (RequestTask RefreshResultProps) :=
  RequestM.asTask do
    match task.val.get with
    | .none => return {}
    | .last html => return { html }
    | .cont html task => loop html task
where
  /-- We implement the following optimization in `awaitRefresh`:
  If the next task has already finished executing, instead of returning that task,
  we get its value, and repeat. -/
  loop (html : Html) (task : RefreshTask) : BaseIO RefreshResultProps := do
    if ← IO.hasFinished task then
      match task.get with
      | .none => return { html }
      | .last html => return { html }
      | .cont html task => loop html task
    else
      return { html, refresh := ← WithRpcRef.mk task }


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
  /-- The refresh task that is queried for updating the display. -/
  refresh : WithRpcRef RefreshTask
  /-- The cancel token that will be set when the component is unloaded/reloaded. -/
  cancelTk : Option (WithRpcRef IO.CancelToken)
  deriving RpcEncodable

/--
Display an inital HTML, and repeatedly update the display with new HTML objects
as they are computed by the given refresh task.

The refresh task is a `Task`, so it starts running on the Lean server before it is sent to the
widget. The advantage is that there is no delay before the computation starts.
The disadvantage is that if a `RefreshComponent` is reloaded/unloaded too quickly,
it doesn't call `cancelRefresh`, and the refresh task will be running unneccessarily.
So, it is recommended to not rely purely on `cancelRefresh` for cancelling outdated refresh tasks.

As a solution, the cancel token can be stored in a global ref, which can be reset at each call.
This has the side effect that there can be at most one instance of that particular widget
running at any time, and any other instances will say `This component was cancelled`.
This is usually not an issue because there is no reason to have the widget duplicated.
-/
@[widget_module]
def RefreshComponent : Component RefreshComponentProps where
  javascript := include_str ".." /  ".." / ".lake" / "build" / "js" / "RefreshComponent.js"

end ProofWidgets
