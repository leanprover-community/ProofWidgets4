import Lean.Data.Json.FromToJson
import Lean.Server.Rpc.RequestHandling
import ProofWidgets.Compat

/-! Experimental support for cancellable RPC requests.

Note: Cancellation should eventually become a feature of the core RPC protocol,
and the requests map should be stored in `RequestM`,
or somewhere in the server anyway. -/

namespace ProofWidgets
open Lean Server Meta

abbrev RequestId := Nat
structure CancellableTask where
  task : Task (Except RequestError (LazyEncodable Json))
  /- Note: we cannot just `IO.cancel task` because it is a result of `map`.
  See https://leanprover.zulipchat.com/#narrow/stream/270676-lean4/topic/Should.20cancelling.20a.20purely.20mapped.20task.20cancel.20the.20original.3F -/
  cancel : IO Unit

/-- Maps the ID of each currently executing request to its task. -/
initialize runningRequests :
    IO.Ref (RequestId × HashMap RequestId CancellableTask) ←
  IO.mkRef (0, HashMap.empty)

/-- Transforms a request handler returning `β`
into one that returns immediately with a `RequestId`.

The ID uniquely identifies the running request:
its results can be retrieved using `checkRequest`,
and it can be cancelled using `cancelRequest`. -/
def mkCancellable [RpcEncodable β] (handler : α → RequestM (RequestTask β)) :
    α → RequestM (RequestTask RequestId) := fun a => do
  RequestM.asTask do
    let t ← handler a
    let t' := t.map (·.map rpcEncode)
    runningRequests.modifyGet fun (id, m) =>
      (id, (id+1, m.insert id ⟨t', IO.cancel t⟩))

/-- Cancel the request with ID `rid`.
Does nothing if `rid` is invalid. -/
@[server_rpc_method]
def cancelRequest (rid : RequestId) : RequestM (RequestTask String) := do
  RequestM.asTask do
    let t? ← runningRequests.modifyGet fun (id, m) => (m.find? rid, (id, m.erase rid))
    if let some t := t? then
      t.cancel
    return "ok"

/-- The status of a running cancellable request. -/
inductive CheckRequestResponse
  | running
  | done (result : LazyEncodable Json)
  deriving RpcEncodable

/-- Check whether a request has finished computing,
and return the response if so.
The request is removed from `runningRequests` the first time it is checked
and found to have finished.
Throws an error if the `rid` is invalid,
or if the request itself threw an error. -/
/- NOTE: a notification-based version would be better than this polling-based one.
But we cannot include RPC references in notifications atm;
another possible addition to the RPC protocol? -/
@[server_rpc_method]
def checkRequest (rid : RequestId) : RequestM (RequestTask CheckRequestResponse) := do
  RequestM.asTask do
    let (_, m) ← runningRequests.get
    match m.find? rid with
    | none =>
      throw $ RequestError.invalidParams
        s!"Request '{rid}' has already finished, or the ID is invalid."
    | some t =>
      if !(← IO.hasFinished t.task) then
        return .running
      runningRequests.modify fun (id, m) => (id, m.erase rid)
      match t.task.get with
      | .error e => throw e
      | .ok v    => return .done v

def cancellableSuffix : Name := `_cancellable

initialize
  registerBuiltinAttribute {
    name := `server_rpc_method_cancellable
    descr := "Like `server_rpc_method`, \
    but requests for this method can be cancelled. \
    The method should check for that using `IO.checkCanceled`. \
    Cancellable methods are invoked differently from JavaScript: \
    see `callCancellable` in `cancellable.ts`."
    applicationTime := AttributeApplicationTime.afterCompilation
    add := fun decl _ _ => Prod.fst <$> MetaM.run do
      let name := decl ++ cancellableSuffix
      let value ← mkAppM ``mkCancellable #[mkConst decl]
      addAndCompile $ .defnDecl {
        name
        levelParams := []
        type := ← inferType value
        value
        hints  := .opaque
        safety := .safe
      }
      registerRpcProcedure name
  }

end ProofWidgets
