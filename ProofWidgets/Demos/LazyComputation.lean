import ProofWidgets.Component.Basic

open ProofWidgets
open Lean Meta Server Elab Tactic

/-- A `MetaM String` continuation, containing both the computation and all monad state. -/
structure MetaMStringCont where
  ci : Elab.ContextInfo
  lctx : LocalContext
  -- We can only derive `TypeName` for type constants, so this must be monomorphic.
  k : MetaM String
  deriving TypeName

structure RunnerWidgetProps where
  /-- A continuation to run and print the results of when the button is clicked. -/
  k : WithRpcRef MetaMStringCont
  -- Make it possible for widgets to receive `RunnerWidgetProps`. Uses the `TypeName` instance.
  deriving RpcEncodable

@[server_rpc_method]
def runMetaMStringCont : RunnerWidgetProps → RequestM (RequestTask String)
  | {k := ⟨{ci, lctx, k}⟩} => RequestM.asTask do
    ci.runMetaM lctx k

@[widget_module]
def runnerWidget : Component RunnerWidgetProps where
  javascript := "
    import { RpcContext, mapRpcError } from '@leanprover/infoview'
    import * as React from 'react';
    const e = React.createElement;

    export default function(props) {
      const [contents, setContents] = React.useState('Run!')
      const rs = React.useContext(RpcContext)
      return e('button', { onClick: () => {
        setContents('Running..')
        rs.call('runMetaMStringCont', props)
          .then(setContents)
          .catch(e => { setContents(mapRpcError(e).message) })
      }}, contents)
    }
  "

syntax (name := makeRunnerTac) "make_runner" : tactic

@[tactic makeRunnerTac] def makeRunner : Tactic
  | `(tactic| make_runner%$tk) => do
    let x : MetaM String := do
      return "Hello, world!"
    -- Store the continuation and monad context.
    let props : RunnerWidgetProps := {
      k := ⟨{
        ci := (← ContextInfo.save)
        lctx := (← getLCtx)
        k := x
      }⟩}
    -- Save a widget together with a pointer to `props`.
    Widget.savePanelWidgetInfo runnerWidget.javascriptHash (rpcEncode props) tk
  | _ => throwUnsupportedSyntax

example : True := by
  make_runner
  trivial
