import Lean.Attributes
import Lean.Widget.UserWidget
import WidgetKit.Json

/-!
A compatibility layer aimed at redefining the user widget API in terms of components.
New features:
- component props are typed in Lean
- props can be RpcEncodable
- we distinguish between an arbitrary 'widget module' (any ES module) and a 'widget component',
  that is a module which can be rendered
- moreover only 'panel widget components' can appear as top-level panels in the infoview

It could eventually replace the current Lean core definitions.
-/

namespace WidgetKit
open Lean Server Elab

instance : RpcEncodable (StateM RpcObjectStore Json) where
  rpcEncode fn := fn
  rpcDecode j := return return j

def widgetDefPostfix : Name := `userWidgetDef

-- This could maybe be a macro but good luck actually writing it.
open Lean Meta Elab Command in
initialize
  registerBuiltinAttribute {
    name := `widget_module
    descr := "Registers a widget module. It must be a String containing an ES Module."
    applicationTime := AttributeApplicationTime.afterCompilation
    -- The implementation is a hack due to the fact that widgetSource is tied to the storage
    -- of user widgets. TODO fix in core.
    add := fun nm _ _ => do
      let const ← getConstInfo nm
      if !const.type.isConstOf ``String then
        throwError m!"type mismatch, expected{Expr.const ``String []}\nbut got{const.type}"
      let proc : CommandElabM Unit := do
        -- Creates a fake widget only in order to register with widgetSourceRegistry
        elabCommand <| ← `(command|
          @[widget]
          def $(mkIdent <| nm ++ widgetDefPostfix) : Lean.Widget.UserWidgetDefinition where
            name := $(quote nm.toString)
            javascript := $(mkIdent nm))
      let ctx ← read
      let st ← get
      -- Cursed manual CommandElabM => CoreM lift punning all fields
      let (_, st') ← proc.run { st, ctx with tacticCache? := none } |>.run { st, ctx with }
      set { st' with : Core.State }
  : AttributeImpl }

-- "goal widget"/"at cursor widget"/"panel widget"/"info block widget"
structure PanelWidgetInfo where
  /-- Name of the `widget_module` to show. -/
  id : Name
  props : StateM RpcObjectStore Json
  -- Compatibility hack. Remove if in core.
  infoId : Name
  deriving TypeName

/-- A widget component with a resolved source hash and its props. -/
structure WidgetInstance where
  /-- Name of the `widget_module`. -/
  id : Name
  srcHash : UInt64
  props : StateM RpcObjectStore Json
  -- Compatibility hack. Remove if in core.
  infoId : Name
  deriving Server.RpcEncodable

structure PanelWidgetInstance extends WidgetInstance where
  range? : Option Lsp.Range
  deriving Server.RpcEncodable

structure GetPanelWidgetsParams where
  pos : Lsp.Position
  deriving FromJson, ToJson

structure GetPanelWidgetsResponse where
  widgets : Array PanelWidgetInstance
  deriving Server.RpcEncodable

def customInfosAt? (text : FileMap) (t : InfoTree) (hoverPos : String.Pos) : List CustomInfo :=
  t.deepestNodes fun
    | _ctx, i@(Info.ofCustomInfo ci), _cs => do
      if let (some pos, some tailPos) := (i.pos?, i.tailPos?) then
        let trailSize := i.stx.getTrailingSize
        -- show info at EOF even if strictly outside token + trail
        let atEOF := tailPos.byteIdx + trailSize == text.source.endPos.byteIdx
        guard <| pos ≤ hoverPos ∧ (hoverPos.byteIdx < tailPos.byteIdx + trailSize || atEOF)
        return ci
      else
        failure
    | _, _, _ => none

open RequestM in
@[server_rpc_method]
def getPanelWidgets (args : GetPanelWidgetsParams) : RequestM (RequestTask GetPanelWidgetsResponse) := do
  let doc ← readDoc
  let filemap := doc.meta.text
  let pos := filemap.lspPosToUtf8Pos args.pos
  withWaitFindSnap doc (·.endPos >= pos) (notFoundX := return ⟨∅⟩) fun snap => do
    let ws := customInfosAt? filemap snap.infoTree pos
    let ws ← ws.toArray.mapM (fun (w : CustomInfo) => do
      let some wi := w.value.get? PanelWidgetInfo
        | throw <| RequestError.invalidParams "Oops! Unknown ofCustomInfo found. TODO add kinds"
      let some widgetDef := Widget.userWidgetRegistry.find? snap.env (wi.id ++ widgetDefPostfix)
        | throw <| RequestError.invalidParams s!"No registered widget source with id {wi.id}"
      return {
        wi with
        srcHash := widgetDef.javascriptHash
        range? := String.Range.toLspRange filemap <$> Syntax.getRange? w.stx
      })
    return {widgets := ws}

@[widget]
def metaWidget : Lean.Widget.UserWidgetDefinition where
  name := "We should get rid of this header and make panels general block elements"
  javascript := "
    import * as React from 'react'
    import { DynamicComponent, RpcContext, useAsync } from '@leanprover/infoview'
    const e = React.createElement

    // TODO add importWidgetModule to @leanprover/infoview

    export default function(props_) {
      const {pos, infoId, ...props} = props_
      const rs = React.useContext(RpcContext)

      const st = useAsync(async () => {
        const ws = await rs.call('WidgetKit.getPanelWidgets', { pos })
        if (ws === undefined) return undefined
        const ret = []
        for (const w of ws.widgets) {
          if (w.infoId === infoId) {
            ret.push(w)
          }
        }
        return ret.map(w => e(DynamicComponent, {
            pos, hash: w.srcHash, props: { ...w.props, ...props, pos }
          }, null))
      }, [rs, infoId, pos])

      return st.state === 'resolved' ? e(React.Fragment, null, st.value)
        : e('div', null, JSON.stringify(st))
    }
  "

open scoped Json in
def savePanelWidgetInfo [Monad m] [MonadInfoTree m] [MonadNameGenerator m]
    (stx : Syntax) (id : Name) (props : StateM RpcObjectStore Json)  : m Unit := do
  let infoId := `panelWidget ++ (← mkFreshId)
  pushInfoLeaf <| .ofUserWidgetInfo {
    stx
    widgetId := ``metaWidget
    props := json% {
      infoId : $(infoId)
    }
  }
  let wi : PanelWidgetInfo := { id, props, infoId }
  pushInfoLeaf <| .ofCustomInfo { stx, value := Dynamic.mk wi }

end WidgetKit
