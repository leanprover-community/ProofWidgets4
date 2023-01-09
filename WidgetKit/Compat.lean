import Lean.Attributes
import Lean.Widget.UserWidget

import WidgetKit.Data.Json

/-!
A compatibility layer aimed at redefining the user widget API in terms of modules and components.
New features:
- component props have Lean types
- props can be RpcEncodable
- we distinguish between an arbitrary 'widget module' (any ES module) and a 'widget component',
  that is a module which can be rendered
- moreover only 'panel widget components' can appear as top-level panels in the infoview

TODO: If the design works out, it could replace the current Lean core definitions.
-/

namespace WidgetKit
open Lean Server Elab

deriving instance TypeName for LocalContext
deriving instance TypeName for Elab.ContextInfo
deriving instance TypeName for Expr

abbrev LazyEncodable α := StateM RpcObjectStore α

instance : RpcEncodable (LazyEncodable Json) where
  rpcEncode fn := fn
  rpcDecode j := return return j

-- back from exile
structure ExprWithCtx where
  ci : Elab.ContextInfo
  lctx : LocalContext
  expr : Expr
  deriving TypeName

def ExprWithCtx.runMetaM (e : ExprWithCtx) (x : Expr → MetaM α) : IO α :=
  e.ci.runMetaM e.lctx (x e.expr)

def widgetDefPostfix : Name := `userWidgetDef

-- NOTE: Compared to core, this is almost like UserWidgetDefinition but with a different "attitude":
-- the module itself need not be a user widget, i.e. it can also be a support library. It doesn't
-- need a displayable `name`.
structure Module where
  /-- An arbitrary JS [module](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Modules)
  intended for use in user widgets. -/
  javascript: String

-- This could maybe be a macro but good luck actually writing it.
open Lean Meta Elab Command in
initialize
  registerBuiltinAttribute {
    name := `widget_module
    descr := "Registers a widget module. Its type must extend WidgetKit.Module."
    applicationTime := AttributeApplicationTime.afterCompilation
    -- The implementation is a hack due to the fact that widgetSource is tied to the storage
    -- of user widgets. I think a single widgetModuleRegistry should suffice. TODO fix in core.
    add := fun nm _ _ => do
      -- The type is not checked; really we just need it to have a `javascript : String` field.
      let proc : CommandElabM Unit := do
        elabCommand <| ← `(command|
          @[widget]
          def $(mkIdent <| nm ++ widgetDefPostfix) : Lean.Widget.UserWidgetDefinition where
            name := $(quote nm.toString)
            javascript := $(mkIdent nm).javascript)
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
  props : LazyEncodable Json
  -- Compatibility hack. Remove if in core.
  infoId : Name
  deriving TypeName

/-- A widget component with a resolved source hash and its props. -/
structure WidgetInstance where
  /-- Name of the `widget_module`. -/
  id : Name
  srcHash : UInt64
  props : LazyEncodable Json
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
  javascript := include_str ".." / "widget" / "dist" / "compat.js"

open scoped Json in
def savePanelWidgetInfo [Monad m] [MonadInfoTree m] [MonadNameGenerator m]
    (stx : Syntax) (id : Name) (props : LazyEncodable Json)  : m Unit := do
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
