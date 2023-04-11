import Lean.Attributes
import Lean.Widget.UserWidget

import ProofWidgets.Data.Json

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

namespace ProofWidgets
open Lean Server Elab

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

def ExprWithCtx.save (e : Expr) : MetaM ExprWithCtx :=
  return {
    ci := ← ContextInfo.save
    lctx := ← getLCtx
    expr := e
  }

open Command in
/-- Derive `Lean.Server.RpcEncodable` for a type.

HACK: around https://leanprover.zulipchat.com/#narrow/stream/341532-lean4-dev/topic/Should.20instance.20names.20inherit.20macro.20scopes.3F -/
elab "#mkrpcenc" n:ident : command => do
  elabCommand <| ← `(
    namespace $n
    deriving instance Lean.Server.RpcEncodable for $n
    end $n
  )

def widgetDefPostfix : Name := `userWidgetDef

-- NOTE: Compared to core, this is almost like UserWidgetDefinition but with a different "attitude":
-- the module itself need not be a user widget, i.e. it can also be a support library. It doesn't
-- need a displayable `name`.
/-- A widget module is a unit of source code that can execute in the infoview. -/
structure Module where
  /-- A JS [module](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Modules) intended
  for use in user widgets.

  To initialize this field from an external JS file, use `include_str "path"/"to"/"file.js"`.
  However **beware** that this does not register a dependency with Lake, so your Lean module will
  not automatically be rebuilt when the `.js` file changes. -/
  javascript : String

-- This could maybe be a macro but good luck actually writing it.
open Lean Meta Elab Command in
initialize
  registerBuiltinAttribute {
    name := `widget_module
    descr := "Registers a widget module. Its type must extend ProofWidgets.Module."
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

#mkrpcenc WidgetInstance

structure PanelWidgetInstance extends WidgetInstance where
  range? : Option Lsp.Range

#mkrpcenc PanelWidgetInstance

structure GetPanelWidgetsParams where
  pos : Lsp.Position
  deriving FromJson, ToJson

structure GetPanelWidgetsResponse where
  widgets : Array PanelWidgetInstance

#mkrpcenc GetPanelWidgetsResponse

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
def getPanelWidgets (args : GetPanelWidgetsParams) : RequestM (RequestTask GetPanelWidgetsResponse)
    := do
  let doc ← readDoc
  let filemap := doc.meta.text
  let pos := filemap.lspPosToUtf8Pos args.pos
  withWaitFindSnap doc (·.endPos >= pos) (notFoundX := return ⟨∅⟩) fun snap => do
    let ws := customInfosAt? filemap snap.infoTree pos
    let mut widgets := #[]
    for w in ws do
      let some wi := w.value.get? PanelWidgetInfo
        -- We may encounter other custom infos of unknown type. Ignore them.
        | continue
      let some widgetDef := Widget.userWidgetRegistry.find? snap.env (wi.id ++ widgetDefPostfix)
        | throw <| RequestError.invalidParams s!"No registered widget source with id {wi.id}"
      widgets := widgets.push { wi with
        srcHash := widgetDef.javascriptHash
        range? := String.Range.toLspRange filemap <$> Syntax.getRange? w.stx
      }
    return {widgets}

@[widget]
def metaWidget : Lean.Widget.UserWidgetDefinition where
  -- The header is sometimes briefly visible before compat.tsx loads and hides it
  name := "Loading ProofWidgets.."
  javascript := include_str ".." / "build" / "js" / "compat.js"

open scoped Json in
/-- Save the data of a panel widget which will be displayed whenever the text cursor is on `stx`.
`id` must be the name of a definition annotated with `@[widget_module]`. See `PanelWidgetProps`. -/
def savePanelWidgetInfo [Monad m] [MonadInfoTree m] [MonadNameGenerator m]
    (stx : Syntax) (id : Name) (props : LazyEncodable Json) : m Unit := do
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

end ProofWidgets
