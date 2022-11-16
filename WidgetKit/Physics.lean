import Lean
import WidgetKit.Html
import WidgetKit.HtmlWidget
import WidgetKit.Demos.Graphics2D
open Lean Widget Jsx


structure GeometryState where
  geom : GeometryData
  t : Float := 0
  v := 0.0002
  deriving ToJson, FromJson

def GeometryState.init : GeometryState := {
  geom := initialData,
  t := 0
}

inductive ActionKind where
  | timeout
  | click
  deriving ToJson, FromJson, DecidableEq

structure Action where
  -- can be 'timeout' or '
  kind : ActionKind
  deriving ToJson, FromJson

structure UpdatePhysicsParams where
  elapsed : Float
  actions : Array Action
  state : GeometryState
  deriving ToJson, FromJson

structure UpdatePhysicsResult where
  html : Widget.Html
  state : GeometryState
  /-- Approximate number of milliseconds to wait before calling again. -/
  callbackTime : Option Float := some 100
  deriving ToJson, FromJson

open Server RequestM in

@[server_rpc_method]
def updatePhysics (params : UpdatePhysicsParams ) : RequestM (RequestTask UpdatePhysicsResult) := do
  let δt := (params.elapsed - params.state.t)
  let mut v := params.state.v
  for action in params.actions do
    if action.kind == ActionKind.click then
      v := v * 2
  let s2 := params.state.geom.rotate (δt * v)


  let newState : GeometryState := {geom := s2, t := params.elapsed, v := v}
  let svg := GeometryData.toSvgHtml newState.geom frame
  return RequestTask.pure $ {
    html := <div>
      <div>
        {svg}
      </div>
      {toString params.elapsed} {toString <| toJson <| params.actions}</div>,
    state := newState,
    callbackTime := some 16,
  }

@[widget]
def physics : UserWidgetDefinition where
  name := "Magic physics demo"
  javascript := include_str  ".." / "widget" / "dist" / "physics.js"

def init : UpdatePhysicsResult := {
  html := <div>Init!!!</div>,
  state := GeometryState.init,
}

#widget physics (toJson init)