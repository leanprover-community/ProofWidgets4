import Lean
import WidgetKit.Html
import WidgetKit.HtmlWidget
open Lean Widget Jsx

structure Point where
  x1 : Float
  x2 : Float
  p1 : Float
  p2 : Float
  deriving ToJson, FromJson

def updatePoint (dt : Float) (p : Point) : Point :=
  let p2 := {
    x1 := p.x1 + dt * p.p1,
    x2 := p.x2 + dt * p.p2,
    p1 := p.p1
    p2 := p.p2
    }
  p2


structure GeometryState where
  points : Array Point
  t : Float := 0
  deriving ToJson, FromJson

def GeometryState.init : GeometryState := {
  points := #[{x1 := 20, x2 := 20, p1 := 1, p2 := 1}],
  t := 0
}

inductive ActionKind where
  | timeout
  | click
  deriving ToJson, FromJson

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
  let s2 := params.state.points.map (updatePoint δt)
  let newState : GeometryState := {points := s2, t := params.elapsed}
  return RequestTask.pure $ {
    html := <div>Hello world {toString params.elapsed} {toString <| toJson <| params.actions}</div>,
    state := newState,
    callbackTime := some 1000,
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