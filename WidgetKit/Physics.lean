import Lean
import WidgetKit.Html
import WidgetKit.HtmlWidget
import WidgetKit.Demos.Graphics2D
open Lean Widget Jsx


structure State where
  points : Array (Float × Float)
  t : Float := 0
  v := 0.0002
  deriving ToJson, FromJson


def State.toSvg (s : State) : Svg :=
  let elements : Array Svg.Element := 
    s.points 
      |>.mapIdx (λ idx p => { shape := .circle ⟨p.1,p.2⟩ (.absolute 0.1), id := some s!"circle{idx}"} )
  { elements := elements,
    frame := {min := ⟨-1,-1⟩, xSize := 2, width := 400, height := 400} }

def GeometryState.init : State := {
  points := #[(-0.5,-0.5),(0.5,-0.5),(0,0.5)]
}

inductive ActionKind where
  | timeout
  | click
  deriving ToJson, FromJson, DecidableEq

structure Action where
  -- can be 'timeout' or '
  kind : ActionKind
  value : Json
  deriving ToJson, FromJson

structure UpdatePhysicsParams where
  elapsed : Float
  actions : Array Action
  state : State
  deriving ToJson, FromJson

structure UpdatePhysicsResult where
  html : Widget.Html
  state : State
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

  -- update point position
  let s2 : Array (Float × Float) := 
    let θ := δt * v 
    let c := Float.cos θ
    let s := Float.sin θ
    params.state.points.map λ p => (c*p.1 - s*p.2, s*p.1 + c*p.2)

  let newState : State := {points := s2, t := params.elapsed, v := v}
  let svg := newState.toSvg.toHtml
  return RequestTask.pure $ {
    html := <div>
      <div>
        {svg}
      </div>
      {toString params.elapsed} {toString <| toJson <| params.actions}</div>,
    state := newState,
    callbackTime := some 10,
  }

@[widget]
def physics : UserWidgetDefinition where
  name := "Magic physics demo"
  javascript := include_str ".." / "widget" / "dist" / "physics.js"

def init : UpdatePhysicsResult := {
  html := <div>Init!!!</div>,
  state := GeometryState.init,
}

#widget physics (toJson init)
