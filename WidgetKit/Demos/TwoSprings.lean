import Lean
import WidgetKit.Html
import WidgetKit.HtmlWidget
import WidgetKit.Svg
open Lean Widget Jsx


structure State where
  points : Array ((Float × Float) × (Float × Float) × (Float × Float))
  t : Float := 0
  deriving ToJson, FromJson


def State.toSvg (s : State) : Svg :=
  let elements : Array Svg.Element := 
    #[] 
      |>.append <| s.points.map
        (λ ((x,y),(_,_),(rx,ry)) => 
          { shape := .line ⟨x,y⟩ ⟨rx,ry⟩, 
            strokeWidth := some (.pixels 2), 
            strokeColor := some ⟨1,1,1⟩ } )

      |>.append <| s.points.mapIdx
        (λ idx ((x,y),(vx,vy),(_,_)) => 
          { shape := .circle ⟨x,y⟩ (.absolute 0.1), 
            fillColor := let speed := 100*Float.sqrt (vx*vx + vy*vy); some ⟨1-speed, speed, 0⟩, 
            clickData := some (toJson idx.1)} )

  { elements := elements,
    frame := {min := ⟨-1,-1⟩, xSize := 2, width := 400, height := 400} }

def GeometryState.init : State := {
  points := #[((-0.5,0.0),(0,0), (-0.5,0)),((0.5,0),(0,0), (0.5,0))],
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
def updatePhysics (params : UpdatePhysicsParams) : RequestM (RequestTask UpdatePhysicsResult) := do
  let δt := 0.3*(params.elapsed - params.state.t)

  let mut points := params.state.points

  for action in params.actions do
    match fromJson? (α := Nat) action.value with
    | .error _ => continue
    | .ok idx => 
      points := points.modify idx
        λ ((x,y),(vx,vy),(rx,ry)) => ((x,y), (vx+(Float.cos (1000*params.state.t)) * 0.01, vy + (Float.sin (1000*params.state.t)) * 0.01), (rx,ry)) 
        -- λ ((x,y),(vx,vy),(rx,ry)) => ((x + 0.1*(Float.cos (1000*params.state.t)),y + 0.1 * (Float.sin (1000*params.state.t))), (vx, vy), (rx,ry)) 


  -- update point position
  points :=
    let k := 0.0001
    let β := Float.exp (-0.001*δt)
    points.map λ ((x,y),(vx,vy), (rx,ry)) => 
      let vx := β*(vx-k*δt*(x-rx))
      let vy := β*(vy-k*δt*(y-ry))
      ((x+δt*vx,y+δt*vy), (vx,vy), (rx,ry))

  let newState : State := { points := points, t := params.elapsed }
    return RequestTask.pure $ {
    html := <div>
      <div>
        {newState.toSvg.toHtml}
      </div>

      {toString params.elapsed} {toString <| toJson <| params.actions}</div>,
    state := newState,
    callbackTime := some 10,
  }

@[widget]
def physics : UserWidgetDefinition where
  name := "Magic physics demo"
  javascript := include_str ".." / ".." / "widget" / "dist" / "physics.js"

def init : UpdatePhysicsResult := {
  html := <div>Init!!!</div>,
  state := GeometryState.init,
}

#widget physics (toJson init)
