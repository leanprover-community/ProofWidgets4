import Lean.Elab

import WidgetKit.Svg

open Lean.Widget.Jsx
open Lean Widget


private def Float.toInt (x : Float) : Int :=
  if x >= 0 then
    x.toUInt64.toNat
  else
    -((-x).toUInt64.toNat)

private def Int.toFloat (i : Int) : Float :=
  if i >= 0 then
    i.toNat.toFloat
  else
    -((-i).toNat.toFloat)

-- namespace Svg

inductive ActionKind where
  | timeout
  | mousedown
  | mouseup
  | mousemove -- [note] mouse moves only happen when mouse button is down.
  deriving ToJson, FromJson, DecidableEq

structure Action where
  kind : ActionKind
  id : Option String
  data : Option Json
  deriving ToJson, FromJson

/-- The input type `State` is any state a user wants to use and update 

SvgState in addition automatically handles tracking of time and selection -/
structure SvgState (State : Type) where
  state : State
  time : Float /-- time in milliseconds -/
  selected : Option String
  mousePos : Option (Int × Int)
deriving ToJson, FromJson

structure UpdateParams (State : Type) where
  elapsed : Float
  actions : Array Action
  state : SvgState State
  mousePos : Option (Float × Float) -- TODO: change to Option (Int × Int) or do we want to support subpixel precision?
  deriving ToJson, FromJson


structure UpdateResult (State : Type) where
  html : Widget.Html
  state : SvgState State
  /-- Approximate number of milliseconds to wait before calling again. -/
  callbackTime : Option Float := some 33
  deriving ToJson, FromJson


structure InteractiveSvg (State : Type) where
  init : State
  frame : Svg.Frame
  update (time Δt : Float) (action : Action) 
         (mouseStart mouseEnd : Option (Svg.Point frame)) (selected : Option String)
         : State → State
  render (mouseStart mouseEnd : Option (Svg.Point frame)) : State → Svg frame

abbrev State := Array (Float × Float)

def isvg : InteractiveSvg State where
  init := #[(-0.5, -0.5), (0.5, -0.5), (0.5, 0.5), (-0.5, 0.5)]
  frame := 
    { xmin := -1
      ymin := -1
      xSize := 2
      width := 400
      height := 400 }
  update time Δt action mouseStart mouseEnd selected state := state
    -- state.map λ (x,y) => (x + Δt/10000 * Float.sin (time/1000), y)
  render mouseStart mouseEnd state := 
    { 
      elements := 
        match mouseStart, mouseEnd with
        | some s, some e => 
          #[ 
            Svg.circle e (.px 5) |>.setFill (1.,1.,1.),
            Svg.line s e |>.setStroke (1.,1.,1.) (.px 2)
          ].append (state.mapIdx fun idx (p : Float × Float) => 
            Svg.circle p (.abs 0.2) |>.setFill (0.7,0.7,0.7) |>.setId s!"circle{idx}" |>.setData idx.1
            )
        | _, _ => #[]
    }


open Server RequestM in
@[server_rpc_method]
def updatePhysics (params : UpdateParams State) : RequestM (RequestTask (UpdateResult State)) := do

  -- Ideally, each action should have time and mouse position attached
  -- so right now we just assume that all actions are 
  let Δt := (params.elapsed - params.state.time) / params.actions.size.toFloat

  let mut time := params.state.time
  let mut state := params.state.state
  let mut selected := params.state.selected
  
  let mouseStart := params.state.mousePos.map λ (i,j) => (i, j)
  let mouseEnd := params.mousePos.map λ (x,y) => (x.toInt, y.toInt)

  for action in params.actions do
    -- todo: interpolate mouse movenment!

    state := isvg.update time Δt action mouseStart mouseEnd selected state

    if action.kind == ActionKind.mousedown then
      selected := action.id
    if action.kind == ActionKind.mouseup then
      selected := none

    time := time + Δt
  
  let svgState : SvgState State := 
    { state := state 
      time := params.elapsed
      selected := selected
      mousePos := mouseEnd.map λ p => p.toPixels }

  let mut svg := isvg.render mouseStart mouseEnd state

  -- highlight selection
  if let some id := selected then
    if let some idx := svg.idToIdx[id] then
      let el := svg.elements[idx].setStroke (1.,1.,0.) (.px 5)
      svg := { elements := svg.elements.set idx el }


  return RequestTask.pure {
    html := <div>
      <div>
        {svg.toHtml}
      </div>

      {toString params.elapsed}
      {toString <| toJson <| params.actions}
      {toString <| toJson <| mouseStart}
      {toString <| toJson <| mouseEnd}
      {toString <| toJson <| selected}</div>,
    state := svgState,
    callbackTime := some 33,
  }
 


@[widget]
def svgWidget : UserWidgetDefinition where
  name := "Interactive SVG"
  javascript := include_str ".." / "widget" / "dist" / "interactiveSvg.js"


def init : UpdateResult State := {
  html := <div>Init!!!</div>,
  state := { state := isvg.init
             time := 0
             selected := none
             mousePos := none }
}

#widget svgWidget (toJson init)
