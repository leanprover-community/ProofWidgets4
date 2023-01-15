import WidgetKit.Component.InteractiveSvg

open Lean
open WidgetKit Svg Jsx

abbrev State := Array (Float × Float)

def isvg : InteractiveSvg State where
  init := #[(-0.5, -0.5), (0.5, -0.5), (0.5, 0.5), (-0.5, 0.5)]

  frame :=
    { xmin := -1
      ymin := -1
      xSize := 2
      width := 400
      height := 400 }

  update time Δt action mouseStart mouseEnd selected getData state :=
    match getData Nat, mouseEnd with
    | some id, some p => state.set! id p.toAbsolute
    | _, _ => state

  render time mouseStart mouseEnd state :=
    {
      elements :=
        let mousePointer :=
          match mouseStart, mouseEnd with
          | some s, some e =>
            #[
              Svg.circle e (.px 5) |>.setFill (1.,1.,1.),
              Svg.line s e |>.setStroke (1.,1.,1.) (.px 2)
            ]
          | _, _ => #[]
        let circles := (state.mapIdx fun idx (p : Float × Float) =>
              Svg.circle p (.abs 0.2) |>.setFill (0.7,0.7,0.7) |>.setId s!"circle{idx}" |>.setData idx.1
            )
        mousePointer.append circles
    }


open Server RequestM in
@[server_rpc_method]
def updateSvg (params : UpdateParams State) : RequestM (RequestTask (UpdateResult State)) := isvg.serverRpcMethod params

-- TODO: the tsx file is pretty broken
@[widget_module]
def SvgWidget : Component (UpdateResult State) where
  javascript := include_str ".." / ".." / "build" / "js" / "interactiveSvg.js"

def init : UpdateResult State := {
  html := EncodableHtml.ofHtml <div>Init!!!</div>,
  state := { state := isvg.init
             time := 0
             selected := none
             mousePos := none
             idToData := isvg.render 0 none none isvg.init |>.idToDataList}
}

#html <SvgWidget html={init.html} state={init.state}/>
