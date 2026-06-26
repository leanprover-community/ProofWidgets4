/-
Copyright (c) 2026 Lean FRO, LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Wojciech Nawrocki
-/

module

public import ProofWidgets.Component.Basic

public meta section

namespace ProofWidgets.GraphvizDisplay
open Lean Server

/-- Layout engine used by Graphviz.
See https://www.graphviz.org/docs/layouts/. -/
inductive GraphvizLayoutEngine where
  | circo | dot | fdp | sfdp | neato | osage | patchwork | twopi
  deriving Inhabited, FromJson, ToJson

inductive D3GraphvizKeyMode where
  | title | id | «tag-index» | index
  deriving Inhabited, FromJson, ToJson

/-- Options passed to the d3-graphviz renderer.
See https://github.com/magjac/d3-graphviz#selection_graphviz. -/
structure D3GraphvizOptions where
  useWorker? : Option Bool := none
  engine? : Option GraphvizLayoutEngine := none
  totalMemory? : Option Float := none
  keyMode? : Option D3GraphvizKeyMode := none
  fade? : Option Bool := none
  tweenPaths? : Option Bool := none
  tweenShapes? : Option Bool := none
  convertEqualSidedPolygons? : Option Bool := none
  /-- d3-graphviz accepts `number | string`. -/
  tweenPrecision? : Option Json := none
  growEnteringEdges? : Option Bool := none
  zoom? : Option Bool := none
  zoomScaleExtent? : Option (Float × Float) := none
  zoomTranslateExtent? : Option ((Float × Float) × (Float × Float)) := none
  /-- If not provided, the SVG dynamically resizes to its parent `div`'s width. -/
  width? : Option Float := none
  /-- If not provided, the SVG dynamically resizes to its parent `div`'s width. -/
  height? : Option Float := none
  scale? : Option Float := none
  fit? : Option Bool := none
  deriving Inhabited, FromJson, ToJson

structure Props where
  /-- Graphviz source of the graph. -/
  dot : String
  options : D3GraphvizOptions := {}
  /-- How long to wait before re-rendering the graph when the target SVG size changes.
  Larger graphs should use longer timeouts to avoid many expensive re-renders. -/
  renderDebounceMs : Nat := 200
  /-- Additional classes to place on the SVG's parent `div`. -/
  className? : Option String := none
  /-- Additional styling to place on the SVG's parent `div`. -/
  style? : Option Json := none
  deriving Inhabited, FromJson, ToJson

end GraphvizDisplay

/-- Renders a graph from a Graphviz source string. -/
@[widget_module]
def GraphvizDisplay : Component GraphvizDisplay.Props where
  javascript := include_str ".." / ".." / "widget" / "js" / "graphvizDisplay.js"

end ProofWidgets
