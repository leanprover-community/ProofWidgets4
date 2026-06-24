/-
Copyright (c) 2026 Lean FRO, LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Wojciech Nawrocki
-/

module

public import ProofWidgets.Component.Maximizable
public import ProofWidgets.Component.HtmlDisplay

open ProofWidgets Jsx

#html
  <Maximizable>
    <div style={json%{background: "yellow", width: "100%", height: "100px"}}>Maximize me!</div>
  </Maximizable>
