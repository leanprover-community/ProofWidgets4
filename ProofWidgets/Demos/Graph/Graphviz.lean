/-
Copyright (c) 2026 Lean FRO, LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Wojciech Nawrocki
-/

module

public import ProofWidgets.Component.GraphvizDisplay
public import ProofWidgets.Component.HtmlDisplay

open ProofWidgets Jsx

#html
  <GraphvizDisplay
    dot=r##"
      digraph {
        node [style="filled"]
        a [fillcolor="#d62728", shape="triangle"]
        b [fillcolor="#1f77b4", shape="diamond"]
        c [fillcolor="#2ca02c", shape="trapezium"]
        a -> b
        a -> c
        b -> c
      }"## />
