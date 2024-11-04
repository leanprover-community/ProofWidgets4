import ProofWidgets.Component.Digraph
import ProofWidgets.Component.HtmlDisplay

/-! ## Directed graphs with `DigraphDisplay` -/

open ProofWidgets Jsx

/-! ### Basic usage -/

def mkEdge (st : String × String) : DigraphDisplay.Edge := {source := st.1, target := st.2}

-- Place your cursor here.
#html <DigraphDisplay
    vertices={#["a", "b", "c", "d", "e", "f"].map ({id := ·})}
    edges={#[("b","c"), ("d","e"), ("e","f"), ("f","d")].map mkEdge}
  />

/-! ### Custom layout

Forces acting on the vertices can be customized
in order to control graph layout. -/

def complete (n : Nat) : Array DigraphDisplay.Vertex × Array DigraphDisplay.Edge := Id.run do
  let mut verts := #[]
  let mut edges := #[]
  for i in [:n] do
    verts := verts.push {id := toString i}
    for j in [:i] do
      edges := edges.push {source := toString i, target := toString j}
      edges := edges.push {source := toString j, target := toString i}
  return (verts, edges)

def K₁₀ := complete 10

#html <DigraphDisplay
    vertices={K₁₀.1}
    edges={K₁₀.2}
    -- Specify forces here.
    forces={#[
      .link { distance? := some 150 }
    ]}
  />

/-! ### Vertex labels

Arbitrary SVG elements can be used as vertex labels. -/

#html <DigraphDisplay
    vertices={#[
      { id := "a"
        -- Specify a label here.
        label := <circle r={5} fill="#ff0000" />
      },
      { id := "b"
        -- Use `<foreignObject>` to draw non-SVG elements.
        label := <foreignObject height={50} width={50}>
          -- TODO: the extra `<p>` node messes up positioning
          <MarkdownDisplay contents="$xyz$" />
        </foreignObject>
      }
    ]}
    edges={#[{ source := "a", target := "b" }]}
  />

/-! ### Edge labels

Arbitrary SVG elements can be used as edge labels. -/

#html <DigraphDisplay
    vertices={#["a", "b", "c"].map ({ id := ·})}
    edges={#[
      { source := "a", target := "b"
        -- Specify a label here.
        label? := <g>
          {DigraphDisplay.mkCircle #[("r", "10")]}
          <text textAnchor="middle" dominantBaseline="middle">1</text>
        </g>
      },
      { source := "b", target := "c"
        -- Use `<foreignObject>` to draw non-SVG elements.
        label? := <foreignObject height={50} width={50}>
          -- TODO: the extra `<p>` node messes up positioning
          <MarkdownDisplay contents="$e_2$" />
        </foreignObject>
      }
    ]}
    forces={#[
      .link { distance? := some 100 }
    ]}
  />

/-! ### Extra details

A details box with extra information can be displayed below the graph.
Click on vertices and edges to view their details. -/

#html <DigraphDisplay
    vertices={#[
      { id := "a"
        -- Specify details here.
        details? := Html.text "Vertex a."
        -- Add class to indicate clickability.
        label := DigraphDisplay.mkCircle #[("className", "dim")]
      },
      { id := "b" }
    ]}
    edges={#[
      { source := "a", target := "b"
        details? := Html.text "Edge a → b."
        attrs := #[("className", "dim"), ("strokeWidth", 3)]
      }
    ]}
    -- Set this to display details.
    showDetails={true}
  />
