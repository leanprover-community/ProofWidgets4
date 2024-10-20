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

/-! ### Custom layout -/

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
    -- Specify forces to control graph layout.
    forces={#[
      .link { distance? := some 150 }
    ]}
  />

/-! ### Styling -/

#html <DigraphDisplay
    vertices={#[
      { id := "a"
        -- Arbitrary SVG elements can be used as vertex labels.
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

/-! ### Extra details -/

#html <DigraphDisplay
    vertices={#[
      { id := "a", details? := Html.text "Vertex a." },
      { id := "b", details? := <b>Vertex b.</b> }
    ]}
    edges={ #[ { source := "a", target := "b", details? := Html.text "Edge a → b." } ] }
    -- Use this to display a details box with extra information
    -- about vertices and edges.
    showDetails={true}
  />
