import ProofWidgets.Component.Digraph
import ProofWidgets.Component.HtmlDisplay

open ProofWidgets Jsx

#html <DigraphDisplay
    vertices={#[
      {id := "a"},
      {id := "b"}
    ]}
    edges={#[
      {source := "a", target := "b"}
    ]}
  />
#html <DigraphDisplay
    vertices={#[
      {id := "a"},
      {id := "b"},
      {id := "c"}
    ]}
    edges={#[
      {source := "a", target := "b"}
    ]}
  />
#html <DigraphDisplay
    vertices={#[
      {id := "a"},
      {id := "b"},
      {id := "c"},
      {id := "d"},
      {id := "e"}
    ]}
    edges={#[
      {source := "a", target := "b"},
      {source := "c", target := "d"},
      {source := "d", target := "e"},
      {source := "e", target := "c"}
    ]}
  />
