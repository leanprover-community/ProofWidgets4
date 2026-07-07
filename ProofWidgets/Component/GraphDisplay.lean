module

public import ProofWidgets.Component.ForceGraphDisplay

public meta section

namespace ProofWidgets.GraphDisplay

open Lean in
local macro "#reexport" n:ident : command =>
  `(@[deprecated "Namespace moved to ForceGraphDisplay." (since := "2026-06-25")]
    abbrev $n := @$(mkIdent <| `ForceGraphDisplay ++ n.getId))

#reexport mkCircle
#reexport BoundingShape
#reexport Vertex
#reexport Edge
#reexport ForceCenterParams
#reexport ForceCollideParams
#reexport ForceLinkParams
#reexport ForceManyBodyParams
#reexport ForceXParams
#reexport ForceYParams
#reexport ForceRadialParams
#reexport ForceParams
#reexport Props

end GraphDisplay

@[deprecated ForceGraphDisplay (since := "2026-06-25")]
def GraphDisplay : Component GraphDisplay.Props := ForceGraphDisplay

end ProofWidgets
