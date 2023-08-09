import ProofWidgets.Component.HtmlDisplay

-- See the `Jsx.lean` demo for more about JSX.
open scoped ProofWidgets.Jsx

/-! # Widgets in macros

Macros may expand to commands or terms that end up saving widget info,
but the syntax to which the widget info is associated must be marked as `canonical`
for the widget to be displayed.
-/

def Lean.SourceInfo.mkCanonical : SourceInfo → SourceInfo
  | .synthetic s e _ => .synthetic s e true
  | si => si

def Lean.Syntax.mkInfoCanonical : Syntax → Syntax
  | .missing => .missing
  | .node i k a => .node i.mkCanonical k a
  | .atom i v => .atom i.mkCanonical v
  | .ident i r v p => .ident i.mkCanonical r v p

def Lean.TSyntax.mkInfoCanonical : TSyntax k → TSyntax k :=
  (.mk ·.raw.mkInfoCanonical)

macro "#browse " src:term : command =>
  .mkInfoCanonical <$> `(#html <iframe src={$src} width="100%" height="600px" />)

#browse "https://leanprover-community.github.io/"
-- Do you like recursion?
#browse "https://lean.math.hhu.de"
