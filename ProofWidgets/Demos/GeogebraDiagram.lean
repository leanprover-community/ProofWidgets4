import ProofWidgets.Component.GeogebraDiagram


/-
  The following code is based on the original source:
  File: ProofWidgets/Demos/Euclidean.lean
  Repo: https://github.com/EdAyers/ProofWidgets4
  License: Apache 2.0

  Modifications made:
  - Changed the content `EuclideanDisplayPanel` inside `[]` (at line 163 in the original source) to `GeogebraDiagram` at line 35
  - Added the comment "-- Place your cursor here." at line 38
-/

class IncidenceGeometry where
  Point : Type u₁
  Line : Type u₂

  between : Point → Point → Point → Prop -- implies colinearity
  onLine : Point → Line → Prop
  ne_23_of_between : ∀ {a b c : Point}, between a b c → b ≠ c
  line_unique_of_pts : ∀ {a b : Point}, ∀ {L M : Line}, a ≠ b → onLine a L → onLine b L → onLine a M → onLine b M → L = M
  onLine_2_of_between : ∀ {a b c : Point}, ∀ {L : Line}, between a b c → onLine a L → onLine c L → onLine b L

variable [i : IncidenceGeometry]

open IncidenceGeometry

/-! # Example usage -/

open ProofWidgets

example {a b c : Point} {L M : Line} (Babc : between a b c) (aL : onLine a L) (bM : onLine b M)
    (cL : onLine c L) (cM : onLine c M) : L = M := by
  with_panel_widgets [GeogebraDiagram]
    -- Place your cursor here.
  have bc := ne_23_of_between Babc
    -- Place your cursor here.
  have bL := onLine_2_of_between Babc aL cL
  exact line_unique_of_pts bc bL cL bM cM
