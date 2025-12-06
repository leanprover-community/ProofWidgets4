import ProofWidgets.Extra.CheckHighlight

/-- info: Decidable.em (p : Prop) : p ∨ ¬p -/
#guard_msgs in
#checkh Decidable.em
/-- info: Decidable.em (p : Prop) [Decidable p] : p ∨ ¬p -/
#guard_msgs in
#checkh' Decidable.em
