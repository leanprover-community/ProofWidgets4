module

public meta import Lean.Meta.Basic
public meta import Lean.Elab.Command
public meta import ProofWidgets.Component.HtmlDisplay

public meta section

open Lean ProofWidgets Jsx in
/--
A function printing the signature in the InfoView with
dimmed implicit arguments (opacity 50%).
Arguments:
* `name` the full Lean name to print the signature of
* `showAlt` if `false`, implicit arguments are omitted from export to string
-/
def ProofWidgets.CheckHighlight (name : Lean.Name) (showAlt : Bool) : MetaM Unit := do
  let constVal ← getConstVal name
  Meta.forallTelescope constVal.type <| fun args outType => do
    let mut msg : MessageData := toString name
    if !constVal.levelParams.isEmpty then
      let univStr : String := ",".intercalate (constVal.levelParams.map toString)
      let univStr := ".{" ++ univStr ++ "}"
      let alt := if showAlt then univStr else ""
      msg := msg ++ (←MessageData.ofHtml <span «class»="o-50">{
        .text <| univStr
      }</span> alt)
    let decls ← args.mapM Meta.getFVarLocalDecl
    let groups := decls.toList.splitBy
      (fun d1 d2 => declCore d1 == declCore d2)
    for group in groups do
      let decl : LocalDecl := group.head!
      let (p1,p2) : String × String := match decl.binderInfo with
      | .default => ("(",")")
      | .implicit => ("{","}")
      | .instImplicit => ("[","]")
      | .strictImplicit => ("⦃","⦄")
      let mut paramParts : Array (String ⊕ Expr) := #[.inl " "]
      paramParts := paramParts.push (.inl p1)
      if group.length > 1 ∨ ¬decl.binderInfo.isInstImplicit then
        for arg in group do
          let e : Expr := .fvar arg.fvarId
          paramParts := paramParts.push (.inr e)
          paramParts := paramParts.push (.inl " ")
        paramParts := paramParts.push (.inl ": ")
      paramParts := paramParts.push (.inr decl.type)
      paramParts := paramParts.push (.inl p2)
      let paramMsg : MessageData ←
        if decl.binderInfo.isExplicit then
          pure <| paramParts.foldl (fun m part =>
            m ++ match part with
            | .inl s => toMessageData s
            | .inr e => toMessageData e
          ) m!""
        else
          let htmls ← paramParts.mapM (fun part => match part with
          | .inl s => pure (.text s)
          | .inr e => do
            let widget ← Lean.Widget.ppExprTagged e
            pure <InteractiveCode fmt={widget} />
          )
          let mut alt := ""
          if showAlt then
            let strs : Array String ← paramParts.mapM (fun part => match part with
            | .inl s => pure s
            | .inr e => toString <$> Meta.ppExpr e
            )
            alt := "".intercalate strs.toList
          pure <| ←MessageData.ofHtml
            (.element "span" #[("class", "o-50")] htmls) alt
      msg := msg ++ paramMsg
    msg := msg ++ " : "
    msg := msg ++ outType
    logInfo msg
where
  declCore (decl : Lean.LocalDecl) :=
    (decl.type, decl.binderInfo, decl.kind)

/--
A version of `#check` that dims implicit arguments in the infoview,
and omits them completely in non-interactive output (e.g. on the command-line).
-/
elab "#checkh " name:ident : command =>
  Lean.Elab.Command.runTermElabM fun _ => do
    for fullName in (← Lean.Elab.realizeGlobalConstWithInfos name) do
      ProofWidgets.CheckHighlight fullName false
/--
A version of `#check` that dims implicit arguments in the infoview,
and displays them as usual in non-interactive output (e.g. on the command-line).
See also `#checkh`.
-/
elab "#checkh' " name:ident : command =>
  Lean.Elab.Command.runTermElabM fun _ => do
    for fullName in (← Lean.Elab.realizeGlobalConstWithInfos name) do
      ProofWidgets.CheckHighlight fullName true
