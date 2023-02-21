import {
  DocumentPosition, GoalsLocation, InteractiveGoal, mapRpcError, PanelWidgetProps,
  RpcContext, RpcPtr, useAsync
} from "@leanprover/infoview";
import * as  React from "react";
import ExprPresentation from "./exprPresentation";

function findGoalForLocation(goals: InteractiveGoal[], loc: GoalsLocation): InteractiveGoal {
  for (const g of goals) {
    if (g.mvarId === loc.mvarId) return g
  }
  throw new Error(`Could not find goal for location ${JSON.stringify(loc)}`)
}

type ExprWithCtx = RpcPtr<'ProofWidgets.ExprWithCtx'>

interface GoalsLocationsToExprsResponse {
  exprs: ExprWithCtx[]
}

/**
 * Display the expression corresponding to a given `GoalsLocation` using {@link ExprPresentation}.
 */
function GoalsLocationPresentation({ pos, goals, loc }:
  { pos: DocumentPosition, goals: InteractiveGoal[], loc: GoalsLocation }) {
  const rs = React.useContext(RpcContext)
  const st = useAsync<ExprWithCtx>(async () => {
    const g = findGoalForLocation(goals, loc)
    if (g.ctx === undefined) throw new Error(`Lean server 1.1.2 or newer is required.`)
    const ret: GoalsLocationsToExprsResponse =
      await rs.call('ProofWidgets.goalsLocationsToExprs', { locations: [[g.ctx, loc]] })
    return ret.exprs[0]
  }, [rs, goals, loc])

  if (st.state === 'loading')
    return <>Loading..</>
  else if (st.state === 'rejected')
    return <>Error: {mapRpcError(st.error).message}</>
  else
    return <ExprPresentation pos={pos} expr={st.value} />
}

export default function (props: PanelWidgetProps) {
  return <details open>
    <summary className='mv2 pointer'>Selected expressions</summary>
    {props.selectedLocations.length > 0 ?
      props.selectedLocations.map(loc =>
        <GoalsLocationPresentation
          key={JSON.stringify(loc)} pos={props.pos} goals={props.goals} loc={loc} />) :
      <>Nothing selected. You can use shift-click to select expressions in the goal.</>}
  </details>
}
