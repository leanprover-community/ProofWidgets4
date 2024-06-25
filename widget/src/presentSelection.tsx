import {
  DocumentPosition, GoalsLocation, InteractiveGoal, mapRpcError, PanelWidgetProps,
  RpcPtr, useAsyncPersistent,
  useRpcSession
} from "@leanprover/infoview";
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
export function GoalsLocationPresentation({ pos, goals, loc }:
  { pos: DocumentPosition, goals: InteractiveGoal[], loc: GoalsLocation }) {
  const rs = useRpcSession()
  const st = useAsyncPersistent<ExprWithCtx>(async () => {
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
    return <ExprPresentation expr={st.value} />
}

export default function (props: PanelWidgetProps) {
  return <details open>
    <summary className='mv2 pointer'>Selected expressions</summary>
    {props.selectedLocations.length > 0 ?
      props.selectedLocations.map(loc =>
        <GoalsLocationPresentation
          key={JSON.stringify(loc.loc)} pos={props.pos} goals={props.goals} loc={loc} />) :
      <>Nothing selected. You can use shift-click to select expressions in the goal.</>}
  </details>
}
