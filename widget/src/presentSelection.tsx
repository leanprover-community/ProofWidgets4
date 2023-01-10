import { GoalsLocation, InteractiveGoal, mapRpcError, PanelWidgetProps, RpcContext, RpcPtr, useAsync }
  from "@leanprover/infoview";
import * as  React from "react";
import ExprPresentation from "./exprPresentation";

function findGoalForLocation(goals: InteractiveGoal[], loc: GoalsLocation) {
  for (const g of goals) {
    if (g.mvarId === loc.mvarId) return g
  }
  throw new Error(`Could not find goal for location ${JSON.stringify(loc)}`)
}

type ExprWithCtx = RpcPtr<'WidgetKit.ExprWithCtx'>

interface LocationsToExprResponse {
  exprs : ExprWithCtx[]
}

export default function(props: PanelWidgetProps) {
  const rs = React.useContext(RpcContext)
  const st = useAsync<LocationsToExprResponse>(async () => {
    const locations = []
    for (const loc of props.selectedLocations) {
      const g = findGoalForLocation(props.goals, loc)
      if (g.ctx === undefined) throw new Error(`Lean server 1.1.2 or newer is required.`)
      locations.push([g.ctx, loc])
    }
    return await rs.call('WidgetKit.locationsToExpr', {locations})
  }, [rs, props.goals, props.selectedLocations])

  if (st.state === 'loading')
    return <>Loading..</>
  else if (st.state === 'rejected')
    return <>Error: {mapRpcError(st.error).message}</>
  else
    return <>{st.value.exprs.map(expr => <ExprPresentation pos={props.pos} expr={expr} />)}</>
}
