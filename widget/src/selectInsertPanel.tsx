import * as React from 'react'
import { EditorContext, RpcContext, mapRpcError, useAsync, DocumentPosition, GoalsLocation } from '@leanprover/infoview'
import { InteractiveGoal } from '@leanprover/infoview-api';
import { Range } from 'vscode-languageserver-protocol'

const e = React.createElement

function findGoalForLocation(goals: InteractiveGoal[], loc: GoalsLocation) {
    for (const g of goals) {
        if (g.mvarId === loc.mvarId) return g
    }
    throw new Error('could not find goal for location' + JSON.stringify(loc))
}

interface SelectInsertParams {
    pos: DocumentPosition
    goals: InteractiveGoal[]
    selectedLocations: GoalsLocation[]
    replaceRange: Range
    title: string
    help: string
    callback: string
}

export default function ({ pos, goals, selectedLocations, replaceRange, title, help, callback }: SelectInsertParams) {
    const rs = React.useContext(RpcContext)
    const ec = React.useContext(EditorContext)
    const st: any = useAsync(async () => {
        const g = findGoalForLocation(goals, selectedLocations[0])
        return await rs.call(callback,
            { cursorPos: pos, ctx: g.ctx, selectedLocations, replaceRange })
    }, [goals, selectedLocations])

    const onClick = () => void (async () => {
        if (st.value) {
            await ec.api.applyEdit(st.value.edit)
            await ec.revealPosition({ ...st.value.newCursorPos, uri: pos.uri })
        }
    })()

    const result = st.state === 'resolved' ? e('pre', { className: 'font-code pre-wrap' }, e('a', { onClick, className: 'link pointer dim', title: 'Apply suggestion' }, st.value.content))
        : st.state === 'rejected' ? e('p', { style: { color: 'red' } }, mapRpcError(st.error).message)
            : e('p', null, 'Loading...')
    const inner = selectedLocations.length === 0 ? e('span', null, help) : result

    return e('details', { open: true }, [
        e('summary', { className: 'mv2 pointer' }, title),
        e('div', { className: 'ml1' }, inner)
    ])
}
