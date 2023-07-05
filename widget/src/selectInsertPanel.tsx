import * as React from 'react'
import { EditorContext, RpcContext, mapRpcError, useAsync, DocumentPosition, GoalsLocation }
    from '@leanprover/infoview'
import { InteractiveGoal } from '@leanprover/infoview-api'
import { Position, Range, WorkspaceEdit } from 'vscode-languageserver-protocol'

function findGoalForLocation(goals: InteractiveGoal[], loc: GoalsLocation) {
    for (const g of goals) {
        if (g.mvarId === loc.mvarId) return g
    }
    throw new Error(`could not find goal for location ${JSON.stringify(loc)}`)
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

interface SelectInsertResponse {
    content: string
    edit: WorkspaceEdit
    newCursorPos: Position
}

export default function({
        pos, goals, selectedLocations, replaceRange, title, help, callback }: SelectInsertParams) {
    const rs = React.useContext(RpcContext)
    const ec = React.useContext(EditorContext)
    const st  = useAsync<SelectInsertResponse | undefined>(async () => {
        const g = findGoalForLocation(goals, selectedLocations[0])
        return await rs.call(callback, {
            cursorPos: pos,
            ctx: g.ctx,
            selectedLocations,
            replaceRange
        })
    }, [goals, selectedLocations, pos, replaceRange])

    const onClick = async () => {
        if (st.state === 'resolved' && st.value) {
            await ec.api.applyEdit(st.value.edit)
            await ec.revealPosition({ ...st.value.newCursorPos, uri: pos.uri })
        }
    }

    const result =
        st.state === 'resolved' ?
            <pre className='font-code pre-wrap'>
                <a onClick={onClick} className='link pointer dim' title='Apply suggestion'>
                    {st.value && st.value.content}
                </a>
            </pre> :
        st.state === 'rejected' ?
            <p style={{color: 'red'}}>{mapRpcError(st.error).message}</p>
        :
            <p>Loading...'</p>
    const inner = selectedLocations.length === 0 ? <span>{help}</span> : result

    return <details open>
        <summary className='mv2 pointer'>{title}</summary>
        <div className='ml1'>{inner}</div>
    </details>
}
