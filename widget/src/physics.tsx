import {Html, StaticHtml, Elt} from './staticHtml';
import * as React from 'react';
import { RpcContext, mapRpcError } from '@leanprover/infoview';

type State = any

type Action =
  ( {kind : 'timeout'}
  | {kind : 'onClick' | 'onMouseDown' | 'onMouseUp', value : any}
  )

interface UpdateParams {
    /** Number of milliseconds of elapsed time since component was created. */
    elapsed : number;
    actions : Action[];
    state : State;
    mousePos? : [number, number];
}

interface UpdateResult {
    html : Html;
    state : State
    callbackTime? : number

}

function useMousePos() {
    const [mousePos, setMousePos] = React.useState<[number, number] | undefined>(undefined)
    React.useEffect(() => {
        function handler(event : MouseEvent) {
            setMousePos([event.clientX, event.clientY])
        }
        window.addEventListener('mousemove', handler)
        return () => window.removeEventListener('mousemove', handler)
    }, [])
    return mousePos
}

export function Physics(props : UpdateResult) {
    const rs = React.useContext(RpcContext)
    const state = React.useRef(props)
    const frameNo = React.useRef(0)
    const startTime = React.useRef(new Date())
    const pending = React.useRef<Action[]>([])
    const asyncState = React.useRef('init')
    const [frame, setFrame] = React.useState(frameNo.current)
    const mousePos = useMousePos()


    React.useEffect(() => {
        if (state.current.callbackTime) {
            const t = setTimeout(() => increment({kind : 'timeout'}), state.current.callbackTime)
            return () => clearTimeout(t)
        }
    }, [state.current.callbackTime, frameNo.current])

    function increment(action : Action) {
        pending.current.push(action)
        dispatch()
    }
    async function dispatch() {
        if (asyncState.current === "loading") {
            return
        }
        const actions = pending.current
        pending.current = []
        asyncState.current = 'loading'
        const elapsed = (new Date() as any) - (startTime.current as any)
        const result = await rs.call<UpdateParams, UpdateResult>(
            'updatePhysics',
            { elapsed, actions, state : state.current.state, mousePos })
        asyncState.current = 'resolved'
        frameNo.current = (frameNo.current + 1)
        setFrame(frameNo.current) // [hack] this is just to get the component to refresh.
        state.current = result
        if (pending.current.length > 0) {
            dispatch()
        }
    }

    function visitor(e : Elt) : Elt {
        let attrs = {...e.attrs}
        const mouseEvents : Action['kind'][] = ['onClick', 'onMouseDown', 'onMouseUp']
        for (const me of mouseEvents) {
            if (me in attrs) {
                const value = attrs[me]
                attrs[me] = () => increment({kind: me, value})
            }
        }
        return {...e, attrs}
    }

    return <div>
        <StaticHtml html={state.current.html} visitor={visitor}/>
        <div>frame: {frame}. state: {asyncState.current}</div>
    </div>
}

export default Physics