import HtmlDisplay, { Html } from './htmlDisplay';
import * as React from 'react';
import { DocumentPosition, RpcContext } from '@leanprover/infoview';

type State = any

type Action =
  ( {kind : 'timeout'}
  | {kind : 'mousedown' | 'mouseup' | 'mousemove', id?: string, data?: string}
  )

type MouseButtonState = "pressed" | "released"

interface UpdateParams {
    /** Number of milliseconds of elapsed time since component was created. */
    elapsed : number;
    actions : Action[];
    state : State;
    mousePos? : [number, number];
    mouseButtonState : MouseButtonState;
}

interface UpdateResult {
    pos : DocumentPosition
    html : Html
    state : State
    callbackTime? : number

}

function useMousePos(ref : React.MutableRefObject<Element | null>) {
    const [mousePos, setMousePos] = React.useState<[number, number] | undefined>(undefined)
    const [mouseButtonState, setMBS] = React.useState<MouseButtonState>("released")
    React.useEffect(() => {
        const types : any[] = ['mousemove', 'mouseup', 'mousedown']
        function handler(event : MouseEvent) {
            const elt = ref.current
            if (!elt) {
                return
            }
            const rec = elt.getBoundingClientRect()
            setMousePos([event.clientX - rec.x, event.clientY - rec.y])
            setMBS(event.buttons ? 'pressed' : 'released')
        }
        types.forEach(t => window.addEventListener(t, handler))
        return () => types.forEach(t => window.removeEventListener(t, handler))
    }, [])
    return [mousePos, mouseButtonState]
}

export function Svg(props : UpdateResult) {
    const rs = React.useContext(RpcContext)
    const state = React.useRef(props)
    const startTime = React.useRef(new Date())
    const pending = React.useRef<Action[]>([])
    const asyncState = React.useRef('init')
    const rootDiv = React.useRef(null)
    const [html, setHtml] = React.useState<Html>(props.html)
    const [frame, setFrame] = React.useState<number>(0)
    const [mousePos, mouseButtonState] = useMousePos(rootDiv)

    React.useEffect(() => {
        if (state.current.callbackTime) {
            const t = setTimeout(() => increment({kind : 'timeout'}), state.current.callbackTime)
            return () => clearTimeout(t)
        }
    }, [state.current.callbackTime, frame])

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
            'updateSvg',
            { elapsed, actions, state : state.current.state, mousePos, mouseButtonState })
        asyncState.current = 'resolved'
        setFrame((x : number) => x + 1)
        state.current = result
        setHtml(result.html)
        if (pending.current.length > 0) {
            dispatch()
        }
    }

    // function visitor(e : Elt) : Elt {
    //     let attrs = {...e.attrs}
    //     const mouseEvents : Action['kind'][] = []
    //     for (const me of mouseEvents) {
    //         if (me in attrs) {
    //             const value = attrs[me]
    //             attrs[me] = () => increment({kind: me, value})
    //         }
    //     }
    //     return {...e, attrs}
    // }

    function handleMouseEvent(e : MouseEvent) {
        console.log(e)
        const id = e.target.id || undefined
        const data = e.target.data || undefined
        console.log(id)
        console.log(data)
        if (e.type === "mouseup" || e.type === "mousedown") {
            increment({kind : e.type, id, data})
        }
        if (e.type === "mousemove" && e.buttons !== 0) {
            increment({kind : e.type, id, data})
        }

    }

    return <div onMouseDown={handleMouseEvent} onMouseUp={handleMouseEvent} onMouseMove={handleMouseEvent} ref={rootDiv}>
        <HtmlDisplay pos={props.pos} html={html} />
        <div>frame: {frame}. state: {asyncState.current}, mousePos: {mousePos ? mousePos.join(", ") : "none"}, mouseButtonState: {mouseButtonState}</div>
    </div>
}

export default Svg
