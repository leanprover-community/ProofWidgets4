import * as React from 'react'
import * as d3 from 'd3'
import useResizeObserver from 'use-resize-observer'
import HtmlDisplay, { Html } from './htmlDisplay'

interface Vertex {
  id: string
  label: Html
  details?: Html
}

interface Edge {
  source: string
  target: string
  details?: Html
}

namespace Edge {
export const calcId = (e: Edge): string => `${e.source} ${e.target}`
}

/** The input to this component. */
interface Graph {
  vertices: Vertex[]
  edges: Edge[]
}

/** An extension of {@link Vertex} with simulation-related data. */
interface SimVertex extends Vertex, d3.SimulationNodeDatum {}

/** An extension of {@link Edge} with simulation-related data. */
interface SimEdge extends d3.SimulationLinkDatum<SimVertex> {
  id: string
  source: Vertex | string | number
  target: Vertex | string | number
  label?: Html
  details?: Html
}

/** An extension of {@link Graph} with simulation-related data. */
interface SimGraph {
  vertices: Map<string, SimVertex>
  edges: Map<string, SimEdge>
}

namespace SimGraph {
export const empty = (): SimGraph =>
  ({ vertices: new Map(), edges: new Map() })

/**
 * Build graph simulation data from an input graph.
 * This makes a copy of the input graph
 * (though `Html` objects are not copied: they are never mutated)
 * that the force simulation will be able to mutate.
 */
export function ofGraph(g: Graph): SimGraph {
  return {
    vertices: new Map(g.vertices.map(v => [v.id, {...v}])),
    edges: new Map(g.edges.map(e => [Edge.calcId(e), {...e, id: Edge.calcId(e)}]))
  }
}

/**
 * Merge an input graph into an existing simulated graph.
 * Mutates `newG`.
 *
 * Reference identity of existing vertex/edge objects is preserved,
 * in that they are the same objects in the new graph as in the old,
 * only with their properties overwritten by those in the input graph.
 *
 * Returns `true` iff `vertices` or `edges` has grown or shrunk
 * in the sense that an object was added or removed
 * (this entails needing to update `d3-force` simulation arrays).
 */
export function updateFrom(g: SimGraph, newG: SimGraph): [SimGraph, boolean] {
  let changed = false
  for (const [vId, v] of newG.vertices) {
    const oldV = g.vertices.get(vId)
    if (!oldV) {
      changed = true
    } else {
      oldV.label = v.label
      oldV.details = v.details
      newG.vertices.set(vId, oldV)
    }
  }
  for (const [eId, e] of newG.edges) {
    const oldE = g.edges.get(eId)
    if (!oldE) {
      changed = true
    } else {
      oldE.label = e.label
      oldE.details = e.details
      newG.edges.set(eId, oldE)
    }
  }
  for (const vId of g.vertices.keys()) {
    if (!newG.vertices.has(vId))
      changed = true
  }
  for (const eId of g.edges.keys()) {
    if (!newG.edges.has(eId))
      changed = true
  }
  return [newG, changed]
}
}

export default (graph0: Graph) => {
  const graph = React.useMemo(() => SimGraph.ofGraph(graph0), [graph0])
  const svgRef = React.useRef<SVGSVGElement>(null)
  const { ref: setRef, width, height } = useResizeObserver<HTMLDivElement>({
    round: Math.floor,
  })

  /**
   * A common practice in `d3-force` simulations
   * is to store simulation data in the DOM using d3's `__data__` mechanism.
   * However, ensuring correct interactions between this and React is complex.
   * Instead, we store the simulation state and related data here.
   */
  interface State {
    g: SimGraph
    sim: d3.Simulation<SimVertex, SimEdge>
    tickCallbacks: Map<string, () => void>
  }
  const state = React.useRef<State>({
    g: SimGraph.empty(),
    sim: d3.forceSimulation<SimVertex, SimEdge>().force('charge', d3.forceManyBody()).stop(),
    tickCallbacks: new Map(),
  })
  // Stop the simulation on unmount
  React.useEffect(() => { return () => { state.current.sim.stop() } }, [])
  /** Reheat and restart the simulation. */
  const simRestart = () => { state.current.sim.alpha(1).restart() }
  /** Runs on every tick of the simulation. */
  const onTick = () => { for (const c of state.current.tickCallbacks.values()) c() }

  /** A selected element in the graph. */
  type Selection =
    { type: 'none' } |
    { type: 'vertex', id: string } |
    { type: 'edge', id: string }
  const [selection, setSelection] = React.useState<Selection>({ type: 'none' })
  if ((selection.type === 'vertex' && !graph.vertices.has(selection.id)) ||
    (selection.type === 'edge' && !graph.edges.has(selection.id))) {
    setSelection({ type: 'none' })
  }

  /* Update simulation state and selection given new input graph. */
  React.useEffect(() => {
    const [g, changed] = SimGraph.updateFrom(state.current.g, graph)
    state.current.g = g
    if (!changed) return
    state.current.sim.nodes(Array.from(g.vertices.values()))
      .force('link',
        d3.forceLink<SimVertex, SimEdge>(Array.from(g.edges.values()))
        .id(d => d.id))
      .on('tick', () => { onTick() })
    simRestart()
  }, [graph])

  /* Update simulation targets given new dimensions. */
  React.useEffect(() => {
    if (!width || !height) return
    const [midX, midY] = [width / 2, height / 2]
    state.current.sim
      .force('center', d3.forceCenter(midX, midY))
      .force('x', d3.forceX(midX))
      .force('y', d3.forceY(midY))
    simRestart()
  }, [width, height])

  const EmbedVert = ({v}: {v: Vertex}) => {
    const ref = React.useRef<SVGSVGElement>(null)
    React.useEffect(() => {
      const cb = () => {
        if (!ref.current) return
        const x = state.current.g.vertices.get(v.id)?.x || 0
        const y = state.current.g.vertices.get(v.id)?.y || 0
        d3.select<SVGSVGElement, unknown>(ref.current)
          .attr('transform', `translate(${x}, ${y})`)
          .call(
            d3.drag<SVGSVGElement, unknown>()
              .on('start', (ev: d3.D3DragEvent<SVGSVGElement, unknown, unknown>) => {
                if (!ev.active) state.current.sim.alphaTarget(0.3).restart()
                const sv = state.current.g.vertices.get(v.id)
                if (!sv) return
                sv.fx = sv.x
                sv.fy = sv.y
              })
              .on('drag', (ev: d3.D3DragEvent<SVGSVGElement, unknown, unknown>) => {
                const sv = state.current.g.vertices.get(v.id)
                if (!sv) return
                sv.fx = ev.x
                sv.fy = ev.y
              })
              .on('end', (ev: d3.D3DragEvent<SVGSVGElement, unknown, unknown>) => {
                if (!ev.active) state.current.sim.alphaTarget(0)
                const sv = state.current.g.vertices.get(v.id)
                if (!sv) return
                sv.fx = null
                sv.fy = null
              })
          )
      }
      cb()
      state.current.tickCallbacks.set(v.id, cb)
      return () => {
        if (state.current.tickCallbacks.get(v.id) === cb)
          state.current.tickCallbacks.delete(v.id)
      }
    }, [])
    // https://stackoverflow.com/a/479643
    return <g
      ref={ref}
      key={v.id}
      onClick={() => { setSelection({ type: 'vertex', id: v.id }) }}
    >
      <HtmlDisplay html={v.label} />
    </g>
  }

  const EmbedEdge = ({e}: {e: Edge}) => {
    const eId = Edge.calcId(e)
    const ref = React.useRef<SVGLineElement>(null)
    React.useEffect(() => {
      const cb = () => {
        d3.select(ref.current)
          .attr('x1', state.current.g.vertices.get(e.source)?.x || 0)
          .attr('y1', state.current.g.vertices.get(e.source)?.y || 0)
          .attr('x2', state.current.g.vertices.get(e.target)?.x || 0)
          .attr('y2', state.current.g.vertices.get(e.target)?.y || 0)
        }
      cb()
      state.current.tickCallbacks.set(eId, cb)
      return () => {
        if (state.current.tickCallbacks.get(eId) === cb)
          state.current.tickCallbacks.delete(eId)
      }
    }, [])
    return <line
      ref={ref}
      key={Edge.calcId(e)}
      className='dim'
      onClick={() => { setSelection({ type: 'edge', id: eId }) }}
    />
  }

  return (
    <div ref={setRef}
      style={{ maxWidth: '100%', height: 'auto' }}
    >
      <svg
        ref={svgRef}
        width={width || 400}
        // TODO: adaptive height
        height={400}
        viewBox={`0 0 ${width || 400} ${400}`}
      >
        <g stroke='var(--vscode-editor-foreground)' strokeWidth={2}>
          {graph0.edges.map(e => <EmbedEdge e={e} />)}
        </g>
        <g>
          {graph0.vertices.map(v => <EmbedVert v={v} />)}
        </g>
      </svg>
      <div className="pa1 ba bw1">
        {selection.type === 'none' ?
          'Click a vertex or an edge to learn more about it.' :
          selection.type === 'vertex' ?
          <HtmlDisplay html={state.current.g.vertices.get(selection.id)?.details ||
            {text: `Vertex '${selection.id}' has no details.`}} /> :
          <HtmlDisplay html={state.current.g.edges.get(selection.id)?.details ||
            {text: `Edge '${selection.id}' has no details.`}} />}
      </div>
    </div>
  )
}
