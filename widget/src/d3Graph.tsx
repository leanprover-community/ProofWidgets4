import * as React from 'react'
import * as d3 from 'd3'
import equal from 'deep-equal'
import useResizeObserver from 'use-resize-observer'
import HtmlDisplay, { Html } from './htmlDisplay'

interface Vertex {
  id: string
  label: Html
  radius: number
  details?: Html
}

interface Edge {
  source: string
  target: string
  attrs: [string, any][]
  label?: Html
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

interface ForceCenterParams {
  x?: number
  y?: number
  strength?: number
}

interface ForceCollideParams {
  radius?: number
  strength?: number
  iterations?: number
}

interface ForceLinkParams {
  distance?: number
  strength?: number
  iterations?: number
}

interface ForceManyBodyParams {
  strength?: number
  theta?: number
  distanceMin?: number
  distanceMax?: number
}

interface ForceXParams {
  x?: number
  strength?: number
}

interface ForceYParams {
  y?: number
  strength?: number
}

interface ForceRadialParams {
  radius: number
  x?: number
  y?: number
  strength?: number
}

type ForceParams =
    { center: ForceCenterParams }
  | { collide: ForceCollideParams }
  | { link: ForceLinkParams }
  | { manyBody: ForceManyBodyParams }
  | { x: ForceXParams }
  | { y: ForceYParams }
  | { radial: ForceRadialParams }

interface Props {
  vertices: Vertex[]
  edges: Edge[]
  defaultEdgeAttrs: [string, any][]
  forces: ForceParams[]
  showDetails: boolean
}

export default ({vertices, edges, defaultEdgeAttrs, forces: forces0, showDetails}: Props) => {
  const graph = React.useMemo(() => SimGraph.ofGraph({vertices, edges}), [vertices, edges])
  const svgRef = React.useRef<SVGSVGElement>(null)
  const { ref: setRef, width: width_, height: _height } = useResizeObserver<HTMLDivElement>({
    round: Math.floor,
  })
  const svgWidth = width_ || 400
  // TODO: adaptive height
  const svgHeight = 400

  const [forces, setForces] = React.useState(forces0)
  if (!equal(forces0, forces, { strict: true })) {
    setForces(forces0)
  }

  interface State {
    g: SimGraph
    sim: d3.Simulation<SimVertex, SimEdge>
    tickCallbacks: Map<() => void, () => void>
  }
  /** Runs on every tick of the simulation. */
  const onTick = () => { for (const c of state.current.tickCallbacks.values()) c() }
  /**
   * A common practice in `d3-force` simulations
   * is to store simulation data in the DOM using d3's `__data__` mechanism.
   * However, ensuring correct interactions between this and React is complex.
   * Instead, we store the simulation state and related data here.
   */
  const state = React.useRef<State>({
    g: SimGraph.empty(),
    sim: d3.forceSimulation<SimVertex, SimEdge>().stop(),
    tickCallbacks: new Map(),
  })
  /* Stop the simulation on unmount. */
  React.useEffect(() => { return () => { state.current.sim.stop() } }, [])
  /** Restart the simulation using vertex/edge arrays from the current state. */
  const simRestart = () => {
    const sim =
      d3.forceSimulation<SimVertex, SimEdge>(Array.from(state.current.g.vertices.values()))
        .on('tick', () => { onTick() })
    for (let i = 0; i < forces.length; i++) {
      const f = forces[i]
      let force = null
      if ('center' in f) {
        force = d3.forceCenter(f.center.x, f.center.y)
        if (f.center.strength !== undefined) force.strength (f.center.strength)
      } else if ('collide' in f) {
        force = d3.forceCollide(f.collide.radius)
        if (f.collide.strength !== undefined) force.strength(f.collide.strength)
        if (f.collide.iterations !== undefined) force.iterations(f.collide.iterations)
      } else if ('link' in f) {
        force =
          d3.forceLink<SimVertex, SimEdge>(Array.from(state.current.g.edges.values()))
            .id(d => d.id)
        if (f.link.distance !== undefined) force.distance(f.link.distance)
        if (f.link.strength !== undefined) force.strength(f.link.strength)
        if (f.link.iterations !== undefined) force.iterations(f.link.iterations)
      } else if ('manyBody' in f) {
        force = d3.forceManyBody()
        if (f.manyBody.strength !== undefined) force.strength(f.manyBody.strength)
        if (f.manyBody.theta !== undefined) force.theta(f.manyBody.theta)
        if (f.manyBody.distanceMin !== undefined) force.distanceMin(f.manyBody.distanceMin)
        if (f.manyBody.distanceMax !== undefined) force.distanceMax(f.manyBody.distanceMax)
      } else if ('x' in f) {
        force = d3.forceX(f.x.x)
        if (f.x.strength !== undefined) force.strength(f.x.strength)
      } else if ('y' in f) {
        force = d3.forceY(f.y.y)
        if (f.y.strength !== undefined) force.strength(f.y.strength)
      } else if ('radial' in f) {
        force = d3.forceRadial(f.radial.radius, f.radial.x, f.radial.y)
        if (f.radial.strength !== undefined) force.strength(f.radial.strength)
      }
      sim.force(`force${i}`, force)
    }
    state.current.sim = sim
  }

  /* Update force simulation given new forces. */
  React.useEffect(() => { simRestart() }, [forces])

  /* Update simulation state and selection given new input graph. */
  React.useEffect(() => {
    const [g, changed] = SimGraph.updateFrom(state.current.g, graph)
    state.current.g = g
    if (!changed) return
    simRestart()
  }, [graph])

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

  const EmbedVert = ({v}: {v: Vertex}) => {
    const ref = React.useRef<SVGSVGElement>(null)
    React.useEffect(() => {
      const cb = () => {
        if (!ref.current) return
        const verts = state.current.g.vertices
        const x = verts.get(v.id)?.x || 0
        const y = verts.get(v.id)?.y || 0
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
      state.current.tickCallbacks.set(cb, cb)
      return () => { state.current.tickCallbacks.delete(cb) }
    }, [])
    // https://stackoverflow.com/a/479643
    return <g
      ref={ref}
      key={v.id}
      onClick={() => { if (showDetails && v.details) setSelection({ type: 'vertex', id: v.id }) }}
    >
      <HtmlDisplay html={v.label} />
    </g>
  }

  const EmbedEdge = ({e}: {e: Edge}) => {
    const eId = Edge.calcId(e)
    const lineRef = React.useRef<SVGLineElement>(null)
    const labelGRef = React.useRef<SVGGElement>(null)
    React.useEffect(() => {
      const cb = () => {
        const verts = state.current.g.vertices
        const vSrc = verts.get(e.source)
        const vTgt = verts.get(e.target)
        const xSrc = vSrc?.x || 0
        const ySrc = vSrc?.y || 0
        const xTgt = vTgt?.x || 0
        const yTgt = vTgt?.y || 0
        const alpha = Math.atan2(yTgt - ySrc, xTgt - xSrc)
        d3.select(lineRef.current)
          .attr('x1', xSrc + Math.cos(alpha) * (vSrc?.radius || 0))
          .attr('y1', ySrc + Math.sin(alpha) * (vSrc?.radius || 0))
          /* `+ 2` to accommodate arrowheads. */
          .attr('x2', xTgt - Math.cos(alpha) * ((vTgt?.radius || 0) + 2))
          .attr('y2', yTgt - Math.sin(alpha) * ((vTgt?.radius || 0) + 2))
        d3.select(labelGRef.current)
          .attr('transform', `translate(${(xSrc + xTgt) / 2}, ${(ySrc + yTgt) / 2})`)
      }
      cb()
      state.current.tickCallbacks.set(cb, cb)
      return () => { state.current.tickCallbacks.delete(cb) }
    }, [])
    return <g
      key={Edge.calcId(e)}
      onClick={() => { if (showDetails && e.details) setSelection({ type: 'edge', id: eId }) }}
    >
      <line
        {...defaultEdgeAttrs.reduce((o, [k, v]) => ({ ...o, [k]: v }), {})}
        {...e.attrs.reduce((o, [k, v]) => ({ ...o, [k]: v }), {})}
        ref={lineRef}
      />
      <g ref={labelGRef}>
        {e.label && <HtmlDisplay html={e.label} />}
      </g>
    </g>
  }

  return (
    <div ref={setRef}
      style={{ maxWidth: '100%', height: 'auto' }}
    >
      <svg
        ref={svgRef}
        width={svgWidth}
        height={svgHeight}
        viewBox={`-${svgWidth/2} -${svgHeight/2} ${svgWidth} ${svgHeight}`}
      >
        <defs>
          <marker
            id="arrow"
            viewBox="0 0 10 10"
            refX="8"
            refY="5"
            markerWidth="3"
            markerHeight="3"
            orient="auto-start-reverse"
          >
            <g stroke="context-stroke" fill="context-fill">
              <path d="M 0 0 L 10 5 L 0 10 z" />
            </g>
          </marker>
        </defs>
        <g>{edges.map(e => <EmbedEdge e={e} />)}</g>
        <g>{vertices.map(v => <EmbedVert v={v} />)}</g>
      </svg>
      {showDetails &&
        <div className="pa1 ba bw1">
          {selection.type === 'none' ?
          'Click a vertex or an edge to learn more about it.' :
          selection.type === 'vertex' ?
          <HtmlDisplay html={state.current.g.vertices.get(selection.id)?.details ||
            {text: `Vertex '${selection.id}' has no details.`}} /> :
          <HtmlDisplay html={state.current.g.edges.get(selection.id)?.details ||
            {text: `Edge '${selection.id}' has no details.`}} />}
        </div>}
    </div>
  )
}
