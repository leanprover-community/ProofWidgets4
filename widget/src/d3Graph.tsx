import * as React from 'react'
import * as d3 from 'd3'
import useResizeObserver from 'use-resize-observer'
import { Html } from './htmlDisplay'

interface Vertex extends d3.SimulationNodeDatum {
  id: string
  label?: Html
  details?: Html
}

interface Edge {
  source: string
  target: string
  label?: Html
  details?: Html
}

/** The input to this component. */
interface Graph {
  vertices: Vertex[]
  edges: Edge[]
}

function calcEdgeId(e: Edge): string {
  return `${e.source} ${e.target}`
}

/** An extension of {@link Edge} with simulation-related data. */
interface SimEdge extends d3.SimulationLinkDatum<Vertex> {
  id: string
  source: Vertex | string | number
  target: Vertex | string | number
  label?: Html
  details?: Html
}

/** An extension of {@link Graph} with simulation-related data. */
interface SimGraph {
  vertices: Vertex[]
  edges: SimEdge[]
}

function getVertex(g: SimGraph, v: Vertex | string | number): Vertex {
  if (typeof v === 'object') return v
  if (typeof v === 'string') return g.vertices.find(w => w.id === v)!
  return g.vertices[v]
}

export default (graph: Graph) => {
  const { ref: setRef, width, height } = useResizeObserver<HTMLDivElement>({
    round: Math.floor,
  })

  const svgRef = React.useRef<SVGSVGElement>(null)
  const sim = React.useRef(
    d3.forceSimulation<Vertex, SimEdge>()
      .force('charge', d3.forceManyBody())
  )
  // Stop the simulation on unmount
  React.useEffect(() => { return () => { sim.current.stop() } }, [])

  /** Set the vertex and edge arrays to ones stored in the given selections.
   * If `needRestart` is true, also reheat and restart the simulation. */
  const updateSimData = (
      vertSel: d3.Selection<SVGCircleElement, Vertex, d3.BaseType, unknown>,
      edgeSel: d3.Selection<SVGLineElement, SimEdge, d3.BaseType, unknown>,
      needRestart: boolean) => {
    const g: SimGraph = {
      vertices: vertSel.data(),
      edges: edgeSel.data()
    }
    sim.current.nodes(g.vertices)
      .force('link', d3.forceLink<Vertex, SimEdge>(g.edges).id(d => d.id))
      .on('tick', () => {
        vertSel
          .attr('cx', d => d.x || 0)
          .attr('cy', d => d.y || 0)
        edgeSel
          .attr('x1', d => getVertex(g, d.source).x || 0)
          .attr('y1', d => getVertex(g, d.source).y || 0)
          .attr('x2', d => getVertex(g, d.target).x || 0)
          .attr('y2', d => getVertex(g, d.target).y || 0)
      })
      .stop()
    vertSel.call(
      d3.drag<SVGCircleElement, Vertex>()
        .on('start', (ev: d3.D3DragEvent<SVGCircleElement, Vertex, Vertex>) => {
          if (!ev.active) sim.current.alphaTarget(0.3).restart()
          ev.subject.fx = ev.subject.x
          ev.subject.fy = ev.subject.y
        })
        .on('drag', (ev: d3.D3DragEvent<SVGCircleElement, Vertex, Vertex>) => {
          ev.subject.fx = ev.x
          ev.subject.fy = ev.y
        })
        .on('end', (ev: d3.D3DragEvent<SVGCircleElement, Vertex, Vertex>) => {
          if (!ev.active) sim.current.alphaTarget(0)
          ev.subject.fx = null
          ev.subject.fy = null
        })
    )
    if (needRestart) sim.current.alpha(1).restart()
  }

  React.useEffect(() => {
    if (!svgRef.current) return
    const svg = d3.select(svgRef.current)
    // We need to make copies of the graph data
    // that the simulation will be able to mutate.
    // We store these copies in the DOM using d3's `__data__` storage.
    // We cannot use `selection.data` because it overwrites the stored data,
    // whereas we want to merge it with the new graph
    // while keeping the simulated positions
    // of entities that are still present in the new graph.
    // So, we implement update/remove/append using other selection methods.
    const [vertMap, edgeMap] = [
      new Map<string, Vertex>(graph.vertices.map(v => [v.id, {...v}])),
      new Map<string, SimEdge>(graph.edges.map(e => [calcEdgeId(e), {...e, id: calcEdgeId(e)}])),
    ]

    const vertSel = svg.select('#vs')
      .selectAll<SVGCircleElement, Vertex>('circle')
      .datum(d => {
        const newD = vertMap.get(d.id)
        if (!newD) return null
        vertMap.delete(d.id)
        // It's important not to create a new object here
        // so that the simulation's object reference remains valid
        return Object.assign(d, newD)
      })
    const rmVertSel = vertSel.filter(d => !d).remove()
    const addVertSel = svg.select('#vs').selectAll()
      .data(vertMap.values())
      .join('circle')
      .attr('r', 5)
      .attr('fill', '#ffff00')

    const edgeSel = svg.select('#es')
      .selectAll<SVGLineElement, SimEdge>('line')
      .datum(d => {
        const newD = edgeMap.get(d.id)
        if (!newD) return null
        edgeMap.delete(d.id)
        return Object.assign(d, newD)
      })
    const rmEdgeSel = edgeSel.filter(d => !d).remove()
    const addEdgeSel = svg.select('#es').selectAll()
      .data(edgeMap.values())
      .join('line')
      .attr('stroke-width', 0.5)

    const needRestart =
      !rmVertSel.empty() || !addVertSel.empty() || !rmEdgeSel.empty() || !addEdgeSel.empty()
    updateSimData(svg.selectAll('#vs circle'), svg.selectAll('#es line'), needRestart)
  }, [graph])

  const updateSimDims = (width: number, height: number) => {
    const [midX, midY] = [width / 2, height / 2]
    sim.current
      .force('center', d3.forceCenter(midX, midY))
      .force('x', d3.forceX(midX))
      .force('y', d3.forceY(midY))
      .alpha(1).restart()
  }

  React.useEffect(() => {
    if (!width || !height) return
    updateSimDims(width, height)
  }, [width, height])

  return (
    <div ref={setRef}
      style={{ maxWidth: '100%', height: 'auto' }}
    >
      <svg
        ref={svgRef}
        width={width || 400}
        height={400}
        viewBox={`0 0 ${width || 400} ${400}`}
      >
        <g id='es' stroke='#888' strokeWidth={1.5} />
        <g id='vs' stroke='#999' strokeOpacity={0.6} />
      </svg>
    </div>
  )
}
