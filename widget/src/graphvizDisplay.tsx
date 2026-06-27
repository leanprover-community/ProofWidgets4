/*
Copyright (c) 2026 Lean FRO, LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Wojciech Nawrocki
*/

import * as React from 'react'
import { graphviz } from 'd3-graphviz'
import type { GraphvizOptions } from 'd3-graphviz'
import deepEqual from 'deep-equal'
import useResizeObserver from 'use-resize-observer'

/** See the Lean API for documentation. */
export interface Props extends React.HTMLAttributes<HTMLDivElement> {
  dot: string
  options: GraphvizOptions
  renderDebounceMs: number
  centerOnVertex?: string
}

function useDeepEqualMemo<T>(value: T): T {
  const ref = React.useRef(value)
  if (!deepEqual(ref.current, value, { strict: true })) ref.current = value
  return ref.current
}

export default function({ dot, options: options0, renderDebounceMs, centerOnVertex, style, ...props }: Props) {
  const options = useDeepEqualMemo(options0)

  const divRef = React.useRef<HTMLDivElement>(null)
  const graphvizRef = React.useRef<ReturnType<typeof graphviz> | null>(null)
  const [error, setError] = React.useState<string | null>(null)

  type State = 'init' | 'will-layout' | 'will-render' | 'ready' | 'will-unmount'
  interface StateRef {
    state: State
    dotId: number
    /** Resolves on the next state transition. */
    onTransition: PromiseWithResolvers<void>
  }
  const stateRef = React.useRef<StateRef>({ state: 'init', dotId: 0, onTransition: Promise.withResolvers() })
  const transitionState = (s: State) => {
    stateRef.current.state = s
    stateRef.current.onTransition.resolve()
    stateRef.current.onTransition = Promise.withResolvers()
  }
  /** Resolved when a state accepted by `p` is reached.
   * Rejected when `will-unmount` is reached first, and `p` does not accept it. */
  const awaitState = async (p: (_: State) => boolean) => {
    while (true) {
      const currSt = stateRef.current.state
      if (p(currSt)) return
      if (currSt === 'will-unmount') throw new Error('will-unmount')
      await stateRef.current.onTransition.promise
    }
  }

  const { width = 400, height = 400 } = useResizeObserver<HTMLDivElement>({
    ref: divRef,
    round: Math.floor,
  })
  const svgWidth = options.width ?? width
  const svgHeight = options.height ?? height

  React.useEffect(() => {
    const div = divRef.current
    if (!div) return

    graphvizRef.current = graphviz(div, {
      ...options,
      width: svgWidth,
      height: svgHeight,
    })
    graphvizRef.current.onerror(err => {
      setError(err)
      transitionState('init')
    })

    return () => {
      graphvizRef.current?.destroy()
      graphvizRef.current = null
      transitionState('init')
    }
  }, [options])

  React.useEffect(() => {
    if (stateRef.current.state === 'ready') transitionState('will-render')
    graphvizRef.current?.width(svgWidth)
    queueRender()
  }, [svgWidth])

  React.useEffect(() => {
    if (stateRef.current.state === 'ready') transitionState('will-render')
    graphvizRef.current?.height(svgHeight)
    queueRender()
  }, [svgHeight])

  const renderTimeoutRef = React.useRef<number | null>(null)
  const queueRender = () => {
    if (renderTimeoutRef.current !== null) window.clearTimeout(renderTimeoutRef.current)
    const currDotId = stateRef.current.dotId
    renderTimeoutRef.current = window.setTimeout(() => {
      renderTimeoutRef.current = null
      void awaitState(s => s === 'will-render').then(() => {
        if (stateRef.current.dotId !== currDotId) return
        graphvizRef.current?.render(() => {
          if (stateRef.current.dotId === currDotId) {
            setError(null)
            transitionState('ready')
          }
        })
      }).catch(() => {})
    }, renderDebounceMs)
  }

  React.useEffect(() => {
    transitionState('will-layout')
    const currDotId = ++stateRef.current.dotId
    // This starts layout immediately in the background.
    // We assume that `dot` doesn't change often enough to make this worth debouncing.
    graphvizRef.current?.dot(dot, () => {
      if (stateRef.current.dotId == currDotId)
        transitionState('will-render')
    })
    queueRender()
  }, [dot, options])

  /* Animate to the chosen vertex when it changes. */
  const centerOnVertexRef = React.useRef<string | undefined>(centerOnVertex)
  React.useEffect(() => {
    centerOnVertexRef.current = centerOnVertex
    void awaitState(s => s === 'ready').then(() => {
      const gv = graphvizRef.current
      const vertex = centerOnVertexRef.current
      if (!gv || !vertex) return

      const zoom = gv.zoomBehavior()
      const svg = gv.zoomSelection()
      if (!zoom || !svg) return

      const node: SVGGElement | null =
        svg.selectAll<SVGGElement, unknown>('g')
          .filter((d: any) => d.key === vertex)
          .node()
      if (!node) return

      let bbox: DOMRect
      try {
        bbox = node.getBBox()
      } catch {
        return
      }
      const x = bbox.x + bbox.width / 2
      const y = bbox.y + bbox.height / 2
      svg.transition().duration(100)
        .call(zoom.translateTo, x, y)

      centerOnVertexRef.current = undefined
    }).catch(() => {})
  }, [centerOnVertex])

  React.useEffect(() => {
    // Cleanup function run on unmount.
    return () => {
      transitionState('will-unmount')
      if (renderTimeoutRef.current !== null) window.clearTimeout(renderTimeoutRef.current)
    }
  }, [])

  const divHeight = options.height ?? 400
  return <>
    <div
      ref={divRef}
      style={{
        overflow: 'hidden',
        resize: 'vertical',
        height: divHeight,
        ...style,
      }}
      {...props} />
    {error && <div className="ba bw1 br1 pa1 ma1 red">{error}</div>}
  </>
}
