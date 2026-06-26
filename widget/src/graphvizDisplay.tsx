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
interface Props {
  dot: string
  options: GraphvizOptions
  renderDebounceMs: number
}

function useDeepEqualMemo<T>(value: T): T {
  const ref = React.useRef(value)
  if (!deepEqual(ref.current, value, { strict: true })) ref.current = value
  return ref.current
}

export default function({ dot, options: options0, renderDebounceMs, ...props }: Props) {
  const options = useDeepEqualMemo(options0)

  const divRef = React.useRef<HTMLDivElement>(null)
  const graphvizRef = React.useRef<ReturnType<typeof graphviz> | null>(null)

  React.useEffect(() => {
    const div = divRef.current
    if (!div) return

    graphvizRef.current = graphviz(div, options)

    return () => {
      graphvizRef.current?.destroy()
      graphvizRef.current = null
    }
  }, [options])

  const renderTimeoutRef = React.useRef<number | null>(null)
  const render = () => {
    if (renderTimeoutRef.current !== null) window.clearTimeout(renderTimeoutRef.current)
    renderTimeoutRef.current = window.setTimeout(() => {
      renderTimeoutRef.current = null
      graphvizRef.current?.render()
    }, renderDebounceMs)
  }

  React.useEffect(() => {
    // This starts layout immediately in the background.
    // We assume that `dot` doesn't change often enough to make this worth debouncing.
    graphvizRef.current?.dot(dot)
    render()
  }, [dot])

  const { width = 400, height = 400 } = useResizeObserver<HTMLDivElement>({
    ref: divRef,
    round: Math.floor,
  })
  const svgWidth = options.width ?? width
  const svgHeight = options.height ?? height

  React.useEffect(() => {
    graphvizRef.current?.width(svgWidth)
    render()
  }, [svgWidth])

  React.useEffect(() => {
    graphvizRef.current?.height(svgHeight)
    render()
  }, [svgHeight])

  React.useEffect(() => {
    // Cleanup function run on unmount.
    return () => {
      if (renderTimeoutRef.current !== null) window.clearTimeout(renderTimeoutRef.current)
    }
  }, [])
  
  const divHeight = options.height ?? 400
  return <div
    ref={divRef}
    style={{
      overflow: 'hidden',
      resize: 'vertical',
      height: divHeight,
    }}
    {...props} />
}
