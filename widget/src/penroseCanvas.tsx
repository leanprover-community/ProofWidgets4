/*
Copyright (c) 2022-2023 Wojciech Nawrocki. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Wojciech Nawrocki
*/
import * as React from "react"
import * as penrose from "@penrose/core"
import { ok } from "@penrose/core/dist/utils/Error"
import { andThen, err } from "@penrose/core/dist/utils/Error"
import { collectLabels, insertPending, mathjaxInit } from "@penrose/core/dist/utils/CollectLabels"
import { compileStyle } from "@penrose/core/dist/compiler/Style"
import * as SVG from "@svgdotjs/svg.js"
import useResizeObserver from "use-resize-observer";

/** See [here](https://penrose.cs.cmu.edu/docs/tutorial/welcome#what-makes-up-a-penrose-program). */
export interface PenroseTrio {
  dsl: string
  sty: string
  sub: string
}

/** Compute the hash of inputs that determine a diagram:
 * the Penrose trio and embed sizes. */
async function hashInputs({dsl, sty, sub}: PenroseTrio, embedSizes: Map<string, [number, number]>):
    Promise<string> {
  const sizesStr = Array.from(embedSizes.entries())
    // lexicographic sort on keys
    .sort(([n1, _a], [n2, _b]) => n1 === n2 ? 0 : n1 < n2 ? -1 : 1)
    .toString()
  // https://developer.mozilla.org/en-US/docs/Web/API/TextEncoder/encodeInto#buffer_sizing
  const data = new Uint8Array(3 * (dsl.length + sty.length + sub.length + sizesStr.length))
  const encoder = new TextEncoder()
  let dataView = data
  const {written} = encoder.encodeInto(dsl, dataView)
  dataView = data.subarray(written)
  const {written: written2} = encoder.encodeInto(sty, dataView)
  dataView = data.subarray(written2)
  const {written: written3} = encoder.encodeInto(sub, dataView)
  dataView = data.subarray(written3)
  encoder.encodeInto(sizesStr, dataView)
  const digest = await crypto.subtle.digest("SHA-1", data)
  const digestArr = Array.from(new Uint8Array(digest))
  // https://developer.mozilla.org/en-US/docs/Web/API/SubtleCrypto/digest#converting_a_digest_to_a_hex_string
  return digestArr.map(b => b.toString(16).padStart(2, '0')).join('')
}

/** The compile -> optimize -> prepare SVG sequence is not cheap (up to seconds),
 * so we cache its SVG outputs by the {@link hashTrio} of the input trio.
 * The values are `[svg, variation]` where `variation` is the one
 * that was used to draw the diagram.
 * It is not part of the hash: a 'good variation' is instead viewed as an output
 * associated to a trio. */
const diagramSvgCache = new Map<string, [SVGSVGElement, string]>()

function svgNumberToNumber (x: SVG.NumberAlias): number {
  let y: string | number
  if (x instanceof Number) y = x.valueOf()
  else y = x as any

  if (typeof y === 'string') return parseFloat(y)
  else return y
}

/** Run the Penrose optimizer starting from `state` for at most `maxSteps` total steps.
 * There is a single step counter across all optimization stages,
 * i.e. it is not reset when advancing the stage. */
function optimizeMaxSteps(
  state: penrose.PenroseState,
  maxSteps: number,
): penrose.Result<penrose.PenroseState, penrose.PenroseError> {
  let i = 0
  const until = () => i++ >= maxSteps
  while ((!penrose.isOptimized(state) || !penrose.finalStage(state)) && !until()) {
    if (penrose.isOptimized(state))
      state = penrose.nextStage(state)
    const res = penrose.step(state, { until })
    if (res.isErr())
      return res
    state = res.value
  }
  return ok(state)
}

/** Like {@link penrose.compile} but also assigns `textBox.width/height`s. See main doc. */
async function compileWithSizes(prog: {
  substance: string
  style: string
  domain: string
  variation: string
  embedSizes: Map<string, [number, number]>
  excludeWarnings?: string[]
}): Promise<penrose.Result<penrose.PenroseState, penrose.PenroseError>> {
  const domainRes = penrose.compileDomain(prog.domain);
  const subRes = andThen(env => penrose.compileSubstance(prog.substance, env), domainRes)
  if (subRes.isErr()) return err(subRes.error)
  const [env, varEnv] = subRes.value
  let sty = prog.style
  for (const [name, [w, h]] of prog.embedSizes.entries()) {
    if (!varEnv.vars.has(name))
      throw new Error(`could not find object ${name} in the substance program`)
    const tp = varEnv.vars.get(name)!.name.value
    // KC's hack: https://github.com/penrose/penrose/issues/1057#issuecomment-1164313880
    sty = sty + `
      forall ${tp} \`${name}\` {
        override \`${name}\`.textBox.width = ${w}
        override \`${name}\`.textBox.height = ${h}
      }`
  }
  const styRes =
    await compileStyle(prog.variation, sty, prog.excludeWarnings ?? [], env, varEnv)
  if (styRes.isErr()) return err(styRes.error)
  const state = styRes.value
  // collect labels and return state
  const convert = mathjaxInit()
  const labelCache = await collectLabels(state.shapes, convert)
  if (labelCache.isErr()) {
      return err(labelCache.error)
  }
  return ok(insertPending({ ...state, labelCache: labelCache.value }))
}

async function renderPenroseTrio(trio: PenroseTrio, embedSizes: Map<string, [number, number]>,
    hash: string, variation: string | undefined, maxOptSteps: number): Promise<SVGSVGElement> {
  if (diagramSvgCache.has(hash)) {
    const [svg, svgVariation] = diagramSvgCache.get(hash)!
    // When `variation === undefined`, an SVG for the given `trio/embedSizes` inputs
    // drawn with any variation can be retrieved from cache.
    // Otherwise an SVG drawn with the specific provided `variation` must be returned.
    if (variation) {
      if (svgVariation === variation)
        return svg.cloneNode(true) as any
    } else {
      return svg.cloneNode(true) as any
    }
  }
  const {dsl, sty, sub} = trio
  variation = variation ?? ''
  const compileRes = await compileWithSizes({
    domain: dsl,
    style: sty,
    substance: sub,
    variation,
    embedSizes,
  })
  if (compileRes.isErr()) throw new Error(penrose.showError(compileRes.error))
  const stateRes = optimizeMaxSteps(compileRes.value, maxOptSteps)
  if (stateRes.isErr()) throw new Error(penrose.showError(stateRes.error))
  if (!penrose.isOptimized(stateRes.value))
    console.warn(`Diagram failed to converge in ${maxOptSteps} steps`)
  const svg = await penrose.toSVG(stateRes.value, async path => path, '')

  // The canvas is usually too large, so trim the SVG as a postprocessing step
  const obj = SVG.SVG(svg)
  const view = obj.viewbox()
  let minX = view.width, maxX = 0, minY = view.height, maxY = 0

  obj.each((i, children) => {
    const child = children[i]
    if (!child.x) return
    minX = Math.min(minX, svgNumberToNumber(child.x()))
    maxX = Math.max(maxX, svgNumberToNumber(child.x()) + svgNumberToNumber(child.width()))
    minY = Math.min(minY, svgNumberToNumber(child.y()))
    maxY = Math.max(maxY, svgNumberToNumber(child.y()) + svgNumberToNumber(child.height()))
  })

  const padding = 10
  const newX = minX - padding, newY = minY - padding,
        newW = (maxX - minX) + padding, newH = (maxY - minY) + padding
  const newSvg = obj.viewbox(newX, newY, newW, newH).width(newW).height(newH)
  diagramSvgCache.set(hash, [newSvg.node, variation])

  return newSvg.node
}

/** Return all elements in a Penrose-generated SVG
 * which have names corresponding to objects in the substance program.
 * HACK: These are detected by looking for strings in the elements' `textContent`s. */
function getPenroseNamedElements(svg: SVG.Svg): Map<string, SVG.Element> {
  const res = new Map<string, SVG.Element>()
  for (const child of svg.find('g, rect')) {
    if (!child.node.textContent) continue
    const groups = child.node.textContent.match(/`(\w+)`.textBox/)
    if (!groups) continue
    const name = groups[1]
    res.set(name, child)
  }
  return res
}

/** A drawn diagram in SVG form. */
interface Diagram {
  svg: SVGSVGElement
  /** The offset of each named element from the top-left corner of the SVG, in pixels. */
  embedOffs: Map<string, [number, number]>
}

/** The state of a {@link PenroseCanvas} component. */
type CanvasState =
  { tag: 'loading',
    /** If present, there is a timeout in place which, upon realization,
     * will transition to `drawing` and start drawing a diagram.
     * We don't transition immediately in order to debounce rapid changes to diagram inputs,
     * e.g. when the embeds render for the first time. */
    timeout?: number,
    diag?: Diagram } |
  { tag: 'drawing',
    /** Hash of the inputs making up the diagram being drawn.
     * This is used for consistency:
     * if a drawing job A finishes and sees that the `hash` is different,
     * this means that another job B is in flight and A should discard its results. */
    hash: string,
    diag?: Diagram } |
  { tag: 'drawn',
    diag: Diagram } |
  { tag: 'error',
    error: Error }

namespace CanvasState {
/** If present and the state is `loading`/`drawing`,
 * then we had already drawn a correct diagram in the past.
 * We keep showing it (grayed out) as the last-known-good state. */
export function getDiag(cs : CanvasState): Diagram | undefined {
  if ('diag' in cs) return cs.diag
  return undefined
}
}

interface EmbedData {
  elt: HTMLDivElement | undefined
  width: number
  height: number
}

type EmbeddedNodeProps =
  React.PropsWithChildren<
      React.DetailedHTMLProps<React.HTMLAttributes<HTMLDivElement>, HTMLDivElement>> & {
    name: string
    setEmbedData: (nm: string, f: (curr: EmbedData | undefined) => EmbedData | undefined) => void
  }

/** A React node embedded in an SVG.
 * We inform the parent about resizes and (un)mounts of the embedded node. */
function EmbeddedNode(props_: EmbeddedNodeProps): JSX.Element {
  const {name, setEmbedData, ...props} = props_
  const { ref: setRef } = useResizeObserver<HTMLDivElement>({
    round: Math.ceil,
    onResize: ({width, height}) => {
      if (!width || !height) return
      setEmbedData(name, curr => {
        if (curr && curr.width === width && curr.height === height) return curr
        if (curr) return { ...curr, width, height }
        return { elt: undefined, width, height }
      })
    }
  })

  React.useEffect(() => {
    // This is called on unmount. Returning `undefined` deletes the entry from `embedsData`.
    return () => setEmbedData(name, () => undefined)
  }, [])

  return <div
    {...props}
    ref={newDiv => {
      // Gets called with `null` on every commit phase, just ignore that.
      if (!newDiv) return
      setRef(newDiv)
      setEmbedData(name, curr => {
        if (curr && curr.elt === newDiv) return curr
        if (curr) return { ...curr, elt: newDiv }
        return { elt: newDiv, height: 0, width: 0 }
      })
    }}>{props.children}</div>
}

export interface PenroseCanvasProps {
  trio: PenroseTrio
  maxOptSteps: number
  embedNodes: Map<string, React.ReactNode>
}

/** Renders an interactive [Penrose](https://github.com/penrose/penrose) diagram
 * with the specified `trio`.
 * The Penrose optimizer is ran for at most `maxOptSteps`,
 * a heuristic for when to stop trying and return a non-converged diagram.
 *
 * Values of `canvas.width` and `canvas.height` (required by Penrose)
 * matching the dimensions of this component
 * are prepended to the style program.
 *
 * For every `[name, nd]` in `embedNodes`,
 * we locate an object with the name `name` in the substance program.
 * The object *must* be assigned a `name.textBox : Rectangle` field in the style program.
 * We fix the dimensions of `name.textBox` to those of the React node `nd`,
 * and draw `nd` over `name.textBox` in the SVG diagram.
 *
 * The diagram is redrawn when the dimensions of this component or any `embedNode` change. */
export function PenroseCanvas(props: PenroseCanvasProps): JSX.Element {
  const [state, setState] = React.useState<CanvasState>({tag: 'loading'})

  // If present, used as the random seed from which the diagram is drawn.
  const [variation, setVariation] = React.useState<string | undefined>(undefined)
  React.useEffect(() => { setVariation(undefined) }, [props.trio])

  const drawDiagram =
      (trio: PenroseTrio, embedSizes: Map<string, [number, number]>) =>
      async () => {
    const hash = await hashInputs(trio, embedSizes)
    try {
      setState(st => ({tag: 'drawing', hash, diag: CanvasState.getDiag(st)}))
      const svg = await renderPenroseTrio(trio, embedSizes, hash, variation, props.maxOptSteps)

      // Compute embed offsets.
      const obj = SVG.SVG(svg)
      const diagramBoxes = getPenroseNamedElements(obj)
      const embedOffs = new Map<string, [number, number]>()
      for (const [name, _] of embedSizes) {
        const gElt = diagramBoxes.get(name)
        if (!gElt) throw new Error(`Could not find object named '${name}' in the diagram.`)
        // Note: this calculation assumes that one SVG user unit is one pixel. We achieve
        // this by setting the `viewBox` width/height to the `<svg>` width/height.
        const userY = svgNumberToNumber(gElt.y()) - obj.viewbox().y
        const userX = svgNumberToNumber(gElt.x()) - obj.viewbox().x
        embedOffs.set(name, [userX, userY])
      }

      setState(st => {
        if (st.tag !== 'drawing') return st
        if (st.hash !== hash) return st
        return {tag: 'drawn', diag: {svg, embedOffs}}
      })
    } catch (e) {
      setState(st => {
        if (st.tag !== 'drawing') return st
        if (st.hash !== hash) return st
        console.warn(e)
        const error = e instanceof Error ? e : new Error(JSON.stringify(e))
        return {tag: 'error', error}
      })
    }
  }

  const { ref: containerRef, width: containerWidth = 1 } = useResizeObserver<HTMLDivElement>({
    round: Math.ceil
  })

  // The map has entries for exactly those embeds for which divs have been mounted.
  // It has immutable reference semantics: whenever any of the stored data change,
  // the reference identity of the whole map must change.
  const [embedsData, setEmbedsData] = React.useState<Map<string, EmbedData>>(new Map())
  const setEmbedData = React.useCallback(
    (nm: string, f: (curr: EmbedData | undefined) => EmbedData | undefined) => {
      setEmbedsData(embedsData => {
        const curr = embedsData.get(nm)
        const new_ = f(curr)
        if (new_ === curr) return embedsData
        const embedsDataNew = new Map(embedsData)
        if (new_ === undefined) {
          embedsDataNew.delete(nm)
        } else {
          embedsDataNew.set(nm, new_)
        }
        return embedsDataNew
      })
    },
    [setEmbedsData])

  const diagramWidth = Math.max(400, containerWidth)
  const maxEmbedWidth = Math.ceil(diagramWidth / 2)

  // This effect draws or redraws the diagram
  // whenever any of the inputs that may influence its layout have changed.
  React.useEffect(() => {
    // If not all embeds are mounted yet, return immediately.
    if (embedsData.size !== props.embedNodes.size) return
    // If the container is too small, return immediately. This can happen early after mounting.
    if (containerWidth < 4) return
    const embedSizes = new Map<string, [number, number]>()
    for (const [nm, dat] of embedsData) {
      if (dat.width < 4 || dat.height < 4) return
      embedSizes.set(nm, [dat.width, dat.height])
    }

    let sty = props.trio.sty + `
      canvas {
        width = ${diagramWidth}
        height = ${diagramWidth}
      }`

    setState(st => {
      if (st.tag === 'loading' && st.timeout) window.clearTimeout(st.timeout)
      const timeout = window.setTimeout(drawDiagram({...props.trio, sty}, embedSizes), 300)
      return {tag: 'loading', timeout, diag: CanvasState.getDiag(st)}
    })
  }, [containerWidth, embedsData, variation, props.trio.sub, props.trio.sty, props.trio.dsl])

  // `relative` makes the outer `div` a
  // [containing block](https://developer.mozilla.org/en-US/docs/Web/CSS/Containing_block#identifying_the_containing_block)
  // for `absolute`ly positioned children.
  let cn = 'relative'
  // Decrease opacity when loading or updating.
  if (state.tag !== 'drawn' && state.tag !== 'error')
    cn += ' o-30'

  const diag = CanvasState.getDiag(state)

  return <div className={cn} ref={containerRef}>
    {diag &&
      <>
        <div ref={ref => {
            if (!ref) return
            if (ref.firstChild) ref.replaceChild(diag.svg, ref.firstChild)
            else ref.appendChild(diag.svg)
          }} />
        {/* `absolute` positioning` relative to the outer `div` */}
        <a className='absolute top-0 right-0 link pointer dim codicon codicon-refresh'
          onClick={() => setVariation(Math.random().toString())} />
      </>
      }
    {!diag && state.tag === 'loading' && <>Loading..</>}
    {!diag && state.tag === 'drawing' && <>Drawing..</>}
    {state.tag === 'error' && <span className='red'>{state.error.toString()}</span>}
    <div style={{visibility: diag ? 'visible' : 'hidden'}}>
      {Array.from(props.embedNodes, ([nm, nd]) => {
        const [userX, userY] = diag?.embedOffs.get(nm) ?? [0, 0]
        return <EmbeddedNode
            key={nm}
            name={nm}
            setEmbedData={setEmbedData}
            className='dib absolute'
            style={{
              // Limit how wide nodes in the diagram can be.
              maxWidth: `${maxEmbedWidth}px`,
              left: `${userX}px`,
              top: `${userY}px`
          }}>
            {nd}
          </EmbeddedNode>
        })}
    </div>
  </div>
}
