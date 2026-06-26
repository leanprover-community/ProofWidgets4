import 'd3-graphviz'
import type { BaseType, Selection } from 'd3-selection'
import type { ZoomBehavior } from 'd3-zoom'

/* Type information missing from or broken in @types/d3-graphviz. */

declare module 'd3-graphviz' {
  interface Graphviz<GElement extends BaseType, Datum, PElement extends BaseType, PDatum> {
    /**
     * Removes the Graphviz renderer from the element it was created on,
     * terminates any active dedicated web worker
     * and closes any port connected to a shared web worker.
     */
    destroy(): this

    /**
     * Returns the zoom behavior if zooming is enabled and a graph has been rendered,
     * else returns `null`.
     */
    zoomBehavior(): ZoomBehavior<SVGSVGElement, Datum> | null

    /**
     * Returns the selection to which the zoom behavior has been applied
     * if zooming is enabled and a graph has been rendered,
     * else returns `null`.
     */
    zoomSelection(): Selection<SVGSVGElement, Datum, PElement, PDatum> | null
  }
}
