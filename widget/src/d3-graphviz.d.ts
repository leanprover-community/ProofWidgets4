import 'd3-graphviz'
import type { BaseType } from 'd3-selection'

/* Type information missing from @types/d3-graphviz. */

declare module 'd3-graphviz' {
  interface Graphviz<GElement extends BaseType, Datum, PElement extends BaseType, PDatum> {
    /**
     * Removes the Graphviz renderer from the element it was created on,
     * terminates any active dedicated web worker
     * and closes any port connected to a shared web worker.
     */
    destroy(): this
  }
}
