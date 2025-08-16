/*
Copyright (c) 2022 Wojciech Nawrocki. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Wojciech Nawrocki
*/
import * as React from 'react';

import { PenroseCanvas } from './penroseCanvas';
import { Html, default as HtmlDisplay } from './htmlDisplay';

interface DiagramData {
    embeds: [string, Html][],
    dsl: string,
    sty: string,
    sub: string,
    maxOptSteps: number,
}

export default function(props: DiagramData): JSX.Element {
    const {embeds, dsl, sub} = props
    const mkElt = (html: Html): JSX.Element =>
        <div className="pv1 ph2">
            <HtmlDisplay html={html} />
        </div>

    const embedNodes =
        embeds.reduce(
            (acc, [nm, cwi]) => acc.set(nm, mkElt(cwi)),
            new Map<string, React.ReactNode>())

    /* A temporary element that we use to put colors in canonical form;
     * there seems to be no way of doing this with a pure function.
     * It must be in the DOM to pick up CSS styles. */
    const colEl = document.createElement('div')
    colEl.style.display = 'none'
    document.body.appendChild(colEl)
    const getCssColour = (col: string) => {
        colEl.style.color = col
        col = getComputedStyle(colEl).color
        /* The value is guaranteed to be `rgb` or `rgba` in [0, 255].
         * We convert this to `rgba` in [0, 1]. */
        const gps = col.match(/[( ]\w+/g)
        if (!gps) throw new Error(`cannot parse colour '${col}'`)
        const cols = gps.map(x => parseInt(x.substring(1), 10))
        let r, g, b, a
        if (cols.length === 3) {
            [r, g, b] = cols
            a = '255'
        } else if (cols.length === 4) {
            [r, g, b, a] = cols
        } else {
            throw new Error(`unexpected channels in '${col}'`)
        }
        return `rgba(${r}/255,${g}/255,${b}/255,${a}/255)`
    }

    const colForeground = getCssColour('var(--vscode-editor-foreground)')
    const colBackground = getCssColour('var(--vscode-editor-background)')
    const colTooltipBackground = getCssColour('var(--vscode-editorHoverWidget-background)')
    const colTooltipForeground = getCssColour('var(--vscode-editorHoverWidget-foreground)')
    const colTooltipBorder = getCssColour('var(--vscode-editorHoverWidget-border)')

    document.body.removeChild(colEl)

    const sty = props.sty +
`
theme {
    color foreground = ${colForeground}
    color background = ${colBackground}
    color tooltipBackground = ${colTooltipBackground}
    color tooltipForeground = ${colTooltipForeground}
    color tooltipBorder = ${colTooltipBorder}
}
`

    return <PenroseCanvas
        trio={{dsl, sty, sub}}
        embedNodes={embedNodes} maxOptSteps={props.maxOptSteps}
    />
}
