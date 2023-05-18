/*
Copyright (c) 2022 Wojciech Nawrocki. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Wojciech Nawrocki
*/
import * as React from 'react';

import { PenroseCanvas } from './penroseCanvas';
import { DocumentPosition } from '@leanprover/infoview/infoview/util';
import { Html, default as HtmlDisplay } from './htmlDisplay';

interface DiagramData {
    embeds: [string, Html][],
    dsl: string,
    sty: string,
    sub: string,
}

export default function(props: DiagramData & {pos: DocumentPosition}): JSX.Element {
    const {embeds, dsl, sub} = props
    const mkElt = (html: Html): JSX.Element =>
        <div className="pa2">
            <HtmlDisplay pos={props.pos} html={html} />
        </div>

    const embedNodes =
        embeds.reduce(
            (acc, [nm, cwi]) => acc.set(nm, mkElt(cwi)),
            new Map<string, React.ReactNode>())

    const cssColourToRgba = (col: string, alpha: number = 255) => {
        if (col.startsWith('#')) {
            const gps = col.match(/\w\w/g)
            if (!gps) throw new Error(`cannot parse colour '${col}'`)
            const [r, g, b] = gps.map(x => parseInt(x, 16))
            return `rgba(${r}/255,${g}/255,${b}/255,${alpha}/255)`
        } else throw new Error(`cannot parse colour '${col}'`)
    }

    const getCssColour = (col: string) =>
        cssColourToRgba(
            getComputedStyle(document.documentElement)
                .getPropertyValue(col)
        )

    const colForeground = getCssColour('--vscode-editor-foreground')
    const colTooltipBackground = getCssColour('--vscode-editorHoverWidget-background')
    const colTooltipForeground = getCssColour('--vscode-editorHoverWidget-foreground')
    const colTooltipBorder = getCssColour('--vscode-editorHoverWidget-border')

    const sty = props.sty +
`
theme {
    color foreground = ${colForeground}
    color tooltipBackground = ${colTooltipBackground}
    color tooltipForeground = ${colTooltipForeground}
    color tooltipBorder = ${colTooltipBorder}
}
`

    return <PenroseCanvas
        trio={{dsl, sty, sub}}
        embedNodes={embedNodes} maxOptSteps={500}
    />
}
