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
    const {embeds, dsl, sty, sub} = props
    const mkElt = (html: Html): JSX.Element =>
        <div className="pa2">
            <HtmlDisplay pos={props.pos} html={html} />
        </div>

    const embedNodes =
        embeds.reduce(
            (acc, [nm, cwi]) => acc.set(nm, mkElt(cwi)),
            new Map<string, React.ReactNode>())

    return <PenroseCanvas
        trio={{dsl, sty, sub}}
        embedNodes={embedNodes} maxOptSteps={500}
    />
}
