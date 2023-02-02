/*
Copyright (c) 2022 E.W.Ayers. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: E.W.Ayers
*/

import * as React from 'react';
import { DynamicComponent, DocumentPosition } from '@leanprover/infoview';

type HtmlAttribute = [string, any]

export type Html =
    | { element: [string, HtmlAttribute[], Html[]] }
    | { text: string }
    | { component: [string, any, Html[]] }

export default function HtmlDisplay({pos, html} : {pos: DocumentPosition, html: Html}): JSX.Element {
    if ('text' in html) {
        return <>{html.text}</>
    } else if ('element' in html) {
        const [tag, attrsList, cs] = html.element
        const attrs: any = {}
        for (const [k,v] of attrsList) {
            attrs[k] = v
        }
        const children = cs.map(html => HtmlDisplay({ pos, html }))
        if (tag === "hr") {
            // React is greatly concerned by <hr/>s having children.
            return <hr/>
        } else if (children.length === 0) {
            return React.createElement(tag, attrs)
        } else {
            return React.createElement(tag, attrs, children)
        }
    } else if ('component' in html) {
        const [hash, props, cs] = html.component
        const children = cs.map(html => HtmlDisplay({ pos, html }))
        const dynProps = {...props, pos}
        if (children.length === 0) {
            return <DynamicComponent pos={pos} hash={hash} props={dynProps} />
        } else {
            return <DynamicComponent pos={pos} hash={hash} props={dynProps}>
                <>{children}</>
            </DynamicComponent>
        }
    } else {
        return <span className="red">Unknown HTML: {JSON.stringify(html)}</span>
    }
}
