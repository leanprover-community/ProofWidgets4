import * as React from 'react';

type HtmlAttribute = [string, string]

type Html =
    | {element: [string, HtmlAttribute[], Html[]]}
    | {text : string}

export default function StaticHtml({html, ...props} : {html: Html}) : React.ReactNode {
    if ('text' in html) {
        return html.text
    } else if ('element' in html) {
        const [tag, attrsList, children] = html.element
        const attrs : any = {}
        for (const [k,v] of attrsList) {
            attrs[k] = v
        }
        return React.createElement(tag, attrs, children.map(html => StaticHtml({html})))
    } else {
        throw new Error(`Unexpected ${html}`)
    }
}
