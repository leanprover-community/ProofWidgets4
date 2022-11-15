import * as React from 'react';
import * as Recharts from 'recharts'

const htmlTags = "a abbr address area article aside audio b base bdi bdo big blockquote body br button canvas caption cite code col colgroup data datalist dd del details dfn dialog div dl dt em embed fieldset figcaption figure footer form h1 h2 h3 h4 h5 h6 head header hr html i iframe img input ins kbd keygen label legend li link main map mark menu menuitem meta meter nav noscript object ol optgroup option output p param picture pre progress q rp rt ruby s samp script section select small source span strong style sub summary sup table tbody td textarea tfoot th thead time title tr track u ul var video wbr".split(' ')
const svgTags = "circle clipPath defs ellipse g line linearGradient mask path pattern polygon polyline radialGradient rect stop svg text tspan".split(' ')

type HtmlAttribute = [string, any]

export type Html =
    | {element: [string, HtmlAttribute[], Html[]]}
    | {text : string}

export function StaticHtml({html, ...props} : {html: Html}) : React.ReactNode {
    if ('text' in html) {
        return html.text
    } else if ('element' in html) {
        const [tag, attrsList, cs] = html.element
        const attrs : any = {}
        for (const [k,v] of attrsList) {
            attrs[k] = v
        }
        const children = cs.map(html => StaticHtml({html}))
        if (tag === "hr") {
            // React is greatly concerned by <hr/>s having children.
            return <hr/>
        }
        if (htmlTags.includes(tag) || svgTags.includes(tag)) {
            return React.createElement(tag, attrs, children)
        } else if (tag in Recharts) {
            const component = (Recharts as any)[tag]
            return React.createElement(component as any, attrs, children)
        }
        return <span className="red">Unknown component {tag}</span>
    } else {
        throw new Error(`Unexpected ${html}`)
    }
}

/** Quick way of creating time-dependent animations from widgets.
 * Eventually this will get replaced with fancy RPC stuff to stream frames.
 */
export function AnimatedHtml(props : {frames : Html[], framesPerSecond? : number}) {
    const {frames} = props
    const framesPerSecond = props.framesPerSecond ?? 10
    if (framesPerSecond <= 0 || framesPerSecond > 60) {
        throw new Error(`Invalid fps ${framesPerSecond}. Should be between 0 and 60.`)
    }
    const [t, setT] = React.useState(0)
    React.useEffect(() => {
        window.setTimeout(() => setT(t + 1), 1000.0 / framesPerSecond)
    }, [t]);

    const frame = frames[t % frames.length]

    return StaticHtml({html : frame})
}

export default function(props : any) {
    if ("frames" in props) {
        return AnimatedHtml(props)
    } else {
        return StaticHtml(props)
    }
}