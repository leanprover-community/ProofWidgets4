import * as React from 'react';
import { Html, default as HtmlDisplay } from './htmlDisplay';
import { DocumentPosition } from '@leanprover/infoview/*';

interface AnimatedHtmlProps {
    pos: DocumentPosition
    frames: Html[]
    framesPerSecond?: number
}

/** Display the series of HTML frames as a time-dependent animation. This will only look reasonable
 * if the frames are related somehow, e.g. are all instances of a single component with slightly
 * different props. */
export default function AnimatedHtml(props: AnimatedHtmlProps) {
    const { pos, frames } = props
    const framesPerSecond = props.framesPerSecond ?? 10
    if (framesPerSecond <= 0 || framesPerSecond > 60) {
        throw new Error(`Invalid fps ${framesPerSecond}. Should be between 0 and 60.`)
    }
    const [t, setT] = React.useState(0)
    React.useEffect(() => {
        const interval = window.setInterval(() => setT(t => t + 1), 1000.0 / framesPerSecond)
        return () => window.clearInterval(interval)
    }, [framesPerSecond])

    const frame = frames[t % frames.length]

    return <HtmlDisplay pos={pos} html={frame} />
}
