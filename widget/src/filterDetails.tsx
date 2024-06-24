import * as React from 'react';
import HtmlDisplay, { Html } from './htmlDisplay';

interface FilterDetailsProps {
    summary : Html
    filtered : Html
    all : Html
    initiallyFiltered : boolean
}

export default function FilterDetails(props: FilterDetailsProps) {
    const [isFiltered, setFiltered] = React.useState(props.initiallyFiltered);

    return <details open>
        <summary className="mv2 pointer">
            <HtmlDisplay html={props.summary} />
            <span className="fr" onClick={e => { e.preventDefault() }}>
                <a className={"link pointer mh2 dim codicon " +
                    (isFiltered ? "codicon-filter-filled " : "codicon-filter ")}
                    title={isFiltered ? "Show more content" : "Show less content"}
                    onClick={_ => { setFiltered(s => !s) }} />
            </span>
        </summary>
        <HtmlDisplay html={isFiltered ? props.filtered : props.all}/>
    </details>
}
