import * as React from 'react';
import HtmlDisplay, { Html } from './htmlDisplay';
import { DocumentPosition } from '@leanprover/infoview/*';

interface FilterDetailsProps {
    pos : DocumentPosition
    summary : Html
    filtered : Html
    all : Html
    initiallyFiltered : boolean
}

export default function FilterDetails(props: FilterDetailsProps) {
    const [isFiltered, setFiltered] = React.useState(props.initiallyFiltered);

    return <details open>
        <summary className="mv2 pointer">
            <HtmlDisplay pos={props.pos} html={props.summary} />
            <span className="fr" onClick={e => { e.preventDefault() }}>
                <a className="link pointer mh2 dim codicon codicon-filter" title="filter"
                    onClick={_ => { setFiltered(s => !s) }} />
            </span>
        </summary>
        <HtmlDisplay pos={props.pos} html={isFiltered ? props.filtered : props.all}/>
    </details>
}
