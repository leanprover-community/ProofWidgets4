import * as React from 'react'
import { EditorContext } from '@leanprover/infoview'
import { Position, TextDocumentEdit } from 'vscode-languageserver-protocol'

interface MakeEditLinkProps {
  edit : TextDocumentEdit
  newCursorPos? : Position
  title? : string
}

export default function(props: React.PropsWithChildren<MakeEditLinkProps>) {
  const ec = React.useContext(EditorContext)

  return <a className='link pointer dim ' title={props.title ?? ''}
      onClick={async () => {
        await ec.api.applyEdit({ documentChanges: [props.edit] })
        // TODO: https://github.com/leanprover/vscode-lean4/issues/225
        if (props.newCursorPos)
          await ec.revealPosition({ ...props.newCursorPos, uri: props.edit.textDocument.uri })
      }}
    >
      {props.children}
    </a>
}
