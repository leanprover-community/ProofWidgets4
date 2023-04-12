import * as React from 'react';
import Tree from 'react-d3-tree';
import { CodeWithInfos, DocumentPosition, InteractiveCode } from '@leanprover/infoview';
import type { RawNodeDatum, CustomNodeElementProps } from 'react-d3-tree/lib/types/types/common';

type RBTreeVarsColour =
  'red' | 'black' | 'blue'

type RBTreeVars =
  'empty' |
  { var: CodeWithInfos } |
  { node: { color: RBTreeVarsColour, l: RBTreeVars, a: CodeWithInfos, r: RBTreeVars } }

type RBNodeDatum = RawNodeDatum & { expr?: CodeWithInfos, color?: string }

/** Turn {@link RBTreeVars} into a representation `react-d3-tree` can use. */
function treeToData(tree: RBTreeVars): RBNodeDatum {
  // The `name`s are just placeholders. They are not rendered
  if ('empty' === tree) {
    return {
      name: 'empty',
      color: 'black',
    }
  } else if ('var' in tree) {
    return {
      name: 'var',
      expr: tree.var,
    }
  } else if ('node' in tree) {
    const { color, l, a, r } = tree.node
    return {
      name: 'node',
      expr: a,
      color,
      children: [
        treeToData(l),
        treeToData(r)
      ]
    }
  }
  throw new Error(`unknown RBTreeVars constructor '${JSON.stringify(tree)}'`)
}

function renderForeignObjectNode({ nodeDatum }: CustomNodeElementProps, pos: DocumentPosition,
    foreignObjectProps: React.SVGProps<SVGForeignObjectElement>): JSX.Element {
  const nodeDatum_ = nodeDatum as RBNodeDatum
  return (
    <g>
      <circle r={15} stroke={nodeDatum_.color ?? 'white'} fill='white' />
      <foreignObject {...foreignObjectProps}>
        {nodeDatum_.expr && <InteractiveCode fmt={nodeDatum_.expr} />}
      </foreignObject>
    </g>
  )
}

export default function({pos, tree}: {pos: DocumentPosition, tree: RBTreeVars}) {
  const nodeSize = { x: 40, y: 80 }
  const foreignObjectProps = { width: nodeSize.x, height: nodeSize.y, y: -10, x: -4 }
  return (
    <div
      style={{
        height: '200px',
        display: 'inline-flex',
        minWidth: '50px'
      }}
    >
      <Tree
        data={treeToData(tree)}
        nodeSize={nodeSize}
        renderCustomNodeElement={rd3tProps =>
          renderForeignObjectNode(rd3tProps, pos, foreignObjectProps)}
        orientation='vertical' />
    </div>
  )
}
