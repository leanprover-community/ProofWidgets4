import { useEffect, useRef, useState } from "react";
import { DocumentPosition } from "@leanprover/infoview"
import HtmlDisplay, { Html } from './htmlDisplay';
import * as d3 from 'd3';
import { graphviz } from 'd3-graphviz';

interface Node {
  id : string;
  pos : DocumentPosition
  html : Html;
}

interface Graph {
  nodes : Array<Node>
  dot : string
  pos : DocumentPosition
  defaultHtml : Html
}

export default ({nodes, dot, pos, defaultHtml} : Graph) => {
  const graphRef = useRef<HTMLDivElement>(null);
  const [infoState, setInfoState] =
    useState<{ pos : DocumentPosition, html : Html}>({pos : pos, html : defaultHtml});

  useEffect(() => {
    const nodeMap = new Map(nodes.map(node => [node.id, node]))
    graphviz(graphRef.current)
      .renderDot(dot)
      .onerror((e) => {
        d3.select(graphRef.current).text(e);
      })
      .on('end', () => {

        d3.select(graphRef.current).select('polygon').style("fill", "transparent");

        d3.select(graphRef.current).selectAll('text').each(function () {
          const tNode = d3.select(this);
          tNode.style("fill", "var(--vscode-editor-foreground)");
        });

        d3.selectAll<SVGAElement, unknown>(".edge").each(function () {
          const eNode = d3.select(this)
          eNode.select("path").style("stroke","var(--vscode-editor-foreground)");
          eNode.select("polygon")
            .style("stroke","var(--vscode-editor-foreground)")
            .style("fill","var(--vscode-editor-foreground)");
        });

        d3.selectAll<SVGAElement, unknown>(".node").each(function () {
          const gNode = d3.select(this);

          gNode.attr("pointer-events", "fill");

          gNode.on("click", function(event) {
            event.stopPropagation();
            const nodeId = d3.select(this).attr("id");
            const node = nodeMap.get(nodeId);

            if (node) {
              setInfoState({ pos : node.pos, html : node.html });
            };
          });
        });
        d3.select(graphRef.current)
          .on("click", () => {
            setInfoState({ pos : pos, html : defaultHtml });
          });

      });
  }, [nodes, dot, pos, defaultHtml]);

  return (
    <div>
      <div ref={graphRef} />
      <HtmlDisplay pos={infoState.pos} html={infoState.html} />
    </div>
  );
}
