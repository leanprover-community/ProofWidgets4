import { InteractiveGoal, InteractiveGoals, InteractiveHypothesisBundle_nonAnonymousNames, TaggedText_stripTags } from "@leanprover/infoview";
import { useEffect } from "react";


/* Remove `ggbScript` inside `[]` at line 44 to render `ggbDiagram` statically */

function ggbDiagram(ggbScript: string) {
  useEffect(() => {
    const script = document.createElement("script");
    script.src = "https://cdn.geogebra.org/apps/deployggb.js";
    script.async = true;
    script.onload = function() {
      (window as any).ggbOnInit = function() {
        (window as any).ggbApplet.evalCommand(ggbScript);
      };

      const parameters = {
        "id": "ggbApplet",
        "appName": "geometry",
        "width": 800,
        "height": 600,
        "showToolBar": true,
        "borderColor": null,
        "showMenuBar": true,
        "allowStyleBar": true,
        "showAlgebraInput": true,
        "enableLabelDrags": true,
        "enableShiftDragZoom": true,
        "capturingThreshold": 3,
        "showToolBarHelp": false,
        "errorDialogsActive": true,
        "showZoomButtons": true,
        "showLogging": true,
        "useBrowserForJS": true,
        "showResetIcon": true
      };

      const applet = new (window as any).GGBApplet(parameters, true);
      applet.inject("ggb-window");
    };

    document.body.appendChild(script);

  }, [ggbScript]);

  return <div id="ggb-window" />;
}


function preprocessInput(str: string): string {
  return str.replace(/₁/g, "_1")
            .replace(/₂/g, "_2")
            .replace(/₃/g, "_3")
            .replace(/₄/g, "_4")
            .replace(/₅/g, "_5")
            .replace(/₆/g, "_6")
            .replace(/₇/g, "_7")
            .replace(/₈/g, "_8")
            .replace(/₉/g, "_9")
            .replace(/✝/g, "");
}

function toGbbScript(codeBlockStr: string): string {
  const codeBlock = preprocessInput(codeBlockStr).split("\n");
  let newCode = "ZoomIn(3);\n";

  const onLineObj: { [key: string]: string[] } = {};

  for (const line of codeBlock) {
    if (line.includes(": Point")) {
      const points = line.trim().split(" : Point")[0].split(/\s+/);
      for (const point of points) {
        const cleanPoint = preprocessInput(point.replace(/[()]/g, ""));
        newCode += `${cleanPoint} = Point({random(), random()});\n`;
      }
    }
    else if (line.includes("between ")) {
      const splitLine = line.split("between ");
      if (splitLine.length > 1) {
        const points = splitLine[1].trim().split(/\s+/);
        const middlePoint = preprocessInput(points[1]);
        newCode += `Segment(${preprocessInput(points[0])}, ${preprocessInput(points[2])});\n${middlePoint} = Point(Segment(${preprocessInput(points[0])}, ${preprocessInput(points[2])}));\n${middlePoint} = SetColor(${middlePoint}, Blue);\n`;
      }
    }
    else if (line.includes("onLine ")) {
      const splitLine = line.split("onLine ");
      if (splitLine.length > 1) {
        const points = splitLine[1].trim().split(/\s+/);
        if (points.length === 2) {
          const a = preprocessInput(points[0]);
          const b = preprocessInput(points[1]);
          if (onLineObj[b]) {
            onLineObj[b].push(a);
          } else {
            onLineObj[b] = [a];
          }
        }
      }
    }
    else if (line.includes("≠")) {
      const splitLine = line.split(":");
      if (splitLine.length > 1) {
        const points = splitLine[1].trim().split("≠");
        if (points.length === 2) {
          const point1 = preprocessInput(points[0].trim());
          const point2 = preprocessInput(points[1].trim());
          newCode += `${point1} = SetColor(${point1}, Red);\n${point2} = SetColor(${point2}, Red);\n`;
        }
      }
    }
  }

  for (const b in onLineObj) {
    const aValues = onLineObj[b];
    if (aValues.length === 1) {
      newCode += `${b} = Line(${aValues[0]}, Point({random(), random()}));\n`;
    } else {
      newCode += `${b} = Line(${aValues[0]}, ${aValues[1]});\nShowLabel(${b}, true);\n`;
    }
  }

  return newCode;
}


/*
  The following code is copied from source:
  File: lean4-infoview/src/infoview/goals.tsx
  Repo: https://github.com/leanprover/vscode-lean4
  License: Apache 2.0
*/

function goalToString(g: InteractiveGoal): string {
  let ret = ''

  if (g.userName) {
      ret += `case ${g.userName}\n`
  }

  for (const h of g.hyps) {
      const names = InteractiveHypothesisBundle_nonAnonymousNames(h).join(' ')
      ret += `${names} : ${TaggedText_stripTags(h.type)}`
      if (h.val) {
          ret += ` := ${TaggedText_stripTags(h.val)}`
      }
      ret += '\n'
  }

  ret += `⊢ ${TaggedText_stripTags(g.type)}`

  return ret
}

function goalsToString(goals: InteractiveGoals): string {
  return goals.goals.map(goalToString).join('\n\n')
}

/* End of copied code */


/*
  Remove
  ```
  <hr></hr>
  <h3>Geogebra Script:</h3>
  <pre><code>{toGbbScript(goalsToString(goals))}</code></pre>
  ```
  if you don't want to see the current input ggbScript for ggbDiagram in the infoview
*/

export default function (goals: InteractiveGoals) {
  return (
    <details open>
      <summary className="mv2 pointer">Geogebra window</summary>
      <>{ggbDiagram(toGbbScript(goalsToString(goals)))}</>
      <hr></hr>
      <h3>Geogebra Script:</h3>
      <pre><code>{toGbbScript(goalsToString(goals))}</code></pre>
    </details>
  );
}
