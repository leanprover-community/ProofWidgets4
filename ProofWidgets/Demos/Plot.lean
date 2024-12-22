import ProofWidgets.Component.HtmlDisplay
import ProofWidgets.Component.Recharts

open Lean ProofWidgets Recharts

def fn (t : Float) (x : Float): Float :=
   50 * (x - 0.25) * (x - 0.5) * (x - 0.7) + 0.1 * (x * 40 - t * 2 * 3.141).sin

/- After defining a sequence of points from x=0.0 to x=101.0, shift the right
   half of the curve slightly to the right in order to demonstrate the effect
   of changing the axis type.  In XAxis below, change `type={.number}` (the
   ProofWidgets default) to `type={.category}`, which causes intervals between
   x values to be treated as equal no matter how large x₂-x₁ is. -/
open scoped ProofWidgets.Jsx in
def Plot (fn : Float → Float) (steps := 100) : Html :=
  let fndata : Array (Float × Float) :=
    Array.range (steps + 1)
    |> Array.map (fun (x : Nat) => let x : Float := x.toFloat / steps.toFloat;  (x, fn x))
  -- Introduce a gap in the x values to illustrate what axis type does:
  let fndata0 : Array (Float × Float) := fndata[:50]
  let fndata1 : Array (Float × Float) :=
    fndata[50:] |> Array.map (fun (x, y) => (x + 0.1, y))
  let data : Array (Float × Float) := fndata0 ++ fndata1
  let jsonData : Array Json :=
    data |> Array.map (fun (x,y) => json% {x: $(toJson x) , y: $(toJson y)});
  <LineChart width={400} height={400} data={jsonData}>
    <XAxis domain?={#[toJson 0, toJson 1]} dataKey?="x" type={.number} />
    <YAxis domain?={#[toJson (-1), toJson 1]} allowDataOverflow={Bool.false} />
    <Line type={.monotone} dataKey="y" stroke="#8884d8" dot?={Bool.true} />
  </LineChart>

#html Plot (fn 0)
#html Plot (fn 0.2)
#html Plot (fn 0.4)
#html Plot (fn 0.6)

/-!
# Bonus demo: animated plots!
-/

def mkFrames (fn : Float → Float → Float) (steps := 100) : Array Html:=
  List.range (steps + 1) |>.toArray |>.map (fun t => Plot (fn (t.toFloat / steps.toFloat)))

structure AnimatedHtmlProps where
  frames : Array Html
  framesPerSecond? : Option Nat := none
  deriving Server.RpcEncodable

@[widget_module]
def AnimatedHtml : Component AnimatedHtmlProps where
  javascript := include_str ".." / ".." / ".lake" / "build" / "js" / "animatedHtml.js"

open scoped ProofWidgets.Jsx in
-- put your cursor on the below line to see an animated widget
#html <AnimatedHtml frames={mkFrames fn} framesPerSecond?={some 60} />
