import ProofWidgets.Component.HtmlDisplay
import ProofWidgets.Component.Recharts

open Lean ProofWidgets Recharts

def fn (t : Float) (x : Float): Float :=
   50 * (x - 0.25) * (x - 0.5) * (x - 0.7) + 0.1 * (x * 40 - t * 2 * 3.141).sin

open scoped ProofWidgets.Jsx in
open scoped ProofWidgets.Json in
def Plot (fn : Float → Float) (steps := 100) : THtml :=
  let jsonData : Array Json :=
    Nat.fold (flip Array.push) (steps + 1) #[]
    |> Array.map (fun (x : Nat) => let x : Float := x.toFloat / steps.toFloat;  (x, fn x))
    |> Array.map (fun (x,y) => json% {x: $(toJson x) , y: $(toJson y)});
  <LineChart width={400} height={400} data={jsonData}>
    <XAxis domain?={#[toJson 0, toJson 1]} dataKey?="x" />
    <YAxis domain?={#[toJson (-1), toJson 1]} allowDataOverflow={Bool.false} />
    <Line type={.monotone} dataKey="y" stroke="#8884d8" dot?={Bool.false} />
  </LineChart>

#html Plot (fn 0)
#html Plot (fn 0.2)
#html Plot (fn 0.4)
#html Plot (fn 0.6)

/-!
# Bonus demo: animated plots!
-/

def mkFrames (fn : Float → Float → Float) (steps := 100) : Array THtml:=
  List.range (steps + 1) |>.toArray |>.map (fun t => Plot (fn (t.toFloat / steps.toFloat)))

structure AnimatedHtmlProps where
  frames : Array Html
  framesPerSecond? : Option Nat := none

#mkrpcenc AnimatedHtmlProps

@[widget_module]
def AnimatedHtml : Component AnimatedHtmlProps where
  javascript := include_str "../../build/js/animatedHtml.js"

open scoped ProofWidgets.Jsx in
-- put your cursor on the below line to see an animated widget
#html <AnimatedHtml frames={mkFrames fn |>.map Html.ofTHtml} framesPerSecond?={some 60} />
