import WidgetKit.HtmlWidget
import WidgetKit.Json
open Lean.Widget.Jsx -- ⟵ remember this!
open Lean Widget

def fn (t : Float) (x : Float): Float :=
   50 * (x - 0.25) * (x - 0.5) * (x - 0.7) + 0.1 * (x * 40 + t * 2 * 3.141).sin

def Plot (fn : Float → Float) (steps := 100)  : Widget.Html :=
    let jsonData : Json := List.range (steps + 1)
      |>.toArray
      |> Array.map (fun (x : Nat) => let x : Float := x.toFloat / steps.toFloat;  (x, fn x))
      |> Array.map (fun (x,y) => json% {x: $(toJson x) , y: $(toJson y)}) |> toJson;
    -- we use the recharts library which you can get the api for here: https://recharts.org/en-US/api
    <LineChart width={400} height={400} data={jsonData}>
        <XAxis domain={[0,1]} dataKey="x"/>
        <YAxis domain={[-1, 1]} allowDataOverflow={ Bool.true }/>
        <Line type="monotone" dataKey="y" stroke="#8884d8" dot={ Bool.false }/>
    </LineChart>


#html Plot (fn 0)

/-!
# Bonus demo: animated plots!
-/

def mkFrames (fn : Float → Float → Float) (steps := 100) : Array Widget.Html:=
  List.range (steps + 1) |>.toArray |>.map (fun t => Plot (fn (t.toFloat / steps.toFloat)))

-- put your cursor over the below line to see an animated widget
#widget staticHtmlWidget json% {frames : $(toJson (mkFrames fn)), framesPerSecond : 50}

