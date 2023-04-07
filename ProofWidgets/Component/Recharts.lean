import ProofWidgets.Component.Basic

namespace ProofWidgets.Recharts
open Lean

@[widget_module]
def Recharts : Module where
  javascript := include_str "../../build/js/recharts.js"

inductive LineChartLayout where
  | horizontal
  | vertical
  deriving FromJson, ToJson

inductive LineChartSyncMethod where
  | index | value
  deriving FromJson, ToJson

structure LineChartMargin where
  top : Nat := 5
  right : Nat := 5
  bottom : Nat := 5
  left : Nat := 5
  deriving FromJson, ToJson

structure LineChartProps where
  layout : LineChartLayout := .horizontal
  syncId? : Option String := none
  syncMethod? : Option LineChartSyncMethod := some .index
  width : Nat
  height : Nat
  data : Array Json
  margin : LineChartMargin := {}
  deriving FromJson, ToJson

/-- See https://recharts.org/en-US/api/LineChart. -/
def LineChart : Component LineChartProps where
  javascript := Recharts.javascript
  «export» := "LineChart"

structure AxisProps where
  dataKey? : Option Json := none
  domain? : Option (Array Json) := none
  allowDataOverflow : Bool := false
  -- TODO: There are many more props
  deriving FromJson, ToJson

/-- See https://recharts.org/en-US/api/XAxis. -/
def XAxis : Component AxisProps where
  javascript := Recharts.javascript
  «export» := "XAxis"

/-- See https://recharts.org/en-US/api/YAxis. -/
def YAxis : Component AxisProps where
  javascript := Recharts.javascript
  «export» := "YAxis"

inductive LineType where
  | basis | basisClosed | basisOpen | linear | linearClosed | natural | monotoneX | monotoneY
  | monotone | step | stepBefore | stepAfter
  deriving FromJson, ToJson

structure LineProps where
  type : LineType := .linear
  dataKey : Json
  stroke : String
  dot? : Option Bool := none
  -- TODO: There are many more props
  deriving FromJson, ToJson

/-- See https://recharts.org/en-US/api/Line. -/
def Line : Component LineProps where
  javascript := Recharts.javascript
  «export» := "Line"

end ProofWidgets.Recharts
