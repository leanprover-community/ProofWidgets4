# Development kit for Lean 4 widgets

Authors: Wojciech Nawrocki, E.W.Ayers

# Usage

## Building the demos:

```
cd widget
npm i
cd ..
lake build widgets
lake build
```

Now go to the demo folder in VSCode.
Putting your cursor on any `#widget` or `#html` will show you a widget in the infoview. Top tip: use the pushpin icon to keep the widget in view. You can then live code your widgets.

# Features

## `json%` syntax

JSON-like syntax. Invoke with `json%`, escape with `$( _ )`

```lean
#eval json% {
  hello : "world",
  cheese : ["edam", "cheddar", {kind : "spicy", rank : 100.2}],
  lemonCount : 100e30,
  isCool : true,
  isBug : null,
  lookACalc: $(23 + 54 * 2)
}
```

## html syntax

```lean
import WidgetKit.HtmlWidget
open Lean.Widget.Jsx

#html <b>You can use HTML in lean!</b>
```

# Development

- `lake build widgets`
- `lake build`