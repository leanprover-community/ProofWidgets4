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

## JSX-like syntax

```lean
import WidgetKit.HtmlWidget
open Lean.Widget.Jsx

-- click on the line below to see it in your infoview!
#html <b>You can use HTML in lean! {toString <| 1 + 2>}</b>
```

We also support all elements that are exposed by the [Recharts library](https://recharts.org/en-US/api), so you can make your own plots. See `src/Demo/Plot.lean` for an example.

## Animated html

As a hidden feature, you can also make animated widgets by passing an array of `Widget.Html` objects to the `staticHtml` widget. This works particularly well with the rechart plotting library, which eases between different plots.
You can see an example of how to do this in `src/Demo/Plot.lean`