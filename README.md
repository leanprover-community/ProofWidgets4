# ProofWidgets

ProofWidgets is a library of user interface components for [Lean 4](https://leanprover.github.io/). It
supports building:
- symbolic visualizations of mathematical objects and data structures
- data visualizations
- interfaces for tactics and tactic modes
- custom visual proof modes
- custom goal state displays
- interfaces for entering or editing expressions in a domain-specific manner
- really anything that has to do with the infoview

ProofWidgets essentially supersedes [user widgets](https://leanprover.github.io/lean4/doc/examples/widgets.lean.html).
It is just as general, but more user-friendly.

Authors: Wojciech Nawrocki, E.W.Ayers with contributions from Tomáš Skřivan

# Usage

## Viewing the demos

The easiest way to get started is to clone a **release tag** of ProofWidgets and run
`lake build :release`, as follows:

```bash
# You should replace v0.0.1 with the latest version published under Releases
git clone https://github.com/EdAyers/ProofWidgets4 --branch v0.0.1
cd ProofWidgets4/
lake build :release
```

After doing this you will hopefully be able to view the demos in `ProofWidgets/Demos/`. If the demo
contains a `#html` command, put your cursors over it to see a widget in the infoview. Top tip: use
the pushpin icon to keep the widget in view. You can then live code your widgets.

## Using ProofWidgets as a dependency

Put this in your `lakefile.lean`:
```lean
-- You should replace v0.0.1 with the latest version published under Releases
require proofwidgets from git "https://github.com/EdAyers/ProofWidgets4"@"v0.0.1"
```

Note that [developing ProofWidgets](#developing-proofwidgets) involves building TypeScript code with NPM.
When depending on `ProofWidgets` but not writing any custom TypeScript yourself, you likely want to
avoid you or your users having to run NPM. To support this, ProofWidgets is configured to use Lake's
[cloud releases](https://github.com/leanprover/lake/#cloud-releases) feature which will
automatically fetch pre-built files *as long as* you require a release tag rather than our `main`
branch. This is why the snippet above does that.

## Developing ProofWidgets

You must have NPM installed (it is part of Node.js). Run `lake build` to build the TypeScript UI
code as well as all Lean modules. Run `lake build widgetJsAll` to just build the TypeScript. Outputs
of `npm` are not shown by default - use `lake build -v [widgetJsAll]` to see them.

⚠️ We use the `include_str` term elaborator to splice the JavaScript produced this way into our Lean
files. The JS is stored in `build/js/`. Note however that due to Lake issue [#86](https://github.com/leanprover/lake/issues/86),
rebuilding the `.js` will *not* rebuild the Lean file that includes it. To ensure freshness you may
have to resort to hacks like adding a random comment in the file that uses `include_str` in order to
ensure it gets rebuilt. Alternatively, you can run `lake clean` to delete the build directory.

# Features

## `json%` syntax

JSON-like syntax. Invoke with `json%`, escape with `$( _ )`

```lean
import ProofWidgets.Data.Json
open scoped ProofWidgets.Json
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
import ProofWidgets.Component.HtmlDisplay
open scoped ProofWidgets.Jsx

-- click on the line below to see it in your infoview!
#html <b>You can use HTML in lean! {toString <| 1 + 2>}</b>
```

See `ProofWidgets/Demos/Basic.lean` and `ProofWidgets/Demos/Svg.lean` for a more advanced example.

~~We also support all elements that are exposed by the [Recharts library](https://recharts.org/en-US/api),
so you can make your own plots. See `src/Demo/Plot.lean` for an example.~~ (Currently broken.)

## Custom `Expr` displays

Just like delaborators and unexpanders allow you to customize how expressions are displayed as text,
ProofWidgets allows "delaborating" into (potentially interactive) HTML. See
`ProofWidgets/Demos/Presentation.lean`.

## Animated HTML

~~As a hidden feature, you can also make animated widgets by passing an array of `Widget.Html`
objects to the `staticHtml` widget. This works particularly well with the rechart plotting library,
which eases between different plots.  You can see an example of how to do this in
`src/Demo/Plot.lean`.~~ (Currently broken.)
