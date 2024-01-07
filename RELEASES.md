# ProofWidgets4 releases

This file contains work-in-progress notes for the upcoming release, as well as previous releases.
Please check the [releases](https://github.com/leanprover-community/ProofWidgets4/releases) page for the build artifacts.

v0.0.26 (development in progress)
-------

v0.0.25
-------

* Build the demos in CI.

v0.0.24
-------

* Moved the toolchain to `leanprover/lean4:v4.5.0-rc1`.
  This brings changes to the user widget API described [here](https://github.com/leanprover/lean4/blob/master/RELEASES.md#v450).
* **Removed** `ProofWidgets.savePanelWidgetInfo`. For now you should use `Lean.Widget.savePanelWidgetInfo` instead.
  An example migration can be found [here](https://github.com/leanprover-community/ProofWidgets4/compare/v0.0.23..v0.0.24#diff-c48bcbf1b4d226947726f7a0fe8c945f082f4195b34681638ca61a776bbf778eL49-R52).
* The `with_panel_widgets` tactic now optionally accepts props for each listed widget.
* Several components now use `mk_rpc_widget%` instead of JavaScript string literals.
* Fixes and improvements in the `PenroseDiagram` component and the `Euclidean` demo.
