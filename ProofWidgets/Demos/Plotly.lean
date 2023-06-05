import ProofWidgets.Component.HtmlDisplay
import ProofWidgets.Component.Plotly

open scoped ProofWidgets.Jsx ProofWidgets.Json

#plot {
    data: [{
      x: [1, 2, 3],
      y: [4, 24, 3],
      type: "scatter",
      marker: { color: "red" }
    }]
  }

#plot {
  data: [{
    x: [1, 1, 3, 4, 4, 4, 5, 1, 2, 2, 3, 3, 5],
    type: "histogram"
  }],
  layout: {
    title: "A sample histogram"
  }
}
