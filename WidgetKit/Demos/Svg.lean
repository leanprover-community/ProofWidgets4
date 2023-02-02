import WidgetKit.Data.Svg
import WidgetKit.Component.HtmlDisplay

open WidgetKit Svg

private def frame : Frame where
  xmin   := -2
  ymin   := -2
  xSize  := 4
  width  := 400
  height := 400

private def svg : Svg frame :=
  { elements :=
      #[line (0.,0.) (1.,0.) |>.setStroke (1.,0.,0.) (.px 2),
        line (1.,0.) (0.,1.) |>.setStroke (0.,1.,0.) (.px 2),
        line (0.,1.) (0.,0.) |>.setStroke (0.,0.,1.) (.px 2),
        circle (0.,0.) (.abs 0.1) |>.setStroke (0.,0.,0.) (.px 2) |>.setFill (0.,1.,1.) |>.setId "point1",
        circle (1.,0.) (.abs 0.1) |>.setStroke (0.,0.,0.) (.px 2) |>.setFill (1.,0.,1.) |>.setId "point2",
        circle (0.,1.) (.abs 0.1) |>.setStroke (0.,0.,0.) (.px 2) |>.setFill (1.,1.,0.) |>.setId "point3"] }

-- #eval toJson svg.toHtml

#html svg.toHtml
