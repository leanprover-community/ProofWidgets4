import ProofWidgets.Data.Svg
import ProofWidgets.Component.HtmlDisplay

open ProofWidgets Svg

private def frame : Frame where
  xmin   := 0
  ymin   := 0
  xSize  := 450
  width  := 450
  height := 450

private def github : Svg frame :=
  { elements :=
      #[
        circle (200.0, 245.0) (.px 175) |>.setFill (1.0, 1.0, 1.0),

        path "M195,25C99.3,25,20,104.3,20,200C20,276,70.4,340.9,140.8,370.2C151.9,372.2,155.4,365.4,155.4,359.4C155.4,354,155.2,338.7,155.1,320C104.7,332.6,92.9,296.4,92.9,296.4C83.9,270.7,70.5,263.9,70.5,263.9C53.4,250.3,71.8,250.5,71.8,250.5C90.8,251.8,101.2,272.8,101.2,272.8C118.1,305.3,146.8,297.2,155.8,291.4C157.7,277.2,162.7,267.4,168.2,261.9C128.5,256.3,86.7,237.4,86.7,170.8C86.7,148.4,94.3,130.2,101.6,116.2C99.4,110.5,92.7,85.9,103.9,56.8C103.9,56.8,119.7,49.7,154.7,73.9C169.4,68.3,185.2,65.4,200.9,65.2C216.6,65.4,232.5,68.3,247.2,73.9C282.1,49.7,297.9,56.8,297.9,56.8C309.1,85.9,302.4,110.5,300.2,116.2C307.5,130.2,315.1,148.4,315.1,170.8C315.1,237.6,273.2,256.2,233.3,261.7C240.4,268.5,246.7,282,246.7,302.5C246.7,332.5,246.4,352.2,246.4,359.4C246.4,365.5,249.8,372.3,261.2,370.1C331.4,340.8,381.7,276,381.7,200C381.7,104.3,302.4,25,206.7,25L195,25Z"
        |>.setFill (0.2, 0.2, 0.2),

        circle (160.0, 245.0) (.px 15) |>.setFill (0.2, 0.2, 0.2),

        circle (240.0, 245.0) (.px 15) |>.setFill (0.2, 0.2, 0.2),

        text (150.0, 340.0) "GitHub" (.px 36) |>.setFill (0.8, 0.8, 0.8)
      ]
  }

#html github.toHtml

private def triangle01 : Svg frame :=
  {elements := #[
    path "M 100 100 L 300 100 L 200 300 z"
    |>.setFill ( 1.0 , 0. , 0.)
    |>.setStroke (136., 136., 136.) (.px 3)
  ]}

#html triangle01.toHtml

private def cubic01 : Svg frame :=
  { elements :=
      #[
        polyline #[ (100.,200.), (100.,100.)]
        |>.setStroke (136., 136., 136.) (.px 2),

        polyline #[ (250.,100.), (250.,200.)]
        |>.setStroke (136., 136., 136.) (.px 2),

        polyline #[ (250.,200.), (250.,300.)]
        |>.setStroke (136., 136., 136.) (.px 2),

        polyline #[ (400.,300.), (400.,200.)]
        |>.setStroke (136., 136., 136.) (.px 2),

        path "M100,250 C100,350 250,350 250,250 S400,150 400,250"
        |>.setStroke (136., 136., 136.) (.px 2),

        circle (100.0, 200.0) (.px 10)
        |>.setStroke (136., 136., 136.) (.px 2),

        circle (250.0, 200.0) (.px 10)
        |>.setStroke (136., 136., 136.) (.px 2),

        circle (400.0, 200.0) (.px 10)
        |>.setStroke (136., 136., 136.) (.px 2),

        circle (100.0, 100.0) (.px 10)
        |>.setFill (0.8, 0.8, 0.8),

        circle (250.0, 100.0) (.px 10)
        |>.setFill (0.8, 0.8, 0.8),

        circle (400.0, 300.0) (.px 10)
        |>.setFill (0.8, 0.8, 0.8),

        circle (250.0, 300.0) (.px 9)
        |>.setStroke (0., 0., 1.) (.px 4)
      ]
  }

#html cubic01.toHtml

private def svgprims : Svg frame :=
 {elements := #[

        rect (100.0, 100.0) (.abs 70.0) (.abs 70.0)
        |>.setStroke (255., 0., 0.) (.px 2)
        |>.setFill (0.5, 0.5, 0.5),

        ellipse (200.0, 200.0) (.abs 50.0) (.abs 30.0)
        |>.setStroke (0., 255., 0.) (.px 2)
        |>.setFill (0.3, 0.3, 1.0),

        line (300.0, 300.0) (350.0, 350.0)
        |>.setStroke (255., 255., 0.) (.px 2),

        polygon #[ (100., 50.), (50., 150.), (150., 150.), (20.,70.) ]
        |>.setStroke (0., 0., 255.) (.px 2)
        |>.setFill (0., 1., 0.)
 ] }

 #html svgprims.toHtml
