# mosaicCalc package NEWS

## mosaicCalc 0.5.0

 * Calculus materials separated from the `mosaic` package and housed here.
 * Reconfigured `mosaicCalc` as a separate package that works with the `mosaicCore` ecosystem.
 
## mosaicCalc 0.5.1

 * Added cross-references to calculus functions in `mosaic`
 * Internal functions no longer show up in package documentation.
 * Added a vignette oriented toward instructors.
 * Fixed reversal of axes in `interactive_plot()`

## mosaicCalc 0.5.2

  * added `surface_with_contours()`.
  * added `vectorfield_plot()` and `gradient_plot()`
  
## mosaicCalc 0.5.3

  * fixed label colors and alpha in `slice_plot()`
  * contour_plot() now uses boxed labels instead of plain text for contour marking 
  * slice_plot() adds a "singularities" argument to insert breaks in the graph. 
  * more flexible domain specification for slice_plot and contour_plot
  * can use expressions instead of formulas for makeFun()
  * bug fixes in antiD()
  * somewhat better algebraic simplification for D() and antiD()
  
## mosaicCalc 0.5.4

  * added Iterate()
  
  
## mosaicCalc 0.5.5

  * added more domain features: is_in_domain() and %pm% which works inside domain().
  * added Zeros(), which works better with the domain system.
  * added Integrate(), argM()
  * added Picket()
  
## mosaicCalc 0.5.6

  * added transform feature to vectorfield_plot() and centered arrows on the gridpoint
  
## mosaicCalc 0.5.7

  * added traj_plot_3D()
  * better error messages in integrateODE()
  
## mosaicCalc 0.5.8

  * Using Deriv::Deriv() for symbolic differentiation. It's much better than stats:D
  * Attempting symbolic integration with Ryacas. If that fails, then go on to old method. symbolic anti-derivatives are much better with Ryacas than the old home-brew method.
  
## mosaicCalc 0.5.9

  * Improved checking for unbound parameters in graphics functions slice_plot(), contour_plot(), vectorfield_plot(), and gradient_plot().
  * Added ability to bind or re-bind parameters in those graphics functions calls.
  * Exported bind_params(), perhaps useful for demonstrations of difference between arguments and parameters in mathematical functions.
  
  