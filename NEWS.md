# mosaicCalc 

# mosaicCalc 0.6.5

* Updated contour_plot() to work with new ggplot2 syntax.

# mosaicCalc 0.6.4

* Set up to work with WebR.
* Integrate() no longer uses hcubature() from cubature package.

# mosaicCalc 0.6.2

* Fixed bug that caused the dt argument in makeODE() and integrateODE() to be ignored in favor of the default value.

# mosaicCalc 0.6.1

* No new user features. Just maintenance on internal package tests to keep up with changes to other packages.

# mosaicCalc 0.6.0

* Updated vignette and added two new vignettes: an instructor-facing guide and a quick reference.
* Unified argument handling system across most functions, handling optional graphics input from piped-in graphical functions, bounds(), parameter values, etc.
  
# mosaicCalc 0.5.9

* slice_plot(), contour_plot(), vectorfield_plot(), and gradient_plot() have improved checking for unbound parameters.
* Added ability to bind or re-bind parameters in those graphics functions calls.
* Exported bind_params(), perhaps useful for demonstrations of difference between arguments and parameters in mathematical functions.
  
# mosaicCalc 0.5.8

* Using Deriv::Deriv() for symbolic differentiation. It's much better than stats:D
* Attempting symbolic integration with Ryacas. If that fails, then go on to old method. symbolic anti-derivatives are much better with Ryacas than the old home-brew method.
  
# mosaicCalc 0.5.7

* traj_plot_3D() added
* integrateODE() has improved error messages.
   
  
# mosaicCalc 0.5.6

* vectorfield_plot() gets transform feature to (optionally) avoid too-small arrows. 
* vectorfield_plot() arrows now centered on the gridpoint.
  

# mosaicCalc 0.5.5

* added more domain features: is_in_domain() and %pm% which works inside domain().
* added Zeros(), which works better with the domain system.
* added Integrate(), argM()
* added Picket()
      
# mosaicCalc 0.5.4

* added Iterate()
  
  
# mosaicCalc 0.5.3

* fixed label colors and alpha in `slice_plot()`
* contour_plot() now uses boxed labels instead of plain text for contour marking 
* slice_plot() adds a "singularities" argument to insert breaks in the graph. 
* more flexible domain specification for slice_plot and contour_plot
* can use expressions instead of formulas for makeFun()
* bug fixes in antiD()
* somewhat better algebraic simplification for D() and antiD()

# mosaicCalc 0.5.2

* added `surface_with_contours()`.
* added `vectorfield_plot()` and `gradient_plot()`
 
 
# mosaicCalc 0.5.1

 * Added cross-references to calculus functions in `mosaic`
 * Internal functions no longer show up in package documentation.
 * Added a vignette oriented toward instructors.
 * Fixed reversal of axes in `interactive_plot()`

# mosaicCalc 0.5.0

 * Calculus materials separated from the `mosaic` package and housed here.
 * Reconfigured `mosaicCalc` as a separate package that works with the `mosaicCore` ecosystem.
 



