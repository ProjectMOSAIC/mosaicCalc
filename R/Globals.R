# For checking package build
#
# Making some variables as "global" to clean up
# the package check. 
# countour_plot: .output.
# slice_plot: .output.
# vectorfield_plot: x_start, x_end, y_start, y_end
# 
utils::globalVariables(c(
  ".output.", "x_start", "y_start", "x_end", "y_end"
))
