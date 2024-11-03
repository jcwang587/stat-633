# 3D charts -----

## Markers -----

# By adding a `z` attribute `plot_ly()` automatically renders markers,
# lines, and paths in three dimensions. 

# All the techniques we learned earlier can be re-used for 3D charts:

suppressMessages(library(plotly))
suppressMessages(library(tidyverse))
plot_ly(mpg, x = ~cty, y = ~hwy, z = ~cyl) %>%
  add_markers(color = ~cyl)

# Exercise:
# The RANDU generator for sampling random independent integers is based on the recursion:
#     X_(n+1) = 65539 * X_n mod 2^31
  
# Use several 2D plots to demonstrate that dependency is not very evident
# in such a sequence.

# Use a 3D plot to illustrate the undesirable behavior of this generator.

# Answer:



## Paths -----

# To make a path in 3D, use `add_paths()` in the same way you would for a 2D path,
# but add a third variable `z`.

plot_ly(mpg, x = ~cty, y = ~hwy, z = ~cyl) %>%
  add_paths(color = ~displ)

## Lines -----

# We can use `add_lines()` instead of `add_paths()` to ensure the points are connected
# by the x axis instead of the row ordering.

plot_ly(mpg, x = ~cty, y = ~hwy, z = ~cyl) %>%
  add_lines(color = ~displ)


# As with non-3D lines, you can make multiple lines by specifying a grouping variable.

plot_ly(mpg, x = ~cty, y = ~hwy, z = ~cyl) %>%
  group_by(cyl) %>%
  add_lines(color = ~displ)


## Axes -----

# For 3D plots, the axis objects are a part of the [`scene`](https://plot.ly/r/reference/#layout-scene)
# definition, which is part of the `layout()`. 

# That is, if you wanted to set axis titles or something else specific to the axis definition,
# the relation between axes (i.e., [`aspectratio`](https://plot.ly/r/reference/#layout-scene-aspectratio)),
# or the default setting of the camera (i.e., [`camera`](https://plot.ly/r/reference/#layout-scene-camera));
# you would do so via the `scence`.

plot_ly(mpg, x = ~cty, y = ~hwy, z = ~cyl) %>%
  add_lines(color = ~displ) %>%
  layout(
    scene = list(
      xaxis = list(title = "MPG city"),
      yaxis = list(title = "MPG highway"),
      zaxis = list(title = "Number of cylinders")
    )
  )

# Exercise:
# Modify the labels for the 3D plot from the previous exercise to be more instructive

# Answer:




## Surfaces -----

# Creating 3D surfaces with `add_surface()` is a lot like creating heatmaps with
# `add_heatmap()`. In fact, you can even create 3D surfaces over categorical x/y
# (try changing `add_heatmap()` to `add_surface()`)!

# That being said, there should be a sensible ordering to the x/y axes in a surface
# plot since plotly.js interpolates z values. 

# Usually the 3D surface is over a continuous region, e.g. to display the height of a
# volcano. If a numeric matrix is provided to z, the x and y attributes do not have to
# be provided, but if they are, the length of x should match the number of columns in
# the matrix and y should match the number of rows.

x <- seq_len(nrow(volcano)) + 100
y <- seq_len(ncol(volcano)) + 500
plot_ly() %>% add_surface(x = ~x, y = ~y, z = ~volcano)
