# Arranging views -----

# One technique essential to high-dimensional data visualization is the ability
# to arrange multiple views. 

# By arranging multiple low-dimensional graphics of the same (or similar)
# high-dimensional data, one can put local summaries and patterns into a global
# context. 

# Ideally, when displaying multiple related data views, they are linked through
# an underlying data source to foster comparisons and enable posing of data
# queries. 

## Arranging plotly objects -----

# The `subplot()` function provides a flexible interface for merging multiple
# **plotly** objects into a single object. 

# Its capabilities and interface are similar to the `grid.arrange()` function
# from the **gridExtra** package, which allows you to arrange multiple **grid**
# grobs in a single view.

# The most simple way to use `subplot()` is to directly supply plotly objects:

library(plotly)
p1 <- plot_ly(economics, x = ~date, y = ~unemploy) %>% 
  add_lines(name = "unemploy")
p2 <- plot_ly(economics, x = ~date, y = ~uempmed) %>% 
  add_lines(name = "uempmed")
subplot(p1, p2)

# Although `subplot()` accepts an arbitrary number of plot objects, passing a
# _list_ of plots can save typing and redundant code when dealing with a large
# number of plots. 

# The following shows one time series for each variable in the `economics` dataset
# and shares the x-axis so that zoom/pan events are synchronized across each series:

vars <- setdiff(names(economics), "date")
plots <- 
  lapply(vars,
         function(var) {
           plot_ly(economics, x = ~date, y = as.formula(paste0("~", var))) %>%
             add_lines(name = var)
           }
         )
subplot(plots, nrows = length(plots), shareX = TRUE, titleX = FALSE)

# Conceptually, `subplot()` provides a way to place a collection of plots into
# a table with a given number of rows and columns. 

# By default, each row/column shares an equal proportion of the overall
# height/width.

# The default can be changed via the `heights` and `widths` arguments:

par(mar = c(0, 0, 0, 0))
par(mfrow = c(1, 1))
plot(imager::load.image("proportions.png"), axes = FALSE)

# E.g., a joint density plot is really a subplot of joint and marginal densities.


# Exercise:
# Generate the following plot which shows a bivariate heatmap alongside marginal histograms (Hint: 2x2 grid)
rstudioapi::viewer("bivariate_density.html")



### Recursive subplots -----

# The `subplot()` function returns a plotly object that can be modified like
# any other plotly object. This effectively means that subplots work recursively
# (i.e., you can have subplots within subplots). This idea is useful when your
# desired layout doesn't conform to the table structure. 

plotList <- function(nplots) {
  lapply(seq_len(nplots), function(x) plot_ly())
}
s1 <- subplot(plotList(6), nrows = 2, shareX = TRUE, shareY = TRUE)
s2 <- subplot(plotList(2), shareY = TRUE)
subplot(
  s1, s2, plot_ly(), nrows = 3, 
  margin = 0.04, heights = c(0.6, 0.3, 0.1)
)

# The concept is particularly useful when you want plot(s) in a given row to
# have different widths from plot(s) in another row. 

# Place many bar charts in the first row, and a single choropleth in the second row:

# specify some map projection/options
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  lakecolor = toRGB('white')
)
# create a map of population density
density <- state.x77[, "Population"] / state.x77[, "Area"]
map <- plot_geo(
  z = ~density, text = state.name, 
  locations = state.abb, locationmode = 'USA-states'
) %>%
  layout(geo = g)

# create a bunch of horizontal bar charts 
vars <- colnames(state.x77)
barcharts <- lapply(vars, function(var) {
  plot_ly(x = state.x77[, var], y = state.name) %>%
    add_bars(orientation = "h", name = var) %>%
    layout(showlegend = FALSE, hovermode = "y",
           yaxis = list(showticklabels = FALSE))
})

subplot(barcharts, margin = 0.01) %>%
  subplot(map, nrows = 2, heights = c(0.3, 0.7), margin = 0.1) %>%
  layout(legend = list(y = 1)) %>%
  colorbar(y = 0.5)


#### Scatterplot matrices -----

# The plotly.js library provides a trace specifically designed and optimized for
# scatterplot matrices (splom). To use it, provide numeric variables to the
# `dimensions` attribute of the `splom` trace type.

dims <- dplyr::select_if(iris, is.numeric)
dims <- purrr::map2(dims, names(dims), ~list(values = .x, label = .y))
plot_ly(
  type = "splom", dimensions = setNames(dims, NULL), 
  showupperhalf = FALSE, diagonal = list(visible = FALSE)
)


#### Generalized pairs plot -----

# The generalized pairs plot is an extension of the scatterplot matrix to
# support both discrete and numeric variables.
# The `ggpairs()` function from the **GGally** package provides an interface
# for creating these plots via **ggplot2**. To implement `ggpairs()`, **GGally**
# introduces the notion of a matrix of **ggplot2** plot objects that it calls
# `ggmatrix()`. 

# `ggplotly()` function has a method for converting ggmatrix objects directly:

pm <- GGally::ggpairs(iris, aes(color = Species))
class(pm)
ggplotly(pm)

# As it turns out, **GGally** uses `ggmatrix()` as a building block for other
# visualizations, like model diagnostic plots (`ggnostic()`). 

#### ggplot2 subplots -----

# It's possible to combine the convenience of **ggplot2**'s
# `facet_wrap()`/`facet_grid()` with the more flexible arrangement capabilities
# of `subplot()`. 

# Show two different views of the `economics_long` data: 
# the left-hand column displays each variable along time, 
# while the right-hand column shows violin plots of each variable. 

# For the implementation, each column is created through `ggplot2::facet_wrap()`,
# but then the trellis displays are combined with `subplot()`. 

# In this case, **ggplot2** objects are passed directly to `subplot()`,
# but you can also use `ggplotly()` for finer control over the conversion
# of **ggplot2** to **plotly** before supplying that result to `subplot()`.

gg1 <- ggplot(economics_long, aes(date, value)) + geom_line() +
  facet_wrap(~variable, scales = "free_y", ncol = 1)
gg2 <- ggplot(economics_long, aes(factor(1), value)) +
  geom_violin() +
  facet_wrap(~variable, scales = "free_y", ncol = 1) + 
  theme(axis.text = element_blank(), axis.ticks = element_blank())
subplot(gg1, gg2)
