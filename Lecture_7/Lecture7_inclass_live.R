# Foundations -----

# A plotly.js figure contains one (or more) trace(s), and every trace has a type. 

# The trace type `scatter` is great for drawing low-level geometries (e.g., points,
# lines, text, and polygons) and provides the foundation for many `add_*()` functions
# (e.g., `add_markers()`, `add_lines()`, `add_paths()`, `add_segments()`,
# `add_ribbons()`, `add_area()`, and `add_polygons()`) as well as many `ggplotly()`
# charts. 

# A simple example:
# `add_lines()` ensures lines are drawn according to the ordering
# of `x`, which is desirable for a time series plotting. This behavior is subtly different
# than `add_paths()` which uses row ordering instead.

library(plotly)
data(economics, package = "ggplot2")
economics
# sort economics by psavert (personal savings rate), just to 
# show difference between paths and lines
economics %>% arrange(psavert)

(p <- 
  economics %>%
    arrange(psavert) %>%
    plot_ly(x = ~date, y = ~psavert))

subplot(add_lines(p), add_paths(p))
add_lines(p)


(p0 <-
    economics %>%
    arrange(date) %>%
    plot_ly(x = ~unemploy, y = ~psavert))

(p <-
economics %>%
  arrange(date) %>%
  plot_ly(x = ~unemploy, y = ~psavert, text = ~date) %>%
    add_paths())


(p <-
    economics %>%
    arrange(pce) %>%
    plot_ly(x = ~unemploy, y = ~psavert, text = ~date) %>%
    add_paths())

economics_fake <- economics

economics_fake$date <- sample(economics_fake$date)

(p_fake <-
    economics_fake %>%
    arrange(date) %>%
    plot_ly(x = ~unemploy, y = ~psavert, text = ~date) %>%
    add_paths())

subplot(p0, p %>% add_paths(color = ~date), p_fake)

class(economics$date)

economics$date[1] + 5

as.Date("23/05/90", format = "%d/%m/%y")
as.Date("23/05/1990", format = "%d/%m/%Y")
as.Date("5/23/90", format = "%m/%d/%y")

as.numeric(economics$date)

economics$date_num <- as.numeric(economics$date)
plot_ly(economics, x = ~unemploy, y = ~psavert, color = ~date_num)


# Previously, we introduced 'aesthetic mapping' arguments (unique to the R package) which
# make it easier to map data to visual properties (e.g., `color`, `linetype`, etc.). 

# In addition to these arguments, **dplyr** groupings can be used to ensure there is
# at least one geometry per group. 

# 1. `group_by()` could be used to effectively wrap the time series by year, which can
# be useful for visualizing annual **seasonality**. 

# 2. provide a categorical variable to a relevant aesthetic (e.g., `color`).

library(lubridate)
econ <- 
  economics %>%
  mutate(yr = year(date), mnth = month(date))
econ

# 1. One trace:
econ %>%
  group_by(yr) %>%
  plot_ly(x = ~mnth, y = ~uempmed) %>%
  add_lines(text = ~yr)

# subplot(
#   economics %>% plot_ly(x = ~date, y = ~psavert) %>% add_lines, 
#   econ %>% group_by(yr) %>% plot_ly(x = ~mnth, y = ~uempmed) %>% add_lines(text = ~yr)
# )
#   
#   
# (p <- 
#     economics %>%
#     arrange(psavert) %>%
#     plot_ly(x = ~date, y = ~psavert))


# Exercise:
# What is the difference between the above and the following plots?
# Before brute forcing your way into the answer, try `?add_lines` and guess
# what the difference will be.
econ %>%
  group_by(yr) %>%
  plot_ly(x = ~mnth, y = ~uempmed) %>%
  add_lines()

# Answer:
# since we we didn't a text property, we can't see the year in the tooltip.


# Multiple traces:
plot_ly(econ, x = ~mnth, y = ~uempmed) %>%
  add_lines(color = ~ordered(yr))

# Exercise:
# What does this code do?
(p <- 
    plot_ly(econ, x = ~mnth, y = ~uempmed) %>%
      add_lines(color = ~factor(yr)))
# How is it different from (2)?
# What do we learn from the warnings?
# Find a fix that will get rid of the warnings.

# Answers:





# Exercise:
# Check how many traces were defined by examining the plot_build() object, make sure that
# the number makes sense.

# Answer:
p <- plot_ly(econ, x = ~mnth, y = ~uempmed) %>%
  add_lines(color = ~factor(yr), colors = "Accent")
length(plotly_build(p)$x$data)
length(unique(econ$yr))



# The benefit of having multiple traces is that we can perform
# interactive filtering via the legend and compare multiple y-values at a given x.
# The cost of having those capabilities is that plots begin to suffer from performance issues after
# a few hundred traces, whereas thousands of lines can be rendered fairly easily in one trace. 

# Use plotly.js directly -----
# You won't find plotly.js attributes listed as explicit arguments in any **plotly** function
# (except for the special `type` attribute), but they are passed along verbatim to the plotly.js
# figure definition through the `...` operator.
?plot_ly
?add_trace
schema()
# https://plotly.com/r/reference/
  
# The scatter-based layers in this chapter fix the `type` plotly.js attribute to `"scatter"`
# as well as the [`mode`](https://plot.ly/r/reference/#scatter-mode) (e.g., `add_markers()`
# uses `mode='markers'` etc.), but you could also use the lower-level `add_trace()` to work
# more directly with plotly.js. 

# For example, render markers, lines, and text in the same scatter trace and leverage *nested*
# plotly.js attributes, like [`textfont`](https://plot.ly/r/reference/#scatter-textfont) and 
# [`xaxis`](https://plot.ly/r/reference/#layout-xaxis); 

# these attributes contain other attributes, so you need to supply a suitable named list to these arguments.
set.seed(697)
plot_ly() %>%
 add_trace(
   type = "scatter",
   mode = "markers+lines+text",
   x = 4:6, 
   y = 4:6,
   text = replicate(3, praise::praise("You are ${adjective}! ☺")),
   textposition = "right",
   hoverinfo = "text",
   textfont = list(family = "Roboto Condensed", size = 16)
 ) %>%
 layout(xaxis = list(range = c(3, 8)))

# The sections that follow in this chapter demonstrate various types of data views using
# scatter-based layers. 

# A particular emphasis is put on features only currently available from the R package
# (e.g., the aesthetic mapping arguments).

## Markers -----

# This section details scatter traces with a `mode` of `"markers"` (i.e., `add_markers()`). 

# For simplicity, many of the examples here use `add_markers()` with a numeric x and y axis, 
# which results in scatterplot.

### Alpha blending -----

# Scatterplots can be useful for exposing other important features including: 
# casual relationships, outliers, clusters, gaps, barriers, and conditional relationships. 

# A common problem with scatterplots, however, is overplotting, meaning that there are
# multiple observations occupying the same (or similar) x/y locations. 

# One way to combat overplotting is alpha blending. When dealing with tens of thousands
# of points (or more), consider using `toWebGL()` to render plots using Canvas rather
# than SVG, or leveraging 2D density estimation.
subplot(
  plot_ly(mpg, x = ~cty, y = ~hwy, name = "default"),
  plot_ly(mpg, x = ~cty, y = ~hwy) %>% 
    add_markers(alpha = 0.2, name = "alpha")
)

### Colors -----

# Mapping a discrete variable to `color` produces one trace per category, which is desirable
# for its legend and hover properties.

# On the other hand, mapping a *numeric* variable to `color` produces one trace, as well as
# a [colorbar](https://plot.ly/r/reference/#scatter-marker-colorbar) guide for visually decoding
# colors back to data values. 

# The `colorbar()` function can be used to customize the appearance of this automatically generated
# guide. The default colorscale is viridis, a perceptually uniform colorscale (even when converted
# to black-and-white), and perceivable even to those with common forms of color blindness.

# Viridis is also the default colorscale for ordered factors.
p <- plot_ly(mpg, x = ~cty, y = ~hwy, alpha = 0.8)
subplot(
  add_markers(p, color = ~cyl, showlegend = FALSE) %>% 
    colorbar(title = "Viridis"),
  add_markers(p, color = ~factor(cyl))
)

# What changed here compared to the previous code?
# Answer:




# There are numerous ways to alter the default color scale via the `colors` argument.
# Three of those are:

# (1) a color brewer palette name (see the row names of `RColorBrewer::brewer.pal.info` for valid names)
# (2) a vector of colors to interpolate
# (3) a color interpolation function like `colorRamp()` or `scales::colour_ramp()`. 

# Although this grants a lot of flexibility, one should be conscious of using a sequential colorscale
# for numeric variables (and ordered factors), and a qualitative colorscale for discrete variables.

# Which of the color configurations is which (sequential vs. qualitative)

col1 <- c("#132B43", "#56B1F7")
add_markers(p, color = ~cyl, colors = col1) %>%
  colorbar(title = "ggplot2 default")

col2 <- viridisLite::inferno(10)
add_markers(p, color = ~cyl, colors = col2) %>% 
  colorbar(title = "Inferno")

col3(1)
col3(0.75)
col3(0.5)
col3(0)

# col4 <- function(q) {
#   ifelse (q < 0.5) {
#     return(matrix(c(0, 255, 0), nrow = 1))
#   } else {
#     return(matrix(c(0, 0, 255), nrow = 1))
#   }
# }
# add_markers(p, color = ~cyl, colors = col4) %>% 
#   colorbar(title = "colorRamp")


col3 <- colorRamp(c("red", "white", "blue"))
add_markers(p, color = ~cyl, colors = col3) %>% 
  colorbar(title = "colorRamp")


col1 <- "Accent"
add_markers(p, color = ~factor(cyl), colors = col1)

col2 <- colorRamp(c("red", "blue"))
add_markers(p, color = ~factor(cyl), colors = col2)

col3 <- c(`4` = "red", `5` = "black", `6` = "blue", `8` = "green")
add_markers(p, color = ~factor(cyl), colors = col3)

# Color codes can be specified manually (i.e., avoid mapping data
# values to a visual range) by using the `I()` function.

# Any color understood by the `col2rgb()` function from the **grDevices**
# package can be used in this way. 

add_markers(p, color = I("black")) # implicitly uses `col2rgb()`
add_markers(p, color = I(rgb(0, 0, 0)))

# The `color` argument is meant to control the 'fill-color' of a geometric
# object, whereas `stroke` is meant to control the 'outline-color' of a
# geometric object. 

# In the case of `add_markers()`, that means `color` maps to the plotly.js
# attribute [`marker.color`](https://plot.ly/r/reference/#scatter-marker-color)
# and `stroke` maps to [`marker.line.color`](https://plot.ly/r/reference/#scatter-marker-line-color).
# Not all, but many, marker symbols have a notion of stroke.

### Symbols -----

# The `symbol` argument can be used to map data values to the `marker.symbol`
# plotly.js attribute. It uses the same semantics that we've already seen for
# `color`:

# * A numeric mapping generates a trace.
# * A discrete mapping generates multiple traces (one trace per category).
# * The plural, `symbols`, can be used to specify the visual range for the mapping.
# * Mappings are avoided entirely through `I()`.

p <- plot_ly(mpg, x = ~cty, y = ~hwy, alpha = 0.8) 

# A numeric mapping:
add_markers(p, symbol = ~cyl, name = "A single trace")

# A discrete mapping:
add_markers(p, symbol = ~factor(cyl), color = I("black"))

# When plotting multiple traces and no color is specified, the
# plotly.js [colorway](https://plot.ly/r/reference/#layout-colorway) 
# is applied (i.e., each trace will be rendered a different color). 

add_markers(p, symbol = ~factor(cyl))

# To set a fixed color, you can set the color of every trace generated
# from this layer with `color = I("black")`, or similar.

# There are two ways to specify the visual range of `symbols`: 
# (1) numeric codes (interpreted as a `pch` codes) 
# (2) a character string specifying a valid `marker.symbol` value. 

add_markers(p, symbol = ~cyl, symbols = c(17, 18, 19))

add_markers(
  p, color = I("black"),
  symbol = ~factor(cyl), 
  symbols = c("triangle-up", "diamond", "circle")
)


# These `symbols` (i.e., the visual range) can also be supplied directly
# to `symbol` through `I()`. 

plot_ly(mpg, x = ~cty, y = ~hwy) %>%
  add_markers(symbol = I(18), alpha = 0.5)

# To see all the symbols available to **plotly**

# Start exploring:
# Go to:
# https://plotly.com/r/reference/scatter/
# and find marker and symbols.

# Comapre to:
schema()
# and go to traces, scatter, attributes, marker, symbol, values
# to see all possible values.

# Extract all values:
vals <- schema(FALSE)$traces$scatter$attributes$marker$symbol$values
head(vals, 30)

# Careful - the values are all coerced to character values. But the numeric
# ones should be numeric.

# As the reference suggests, each number is equivalent to a name.

# Explore one by one:
p <- plot_ly(alpha = 0.6)

# To display a single marker we can:
add_markers(p, x = 1, y = 1)

# Find how to display a symbol that corresponds to "square", set its size to 30,
# its line width to 2 and show the only the text "square" when hovering above it.

# Answer:
add_markers(
  p,
  x = 1,
  y = 1,
  "",
  "",
  "" = 
    list(
      "",
      "",
      "" = list("")
      )
  )


# Exercise:
# Display all 158 available markers on a scatter grid (10 x 16 grid with two missing
# entries). When hovering over a marker, the display should be of the pattern
# "0 or circle", "100 or circle-open" etc. 

# In addition, reverse the y-axis and hide both axes.

# All as in as in "symbols.html"


vals <- schema(FALSE)$traces$scatter$attributes$marker$symbol$values

# Hint:

# To make our lives a little easier, you may arrange the values in a matrix:
(vals_mat <- matrix(vals, ncol = 3, byrow = TRUE))
# And omit the redundant cnumerical column:
(vals_mat <- vals_mat[ , c(2, 3)])

# Answer:






# Supplying your own custom glyphs, see Chapter \@ref(working-with-symbols).

### Stroke and span -----

# The `stroke` argument follows the same semantics as `color` and `symbol` when
# it comes to variable mappings and specifying visual ranges.

# Typically you don't want to map data values to `stroke`, you just want to
# specify a fixed outline color. 

# By default, the `span`, or width of the stroke, is zero, you'll likely want
# to set the width to be around one pixel.

plot_ly(mpg, x = ~cty, y = ~hwy, alpha = 0.5) %>%
  add_markers(symbol = I(18), stroke = I("black"), span = I(1))

### Size -----

# For scatterplots, the `size` argument controls the area of markers (unless
# otherwise specified via [sizemode](https://plot.ly/r/reference/#scatter-marker-sizemode)),
# and _must_ be a numeric variable.

# The `sizes` argument controls the minimum and maximum size of circles, in pixels:

p <- plot_ly(mpg, x = ~cty, y = ~hwy, alpha = 0.3) 

add_markers(p, size = ~cyl, name = "default")

add_markers(p, size = ~cyl, sizes = c(1, 500), name = "custom")

# Similar to other arguments, `I()` can be used to specify the size directly.

# In the case of markers, `size` controls the [`marker.size`](https://plot.ly/r/reference/#scatter-marker-size)
# plotly.js attribute. 

plot_ly(mpg, x = ~cty, y = ~hwy, alpha = 0.3, size = I(30))

### Dotplots and error bars -----

# A dotplot is similar to a scatterplot, except instead of two numeric axes,
# one is categorical. The usual goal of a dotplot is to compare value(s) on
# a numerical scale over numerous categories. In this context, dotplots are
# preferable to pie charts since comparing position along a common scale is
# much easier than comparing angle or area. Furthermore, dotplots can be
# preferable to bar charts, especially when comparing values within a narrow
# range far away from 0. Also, when presenting point estimates, and uncertainty
# associated with those estimates, bar charts tend to exaggerate the difference
# in point estimates, and lose focus on uncertainty.

# A popular application for dotplots (with error bars) is the so called
# "coefficient plot" for visualizing the point estimates of coefficients and
# their standard error. The `coefplot()` function in the **coefplot** package
# and the `ggcoef()` function in the **GGally** both produce coefficient plots
# for many types of model objects in R using **ggplot2**, which we can translate
# to plotly via `ggplotly()`. 

# Since these packages use points and segments to draw the coefficient plots, the
# hover information is not the best, and it would be better to use 
# [error objects](https://plot.ly/r/reference/#scatter-error_x). 

# Use the `tidy()` function from the **broom** package to obtain a data frame
# with one row per model coefficient, and produce a coefficient plot with error
# bars along the x-axis. 

# Fit a full-factorial linear model
m <- lm(
  Sepal.Length ~ Sepal.Width * Petal.Length * Petal.Width, 
  data = iris
)

summary(m)

# Compare:
library(GGally)
ggp <- ggcoef(m, errorbar_height = .2, color = "black", sort = "ascending")
ggplotly(ggp)

# To:
broom::tidy(m) %>% 
  mutate(
    term = forcats::fct_reorder(term, estimate),
    half_CI = abs(qt(0.975, summary(m)$df[2]) * std.error)
  ) %>%
  plot_ly(x = ~estimate, y = ~term) %>%
  add_markers(
    error_x = ~list(type = "data", array = half_CI), 
    hoverinfo = "x"
  )

# Exercise:
# Modify the above code so that intervals that include zero are colored in gray,
# and those that do not are in black.

# Let's try:
dat <-
  broom::tidy(m) %>% 
    mutate(
      term = forcats::fct_reorder(term, estimate),
      half_CI = abs(qt(0.975, summary(m)$df[2]) * std.error),
      inc_zero = 
        ifelse(
          estimate - half_CI > 0 | estimate + half_CI < 0,
          "zero not included",
          "zero included"
        )
    )

p0 <-
  plot_ly(dat, x = ~estimate, y = ~term, color = ~inc_zero, colors = c("gray", "black")) %>%
    add_markers(
      error_x = ~list(type = "data", array = half_CI), 
      hoverinfo = "x"
    )

# But something breaks... the sizes of the bars are wrong!
# https://github.com/ropensci/plotly/issues/762

# Fix:

# 1. add another factor for the actual colors:
dat$bar_col <-
  with(dat,
       ifelse(
         estimate - half_CI > 0 | estimate + half_CI < 0,
         "black",
         "gray"
         )
  )

# 2. add a trace for each group: 
p <- plot_ly(dat)
for (g in unique(dat$inc_zero)) {
  r <- dat$inc_zero == g
  p <- 
    add_markers(
      p,
      name = g,
      x = dat$estimate[r],
      y = dat$term[r],
      color = I(dat$bar_col[r]),
      error_x = list(array = dat$half_CI[r]))
}

subplot(p0, p)

# Moral: add layers/complexity step by step and check that your additions do not 
# distort the data. Sometimes additional layers/features have unexpected impacts 
# on other features/layers.

## Lines -----

# Many of the same principles we learned about aesthetic mappings with respect
# to markers also apply to lines.

# plotly.js attributes [`line.width` and `line.color`](https://github.com/plotly/plotly.js/issues/147)
# do not support multiple values, meaning a single line trace can only have one
# width/color in 2D line plot, and consequently numeric `color`/`size` mappings
# won't work. This isn't necessarily true for 3D paths/lines and there will likely
# be support for these features for 2D paths/lines in WebGL in the near future.

### Linetypes

# Generally speaking, it's hard to perceive more than 8 different
# colors/linetypes/symbols in a given plot, so sometimes we have to
# filter data to use these effectively. 

# Here we use the **dplyr** package to find the top 5 cities in terms of
# average monthly sales (`top5`), then filtering the original data
# to contain just these cities.

# Once we have the data filtered, mapping city to `color` or `linetype` is
# trivial. The color palette can be altered via the `colors` argument, and
# follows the same rules as scatterplots.

# The linetype palette can be altered via the `linetypes` argument, and accepts
# R's [`lty` values](https://github.com/wch/r-source/blob/e5b21d/src/library/graphics/man/par.Rd#L726-L743)
# or plotly.js [dash values](https://plot.ly/r/reference/#scatter-line-dash).

library(dplyr)
View(txhousing)
top5 <- 
  txhousing %>%
  group_by(city) %>%
  summarise(m = mean(sales, na.rm = TRUE)) %>%
  arrange(desc(m)) %>%
  top_n(5)
tx5 <- txhousing %>% filter(city %in% top5$city)
plot_ly(tx5, x = ~date, y = ~median) %>%
  add_lines(linetype = ~city)

# to control exactly which linetype is used to encode a particular data value, 
# you can provide a named character vector, similarly to how we provided a
# discrete colorscale manually for markers. 

ltys <- c(
  Austin = "dashdot",
  `Collin County` = "longdash",
  Dallas = "dash",
  Houston = "solid",
  `San Antonio` = "dot"
)
plot_ly(tx5, x = ~date, y = ~median) %>%
  add_lines(linetype = ~city, linetypes = ltys)

### Segments

# The `add_segments()` function provides a way to connect two points
# [(`x`, `y`) to (`xend`, `yend`)] with a line. Segments form the building
# blocks for numerous useful chart types, including slopegraphs, dumbell charts,
# candlestick charts, and more. 

#   - Slopegraphs and dumbell charts are useful for comparing numeric values
#     across numerous categories. 
#   - Candlestick charts are typically used for visualizing change in a
#     financial asset over time.

# Segments can also provide a useful alternative to `add_bars()` especially
# for animations. 

#### Dumbell -----

# Dumbell charts are used to compare two different classes of numeric values across numerous groups.

# The following uses the dumbell approach to show average miles per gallon city and highway for different
# car models. With a dumbell chart, it's always a good idea to order the categories by a sensible metric;
# here, the categories are ordered by the city miles per gallon.

mpg %>%
  group_by(model) %>%
  summarise(c = mean(cty), h = mean(hwy)) %>%
  mutate(model = forcats::fct_reorder(model, c)) %>%
  plot_ly() %>%
  add_segments(
    x = ~c, y = ~model,
    xend = ~h, yend = ~model, 
    color = I("gray"), showlegend = FALSE
  ) %>%
  add_markers(
    x = ~c, y = ~model, 
    color = I("blue"), 
    name = "mpg city"
  ) %>%
  add_markers(
    x = ~h, y = ~model, 
    color = I("red"),
    name  = "mpg highway"
  ) %>%
  layout(xaxis = list(title = "Miles per gallon"))


#### Candlestick

# In the follwoing, we use the **quantmod** package to obtain stock price data
# for Microsoft and plots two segments for each day: one to encode the
# opening/closing values, and one to encode the daily high/low.

# This implementation uses `add_segments()` to implement the candlestick chart,
# but more recent versions of plotly.js contain a
# [candlestick](https://plot.ly/r/reference/#candlestick) and
# [ohlc](https://plot.ly/r/reference/#ohlc) trace types, both of which are useful
# for visualizing financial data.

library(quantmod)
msft <- getSymbols("MSFT", auto.assign = FALSE)
dat <- as.data.frame(msft)
dat$date <- index(msft)
dat <- subset(dat, date >= "2016-01-01")
names(dat) <- sub("^MSFT\\.", "", names(dat))

plot_ly(dat, x = ~date, xend = ~date, color = ~Close > Open, 
        colors = c("red", "forestgreen")) %>%
  add_segments(y = ~Low, yend = ~High, size = I(1), hoverinfo = "y") %>%
  add_segments(y = ~Open, yend = ~Close, size = I(3), hoverinfo = "y") %>%
  layout(showlegend = FALSE, yaxis = list(title = "Price")) %>%
  rangeslider()

# This is a little disappointing because the y-range is fixed.

# Exercise: look for a solution.

# Answer:

plot_ly(dat, x = ~date, xend = ~date, color = ~Close > Open, 
        colors = c("red", "forestgreen")) %>%
  add_segments(y = ~Low, yend = ~High, size = I(1), hoverinfo = "y") %>%
  add_segments(y = ~Open, yend = ~Close, size = I(3), hoverinfo = "y") %>%
  layout(showlegend = FALSE, yaxis = list(title = "Price", fixedrange = FALSE)) %>%
  rangeslider()

# 










### Density plots -----

# We can leverage the `density()` function for computing kernel density estimates
# and route the results to `add_lines()`.

kerns <- c("gaussian", "epanechnikov", "rectangular", 
          "triangular", "biweight", "cosine", "optcosine")
p <- plot_ly()
for (k in kerns) {
  d <- density(economics$pce, kernel = k, na.rm = TRUE)
  p <- add_lines(p, x = d$x, y = d$y, name = k)
}
p

### Ribbons -----

# Ribbons are useful for showing uncertainty bounds as a function of x. The
# `add_ribbons()` function creates ribbons and requires the arguments: `x`,
# `ymin`, and `ymax`. 

# The `augment()` function from the **broom** package appends observational-level
# model components (e.g., fitted values stored as a new column `.fitted`) which
# is useful for extracting those components in a convenient form for visualization.

# E.g. the fitted values and uncertainty bounds from a linear model object:

m <- lm(mpg ~ wt, data = mtcars)
broom::augment(m, se_fit = TRUE) %>%
  plot_ly(x = ~wt, showlegend = FALSE, hoverinfo = "x+y") %>%
  add_markers(y = ~mpg, color = I("black")) %>%
  add_ribbons(ymin = ~.fitted - 1.96 * .se.fit, 
              ymax = ~.fitted + 1.96 * .se.fit, 
              color = I("gray80")) %>%
  add_lines(y = ~.fitted, color = I("steelblue"))
