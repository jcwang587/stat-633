# plotly - Overview -----

# This lecture discusses how to leverage the **plotly** R package to create a variety
# of interactive graphics. 

# There are two main ways to creating a **plotly** object: 
# - transforming a **ggplot2** object (via `ggplotly()`) into a **plotly** object
# - directly initializing a **plotly** object with `plot_ly()`/`plot_geo()`/`plot_mapbox()`.

# Both approaches are an implementation of the Grammar of Graphics and both are powered
# by the JavaScript graphing library plotly.js, so many of the same concepts and tools that you
# learn for one interface can be reused in the other.

## Intro to `plot_ly()` -----

# Any graph made with the **plotly** R package is powered by the JavaScript 
# library [plotly.js](https://github.com/plotly/plotly.js).

# The `plot_ly()` function provides a 'direct' interface to plotly.js with some
# additional abstractions to help reduce typing. 

# These abstractions, inspired by the Grammar of Graphics and **ggplot2**, make it much
# faster to iterate from one graphic to another, making it easier to discover interesting
# features in the data. 

# A rich gallery of examples is provided in:
# https://plotly.com/r/

# Example:
# Using `plot_ly()` to explore the `diamonds` dataset from **ggplot2**.

# load the plotly R package
library(plotly)
# load the diamonds dataset from the ggplot2 package
data(diamonds, package = "ggplot2")
diamonds

# If we assign variable names (e.g., `cut`, `clarity`, etc.) to visual 
# properties (e.g., `x`, `y`, `color`, etc.) within `plot_ly()`, it tries to find a
# sensible geometric representation of that information for us.

# Examine `cut`:
str(diamonds$cut)
head(diamonds$cut, 30)
# What can be a "sensible" representation when we assign the values of `cut` to the `x` property?
plot_ly("", x = "")

# Note: the plot appears in the `Viewer` tab in RStudio, not in the `Plots` tab.

# Examine `clarity`:
head(diamonds$clarity, 30)
# What can be a "sensible" representation when we assign the values of `cut` to the `x` property
# and the values of `clarity` to the `y` property?

plot_ly("", "", "")

# (Note the output in the console.)

# What can be a "sensible" representation when we assign the values of `cut` to the `x` property
# and the values of `clarity` to the `color` property?

plot_ly("", "", "")


# The `plot_ly()` function has numerous arguments that are unique to the R
# package (e.g., `color`, `stroke`, `span`, `symbol`, `linetype`, etc.) and make it easier
# to encode data variables (e.g., diamond clarity) as visual properties (e.g., color). 

plot_ly("", x = "", color = "", colors = "")

# In the last example, `color` is used to map each level of diamond clarity
# to a different color, then `colors` is used to specify the range of colors (which,
# in this case, the "Accent" color palette from the **RColorBrewer** package, but one
# can also supply custom color codes or a color palette function like `colorRamp()`). 

# Try:
plot_ly(diamonds, x = ~cut, color = "black")

# Since these arguments map data values to a visual range by default, you will obtain
# unexpected results if you try to specify the visual range directly.

# If you want to specify the visual range directly, use the `I()` function to declare
# this value to be taken 'AsIs':

plot_ly(
  diamonds,
  x = ~cut,
  color = "",
  stroke = "",
  span = ""
)

# A good resource to learn more about these arguments (especially their defaults) is 
# the R documentation page available by entering `help(plot_ly)` in your R console.

# The **plotly** package takes a purely functional approach to a layered grammar of
# graphics: (almost) every function anticipates a **plotly** object as input to
# its first argument and returns a modified version of that **plotly** object.

# For example, the `layout()` function anticipates a **plotly** object in its first
# argument and its other arguments add and/or modify various layout components of
# that object (e.g., the title):

layout(
  "",
  title = "My beatiful histogram"
)

# For more complex plots that modify a **plotly** graph many times over, code written
# in this way can become cumbersome to read. 

# The `%>%` operator simplifies this by placing the object on the left-hand side of the
# `%>%` into the first argument of the function of the right-hand side:

diamonds %>%
  "" %>%
  layout(title = "My beatiful histogram")

# In addition to `layout()` for adding/modifying part(s) of the graph's layout, there are
# also a family of `add_*()` functions (e.g., `add_histogram()`, `add_lines()`, etc.) that
# add a graphical layer to a plot. 

# A *layer* can be thought of as a group of graphical elements that can be sufficiently
# described using only 5 components: data, aesthetic mappings (e.g., assigning `clarity` to
# `color`), a geometric representation (e.g., rectangles, circles, etc.), statistical
# transformations (e.g., sum, mean, etc.), and positional adjustments (e.g., dodge, stack,
# etc.). 

# In the examples thus far, we have not specified a layer.
# The layer has been added for us automatically by `plot_ly()`. 

# To be explicit about what `plot_ly(diamonds, x = ~cut)` generates, we should add
# a `add_histogram()` layer:

add_histogram(plot_ly(diamonds), x = ~cut)

# Exercise: 
# Rewrite the above line using two pipe operators




# As we'll discuss later, **plotly** has both `add_histogram()` and `add_bars()`. 
# The difference is that `add_histogram()` performs *statistics* (i.e., a binning algorithm)
# dynamically in the web browser, whereas `add_bars()` requires the bar heights to be
# pre-specified. That means, to replicate the last example with `add_bars()`, the number
# of observations must be computed ahead of time. 

# Exercise:
# Find out the arguments that are required for `add_bars()` and use the `dplyr::count` function
# to make a bar plot that is, in fact, a histogram.







# There are numerous other `add_*()` functions that calculate statistics in the browser
# (e.g., `add_histogram2d()`, `add_contour()`, `add_boxplot()`, etc.), but most other
# functions aren't considered statistical.

# Generally speaking, non-statistical layers will be faster and more responsive at
# runtime (since they require less computational work), whereas the statistical layers
# allow for more flexibility when it comes to client-side interactivity.


# In many scenarios, it can be useful to combine multiple graphical layers into a single
# plot. In this case, it becomes useful to know a few things about `plot_ly()`:
# 
# * Arguments specified in `plot_ly()` are *global*, meaning that any downstream `add_*()`
#   functions inherit these arguments (unless `inherit = FALSE`). 

# * Data manipulation verbs from the **dplyr** package may be used to transform the
#   `data` underlying a **plotly** object.

# Technically speaking, these **dplyr** verbs are S3 generic functions that have a
# **plotly** method. In nearly every case, that method simply queries the data underlying
# the **plotly** object, applies the **dplyr** function, then adds the transformed data
# back into the resulting **plotly** object.

# Using these two properties of `plot_ly()`, we can (for example):

# 1. _Globally_ assign `cut` to `x`.
# 2. Add a histogram layer (inherits the `x` from `plot_ly()`).
# 3. Use **dplyr** verbs to modify the `data` underlying the **plotly** object.
#    Here we just count the number of diamonds in each `cut` category.
# 4. Add a layer of text using the summarized counts. Note that the global `x` mapping, as
#    well as the other mappings local to this text layer (`text` and `y`), reflects data
#    values from step 3.

# Exercise:
# Complete the following code so that it will accomplish the above 4 steps:
library(dplyr)
(p1 <- 
    diamonds %>%
    plot_ly( "" ) %>%
    add_histogram( "" ) %>%
    dplyr::""( "" ) %>%
    summarise(n = "") %>%
    add_text(
      text = ~scales::comma(n), y = ~n,
      textposition = "top middle",
      cliponaxis = FALSE
    )
)


# Before using multiple `add_*()` in a single plot, make sure that you actually
# want to show those layers of information on the same set of axes.

# When using **dplyr** verbs to modify the `data` underlying the **plotly** object,
# you can use the `plotly_data()` function to obtain the data at any point in
# time, which is primarily useful for debugging purposes.

# What are the differences between the following **plotly** objects and data
# extracted using `plotly_data()`?

(plotly_obj1 <-
    diamonds %>%
    plot_ly(x = ~cut) %>%
    add_histogram())
View(plotly_obj1)

# to:
plotly_dat1 <- 
  diamonds %>%
  plot_ly(x = ~cut) %>%
  add_histogram() %>%
  plotly_data()
plotly_dat1

# And:
(plotly_obj2 <-
    diamonds %>%
    plot_ly(x = ~cut) %>%
    add_histogram() %>%
    group_by(cut) %>%
    summarise(n = n()))
View(plotly_obj2)

# To:
plotly_dat2 <-
  diamonds %>%
  plot_ly(x = ~cut) %>%
  add_histogram() %>%
  group_by(cut) %>%
  summarise(n = n()) %>%
  plotly_data()
plotly_dat2

# And:
(plotly_obj3 <-
    diamonds %>%
    dplyr::count(cut) %>%
    plot_ly() %>%
    add_bars(x = ~cut, y = ~n))
View(plotly_obj3)

# To:
plotly_dat3 <-
  diamonds %>%
  dplyr::count(cut) %>%
  plot_ly() %>%
  add_bars(x = ~cut, y = ~n) %>%
  plotly_data()
plotly_dat3


# The above introduction to `plot_ly()` has mainly focused on concepts unique to the R package
# **plotly** that are generally useful for creating most kinds of data views.

# The next section outlines how **plotly** generates plotly.js figures and how to inspect the
# underlying data structure that plotly.js uses to render the graph. 

# Not only is this information useful for debugging, but it's also a nice way to learn how to
# work with plotly.js directly, which you may need to improve performance in **shiny** apps
# and/or for adding custom behavior with JavaScript.

## Intro to plotly.js -----

# When you print any **plotly** object, the `plotly_build()` function is applied to that
# object, and that generates an R list which adheres to a syntax that plotly.js understands.

# This syntax is a JavaScript Object Notation (JSON) specification that plotly.js uses to
# represent, serialize, and render web graphics. 

# The following figure shows how this workflow applies to a simple bar graph (with
# values directly supplied instead of a data column name reference, but the same concept 
# applies for any graph created via **plotly**.

par(mar = c(0, 0, 0, 0))
par(mfrow = c(1, 1))
plot(imager::load.image("printing.png"), axes = FALSE) 

# Exercise:
# Run:
plot_ly(diamonds, x = ~cut)
# in RStudio, and check in which formats you can export it from the `Viewer` tab.
# Compare to the formats you can export the following standard plot from the
# `Plots` tab:
ggplot(diamonds) +
  geom_bar(aes(x = cut))

# A lot of documentation is available online about plotly (e.g., the online
# [reference](https://plot.ly/r/reference/)) implicitly refers to this JSON
# specification, so it can be helpful to know how to "work backwards" from that
# documentation (i.e., translate JSON into to R code). 

# Recall our plot form the previous exercise:
(p1 <- 
    diamonds %>%
    plot_ly( "" ) %>%
    add_histogram( "" ) %>%
    dplyr::""( "" ) %>%
    summarise(n = "") %>%
    add_text(
      text = ~scales::comma(n), y = ~n,
      textposition = "top middle",
      cliponaxis = FALSE
    )
)

# Exercise:

# The legend in this plot is really redundant. How do we get rid of it?
# First, try with ?plot_ly - this will show the basic functionality of plotly objects in R.

# Answer:



# Second, try ?add_trace

# Answer:



# Third, try:
# https://plotly.com/r/reference/
# Answer:




# Example:
# How do we change the background color of the modebar to red? 
# (notice the hierarchical structure):
p1 %>% layout("")

# Exercise:
# i. Get rid of `trace0` text in the tooltip for the histogram. Get rid of the tooltip for the text layer entirely.
# (hint: `hoverinfo`).
# ii. change the color of the font in the tooltip to red and the background of the label to yellow (hint: `hoverlabel`).
#     Note: there are several ways to do this - what are the differences?

# Answer:










# As the diagram suggests, both the `plotly_build()` and `plotly_json()` functions can be
# used to inspect the underlying data structure on both the R and JSON side of things.

# For example, the following shows the `data` portion of the JSON created for
# the plot stored in `p`:
(p <- plot_ly(diamonds, x = ~cut, color = ~clarity, colors = "Accent"))
plotly_json(p)
# Which is different from:
View(p)

# In plotly.js terminology, a *figure* has two key components:
# `data` (aka, traces) and a `layout`.

# Every trace has a *type* (e.g., histogram, pie, scatter, etc.) and the trace
# type determines what other attributes (i.e., visual and/or interactive properties,
# like `x`, `hoverinfo`, `name`) are available to control the trace mapping.

# That is, not every trace attribute is available to every trace type, but many
# attributes (e.g., the `name` of the trace) are available in every trace type
# and serve a similar purpose.

# A *trace* defines a mapping from data and visuals.

# A trace is similar in concept to a layer, but it's not quite the same. 
# In many cases , as in 
plot_ly(diamonds, x = ~cut, color = ~clarity, colors = "Accent")
# it makes sense to implement a single layer as multiple traces. 
# This is due to the design of plotly.js and how traces are tied to legends and hover
# behavior. 

# Inspecting the JSON object, we see that it takes 8 traces to generate
# the dodged bar chart.
plotly_json(p)

# Instead of clicking through JSON viewer, sometimes it's easier to use
# `plotly_build()` and compute on the plotly.js figure definition to verify
# certain things exist. 

# Since **plotly** uses the **htmlwidgets** standard, the actual plotly.js 
# figure definition appears under a list element named `x`.

# (The **htmlwidgets** package provides a foundation for other packages to
# implement R bindings to JavaScript libraries so that those bindings work
# in various contexts (e.g., the R console, RStudio, inside **rmarkdown** documents,
# **shiny** apps, etc.).

# Use plotly_build() to get at the plotly.js 
# definition behind *any* plotly object:
b <- plotly_build(p)

# Again, notice that 
View(b) # the build object
# is somewhat different from
View(p) # the plotly object

# Confirm that there 8 traces:
length(b$x$data)
# Extract the `name` of each trace. plotly.js uses `name` to
# populate legend entries and tooltips:
purrr::map_chr(b$x$data, "name")
# Every trace has a type of histogram:
purrr::map_chr(b$x$data, "type")

# Here we've learned that **plotly** creates 8 histogram traces to generate
# the dodged bar chart: one trace for each level of `clarity`.
# Although the x-axis is discrete, plotly.js still considers this a histogram
# because it generates counts in the browser.

# Why one trace per category? 
# Answer: 






# If we investigated further, we'd notice that `color` and `colors` are not
# officially part of the plotly.js figure definition; they are arguments of
# the `plot_ly` function in R. 

# The `plotly_build()` function has effectively transformed that information
# into a sensible plotly.js figure definition (e.g., `marker.color` contains
# the actual bar color codes). 

# In fact, the `color` argument in `plot_ly()` is just one example of an abstraction
# the R package has built on top of plotly.js to make it easier to map data values
# to visual attributes. 

# Intro to ggplotly() -----

# The ggplotly() function from the plotly package has the ability to translate
# ggplot2 to plotly. This functionality can be really helpful for quickly adding
# interactivity to your existing ggplot2 workflow.

# Let's explore the relationship between price and other variables from the
# `diamonds` dataset.

# Hexagonal binning (i.e., `geom_hex()`) is useful way to visualize a 2D density (see,
# e.g., https://www.meccanismocomplesso.org/en/hexagonal-binning-a-new-method-of-visualization-for-data-analysis/), 
# like the relationship between `price` and `carat`.

(p <- 
   ggplot(diamonds, aes(x = log(carat), y = log(price))) +
   geom_hex(bins = 100))

# We can see there is a strong positive linear relationship between the _log_ 
# of carat and price. It also shows that for many, the carat is only rounded
# to a particular number (indicated by the light blue bands) and no diamonds
# are priced around $1500. 

# Making this plot interactive makes it easier to decode the hexagonal colors
# into the counts that they represent:
ggplotly(p)

# `ggplotly()` is effective in leveraging **ggplot2**'s consistent and
# expressive interface for exploring statistical summaries across groups. 

# For example, by including a discrete `color` variable (e.g., `cut`) with
# `geom_freqpoly()`, you get a frequency polygon for each level of that variable.

# This ability to quickly generate visual encodings of statistical summaries across
# an arbitrary number of groups works for basically any geom (e.g., `geom_boxplot()`,
# `geom_histogram()`, `geom_density()`, etc.) and is a key feature of **ggplot2**.

(p <- 
    ggplot(diamonds, aes(x = log(price), color = clarity)) +
    geom_freqpoly())
ggplotly(p)

# Now, to see how `price` varies with both `cut` and `clarity`, we could repeat
# this same visualization for each level of `cut`. 

# This is where **ggplot2**'s `facet_wrap()` comes in handy. Moreover, to facilitate
# comparisons, we can have `geom_freqpoly()` display relative rather than absolute
# frequencies.
(p <- 
    ggplot(diamonds, aes(x = log(price), color = clarity)) +
    geom_freqpoly(stat = "density") +
    facet_wrap(~cut))

# By making this plot interactive, we can more easily compare particular levels of
# clarity by leveraging the legend filtering capabilities.
ggplotly(p)

# Play with the above plot - what do you like about this? 
# What do you think is potentially limiting?

# In addition to supporting most of the 'core' **ggplot2** API, `ggplotly()`
# can automatically convert any **ggplot2** extension packages that return a
# 'standard' **ggplot2** object. 

# "Standard" means that the object is comprised of 'core' **ggplot2** data
# structures and not the result of custom geoms.

# (`ggplotly()` can actually convert custom geoms as well, but each one requires
# a custom hook, and many custom geoms are not yet supported.) 

# Some great examples of R packages that extend **ggplot2** using core data
# structures are **ggforce**, **naniar**, and **GGally**.

# Another way of visualizing the same information found in previous plot is by using `geom_sina()` 
# from the **ggforce** package (instead of `geom_freqpoly()`). 

# This visualization jitters the raw data within the density for each group allowing us not 
# only to see where the majority observations fall within a group, but also across all groups.
# The second layer of the plot uses **ggplot2**'s `stat_summary()` to overlay a 95% confidence interval
# estimated via a Bootstrap algorithm via the **Hmisc** package.

(p <- 
    ggplot(diamonds, aes(x = clarity, y = log(price), color = clarity)) +
    ggforce::geom_sina(alpha = 0.1) +
    stat_summary(fun.data = "mean_cl_boot", color = "black") +
    facet_wrap(~cut))

# By making this layer interactive, we can query individual points for more information and zoom into
# interesting regions. 
toWebGL(ggplotly(p))

# It's surprising that the diamond price would decline with an increase of diamond clarity. 

# As it turns out, if we account for the carat of the diamond, then we see that better diamond
# clarity does indeed lead to a higher diamond price (this is a great example of "Simpson's paradox")


# Seeing such a strong pattern in the residuals of simple linear model of carat vs. price indicates
# that our model could be greatly improved by adding `clarity` as a predictor of `price`:
m <- lm(log(price) ~ log(carat), data = diamonds)
(diamonds <- modelr::add_residuals(diamonds, m))
(p <- 
    ggplot(diamonds, aes(x = clarity, y = resid, color = clarity)) +
    ggforce::geom_sina(alpha = 0.1) +
    stat_summary(fun.data = "mean_cl_boot", color = "black") +
    facet_wrap(~cut))

toWebGL(ggplotly(p))

# It is worth mentioning that ggplotly() conversions are not always perfect and ggplot2 doesn't
# provide an API for interactive features, so sometimes it's desirable to modify the return values
# of ggplotly(), which uses the same a plotly.js figure definition we discussed.

# In addition, ggplotly is much slower than plot_ly. E.g.:
microbenchmark::microbenchmark(
  a <- plot_ly(diamonds, x = ~cut),
  b0 <- ggplotly(ggplot(diamonds) + geom_bar(aes(x = cut))),
  b1 <- ggplotly(ggplot(diamonds) + geom_bar(aes(x = cut), fill = "steelblue") + theme_classic()),
  b2 <- toWebGL(ggplotly(ggplot(diamonds) + geom_bar(aes(x = cut), fill = "steelblue") + theme_classic())),
  times = 10,
  unit = "s"
)
# This may not matter so much with a single view, but as the plots grow more complex and especially 
# as we will combine several together, or when data updates rapidly, this may be noticeable.

# As a plotly object, we can do some modifications to it (see Chapters 33, 25.2)
(ggp <- toWebGL(ggplotly(p)))
ggp %>% layout(showlegend = FALSE)