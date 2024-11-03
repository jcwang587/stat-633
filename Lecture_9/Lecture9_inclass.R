# Maps -----

# There are several ways to make a map with **plotly**. 

# The approaches fall under two categories: integrated or custom. 

# Integrated maps leverage plotly.js's built-in support for rendering a basemap layer.
# Currently there are two supported ways of making integrated maps: 
# - via [Mapbox](https://www.mapbox.com/) or
# - via an integrated d3.js powered basemap. 
# The integrated approach is convenient if you need a quick map and don't need
# sophisticated representations of geo-spatial objects.

# Custom mapping approach offers complete control since you're providing all
# the information necessary to render the geo-spatial object(s).
# We will cover making sophisticated maps (e.g., cartograms) using the **sf**
# R package, but it's also possible to make custom **plotly** maps via other
# tools for geo-computing (e.g., **sp**, **ggmap**, etc.). 

# **plotly** doesn't aim to be the most fully featured geo-spatial visualization
# toolkit. There are benefits to using **plotly**-based maps since the mapping APIs
# are very similar to the rest of plotly, and you can leverage the larger **plotly**
# ecosystem (e.g., linking views client-side like Figure \@ref(fig:mapbox-bars)).
# However, if you run into limitations with **plotly**'s mapping functionality, there
# is a very rich set of tools for
# [interactive geospatial visualization in R](https://geocompr.robinlovelace.net/adv-map.html#interactive-maps),
# including but not limited to: **leaflet**, **mapview**, **mapedit**, **tmap**, and
# **mapdeck**.

## Integrated maps -----

### Mapbox -----

# Mapbox is a company that provides online maps. It provides a free account for some of its
# basic functionality.

# Go to mapbox.com, and choose "Start mapping for free". Set up an account, copy
# the public token to R in the following fashion:
Sys.setenv("MAPBOX_TOKEN" = "xxxxx___token_copied_web___xxxxx")

## Overview -----

# If you have simple latitude/longitude data and want to make a quick map,
# you may want to try one of **plotly**'s integrated mapping options (i.e.,
# `plot_mapbox()` and `plot_geo()`). 

# These constructor functions can be thought of as a replacement for
# `plot_ly()`.

# They provide a dynamic basemap rendered behind your data. 
# All the scatter-based layers we learned about work as you'd expect it to with
# `plot_ly()`.

# Non-scatter traces currently don't work with `plot_mapbox()`/ `plot_geo()` meaning that,
# for one, raster (i.e., heatmap) maps are not natively supported.] 

# Example:

library(plotly)
library(tidyverse)
(can_cities <- maps::canada.cities)

(p <- plot_mapbox(can_cities))

p %>%
  add_markers(
    x = ~long, 
    y = ~lat
  )


# Exercise:
# Change the above plot so that 
# 1. The marker size reflects the population of the city.
# 2. The marker color corresponds to the province.
# 3. The tooltip for each city shows its name and population size,
#    something like "Montreal QC, Pop. 3,280,123".
# 4. The map is centered and zoomed to show all (well, most) of Canada.
#    (see <https://plotly.com/r/reference/layout/mapbox/>)



# Basemap style -----

# The Mapbox basemap styling is controlled through the
# [`layout.mapbox.style`](https://plot.ly/r/reference/#layout-mapbox-style) attribute. 

# The **plotly** package comes with support for 7 different styles, but you can also
# supply a custom URL to a 
# [custom mapbox style](https://docs.mapbox.com/help/tutorials/create-a-custom-style/).

# To obtain all the pre-packaged basemap style names, you can grab them from the official
# plotly.js `schema()`:

styles <- schema()$layout$layoutAttributes$mapbox$style$values
styles


# Any one of these values can be used for a mapbox style.

# Example:
layout(
  plot_mapbox(), 
  mapbox = list(style = "satellite")
)


# Question: What is curious about this image?


# We can create an integrated plotly.js dropdown menu to control the
# basemap style via the 
# [`layout.updatemenus`](https://plot.ly/r/reference/#layout-updatemenus-items-updatemenu-buttons) 
# attribute. 

# The idea behind an integrated plotly.js dropdown is to supply a list of buttons
# (i.e., menu items) where each button invokes a plotly.js method with some arguments.
# In this case, each button uses the
# [relayout](https://plot.ly/javascript/plotlyjs-function-reference/) 
# method to modify the `layout.mapbox.style` attribute.

# To see more examples of creating and using plotly.js's integrated dropdown
# functionality to modify graphs, see <https://plot.ly/r/dropdowns/>

style_buttons <- 
  map(styles, function(s) {
    list(
      label = s, 
      method = "relayout", 
      args = list("mapbox.style", s)
    )
  })

plot_mapbox() %>%
  layout(
    mapbox = list(style = "dark"),
    updatemenus = list(
      list(y = 0.8, buttons = style_buttons)
    )
  )


# plot_geo() -----

# The other integrated mapping solution in **plotly** is `plot_geo()`.
# Compared to `plot_mapbox()`, this approach has support for different mapping
# projections, but styling the basemap is limited and can be more cumbersome.

# Example: using `plot_geo()` in conjunction with `add_markers()` and `add_segments()`
# to visualize flight paths within the United States. 

# Whereas `plot_mapbox()` is fixed to a mercator projection, the `plot_geo()` constructor
# has a handful of different projections available to it, including the orthographic
# projection which gives the illusion of the 3D globe.

# Airport locations:
air <- readRDS("airport_locations.rds")

# Flights paths
flights <- readRDS("flights.rds")

# Map projection:
geo <- 
  list(
    projection = list(
      type = ""#,
      # rotation = list(lon = "", lat = "", roll = 0)
    ),
    showland = TRUE,
    landcolor = toRGB("gray95"),
)

plot_geo(color = I("red")) %>%
  add_segments(
    data = flights,
    x = "",
    xend = "",
    y = "",
    yend = "",
    alpha = 0.3,
    size = I(1),
    # hoverinfo = ""
  ) %>%
  add_markers(
    data = air,
    x = "",
    y = "",
    # text = "",
    size = ~cnt,
    # hoverinfo = "",
    alpha = 0.5
  ) %>%
  layout(geo = geo, showlegend = FALSE)
# ```

# Exercise:
# 1. What is the role of `rotation`?
# 2. Why do we need to change `hoverinfo` for the segments trace?

# i.  Use <https://plotly.com/r/reference/layout/geo/> to find how to add borders
#     between countries in gray.
# ii. set two line colors: green(ish) for flights that are shorter than 1000 miles, blue(ish)
#      for longer flights. 

#      Hint, to convert lat/long to miles, you may use the following
#      formula:

# ```
#          acos(
#            sin(pi * start_lat / 180) * sin(pi * end_lat / 180) +
#            cos(pi * start_lat / 180) * cos(pi * end_lat / 180) *
#            cos(pi * start_lon / 180 - pi * end_lon / 180)
#          ) * 3963
# ```

# iii. Show the legend for the long/short flights (but not for the airports). 
#      Set meaningful labels for this legend.

# Answer:






# `plot_geo()` automatically projects geometries into the proper coordinate system
# defined by the map projection. 

# Example:
# The simple line segment is straight when using `plot_mapbox()`,
# yet curved when using `plot_geo()`. 

# It's possible to achieve the same effect using `plot_ly()` or `plot_mapbox()`,
# but the relevant marker/line/polygon data has to be put into an **sf** data
# structure before rendering.

plot_mapbox() %>% 
  add_segments(x = -100, xend = -50, y = 50, yend = 75) %>%
  layout(
    mapbox = list(
      zoom = 0,
      center = list(lat = 65, lon = -75)
    )
  )

# Compare to:
plot_geo() %>% 
  add_segments(x = -100, xend = -50, y = 50, yend = 75) %>%
  layout(geo = list(projection = list(type = "mercator")))

# And to:
plot_geo() %>% 
  add_segments(x = -100, xend = -50, y = 50, yend = 75) %>%
  layout(geo = list(projection = list(type = "orthographic")))


### Choropleths -----

# Both `plot_mapbox()` and `plot_geo()` have an optimized choropleth trace type
# (i.e.,  the [choroplethmapbox](https://plot.ly/r/reference/#choroplethmapbox) and
# [choropleth](https://plot.ly/r/reference/#choropleth) trace types). 

# Comparatively speaking, choroplethmapbox is more powerful because you can fully specify
# the feature collection using GeoJSON, but the choropleth trace can be a bit easier to
# use if it fits your use case.

# Example:
# Population density of the U.S. via the choropleth trace using the U.S. state data from
# the **datasets** package. 

# By simply providing a [`z`](https://plot.ly/r/reference/#choropleth-z) attribute,
# `plotly_geo()` objects will try to create a choropleth, but you'll also need to provide
# [`locations`](https://plot.ly/r/reference/#choropleth-locations) and a
# [`locationmode`](https://plot.ly/r/reference/#choropleth-locationmode).

# `locationmode` is currently limited to countries and US states, so if you need to plot
# a different geo-unit (e.g., counties, municipalities, etc.), you should use the
# choroplethmapbox trace type and/or use a custom mapping approach.

density <- state.x77[, "Population"] / state.x77[, "Area"]

g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  lakecolor = toRGB('white')
)

plot_geo() %>%
  add_trace(
    z = ~density,
    text = state.name,
    span = I(0),
    locations = state.abb,
    locationmode = 'USA-states'
  ) %>%
  layout(geo = g)


# The above isn't an ideal way to visualize state population a graphical perception
# point of view. We typically use the color in choropleths to encode a numeric variable
# (e.g., GDP, net exports, average SAT score, etc.) and the eye naturally perceives the
# area that a particular color covers as proportional to its overall effect. This ends
# up being misleading since the area the color covers typically has no sensible relationship
# with the data encoded by the color. 

# **Cartograms** are an approach to reducing this misleading effect and grant another
# dimension to encode data through the size of geo-spatial features.

## Custom maps -----

### Simple features (sf) -----

# The **sf** R package is a modern approach to working with geo-spatial data structures based
# on tidy data principles. 

# The key idea behind **sf** is that it stores geo-spatial geometries in a 
# [list-column](https://jennybc.github.io/purrr-tutorial/ls13_list-columns.html) of a data frame.
# This allows each row to represent the real unit of observation/interest --- whether it's a
# polygon, multi-polygon, point, line, or even a collection of these features --- and as a result,
# works seamlessly inside larger tidy workflows.

# The **sf** package itself does not really provide geo-spatial data; it provides the framework
# and utilities for storing and computing on geo-spatial data structures in an opinionated way.

# There are numerous packages for accessing geo-spatial data as simple features data structures.
# A couple of notable examples include **rnaturalearth** and **USAboundaries**.

# - The **rnaturalearth** package is better for obtaining any map data in the world via an API
#   provided by <https://www.naturalearthdata.com/>.

# - The **USAboundaries** package is great for obtaining map data for the United States at any
#   point in history. It doesn't really matter what tool you use to obtain or create an **sf**
#   object; once you have one, `plot_ly()` knows how to render it:

library(rnaturalearth)
world <- ne_countries(returnclass = "sf")
class(world)


# How does `plot_ly()` know how to render the countries? 
# It's because the geo-spatial features are encoded in special (geometry) list-column.
# Also, meta-data about the geo-spatial structure are retained as special attributes of the data.

# Note that **sf** provides special **dplyr** methods for this special class of data
# frame so that you can treat data manipulation as if it were a 'tidy' data structure.

# One thing about this method is that the special 'geometry' column is always retained;
# if we try to just select the `name` column, then we get both the name and the geometry.

str(world)
world$geometry
plot_ly(world, color = I("gray90"), stroke = I("black"), span = I(1))


library(sf)
world %>%
  select(name, formal_en) %>%
  print(n = 4)


# There are 4 different ways to render **sf** objects with **plotly**:
# `plot_ly()`, `plot_mapbox()`, `plot_geo()`, and via **ggplot2**'s `geom_sf()`.

# These functions render multiple polygons using a *single* trace by default,
# which is fast, but you may want to leverage the added flexibility of multiple traces.

# Example:

# devtools::install_github("https://github.com/ropensci/rnaturalearthhires/")
library(rnaturalearthhires)
canada <- ne_states(country = "Canada", returnclass = "sf")
plot_ly(canada)

# And we can then:
plot_ly(canada, color = , colors = "")


# An important feature for maps is that it may require you to `split` multiple polygons
# into multiple traces in order to display a different hover-on-fill for each polygon
# (the default behavior is to plot all polygons in a single trace).

# Example:
plot_ly(
  canada, 
  split = "",
  color = "",
  # text = "",
  # hoverinfo = "text",
  showlegend = FALSE
)


# Although the integrated mapping approaches (`plot_mapbox()` and `plot_geo()`) can
# render **sf** objects, the custom mapping approaches (`plot_ly()` and `geom_sf()`)
# are more flexible because they allow for any well-defined mapping projection.


### Cartograms -----

# Cartograms distort the size of geo-spatial polygons to encode a numeric variable other
# than the land size. There are numerous types of cartograms and they are typically categorized
# by their ability to preserve shape and maintain contiguous regions. 
 
# The R package **cartogram** provides an interface to several popular cartogram algorithms.
# A number of other R packages provide cartogram algorithms, but the great thing about
# **cartogram** is that all the functions can take an **sf** (or **sp**) object as input
# and return an **sf** object.

# Example:
# a **continuous area** cartogram of US population in 2014 using a rubber sheet distortion
# algorithm.

library(cartogram)
library(sf)
library(spData)
data(us_states)

us_cont <- 
  us_states %>%
  st_transform(crs = 3857) %>%
  cartogram_cont(weight = "total_pop_15") %>%
  st_cast(to = "MULTIPOLYGON")

plot_ly(us_cont)

plot_ly(us_cont) %>% 
  add_sf(
    color = "",
  ) %>%
  layout(showlegend = FALSE) %>%
  colorbar(title = "Population \n 2015") #%>%
  # config("")

# Exercise:
# Assign a value that is 10 or 15 
# to the variable `yr`:
yr <- 15
# And modify the above code so that the data is selected from the year `yr`.

# Hint:
# if you want to create a formula like 
# ~abc18
# where 18 is the value of the variable `v`, you can use the `as.formula()` function:
v <- 18
as.formula(paste0("~abc", v))

# Answer:





# Example:
# a non-continuous Dorling cartogram of US population in 2014.
# This cartogram does not try to preserve the shape of polygons (i.e., states),
# but instead uses circles to represent each geo-spatial object, then encodes
# the variable of interest (i.e., population) using the area of the circle. 


us <- us_states %>% st_transform(crs = 3857)
us_dor <- 
  cartogram_dorling(us, "total_pop_15") %>%
  st_cast(to = "MULTIPOLYGON")

plot_ly(stroke = I("black"), span = I(1)) %>% 
  add_sf(
    data = us, 
    color = I("gray95"),
    hoverinfo = "none"
  ) %>%
  add_sf(
    data = us_dor, 
    color = ~total_pop_15,
    split = ~NAME, 
    text = ~paste(NAME, total_pop_15), 
    hoverinfo = "text", 
    hoveron = "fills"
  ) %>%
  layout(showlegend = FALSE) %>%
  config(scrollZoom = TRUE)


# Example:
# a non-continuous cartogram of the U.S. population in 2014 from Olson. 
# In contrast to the Dorling cartogram, this approach does preserve the
# shape of polygons. 

us <- us_states %>% st_transform(crs = 3857)
us_dor <- 
  cartogram_ncont(us, "total_pop_15") %>%
  st_cast(to = "MULTIPOLYGON")

plot_ly(stroke = I("black"), span = I(1)) %>% 
  add_sf(
    data = us, 
    color = I("gray95"),
    hoverinfo = "none"
  ) %>%
  add_sf(
    data = us_dor, 
    color = ~total_pop_15,
    split = ~NAME, 
    text = ~paste(NAME, total_pop_15), 
    hoverinfo = "text", 
    hoveron = "fills"
  ) %>%
  layout(showlegend = FALSE) %>%
  config(scrollZoom = TRUE)


# Exercise: 
# What would be the best way to represent each of the following:
# GDP for each state
# GDP per capita for each state
# Gini coefficient for each state
# Number of deaths from COVID-19 for each state
# Number of electors for each state
