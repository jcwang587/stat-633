# Lecture 15 - Parallel Coordinate Plot, Radar, Clustering -----

# Parallel coordinates provide a way to compare values along a common (or
# non-aligned) positional scale(s) - the most basic of all perceptual tasks - in
# more than 3 dimensions.

# Usually each line represents every measurement for a given row (or 
# observation) in a data set.

# Method 1 - `ggparcoord` -----
View(iris[sample(1:nrow(iris)) , 1:4])

library(plotly)
library(GGally)

(gg1 <-
  ggparcoord(
    iris,
    columns = 1:4, 
    showPoints = TRUE,
    title = "Parallel Coordinate Plot for the Iris Data",
    alphaLines = 0.3  ) +
    theme_bw() +
    theme(legend.position = "top"))

ggplotly(gg1)

# Let's compare this to some "null" where all lines are drawn from a Uniform(0, 1) distribution:
null_df <- as.data.frame(matrix(""))
(gg0 <-
    ggparcoord(
      null_df,
      columns = 1:4, 
      showPoints = TRUE,
      title = "Parallel Coordinate Plot for the Iris Data",
      alphaLines = 0.3  ) +
    theme_bw() +
    theme(legend.position = "top"))


# Let's compare the two to null where the columnas are drawn from:
# 1. unifrom, 2. normal, 3. skew-normal, 4. t
null_df2 <- 
  tibble(
    u = "", 
    norm = "", 
    sn = "",
    t = ""
  )
(gg02 <-
    ggparcoord(
      null_df2,
      columns = 1:4, 
      showPoints = TRUE,
      title = "Parallel Coordinate Plot for the Iris Data",
      alphaLines = 0.3  ) +
    theme_bw() +
    theme(legend.position = "top"))



# Points to consider are:
# - y-axis Scaling (for joint y-axis; or ranges for independent y-axes)
# Example: 0-1 univariate scaling

# - Order of variables

# Add a clustering variable, based on your favorite clustering algorithm:
df <- iris
d <- ""
hc_tree <- ""
df$hc_clusters <- ""

(gg2 <-
    ggparcoord(
      df,
      columns = 1:4,
      groupColumn = "",
      order = "", 
      showPoints = TRUE,
      title = "Parallel Coordinate Plot for the Iris Data",
      alphaLines = 0.3  ) +
    theme_bw() +
    theme(legend.position = "top"))

ggplotly(gg2)

# Compare to the true labels:
(gg3 <-
  ggparcoord(
    iris,
    columns = 1:4,
    groupColumn = "",
    order = "",
    showPoints = TRUE,
    title = "Parallel Coordinate Plot for the Iris Data",
    alphaLines = 0.3  ) +
    theme_bw() +
    theme(legend.position = "top"))

ggplotly(gg3)

subplot(gg2, gg3)

# "anyClass": 
# order variables by their separation between any one class and the
# rest (as opposed to their overall variation between classes). This
# is accomplished by calculating the F-statistic for each class vs. the
# rest, for each axis variable. The axis variables are then ordered
# (decreasing) by their maximum of k F-statistics, where k is the number of classes.

# Method 2 - plotly's `parcoords` -----
# https://plotly.com/r/parallel-coordinates-plot/

iris %>% 
  plot_ly(
    type = 'parcoords',
    dimensions = 
      list(
        list(values = ~Sepal.Width),
        list(values = ~Sepal.Length),
        list(values = ~Petal.Width),
        list(values = ~Petal.Length)
      )
  )

iris %>% 
  plot_ly(
    type = 'parcoords',
    line = 
      list(
        color = "" #,
        # colorscale =
        #   list(
        #     "",
        #     "",
        #     ""
        #     )
        ),
    dimensions = 
      list(
        list(range = c(2,4.5),
             label = 'Sepal Width', values = ~Sepal.Width),
        list(range = c(4,8),
             label = 'Sepal Length', values = ~Sepal.Length),
        list(range = c(0,2.5),
             label = 'Petal Width', values = ~Petal.Width),
        list(range = c(1,7),
             label = 'Petal Length', values = ~Petal.Length)
        )
)


# Method 3 - as a plotly line trace ----- 

# Example that shows different scaling methods:
df <- iris
tidyr::gather(df, variable, value, -Species, -obs) %>% 
  group_by(obs) %>%
  plot_ly(x = ~variable, y = ~value, color = ~Species) %>% 
  add_lines(alpha = 0.3)

# Play with different scaling methods:
df <- iris
df$obs <- seq_len(nrow(df))

iris_pcp <- function(transform = identity) {
  df[] <- purrr::map_if(df, is.numeric, transform)
  tidyr::gather(df, variable, value, -Species, -obs) %>% 
    group_by(obs) %>% 
    plot_ly(x = ~variable, y = ~value, color = ~Species) %>% 
    add_lines(alpha = 0.3)
}

subplot(
  iris_pcp(), 
  iris_pcp(scale), # subtract sample mean, divide by sample sd
  iris_pcp(scales::rescale), # all have same min and max
  nrows = 3, shareX = TRUE
) %>% hide_legend()


# Radar charts -----

# Are basically parallel coordinate plots that are folded around a point.

# Method 1 - `ggradar()` -----
library(ggiraphExtra)
ggRadar(
  data = iris[1, ],
  alpha = 0.3,
  rescale = FALSE
  )

# If we try grouping by species we get an average for each species:
ggRadar(
  data = iris,
  aes(group = Species),
  alpha = 0.3,
  rescale = FALSE
  )

# Method 2 - plotly's `scatterpolar` -----
plot_ly(
  type = 'scatterpolar',
  r = "",  # values
  theta = "", # labels
  fill = 'toself',
  mode = "markers"
) %>%
  layout(
    polar = list(
      radialaxis = list(
        visible = T
      )
    ),
    showlegend = F
  )

# Can add trace over trace:
plot_ly(
  type = 'scatterpolar',
  fill = 'toself'
) %>%
  add_trace(
    r = unname(unlist(iris[1, 1:4])),
    theta = colnames(iris)[-5],
    name = 'Obs. 1'
  ) %>%
  add_trace(
    r = unname(unlist(iris[150, 1:4])),
    theta = colnames(iris)[-5],
    name = 'Obs. 150'
  ) %>%
  layout(
    polar = list(
      radialaxis = list(
        visible = T
        )
      )
    )

# Of course, stacking too many layers on top of each other can lead to illegible 
# charts.

# Multiple radar plots and clustering -----

# Multiple radar charts using `stars`:
df <-
  iris %>%
  mutate(idx = 1:nrow(iris)) %>%
  slice_sample(n = 150)

par(mar = c(0, 0, 0, 0))
stars(df[ , 1:4], draw.segments = F, nrow = 10)
# Add color by true labels:
stars(df[ , 1:4], draw.segments = F, nrow = 10, "")

# Multiple radar charts organized by a similarity measure:
b <- 1
base <- 
df$sim_measure <- ""
df_ordered <- ""

# How well did we do?
# Raw results:
stars(df_ordered[ , 1:4], draw.segments = F, nrow = 15, cex = 0.6)
# Add color by true labels:
stars(df_ordered[ , 1:4], draw.segments = F, nrow = 15, col.stars = df_ordered[ , 5], cex = 0.6)



# Clustering with Heatmaps and dendograms -----
library(heatmaply)
heatmaply(scale(iris[ , 1:4]), k_row = 3, k_col = 1)
heatmaply(scale(iris[ , 1:4]), k_row = 3, k_col = 1, show_dendrogram = c(TRUE, FALSE))
