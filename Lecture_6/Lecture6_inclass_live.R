# More on Continuous Distributions -----

# ** Box Plots -----

par(mar = c(0, 0, 0, 0))
plot(imager::load.image("boxplot.png"), axes = FALSE)

library(tidyverse)

set.seed(1)
n_vec <- c(10, 20, 30)
df <- 
  tibble(
    s = rnorm(sum(n_vec)),
    g = rep(as.character(1:length(n_vec)), times  = n_vec)
    )

# Base R:
par(mar = c(5, 5, 5, 5))
bp0 <- boxplot(s ~ g, df)

# To get a sense of the number of observations per group:
boxplot(s ~ g, df, varwidth = TRUE)

# Or slightly fancier:
bp <- 
  boxplot(
    s ~ g,
    data = df,
    col = unique(as.integer(df$g) + 1)
  )

# In ggplot:
(gg_bp <- 
    ggplot(df) +
    geom_boxplot(aes(x = g, y = s)))

(gg_bp <- 
    ggplot(df) +
    geom_boxplot(aes(x = s, y = g)))

# In plotly:
library(plotly)
ggplotly(gg_bp)

# or:
plot_ly(df) %>% add_boxplot(y = ~s, color = ~g)


# Exercise:
# Guess before trying:
# How do you expect the boxplots will look like with
# 1. n_vec = c(10, 10, 10, 10)
# 2. n_vec = c(1000, 1000, 1000, 1000)
# 3. n_vec = c(1000, 1000, 10000, 10000)
# Explain to yourself whether or not your guesses were correct, and why.

# Answer:

# 1. c(10, 10, 10, 10)
# Saw: plenty of variation in all sumarry statistics

# 2. c(1000, 1000, 1000, 1000)
# Saw: as sample size grows, boxplots are much more "similar" across all the summary statistics

# 3. c(1000, 1000, 10000, 10000)
# Saw: 

set.seed(1)
n_vec <- c(1000, 1000, 10000, 10000)
df <- 
  tibble(
    s = rnorm(sum(n_vec)),
    g = rep(as.character(1:length(n_vec)), times  = n_vec)
  )

plot_ly(df) %>% add_boxplot(y = ~s, color = ~g)


# Exercise:

# Compute the expected number of lower and upper "outliers" for:
# 1. a standard normal distribution
# 2. a student-t distribution with 1 degree of freedom
# 3. a student-t distribution with 10 degrees of freedom
# 4. a skew normal distribution with skew parameter 1
# 5. a skew normal distribution with skew parameter 10
# 6. a standard uniform distribution
# (guess what the results might look like before computing the results)

# Answer:
expected_n_outlier_norm <- function(s, mean = 0, sd = 1, fctr = 1.5) {
  q3 <- qnorm(0.75, mean, sd)
  q1 <- qnorm(0.25, mean, sd)
  iqr <- q3 - q1
  prob_upper_outlier <- pnorm(q3 + fctr * iqr, mean, sd, lower.tail = FALSE)
  prob_lower_outlier <- pnorm(q1 - fctr * iqr, mean, sd, lower.tail = TRUE)
  return(s * c(prob_lower_outlier, prob_upper_outlier))
}

expected_n_outlier_norm(1000)
expected_n_outlier_norm(10000)

# Ellipsis -----

plot(x = c(1, 2), y = c(1,2))

example_fn <- function(x, y, ...) {
  plot(x, y, ...)
}


example_fn(x = 4, y = 5, pch = 20, main = "my title", cex = 10)


# More general code to handle all distributions:
expected_n_outlier <- function(dist = "norm", s, fctr = 1.5, ...) {
  q3 <- do.call(paste0("q", dist), list(p = 0.75, ...))
  q1 <- do.call(paste0("q", dist), list(p = 0.25, ...))
  iqr <- q3 - q1
  prob_upper_outlier <- 
    do.call(
      paste0("p", dist), 
      list(q3 + fctr * iqr, ...)
    )
  prob_lower_outlier <- 
    do.call(
      paste0("p", dist), 
      list(q1 - fctr * iqr, ...)
    )
  return(s * c(prob_lower_outlier, 1 - prob_upper_outlier))
}

# do.call("plot", list(x = c(1, 2), y = c(1, 2)))
# plot(x = c(1, 2), y = c(1, 2))

expected_n_outlier("norm", 1000)
expected_n_outlier_norm(1000)

expected_n_outlier("norm", 10000)

expected_n_outlier("t", 1000, df = 1)
expected_n_outlier("t", 1000, df = 10)
expected_n_outlier("t", 1000, df = 32)

expected_n_outlier("t", 10000, df = 1)
expected_n_outlier("t", 10000, df = 10)

library(sn)
expected_n_outlier("sn", 1000, alpha = 1)
expected_n_outlier("sn", 10000, alpha = 1)

expected_n_outlier("sn", 1000, alpha = -1)
expected_n_outlier("sn", 10000, alpha = -1)

expected_n_outlier("sn", 1000, alpha = 10)
expected_n_outlier("sn", 10000, alpha = 10)

expected_n_outlier("unif", 1000)
expected_n_outlier("unif", 10000)


# Exercise:

# Compare the expected number of outliers to some observed
# number of outliers for different settings.

# Answer:

# Play with:
expected_n_outlier("norm", 1000)
# vs.
smpl <- rnorm(1000)
# And then you can use `boxplot.stats()` or compute the number manually.

length(boxplot.stats(smpl)$out)


# A simulation study:
S <- 10000 # even number
N <- 100 # number of observations to draw
fctr <- 1.5
# More thorough:
S <- 10000 # even number
N <- 100
fctr <- 1.5
n <- 
  t(
    replicate(
      S / 2,
      {
        s <- rnorm(N)
        # s <- rt(N, df = 1)
        q1 <- quantile(s, 0.25, type = 8)
        q3 <- quantile(s, 0.75, type = 8)
        iqr <- q3 - q1 
        c(
          sum(s < (q1 - fctr * iqr)),
          sum(s > (q3 + fctr * iqr))
        )
      }
    )
  )
colMeans(n)

expected_n_outlier("norm", N, fctr = fctr)

expected_n_outlier("norm", N, fctr = fctr)


# As you can see, the estimation is not perfect. This is because of several factors: 

# 1. The empirical quantiles are estimators of the exact quantiles. However,
#    they are only **approximately** unbiased (see ?quantile for more details)
#    and are more biased away from the median. See:

fctr <- 1.5
mean(
  replicate(
    1000,
    {
      s <- rnorm(10); # try with larger
      
    }
  )
)

qnorm(0.75) + fctr * (qnorm(0.75) - qnorm(0.25))


# 2. We are trying to capture extreme events (observations that are out in the 
#    tails of the distribution) which increases the sensitivity to the accuracy
#    of the estimates. 

# 3. In addition, sampling methods (`rnorm`, `rt` etc.) may
#    also be less accurate in the tails.
100 * (1 - pnorm(3))
mean(replicate(100000, ""))


# ** Histograms -----
# A histogram is an approximate representation of the density of the distribution 
# underlying observed data.

smpl <- rnorm(500)
hist(smpl, freq = FALSE)
lines(seq(-4, 4, 0.01), dnorm(seq(-4, 4, 0.01)))

# The single most crucial parameter that determines a histogram is the width of a
# bin (which is equivalent, e.g., to the number of breaks). With a small enough
# width of bins, truly continuous data (where no point is exactly equal to any other
# point), may always be represented as a collection of bars of the same height
# (1 if the histogram is not normalized to a density):
set.seed(2)
smpl <- rnorm(10)
hist(smpl)
hist(smpl, breaks = 10)
segments(x0 = smpl, y0 = 0, y1 = 0.6, col = "red")

hist(smpl, breaks = 45)
segments(x0 = smpl, y0 = 0, y1 = 0.2, col = "red")

smpl <- rnorm(200)
hist(smpl, breaks = 10)
segments(x0 = smpl, y0 = 0, y1 = 6, col = "red")

hist(smpl, breaks = 20000)
segments(x0 = smpl, y0 = 0, y1 = 0.4, col = "red")

# In many cases, continuous observations are rounded and so we get identical values
# for different observations.

# We discussed various technical visualzation techniques of histograms at some length
# in lectures.


# ** Density Plots -----
# Density plots (e.g. base R `density()` and `plot(density())`, `ggplot`s
# `geom_density()`) construct a function that estimates the unknown underlying density 
# of continuous data. This is done by fitting an average of "kernel" densities 
# to the data. KDE = Kernel Density Estimator.

hist(smpl)
plot(density(smpl), add = TRUE)

# Each point on the x-axis is assigned with a kernel, those are summed up to generate
# the overall estimator.

# The most common kernel is the normal kernel.

# Principle:

# Individual kernels at three locations:
loc = c(-2, -1, 2)
x = seq(-5, 5, 0.01)

plot(0, 0, xlim = c(-5, 5), ylim=c(0, 0.8), type = "l", ylab = "", xlab = "")
for(i in 1:length(loc)) {
  points(loc[i], 0, pch = "X", col = 2)
  lines(x, dnorm(x, mean = loc[i]), lty = 2)  # var = 1 ~ bandwidth = 1
  lines(x, dnorm(x, mean = loc[i], sd = sqrt(0.3)), col = "orange", lty = 2)  # var = 0.3 ~ bandwidth = 0.3
}
abline(h = 0)


# Sum the kernels:
d = numeric(length(x))
d2 = numeric(length(x))
for(i in 1:length(d)) {
  for(j in 1:length(loc)) {
    d[i] = d[i] + dnorm(x[i], mean = loc[j])
    d2[i] = d2[i] + dnorm(x[i], mean = loc[j], sqrt(0.3))
  }
}

# And plot those:
lines(x, d, col = 1)
lines(x, d2, col = "orange")


# Other kernels:
(kernels <- eval(formals(density.default)$kernel))

plot (density(0, bw = 1), xlab = "",
      main = "R's density() kernels with bw = 1")
for(i in 2:length(kernels))
  lines(density(0, bw = 1, kernel =  kernels[i]), col = i)
legend(1.5,.4, legend = kernels, col = seq(kernels),
       lty = 1, cex = .8, y.intersp = 1)

# The `density()` function in R computes a density estimate, and `plot` has a method for it.

x_range <- seq(-4, 4, 0.01)
plot(x_range, dnorm(x_range), type = "l")
smpl <- rnorm(100)
hist(smpl, freq = FALSE, col = rgb(0, 0, 0, 0.05), border = rgb(0, 0, 0, 0.3), add = TRUE)
d <- density(smpl)
lines(d, col = "red")


# The most important parameter for kernel density estimators is the "bandwidth",
# which serves as a smoothing parameter - the smaller the bandwidth, the more potentially
# sensitive the estimator is:

x_range <- seq(-4, 4, 0.01)
plot(x_range, dnorm(x_range), type = "l")
smpl <- rnorm(100)
hist(smpl, freq = FALSE, col = rgb(0, 0, 0, 0.05), border = rgb(0, 0, 0, 0.3), add = TRUE)
d1 <- density(smpl, bw = 0.1)
lines(d1, col = "orange", lwd = 1.5)
d2 <- density(smpl, bw = 1)
lines(d2, col = "darkgreen", lwd = 1.5)
d3 <- density(smpl, bw = 10)
lines(d3, col = "blue", lwd = 1.5)
# If we don't specify the bandwidth, the `desnity` function optimizes it for us:
d4 <- density(smpl)
d4$bw
lines(d4, col = "red", lwd = 1.5)


# Playing with both kernel and bandwidth:
# If, for example, our data had just five observations:
x <- c(-3, -1, 0, 1, 3)

plot_density <- function(x, kernel, bw) {
  plot(density(x, kernel = kernel, bw = bw))
  points(x, rep(0, length(x)), pch = "X", col = "red")
  segments(x0 = x, y0 = 0, y = 3, lty = 2, col = "red")
}

plot_density(x, kernel = "gaussian", bw = 0.5)
plot_density(x, kernel = "rectangular", bw = 0.5)
plot_density(x, kernel = "triangular", bw = 0.5)

plot_density(x, kernel = "gaussian", bw = 2)
plot_density(x, kernel = "rectangular", bw = 2)
plot_density(x, kernel = "triangular", bw = 2)

plot_density(x, kernel = "gaussian", bw = "nrd0") 
# "nrd0" is the default optimization method `density()` uses for bandwidth selection
plot_density(x, kernel = "rectangular", bw = "nrd0")
plot_density(x, kernel = "triangular", bw = "nrd0")

# And in ggplot (the computation also relies on `density`)
df <- tibble(x = rnorm(1000))
ggplot(df) + geom_density(aes(x = x))


# ** Quantile plots -----

n <- 10
smpl <- rnorm(n)

# Compute f-values manually and use base R:
smpl_sorted <- sort(smpl)

f_val <- (1:n - 0.5) / n
df <- tibble(smpl = smpl_sorted, f_val = f_val)
plot(df)
plot(df$f_val, df$smpl)

# Or using ggplot:
ggplot(df, aes(sample = smpl)) + stat_qq(distribution = qunif) + xlab("f-value")
# (in effect, we are comparing the empirical quantiles to the quantiles of a
# uniform distirbution)


# We can also draw the empirical CDF of the data
ggplot(df, aes(smpl)) + stat_ecdf(geom = "point")

# The quantile plot and the empirical cumulative distribution function (ECDF)
# provide an alternative visualisation of distribution. Compared to other visualisations
# that rely on density (like histograms or density plots), the quantile plot and ECDF
# don't require any tuning parameters and handle both continuous and categorical
# variables. 

# The downside is that they require more training to accurately interpret, and the
# underlying visual tasks are somewhat more challenging.

hist(rnorm(100))
# ** Comparison of plots -----

# Properties of continuous univariate distributions:
# - Location of median / quantiles: boxplots, quantile
# - Location of mean, size of standard deviation: 
#     boxplot (yes for sd, can add mean), histogram (mean so so, sd)
# - Symmetry / Skewness
# - Heavy tails
# - Multimodality
#   For generating a multimodal distribution, you can use a mixture model:
#   With probability p sample from one distribution, with probability 1 - p
#   sample from a second distribution. For example:
p <- 0.3
N <- 100
n1 <- rbinom(1, N, prob = p)
df <- tibble(s = c(rsn(n1, xi = 5, alpha = 9), rt(N - n1, df = 10)))

plot(df$s, rep(0, N), pch = "X", col = "red")


p0 <- plot_ly(df) %>% add_boxplot(y = ~s)
p1 <- plot_ly(df) %>% add_histogram(x = ~s)
g2 <- 
  ggplot(df, aes(x = s)) +
  geom_density() 
p2 <- ggplotly(g2)
g3 <- ggplot(df, aes(sample = s)) + stat_qq(distribution = qunif) + xlab("f-value")
p3 <- ggplotly(g3)

subplot(p0, p1, p2, p3)

# Exercise:
# Explore different sample sizes and properties of distributions, comment about
# which visualization works best for each property, sample size, and scenario 
# (e.g. very skewed, moderately heavy tails). Think about ways to improve each 
# such basic visualization so that it may better reflect the different properties.

# - Location of median / quantiles
# - Location of mean, size of standard deviation
# - Symmetry / Skewness
# - Heavy tails

# - Symmetry / Skewness
df <- tibble(s = rsn(1000, xi = 5, alpha = 9))
df <- tibble(s = rnorm(1000))

p0 <- plot_ly(df) %>% add_boxplot(y = ~s)
p1 <- plot_ly(df) %>% add_histogram(x = ~s)
g2 <- 
  ggplot(df, aes(x = s)) +
  geom_density() 
p2 <- ggplotly(g2)
g3 <- ggplot(df, aes(sample = s)) + stat_qq(distribution = qunif) + xlab("f-value")
p3 <- ggplotly(g3)

subplot(p0, p1, p2, p3)

# - Location of mean, size of standard deviation: 
#     boxplot (yes for sd, can add mean), histogram (mean so so, sd)

p0 <- plot_ly(df) %>% add_boxplot(y = ~s)
p1 <- plot_ly(df) %>% add_histogram(x = ~s)
g2 <- 
  ggplot(df, aes(x = s)) +
  geom_density() 
p2 <- ggplotly(g2)
g3 <- ggplot(df, aes(sample = s)) + stat_qq(distribution = qunif) + xlab("f-value")
p3 <- ggplotly(g3)

subplot(p0, p1, p2, p3)

# - Symmetry / Skewness

p0 <- plot_ly(df) %>% add_boxplot(y = ~s)
p1 <- plot_ly(df) %>% add_histogram(x = ~s)
g2 <- 
  ggplot(df, aes(x = s)) +
  geom_density() 
p2 <- ggplotly(g2)
g3 <- ggplot(df, aes(sample = s)) + stat_qq(distribution = qunif) + xlab("f-value")
p3 <- ggplotly(g3)

subplot(p0, p1, p2, p3)


# - Heavy tails
# - Symmetry / Skewness
df <- tibble(s = rt(1000, df = 8))
df <- tibble(s = rnorm(1000))


p0 <- plot_ly(df) %>% add_boxplot(y = ~s)
p1 <- plot_ly(df) %>% add_histogram(x = ~s)
g2 <- 
  ggplot(df, aes(x = s)) +
  geom_density() 
p2 <- ggplotly(g2)
g3 <- ggplot(df, aes(sample = s)) + stat_qq(distribution = qunif) + xlab("f-value")
p3 <- ggplotly(g3)

subplot(p0, p1, p2, p3)


# ** Some hybrid plots -----

# *** Violin plots -----

# Violin plots combine density plots and boxplots in an attempt
# to offer both the concise summary and some more detail about 
# the shape of the distribution:

df %>% 
  plot_ly(
    y = ~s, 
    type = 'violin', 
    box = list(visible = TRUE),
    meanline = list(visible = TRUE)
    ) %>%
    layout(yaxis = list(title = "", zeroline = F))


# *** Density plots with ECDF fill -----

# With median in dark blue and tail probabilities in yellow:
library(ggridges)
ggplot(df, aes(x = s, y = "a", fill = 0.5 - abs(0.5 - stat(ecdf)))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE) +
  scale_fill_viridis_c(name = "Tail probability", direction = -1)

# With probabilities progressing from zero to 1:
library(ggridges)
ggplot(df, aes(x = s, y = "a", fill = stat(ecdf))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE) +
  scale_fill_viridis_c(name = "Tail probability", direction = -1)

# Or with a gradient, centered at he median:
ggplot(df, aes(x = s, y = "a", fill = stat(ecdf))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE) +
  scale_fill_gradient2(
    low = "darkred",
    mid = "white",
    high = "darkblue",
    midpoint = 0.5,
    space = "Lab",
  )

# *** Adding jitter to boxplot (like) plots
ggplot(df, aes(x = 1, y = s)) + 
  geom_boxplot(aes(y = s)) +
  ggbeeswarm::geom_quasirandom() +
  geom_violin(width = 1.1, alpha = 0.5)

# This is also useful when you want to show the relative size of the samples:

n_vec <- c(10, 100, 200)
df <- 
  tibble(
    s = rnorm(sum(n_vec)),
    g = rep(as.character(1:length(n_vec)), times  = n_vec)
  )

ggplot(df, aes(x = g, y = s)) + 
  geom_boxplot(aes(y = s)) +
  ggbeeswarm::geom_quasirandom() +
  geom_violin(width = 1.1, alpha = 0.5)

  
# And many more options:
# https://exts.ggplot2.tidyverse.org/gallery/

# This last point leads us to:

# ** Comparing distributions -----

# Utilizing the above methods, there are multiple ways to compare
# distributions to one another.
# Some considerations when comparing distributions:
# - Which relative properties are you most intereted in highlighting?
# - How many distributions you wish to compare?
#     Showing 50 boxplots side by side may be reasonable, but showing 50 violin plots may not.
# - Is it important for you to highlight the relative sample sizes?
# - When you have many distributions, you might want to show all as transparent, 
#   and a few of special interest without transparency.
# - If distributions are displayed side-by-side (or top-bottom) how should
#   they be ordered?
# - (as with any visualization) How big is the area of your plot? Are you preparing for print? Do you have limitations
#   on the number of colors? Need monochromatic plots?
# - (as with any visualization) Will you benefit from interactivity? Building an app?


# *** Boxplots
set.seed(1)
n_vec <- floor(sample(runif(100, 10, 1000)))
df <- 
  tibble(
    s = rnorm(sum(n_vec)),
    g = rep(as.character(1:length(n_vec)), times  = n_vec)
  )

par(mar = c(5, 5, 5, 5))
bp0 <- boxplot(s ~ g, df)

# *** Density plots and hisotgrams:
# Aside of stacking density plots and histograms one on top of the other or side by side,
# ridges allow a relatively compact representation of multiple density plots/histograms.

library(plotly)
p <- ggplot(diamonds, aes(x = price)) + 
  geom_density(aes(fill = color), alpha = 0.5) + 
  ggtitle("Kernel Density estimates by group")

ggplotly(p)

p <- ggplot(df, aes(x = s)) + 
  geom_density(aes(fill = g), alpha = 0.5) + 
  ggtitle("Kernel Density estimates by group")

ggplotly(p)

# Note: the bandwidth for all density estimates should be the same in order to 
# make the comparison more meaningful.

# Ridges:
library(ggridges)
ggplot(diamonds, aes(x = price, y = color)) +
  stat_density_ridges(geom = "density_ridges_gradient")

ggplot(diamonds, aes(x = price, y = color, fill = 0.5 - abs(0.5 - stat(ecdf)))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE) +
  scale_fill_viridis_c(name = "Tail probability", direction = -1)

ggplot(df, aes(x = s, y = g)) +
  stat_density_ridges()


# *** Quantile/ECDF plots:
# Comparing distributions may be done with multiple quantile/ECDF plots overlaid.

# Exercise:
# Overlay the quantile plots (compute those manually) for the first soprano and first 
# tenor roles.

# Answer:
singer <- readRDS("Cleveland_singer.rds")




# In addition, Cleveland Ch. 2 recommneds comparing pairs of distributions with QQ plots
# (p. 21-22) and plotting pairwise QQ plots (p. 24)

library(ggplot2)
library(ggcleveland)

singer <- readRDS("Cleveland_singer.rds")
gg_quantiles(singer, height, voice.part)

gg_quantiles(singer, height, voice.part, size = 0.4, color = "red", shape = 3) +
  theme(panel.spacing = unit(2, "lines")) +
  theme_bw()

# Exercise:
# Implement manually the QQ plot for the comparison of two distributions from p. 21 
# of Cleveland. In addition, when the number of observations is large, Cleveland suggests
# in p. 21 to create the plot with fewer quantiles than the number of observations. In 
# your implementation, in addition to full data from two distributions, accept a vector
# of qunatiles to perform the comparison on (say {0.01, 0.03, ..., 0.97, 0.99}).

# Both the full and manually selected quantiles implementations require linear interpolation.
# You may utilize the `approx()` function for this end.

# Answer:




# *** Split violin plot:
library(plotly)

df <- read.csv("violin_data.csv")

(fig <- df %>%
  plot_ly(type = 'violin') )

(fig <- fig %>%
  add_trace(
    x = ~day[df$smoker == 'Yes'],
    y = ~total_bill[df$smoker == 'Yes'],
    legendgroup = 'Yes',
    scalegroup = 'Yes',
    name = 'Yes',
    side = 'negative',
    box = list(
      visible = T
      ),
    meanline = list(
      visible = T
      ),
    color = I("blue")
    ) )

(fig <- fig %>%
  add_trace(
    x = ~day[df$smoker == 'No'],
    y = ~total_bill[df$smoker == 'No'],
    legendgroup = 'No',
    scalegroup = 'No',
    name = 'No',
    side = 'positive',
    box = list(
      visible = T
      ),
    meanline = list(
      visible = T
      ),
    color = I("green")
    ) )

fig %>%
  layout(
    xaxis = list(
      title = ""  
      ),
    yaxis = list(
      title = "",
      zeroline = F
      ),
    violingap = 0,
    violingroupgap = 0,
    violinmode = 'overlay'
    )

