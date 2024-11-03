# Lecture 2 - Part I - Data transformation with dplyr
# (Based on Chapter 5 of "R for Data Science")

# In this chapter we're going to focus on how to use the `dplyr` package.

library(nycflights13)
# Airline on-time data for all flights departing NYC in 2013.
# Also includes useful 'metadata' on airlines, airports, weather, and planes.

library(tidyverse)   # `tidyverse` is a collection of related packages for handling data.

# To explore the basic data manipulation verbs of dplyr, we’ll use nycflights13::flights.
# This data frame contains all 336,776 flights that departed from New York City in 2013.
# The data comes from the US Bureau of Transportation Statistics, and is documented in ?flights.

flights <- nycflights13::flights

# dplyr basics
# ===

# The five key `dplyr`` functions that allow you to solve the vast majority of your data 
# manipulation challenges:
  
# - Pick observations by their values (`filter()`).
# - Reorder the rows (`arrange()`).
# - Pick variables by their names (`select()`).
# - Create new variables with functions of existing variables (`mutate()`).
# - Collapse many values down to a single summary (`summarise()`).

# - These can all be used in conjunction with group_by() which changes the scope
# of each function from operating on the entire dataset to operating on it group-by-group. 

# These six functions provide the **verbs** for a language of data manipulation.

# All verbs work similarly:
  
# - The first argument is a data frame.

# - The subsequent arguments describe what to do with the data frame, 
#   using the variable names (without quotes).

# - The result is a new data frame.

# Filter rows with filter() =====
  
# `filter()` allows you to subset observations based on their values. The first argument 
# is the name of the data frame. The second and subsequent arguments are the expressions
# that filter the data frame.

# For example, we can select all flights on January 1st with:
filter(flights, month == 1, day == 1)
# Moral: the "," translates to "&"

# R either prints out the results, or saves them to a variable.
# If you want to do both, you can wrap the assignment in parentheses:
(dec25 <- filter(flights, month == 12, day == 25))
dec25


# The following code finds all flights that departed in November or December:
filter(flights, month == 11 | month == 12)

# A useful short-hand for this problem is x %in% y. This will select every
# row where x is one of the values in y. We could use it to rewrite the code above:
(nov_dec <- filter(flights, month %in% c(11, 12)))

# Flights that weren’t delayed (on arrival or departure) by more than two hours:
filter(flights, !(dep_delay > 2 | arr_delay > 2))
""

# A useful `dplyr` filtering helper is `between()`:
filter(flights, arr_delay <= 120, between(x = dep_delay, left = 80, right = 120))

# `filter()` only includes rows where the condition is TRUE; 
# it excludes both FALSE and NA values.
# If you want to preserve missing values, ask for them explicitly:
  
df <- data.frame(x = c(1, NA, 3))
filter(df, x > 1)
filter(df, is.na(x) | x > 1)

# Exercises:

# Find all flights that:
# - Had an arrival delay of two or more hours
# - Flew to Houston (IAH or HOU)
# - Were operated by United, American, or Delta
# - Departed in summer (July, August, and September)
# - Arrived more than two hours late, but didn’t leave late
# - Were delayed by at least an hour, but made up over 30 minutes in flight
# - Departed between midnight and 6am (inclusive)

  
# Arrange rows with arrange() =====

# `arrange()` works similarly to `filter()` except that instead of selecting rows, it 
# changes their order. Its inputs are a data frame and a set of column names (or more
# complicated expressions) to order by. If you provide more than one column name, 
# each additional column will be used to break ties in the values of preceding columns:
  
arrange(flights, year, month, day)

# Use `desc()` to re-order by a column in descending order:
arrange(flights, desc(dep_delay))

# Missing values are always sorted at the end:
df <- tibble(x = c(5, 2, NA))
arrange(df, x)
arrange(df, desc(x))

# Exercises:
# - How could you use arrange() to sort all missing values to the start, 
#   followed by the other values in ascending order? (Hint: use is.na()).
df$x
is.na(df$x)

arrange(df, desc(is.na(x)), x)


# - Sort flights to find the most delayed flights. Find the flights that left earliest.
# - Which flights travelled the farthest? Which travelled the shortest?
  
# Select columns with `select()` =====

# It’s not uncommon to get datasets with hundreds or even thousands of variables. 
# In this case, the first challenge is often narrowing in on the variables you’re
# actually interested in. `select()` allows you to rapidly zoom in on a useful subset
# using operations based on the names of the variables.

# Select columns by name
select(flights, year, month, day)

# Select all columns between year and day (inclusive)
select(flights, year:day)
select(flights, day:year)

# Select all columns except those from year to day (inclusive)
select(flights, -(year:day))

# There are a number of helper functions you can use within select():
  
# - `starts_with("abc")`: matches names that begin with “abc”.
# - `ends_with("xyz")`: matches names that end with “xyz”.
# - `contains("ijk")`: matches names that contain “ijk”.
# - `matches("([^a])")`: selects variables that match a regular expression.
# - `num_range("x", 1:3)`: matches x1, x2 and x3.
# 
# See ?select for more details.

# select() can be used to rename variables:
select(flights, year_of_13 = year)


# rename() is a variant of select() that keeps all the variables that
# aren’t explicitly mentioned:
rename(flights, year_of_13 = year)

select(flights, year_of_13 = year, year, year, month, day)


# Another option is to use select() in conjunction with the everything() helper.
# This is useful if you have a handful of variables you’d like to move to the start of the data frame.
select(flights, time_hour, air_time, everything())

# Exercises:

# What happens if you include the name of a variable multiple times in a select() call?
""
# What happens if you include the name of a variable as a string?
year <- "month"
select(flights, "year", year)
# What happens if you include the NA value in a `select()` call? 
select(flights, NA, year)

df <- cbind(df, c(1,1,1))
colnames(df)[2] <- "x"

select(df, x)
# What happens if you include the a variable name that does not exist in the dataframe? 
select(flights, month, year_of_13)

# What happens if you include the NULL value in a `select()` call? Other empty values?
select(flights, year, )
select(flights, NULL, NULL, NULL, year, NULL)

# A bit about the `NULL`:
x <- 5
x <- NULL
x
rm(x)
x


# What does the one_of() function do? Why might it be helpful in conjunction with this vector?
vars <- c("year", "month", "day", "dep_delay", "arr_delay", "not_really_a_variable")
select(flights, one_of(vars))
select(flights, one_of("not_really_a_variable"))

# What would you expect from:
select(flights, dep_time)
select(flights, DEP_TIME)
select(flights, "DEP_TIME")
select(flights, contains("TIME"))
# How do the select helpers deal with case by default? How can you change that default?
select(flights, contains("TIME", ignore.case = FALSE))

# Add new variables with mutate() =====

# Besides selecting sets of existing columns, it’s often useful to add new columns that are 
# functions of existing columns. That’s the job of mutate().

# mutate() always adds new columns at the end of your dataset.
# Often, so we’ll start by creating a narrower dataset so we can see the new variables.

flights_sml <- select(flights, 
                      year:day, 
                      ends_with("delay"), 
                      distance, 
                      air_time
)
mutate(flights_sml,
       gain = dep_delay - arr_delay
)

# Note that you can refer to columns that you’ve just created:
mutate(flights_sml,
       another_year = year,
       gain = dep_delay - arr_delay,
       another_gain = gain,
       gain_squared = gain ^ 2
)
  
# If you only want to keep the new variables, use transmute():
transmute(flights_sml,
       another_year = year,
       gain = dep_delay - arr_delay,
       another_gain = gain,
       gain_squared = gain ^ 2
)

# Useful creation functions =====

# There are many functions for creating new variables that you can use with mutate().
# The key property is that the function must be vectorised: it must take a vector of
# values as input, return a vector with the same number of values as output.

# A selection of useful functions:
  
# Arithmetic operators: +, -, *, /, ^. These are all vectorised, using the “recycling rules”.

# Arithmetic operators are also useful in conjunction with the aggregate functions:
# For example, x / sum(x) calculates the proportion of a total, and y - mean(y) computes
# the difference from the mean.

# Modular arithmetic: %/% (integer division) and %% (remainder), where x == y * (x %/% y) + (x %% y).
# Modular arithmetic is a handy tool because it allows you to break integers up into pieces.
# For example, in the flights dataset, you can compute hour and minute from dep_time with:

transmute(flights,
          dep_time,
          hour = dep_time %/% 100,
          minute = dep_time %% 100
)

# Logs: log(), log2(), log10(). 

# Offsets: lead() and lag() allow you to refer to leading or lagging values.
# This allows you to compute running differences (e.g. x - lag(x)) or find
# when values change (x != lag(x)). They are most useful in conjunction with
# group_by().

(x <- 1:10)
lag(x)
lead(x)

# Cumulative and rolling aggregates: R provides functions for running sums,
# products, mins and maxes: cumsum(), cumprod(), cummin(), cummax(); 
# and dplyr provides cummean() for cumulative means. If you need rolling 
# aggregates (i.e. a sum computed over a rolling window), try the RcppRoll package.

x
cumsum(x)
cummean(x)

# Logical comparisons, <, <=, >, >=, !=, and ==

# Measures of position: first(x), nth(x, 2), last(x).
transmute(flights,
          dep_time,
          rel_to_first_dep_time = dep_time - first(dep_time) + 1,
          sched_dep_time,
          rel_to_last_sched_time = sched_dep_time / last(sched_dep_time)
)

# Ranking: there are a number of ranking functions, but you should start with min_rank().
# It does the most usual type of ranking (e.g. 1st, 2nd, 2nd, 4th). The default gives smallest
# values the small ranks; use desc(x) to give the largest values the smallest ranks.

y <- c(10, 20, 20, NA, 30, 40)
min_rank(y)
min_rank(desc(y))

# If min_rank() doesn’t do what you need, look at the variants row_number(), dense_rank(),
# percent_rank(), cume_dist(), ntile(). See their help pages for more details.

row_number(y)
dense_rank(y)
percent_rank(y)
cume_dist(y)


# Exercise:

# Currently `dep_time` and `sched_dep_time` are convenient to look at, but hard to compute with
# because they’re not really continuous numbers. Convert them to a more convenient representation 
# of number of minutes since midnight.
""
""
""
""
""

# Grouped summaries with summarise() =====

# The last key verb is summarise(). It collapses a data frame to a single row:
mean(flights$dep_delay, na.rm = TRUE)  

summarise(flights, delay = mean(dep_delay, na.rm = TRUE))

""
""
""

# `summarise()` is not terribly useful unless we pair it with `group_by()`.
# This changes the unit of analysis from the complete dataset to individual groups.
# Then, when you use the dplyr verbs on a grouped data frame they’ll be automatically
# applied “by group”. For example, if we applied exactly the same code to a data frame
# grouped by date, we get the average delay per date:
  
by_day <- group_by(flights, year, month, day)
summarise(by_day, delay = mean(dep_delay, na.rm = TRUE))

# Is the order exchangeable?
by_day <- group_by(flights, year, month, day)
summarise(by_day, delay = mean(dep_delay, na.rm = TRUE))

by_day <- group_by(flights, day, year, month)
summarise(by_day, delay = mean(dep_delay, na.rm = TRUE))


# Counts =====

# Whenever you do any aggregation, it’s always a good idea to include either
# a count (n()), or a count of non-missing values (sum(!is.na(x))). That way
# you can check that you’re not drawing conclusions based on very small
# amounts of data. This is demonstrated in the follwing example for the pipe.


# Combining multiple operations with the pipe =====

# Imagine that we want to explore the relationship between the distance and average delay 
# for each location. Using what you know about `dplyr`, you might write code like this:
  
by_dest <- group_by(flights, dest)
delay <- summarise(by_dest,
                   count = n(),
                   dist = mean(distance, na.rm = TRUE),
                   delay = mean(arr_delay, na.rm = TRUE)
)
delay <- filter(delay, count > 20, dest != "HNL")
delay

plot(delay$dist, delay$delay)

pt_size_vec <- (delay$count - min(delay$count)) / (max(delay$count) - min(delay$count)) * (2 - 0.6) + 0.6
plot(delay$dist, delay$delay, cex = pt_size_vec, pch = 20, col = rgb(0, 0, 0, 0.2))
points(delay$dist, delay$delay, cex = pt_size_vec * 0.8)
lowess_values <- lowess(delay$dist, delay$delay)
lines(lowess_values, type = "l", col = rgb(0, 0, 1, 0.5), lwd = 2)

ggplot(data = delay, mapping = aes(x = dist, y = delay)) +
  geom_point(aes(size = count), alpha = 1/3) +
  geom_smooth(method = loess, se = FALSE)

# It looks like delays increase with distance up to ~750 miles 
# and then decrease. Maybe as flights get longer there's more 
# ability to make up delays in the air?


# There are three steps to prepare this data:
  
# 1. Group flights by destination.
# 2. Summarise to compute distance, average delay, and number of flights.
# 3. Filter to remove noisy points and Honolulu airport, which is almost 
#    twice as far away as the next closest airport.

# This code is a little frustrating to write because we have to give each intermediate
# data frame a name, even though we don’t care about it. Naming things is hard, so this
# slows down our analysis.

grouped_flights <- group_by(flights, dest)
unfiltered_summary <- summarise(grouped_flights, count = n(), dist = mean(distance, na.rm = TRUE), delay = mean(arr_delay, na.rm = TRUE))
delays <- filter(unfiltered_summary, count > 20, dest != "HNL")

# There’s another way to tackle the same problem with the pipe, %>%:
flights %>%
  group_by(dest) %>%
  summarise(
    count = n(),
    dist = mean(distance, na.rm = TRUE),
    delay = mean(arr_delay, na.rm = TRUE)) %>%
  filter(count > 20, dest != "HNL")

# This focuses on the transformations, not what’s being transformed, which makes the code 
# easier to read. You can read it as a series of imperative statements: group, then summarise, 
# then filter.

# Working with the pipe is one of the key criteria for belonging to the tidyverse.
# The only exception is ggplot2: it was written before the pipe was discovered.

# Missing values =====

# You may have wondered about the na.rm argument we used above. What happens if we don’t set it?
flights %>% 
  group_by(year, month, day) %>% 
  summarise(mean = mean(dep_delay))

# Aggregation functions obey the usual rule of missing values: if there’s any 
# missing value in the input and `na.rm` is not set to TRUE, the output will 
# be a missing value.
  
""
""
""
  
""
""


# In this case, where missing values represent cancelled flights, we could also tackle the 
# problem by first removing the cancelled flights. We’ll save this dataset so we can reuse 
# it in the next few examples.

not_cancelled <- flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay))

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(mean = mean(dep_delay))


# Useful summary functions =====

# What's the average delay for flights that didn't depart **early**?

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(
    avg_delay1 = mean(arr_delay),
""
)

# When do the first and last flights leave each day?
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(
    first = min(dep_time),
    last = max(dep_time)
  )

# Measures of position: first(x), nth(x, 2), last(x). These work similarly to x[1], x[2], and x[length(x)] but let you set a default value if that position does not exist (i.e. you’re trying to get the 3rd element from a group that only has two elements).
# For example, we can find the first and last departure for each day:
  
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(
    first_dep = first(dep_time), 
    second_dep = nth(dep_time, 2),
    second_to_last_dep = nth(dep_time, length(dep_time) - 1),
    last_dep = last(dep_time)
  )

# Exercise: write summarise with the "[" function to get the same results:
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(
    first_dep = "["(dep_time, 1), 
    second_dep = "["(dep_time, 2), 
    second_to_last_dep = "["(dep_time, length(dep_time) - 1),
    last_dep = "["(dep_time, length(dep_time))
  )
  
# Counts: You’ve seen n(), which takes no arguments, and returns the size of the current group.
# To count the number of non-missing values, use sum(!is.na(x)).
# To count the number of distinct (unique) values, use n_distinct(x).

# Which destinations have the most carriers?
""
""
""
""



# Counts are so useful that dplyr provides a simple helper if all you want is a count:
  
not_cancelled %>% 
  count(dest)


# You can optionally provide a weight variable. For example, you could use this to 
# “count” (sum) the total number of miles a plane flew:
  
not_cancelled %>% 
  count(tailnum, wt = distance)

# Counts and proportions of logical values: sum(x > 10), mean(y == 0).
# When used with numeric functions, TRUE is converted to 1 and FALSE to 0.
# This makes sum() and mean() very useful: 
# sum(x) gives the number of TRUEs in x, and mean(x) gives the proportion.

# How many flights left before 5am? (these usually indicate delayed
# flights from the previous day)
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise("...")

# What proportion of flights are delayed by more than an hour?
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise("...")

# Grouping by multiple variables =====

# When you group by multiple variables, each summary peels off one level 
# of the grouping. That makes it easy to progressively roll up a dataset:
  
daily <- group_by(flights, year, month, day)
(per_day   <- summarise(daily, flights = n()))

(per_month <- summarise(per_day, flights = sum(flights)))

(per_year  <- summarise(per_month, flights = sum(flights)))

# Be careful when progressively rolling up summaries: it’s OK for sums and counts,
# but you need to think about weighting means and variances, and it’s not possible
# to do it exactly for rank-based statistics like the median. In other words, the
# sum of groupwise sums is the overall sum, but the median of groupwise medians is
# not the overall median.

# Ungrouping =====

# If you need to remove grouping, and return to operations on ungrouped data, use ungroup().

# How many flights in total?
daily %>% 
  ungroup() %>%             # no longer grouped by date
  summarise("...")  # all flights

# Grouped mutates (and filters) =====

# Grouping is most useful in conjunction with summarise(), but you can also do
# convenient operations with mutate() and filter():
  
# Find the worst members of each group:
  
flights_sml %>% 
  group_by(year, month, day) %>%
  filter(rank(desc(arr_delay)) < 10)

# Find all groups bigger than a threshold:
  
popular_dests <- flights %>% 
  group_by(dest) %>% 
  filter(n() > 365)
popular_dests

# Standardise to compute per group metrics:

popular_dests %>% 
  filter(arr_delay > 0) %>% 
  mutate(prop_delay = arr_delay / sum(arr_delay)) %>% 
  select(year:day, dest, arr_delay, prop_delay)


# A grouped filter is a grouped mutate followed by an ungrouped filter.
# I generally avoid them except for quick and dirty manipulations: otherwise it’s hard 
# to check that y ou’ve done the manipulation correctly.

# Functions that work most naturally in grouped mutates and filters are known as window
# functions (vs. the summary functions used for summaries). You can learn more about
# useful window functions in the corresponding vignette: vignette("window-functions").
