# Lecture 2 - Part II - Tidy data with `tidyr`
# (Based on Chapter 12 of "R for Data Science")

## Introduction =====

# In this chapter, we will focus on a consistent way to organise your data in R, an
# organisation called __tidy data__. Getting your data into this format requires some
# upfront work, but that work pays off in the long term. 

# `tidyr` is a package that provides a bunch of tools to help
# tidy up your messy datasets. tidyr is a member of the core tidyverse.

library(tidyverse)

## Tidy data =====

# You can represent the same underlying data in multiple ways.
# The example below shows the same data organised in four different ways.
# Each dataset shows the same values of four variables *country*, *year*, *population*,
# and *cases*, but each dataset organises the values in a different way.

table1
table2
table3

# Spread across two tibbles
table4a  # cases
table4b  # population

# These are all representations of the same underlying data, but they are not equally
# easy to use. One dataset, the tidy dataset, will be m uch easier to work with inside
# the tidyverse. 

# There are three interrelated rules which make a dataset tidy:

# 1.  Each variable must have its own column.
# 2.  Each observation must have its own row.
# 3.  Each value must have its own cell.

# A visualization of the the rules:
par(mar = c(0, 0, 0, 0))
par(mfrow = c(1, 1))
plot(imager::load.image("tidy-1.png"), axes = FALSE) 


# These three rules are interrelated because it's impossible to only satisfy two of the
# three. That interrelationship leads to an even simpler set of practical instructions:

# 1.  Put each dataset in a tibble.
# 2.  Put each variable in a column.

# In this example, only `table1` is tidy. It's the only representation where each column
# is a variable.

# Why ensure that your data is tidy? There are two main advantages:

# 1.  There's a general advantage to picking one consistent way of storing
#     data. If you have a consistent data structure, it's easier to learn the
#     tools that work with it because they have an underlying uniformity.
    
# 2.  There's a specific advantage to placing variables in columns because
#     it allows R's vectorised nature to shine. As we saw in
#     Lecture 2 Part I, most built-in R functions work with vectors of values.
#     That makes transforming tidy data feel particularly natural.

# dplyr, ggplot2, and all the other packages in the tidyverse, as well as plotly, are
# designed to work with tidy data.

# Here are a couple of small examples showing how you might work with `table1`.

# Compute rate per 10,000
table1 %>% 
  mutate(rate = cases / population * 10000)

# Compute cases per year
table1 %>% 
  count(year, wt = cases)

# Visualise changes over time
library(ggplot2)
ggplot(table1, aes(year, cases)) + 
  geom_line(aes(group = country), colour = "grey50") + 
  geom_point(aes(colour = country))


## Pivoting =====

# Most data that you will encounter will be untidy. 
# This means for most real analyses, you'll need to do some tidying. 

# The first step is always to figure out what the variables and observations are.
# Sometimes this is easy; other times you'll need to consult with the people who
# originally generated the data. 

# The second step is to resolve one of two common problems:

# 1. One variable might be spread across multiple columns.
# 2. One observation might be scattered across multiple rows.

# To fix these problems, you'll need the two most important functions in tidyr:
# `pivot_longer()` and `pivot_wider()`.

### Longer =====

# A common problem is a dataset where some of the column names are not names of variables,
# but _values_ of a variable. Take `table4a`: the column names `1999` and `2000` represent
# values of the `year` variable, the values in the `1999` and `2000` columns represent
# values of the `cases` variable, and each row represents two observations, not one.

table4a

# To tidy a dataset like this, we need to __pivot__ the offending columns into a new pair
# of variables. To describe that operation we need three parameters:

# * The set of columns whose names are values, not variables. In this example, 
#   those are the columns `1999` and `2000`.

# * The name of the variable to move the column names to. Here it is `year`.

# * The name of the variable to move the column values to. Here it's `cases`.
  
# Together those parameters generate the call to `pivot_longer()`:

table4a %>% 
  pivot_longer(c(`1999`, `2000`), names_to = "year", values_to = "cases")

# The columns to pivot are specified with `dplyr::select()` style notation.
# Here there are only two columns, so we list them individually. Note that "1999"
# and "2000" are non-syntactic names (because they don't start with a letter) so
# we have to surround them in backticks.

# `year` and `cases` do not exist in `table4a` so we put their names in quotes.

plot(imager::load.image("tidy-1.png"), axes = FALSE) 

# In the final result, the pivoted columns are dropped, and we get new `year` and
# `cases` columns. Otherwise, the relationships between the original variables are
# preserved. 

# `pivot_longer()` makes datasets longer by increasing the number of rows and decreasing
# the number of columns. 

# Exercise:
# Use `pivot_longer` to tidy `table4b`

# ""

# To combine the tidied versions of `table4a` and `table4b` into a single tibble, we
# need to use `dplyr::left_join()`.

tidy4a <- table4a %>% 
  pivot_longer(c(`1999`, `2000`), names_to = "year", values_to = "cases")
tidy4b <- table4b %>% 
  pivot_longer(c(`1999`, `2000`), names_to = "year", values_to = "population")
left_join(tidy4a, tidy4b)

### Wider =====

# `pivot_wider()` is the opposite of `pivot_longer()`. You use it when an observation
# is scattered across multiple rows. For example, take `table2`: an observation is a
# country in a year, but each observation is spread across two rows.

table2

# To tidy this up, we first analyse the representation in similar way to `pivot_longer()`.
# This time, however, we only need two parameters:

# * The column to take variable names from. Here, it's `type`.

# * The column to take values from. Here it's `count`.

# Once we've figured that out, we can use `pivot_wider()`, as shown below.
plot(imager::load.image("tidy-8.png"), axes = FALSE)


table2 %>%
    pivot_wider(names_from = type, values_from = count)

### Exercises =====

# 1.  Why are `pivot_longer()` and `pivot_wider()` not perfectly symmetrical?  
#     Carefully consider the following example:
    
(stocks <- tibble(
  year   = c(2015, 2015, 2016, 2016),
  half  = c(   1,    2,     1,    2),
  return = c(1.88, 0.59, 0.92, 0.17)
))

stocks %>% 
  pivot_wider(names_from = year, values_from = return) %>% 
  pivot_longer(`2015`:`2016`, names_to = "year", values_to = "return")

# (Hint: look at the variable types and think about column _names_.)


# 2. `pivot_longer()` has a `names_ptypes` argument, e.g. 
# `names_ptypes = list(year = as.numeric)`. What does it do?


# 3. Why does this code fail?
table4a %>% 
  pivot_longer(c(1999, 2000), names_to = "year", values_to = "cases")

    
# 4. Tidy the simple tibble below. Do you need to make it wider or longer?
#    What are the variables?

(preg <- tribble(
  ~pregnant, ~male, ~female,
  "yes",     NA,    10,
  "no",      20,    12
))


## Separating and uniting =====

# `table3` has a different problem: we have one column (`rate`) that contains two
# variables (`cases` and `population`). To fix this problem, we'll need the `separate()`
# function. 

### Separate =====

# `separate()` pulls apart one column into multiple columns, by splitting wherever a
# separator character appears. Take `table3`:

table3

# The `rate` column contains both `cases` and `population` variables, and we need to split
# it into two variables. `separate()` takes the name of the column to separate, and the
# names of the columns to separate into.


plot(imager::load.image("tidy-17.png"), axes = FALSE) 


table3 %>% 
  separate(rate, into = c("cases", "population"))

# By default, `separate()` will split values wherever it sees a non-alphanumeric character
# (i.e. a character that isn't a number or letter). For example, in the code above,
# `separate()` split the values of `rate` at the forward slash characters.

# To use a specific character to separate a column, pass the character to the `sep` argument
# of `separate()`. For example, we could rewrite the code above as:

table3 %>% 
  separate(rate, into = c("cases", "population"), sep = "/")

# (Formally, `sep` is a regular expression)

# Look carefully at the column types: you'll notice that `cases` and `population` are
# character columns. This is the default behaviour in `separate()`: it leaves the type
# of the column as is. Here, however, it's not very useful as those really are numbers.
# We can ask `separate()` to try and convert to better types using `convert = TRUE`:

table3 %>% 
  separate(rate, into = c("cases", "population"), convert = TRUE)

# You can also pass a vector of integers to `sep`. `separate()` will interpret the
# integers as positions to split at. Positive values start at 1 on the far-left of the
# strings; negative value start at -1 on the far-right of the strings. When using integers
# to separate strings, the length of `sep` should be one less than the number of names in
# `into`. 

# You can use this arrangement to separate the last two digits of each year. This make
# this data less tidy, but is useful in other cases, as you'll see in a little bit.

table3 %>% 
  separate(year, into = c("century", "year"), sep = 2)


### Unite =====

# `unite()` is the inverse of `separate()`: it combines multiple columns into a single
# column. You'll need it much less frequently than `separate()`, but it's still a useful
# tool to have in your back pocket.

plot(imager::load.image("tidy-18.png"), axes = FALSE) 


# We can use `unite()` to rejoin the *century* and *year* columns that we created in
# the last example. That data is saved as `tidyr::table5`. `unite()` takes a data frame,
# the name of the new variable to create, and a set of columns to combine, again specified
# in `dplyr::select()` style:

table5 %>% 
  unite(new, century, year)


# In this case we also need to use the `sep` argument. The default will place an
# underscore (`_`) between the values from different columns. Here we don't want any
# separator so we use `""`:

table5 %>% 
  unite(new, century, year, sep = "")

# And we can use "as.integer":
table5 %>% 
  unite(new, century, year, sep = "") %>%
  rename(year = new) %>%
  mutate(year = as.integer(year))


## Missing values =====

# Changing the representation of a dataset brings up an important subtlety of missing
# values. A value can be missing in one of two possible ways:

# * __Explicitly__, i.e. flagged with `NA`.
# * __Implicitly__, i.e. simply not present in the data.

# Let's illustrate this idea with a very simple data set:

stocks <- tibble(
  year   = c(2015, 2015, 2015, 2015, 2016, 2016, 2016),
  qtr    = c(   1,    2,    3,    4,    2,    3,    4),
  return = c(1.88, 0.59, 0.35,   NA, 0.92, 0.17, 2.66)
)

# There are two missing values in this dataset:

# * The return for the fourth quarter of 2015 is explicitly missing, because
#   the cell where its value should be instead contains `NA`.
  
# * The return for the first quarter of 2016 is implicitly missing, because it
#   simply does not appear in the dataset.
  
# The way that a dataset is represented can make implicit values explicit. For example,
# we can make the implicit missing value explicit by putting years in the columns:

stocks %>% 
  pivot_wider(names_from = year, values_from = return)

# Because these explicit missing values may not be important in other representations
# of the data, you can set `values_drop_na = TRUE` in `pivot_longer()` to turn explicit
# missing values implicit:

stocks %>% 
  pivot_wider(names_from = year, values_from = return) %>% 
  pivot_longer(
    cols = c(`2015`, `2016`), 
    names_to = "year", 
    values_to = "return", 
    values_drop_na = TRUE
  )

# Another important tool for making missing values explicit in tidy data is `complete()`:

stocks %>% 
  complete(year, qtr)


# `complete()` takes a set of columns, and finds all unique combinations. It then ensures
# the original dataset contains all those values, filling in explicit `NA`s where necessary.

# Sometimes when a data source has primarily been used for data entry, missing values
# indicate that the previous value should be carried forward:

treatment <- tribble(
  ~ person,           ~ treatment, ~response,
  "Derrick Whitmore", 1,           7,
  NA,                 2,           10,
  NA,                 3,           9,
  "Katherine Burke",  1,           4
)

# You can fill in these missing values with `fill()`. It takes a set of columns where
# you want missing values to be replaced by the most recent non-missing value (sometimes
# called last observation carried forward).

treatment %>% 
  fill(person)