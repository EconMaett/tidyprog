# *******************************************************************
# 07 - Tidy evaluation ----
# URL: https://krlmlr.github.io/tidyprog/index.html
# *******************************************************************

library(tidyverse)
library(here)
library(rlang)

# Thios chapter offers an introduction to tidy evaluation.

# Knowledge of tidy evaluation can become necessary when creating your own functions
# within the tidyverse framework.

# This chapter requires the "rlang" package that provides helper functions
# used in tidy evaluation.


## 7.1 A custom plotting function -----

# Let us try to build a function that takes a data frame as its argument
# and an unquoted column name and returns a histogram.

# A naive approach would look like this:
tidy_histogram <- function(.data, x) {
  .data |> 
    ggplot(mapping = aes(x = x)) +
    geom_histogram()
}

# We create some data and try to use the function from above:
data <- tibble(a = 1:10)

try(print(
  data |> 
    tidy_histogram(a)
))
# Error in geom_histogram() : object 'a' not found

try(print(
  data |> 
    tidy_histogram("a")
))
# Error in geom_histogram() : Caused by error in `setup_params()`:

# Neither the first nor the second attempt worked.
# If we add another column called "x", it suddently works:
data <- tibble(a = 1:10, x = 11:20)

data |> 
  tidy_histogram(a)

# The reason is that our function is hard-coded to display a variable
# called "x".
# In the code-snippet mapping = aes(x = x).

# The solution to avoid this ambiguity is by using an expression to
# capture the meaning (here: the user input) of an unquoted variable and
# subsequently use the bang-bang-operator (!!); also called "unquote"
# to pass on the meaning of the variable at the right place.

tidy_histogram <- function(.data, x) {
  # Treat the argument as a variable name
  expr <- rlang::enquo(x)
  
  .data |> 
    # Tell ggplot2 that "expr" *contains* the name of the variable,
    # instead of expecting a variable named `expr`
    ggplot(mapping = aes(x = !!expr)) +
    geom_histogram()
}


data |> 
  tidy_histogram(a)

data |> 
  tidy_histogram(x)

try(print(
  data |> 
    tidy_histogram(y)
))
# Error in geom_histogram() : Caused by error: object 'y' not found

# This behavior is different from the usual base-R usage of variables in
# functions.

# How do some functions behave one way and not the other?
# The problematic function in the example before was aes():
aes
# function (x, y, ...) 
# {
#   xs <- arg_enquos("x")
#   ys <- arg_enquos("y")
#   dots <- enquos(...)
#   args <- c(xs, ys, dots)
#   args <- Filter(Negate(quo_is_missing), args)
#   local({
#     aes <- function(x, y, ...) NULL
#     inject(aes(!!!args))
#   })
#   aes <- new_aes(args, env = parent.frame())
#   rename_aes(aes)
# }

# The aes() function itself makes use of capturing user input as an expression
# with xs <- arg_enquos("x"), ys <- arg_enquos("y"), dots <- enquos(...)
# args <- Filter(Negate(quo_is_missing), args)
# local(expr = {aes <- function(x, y, ...) NULL inject(aes(!!!args))})
# aes <- new_aes(args, env = parent.frame())
# rename_aes(aes)

# Previously, aes() use the function rlang::enquos():

## function (x, y, ...) 
## {
##     exprs <- rlang::enquos(x = x, y = y, ...)
##     is_missing <- vapply(exprs, rlang::quo_is_missing, logical(1))
##     aes <- new_aes(exprs[!is_missing], env = parent.frame())
##     rename_aes(aes)
## }

# rlang::enquos() captures one or more expressions along with a unique
# identifier for the environment in which they are supposed
# to be evaluated eventually.

# The default is to evaluate an expression ("standard evaluation").

# With rlang::enquo() and rlang::enquos() the expressions that
# correspond to user input are captured.

# As an example of a newly-built functin in the tidyverse, here is a 
# function that combines the functionalities of 
# dplyr::mutate() and purrr::map_dbl().

# It takes its input arguments:
# - a data frame
# - the column it is supposed to act upon
# - the function call it is supposed to use on each of the columns
mutate_map_dbl <- function(.data, col, expr) {
  quo <- rlang::enquo(arg = col)
  
  .data |> 
    mutate(new_column = purrr::map_dbl(.x = !!quo, .f = expr))
}

iris_nested <- iris |> 
  nest(data = -Species)

iris_nested |> 
  mutate_map_dbl(col = data, expr = ~ mean(.$Petal.Width))


## 7.2 Do you need tidy evaluation? ----

# With a slight extension of dplyr::summarize(), we can create a function where
# we do not need to explicitly capture any expressions.

# The function takes a data frame and an ellipsis.

# The ellipsis can be directly passed on to a tidyverse function.
# buzzphrase: "pass the dots"
summarize_ungroup <- function(.data, ...) {
  .data |> 
    summarise(...) |> 
    ungroup()
}

# The function does what it promised to do:
mean_airtime_per_day <- nycflights13::flights |> 
  group_by(year, month, day) |> 
  summarize_ungroup(mean(air_time, na.rm = TRUE))

mean_airtime_per_day  

mean_airtime_per_day |> 
  groups()
# list() 
# An empty list was returned. Previously, NULL was returned.


## 7.3 Explicit quote-unquote of ellipsis ----

# There are cases when you need knowledge about what the user added to
# the ellipsis.

# This is then handled by capturing the content in a list of quosures,
# which can be unquoted by the triple-bang-operator (!!!).

# Use !! for one expression and !!! for lists of multiple expressions.

# The triple-bang-operator does two things:
# - It unquotes the content
# - It splices the content into the current call

summarize_ungroup <- function(.data, ...) {
  # Capture (quote) with rlang::enquos()
  quos <- rlang::enquos(...)
  
  # Use (unqote-splice) with !!!
  .data |> 
    summarise(!!!quos) |> 
    ungroup()
}

# We did not need to process the content, but it still works:
mean_airtime_per_day <- nycflights13::flights |> 
  group_by(year, month, day) |> 
  summarize_ungroup(mean(air_time, na.rm = TRUE))

mean_airtime_per_day

mean_airtime_per_day |> 
  groups()
# list()

# To come back to the original exmple:
# aes() uses the quote-unquote-splice pattern:
aes
# First the expressions are captured, that is, the coordinates
# and the ellipses.
# Then they are unquoted with the triple bang operator !!!
# and evaluated locally.

# At the time when this course material was produced, dplyr::summarize()
# did not make use of the quote-unquote-splice pattern.

# Now, however, it does.
summarise

# The new .by = NULL argument is captured with 
# by <- rlang::enquo(.by)


## 7.4 Names ----

# User input in an ellipsis can be named or unnamed.

# We create a special interface for the function:
gsu <- function(.data, ...) {
  # Capture (quote) with rlang::enquos()
  quos <- rlang::enquos(...)
  
  
  is_named <- (rlang::names2(quos)  != "")
  named_quos <- quos[is_named]
  unnamed_quos <- quos[!is_named]
  
  # Use (unquote-splice) with !!!
  .data |> 
    group_by(!!!unnamed_quos) |> 
    summarize(!!!named_quos) |> 
    ungroup()
}

# The "named_quos" are our summary columns, and the "unnamed_quos" are
# now the grouping columns:
mean_airtime_per_day <- nycflights13::flights |> 
  gsu(year, month, day, mean_air_time = mean(air_time, na.rm = TRUE))

mean_airtime_per_day


## 7.5 Debugging ----

# You can use the capturing functions outside of functions:
rlang::quos(x = a)
# <list_of<quosure>>

# $x
# <quosure>
# expr: ^a
# env: global

a <- rlang::sym("b")
a # b

x_quos <- rlang::quos(x = !!a)
x_quos
# <list_of<quosure>>

# $x
# <quosure>
# expr: ^b
# env: global

# The rlang::sym() function creates a so-called "symbol" form a "character"
# variable.
# Unquoting the symbol with !!a means that the symbol is interpreted
# as a variable in the dataset.

# Capturing expressions in quosures can help you understand what is happening
# behidn the scenes and for example give you clues as to why your code is not
# doing what it is supposed to.

# Quosures can also be nested:
rlang::quos(y = c, !!!x_quos)
# <list_of<quosure>>

# $y
# <quosure>
# expr: ^c
# env: global

# $x
# <quosure>
# expr: ^b
# env: global


## 7.6 Argument names ----

# In the previously created custom plotting function mutate_map_dbl(),
# we could not name the desired new column inside the function call.
mutate_map_dbl <- function(.data, col, ...) {
  quos <- build_quos(!!rlang::enquo(col), ...)
  .data |> 
    mutate(!!!quos)
}

build_quos <- function(col, ...) {
  args <- list(...)
  stopifnot(length(args) == 1)
  
  expr <- args[[1]]
  
  map_quo <- build_map_quo(!!enquo(col), expr)
  
  rlang::set_names(list(map_quo), names(args))
}

build_map_quo <- function(col, expr) {
  quo <- rlang::enquo(col)
  rlang::quo(map_dbl(!!quo, expr))
}

# Like in the "Names" section we can use an expression in an ellipsis
# so that x = y is treated such that x is the name and y is the
# value of the object.
# Here we used this in the code snipped names(args).

# The main function mutate_map_dbl() calls a helper function
# build_quos() that in turn calls the helper function
# build_map_quo.

# mutate_map_dbl() then uses the output of the nested function calls
# to produce the desired output.
build_quos(col = data, mean_petal_width = ~ mean(.$Petal.Width))
# $mean_petal_width
# <quosure>
# expr: ^map_dbl(^data, expr)
# env: ...

build_map_quo(col = mean_petal_width, expr = ~ mean(.$Petal.Width))
# <quosure>
# expr: ^map_dbl(^mean_petal_width, expr)
# env: ...

# And finally, let's see if the function does what it should:
iris |> 
  nest(data = -Species) |> 
  mutate_map_dbl(col = data, mean_petal_width = ~ mean(.$Petal.Width))


## 7.7 purrr-style mappers ----

# We try to re-create the function mutate_map_dbl() so that it works
# without the tilde before the function.
build_quos <- function(col, ...) {
  args <- rlang::enquos(...)
  stopifnot(length(args) == 1)
  
  
  expr <- args[[1]]
  
  map_quo <- build_map_quo(!!enquo(col), !!expr)
  
  
  rlang::set_names(list(map_quo), names(args))
}

build_map_quo <- function(col, expr) {
  quo <- rlang::enquo(col)
  mapper <- as_mapper_quosure(!!rlang::enquo(expr))
  rlang::quo(purrr::map_dbl(.x = !!quo, .f = !!mapper))
}

as_mapper_quosure <- function(expr) {
  quo <- rlang::enquo(expr)
  
  rlang::new_function(
    args = alist(
      ... = ,
      .   = ..1,
      .x  = ..1,
      .y  = ..2
    ),
    body = rlang::quo_get_expr(quo),
    env = rlang::quo_get_env(quo)
  )
}


# We needed to add one more level in the hierarchy of function calling.
# The helper as_mapper_quosure() creates a new function with the help of
# rlang::new_function(args, body, env), which makes it possible to leave out
# the tilde operator (~):
as_mapper(.f = ~ mean(.$Petal.Width))
# <lambda>
# function (..., .x = 1, .y = ..2, . = ..1)
# mean(.$Petal.Width)
# attr(,"class") "rlang_lambda_function" "function"

build_map_quo(col = mean_petal_width, expr = mean(.Petal.Width))
# <quosure>
# expr: ^purrr::map_dbl(.x = ^mean_petal_width, .f = <function(..., . = ..1, .x = ..1, .y = ..2) mean(.Petal.Width)>)
# env: ...

# Our function as_mapper_quosure() is closely related to the function
# purrr::as_mapper() but it produces a quosure of a proper function and not a 
# lambda function.
# Also, it does not require the tilde.
iris |> 
  nest(data = -Species) |> 
  mutate_map_dbl(col = data, mean_petal_width = mean(.$Petal.Width))

# END