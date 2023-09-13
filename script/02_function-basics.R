# ****************************************************
# 02 - Function basics ----
# URL: https://krlmlr.github.io/tidyprog/index.html
# ****************************************************

# Structuring code to avoid copying and pasting.

## 2.1 Definition and execution
library(tidyverse)
library(here)

# Create functions for tasks that need to be executed repeatedly,
# or to hide implementation details.
read_weather_data <- function() {
  # Read all files
  berlin   <- readxl::read_excel(path = here("data/weather", "berlin.xlsx"))
  toronto  <- readxl::read_excel(path = here("data/weather", "toronto.xlsx"))
  tel_aviv <- readxl::read_excel(path = here("data/weather", "tel_aviv.xlsx"))
  zurich   <- readxl::read_excel(path = here("data/weather", "zurich.xlsx"))


  # Create ensemble dataset
  weather_data <- bind_rows(
    berlin = berlin,
    toronto = toronto,
    tel_aviv = tel_aviv,
    zurich = zurich,
    .id = "city_code"
  )

  return(weather_data)
}

# Display the code of the function:
read_weather_data

# Call the function to load the data
read_weather_data()

# This has not saved the data set in the global environment:
ls() # "read_weather_data"

# Only the function "read_weather_data" lives in the global environment.

# We can either use the function directly:
read_weather_data() |>
  count(city_code)
# 49 observations per city

# Or we assign the returned data frame to a global variable
weather_data <- read_weather_data()


### 2.1.1 Exercises ----

# 1. Only call Non-European cities:
read_weather_data_non_europe <- function() {
  weather_data <- read_weather_data() |>
    filter(city_code %in% c("toronto", "tel_avivi"))

  return(weather_data)
}

read_weather_data_non_europe()


## 2.2 Arguments ----

# Functions with arguments:
weather_path <- function(filename) {
  # Returned value
  here("data/weather", filename)
}

weather_path("milan.xlsx")

# Call functions from within functions:
read_weather_data <- function() {
  # Read all files
  berlin   <- readxl::read_excel(path = weather_path("berlin.xlsx"))
  toronto  <- readxl::read_excel(path = weather_path("toronto.xlsx"))
  tel_aviv <- readxl::read_excel(path = weather_path("tel_aviv.xlsx"))
  zurich   <- readxl::read_excel(path = weather_path("zurich.xlsx"))

  # Create ensemble dataset
  weather_data <- bind_rows(
    berlin = berlin,
    toronto = toronto,
    tel_aviv = tel_aviv,
    zurich = zurich,
    .id = "city_code"
  )

  return(weather_data)
}

read_weather_data()


## 2.3 Use case: Intermediate variables ----

# Functions avoid intermediate variables:
read_weather_file <- function(filename) {
  readxl::read_excel(path = weather_path(filename))
}

read_weather_data <- function() {
  # Create ensemble dataset from files on disk
  weather_data <- bind_rows(
    berlin = read_weather_file("berlin.xlsx"),
    toronto = read_weather_file("toronto.xlsx"),
    tel_aviv = read_weather_file("tel_aviv.xlsx"),
    zurich = read_weather_file("zurich.xlsx"),
    .id = "city_code"
  )
  return(weather_data)
}

read_weather_data()


## 2.4 Default values ----
weather_path <- function(filename) {
  here("data/weather", filename)
}

read_weather_file <- function(filename) {
  readxl::read_excel(weather_path(filename))
}

get_weather_file_for <- function(city_code) {
  paste0(city_code, ".xlsx")
}

get_weather_data_for <- function(city_code) {
  read_weather_file(get_weather_file_for(city_code))
}

# We start with the function get_weather_data_for() from the
# previous section.
# An example for a boolean argument is when TRUE leads to
# all the data from Zurich being dropped:
read_weather_data <- function(omit_zurich = FALSE) {
  # Create ensemble dataset from files on disk
  weather_data <- bind_rows(
    berlin   = get_weather_data_for("berlin"),
    toronto  = get_weather_data_for("toronto"),
    tel_aviv = get_weather_data_for("tel_aviv"),
    zurich   = get_weather_data_for("zurich"),
    .id = "city_code"
  )

  # Return it (filtered)
  weather_data |>
    filter(!(city_code == "zurich" & omit_zurich))
}

read_weather_data()
read_weather_data(omit_zurich = FALSE)
read_weather_data(omit_zurich = TRUE)


## 2.5 Multiple arguments ----
weather_path <- function(filename) {
  here("data/weather", filename)
}

read_weather_file <- function(filename) {
  readxl::read_excel(weather_path(filename))
}

get_weather_file_for <- function(city_code) {
  paste0(city_code, ".xlsx")
}

# What are the considerations when suing multiple function arguments?
# We can add a drop_toronto argument:
read_weather_data <- function(omit_zurich = FALSE, omit_toronto = FALSE) {
  # Create ensemble dataset from files on disk
  weather_data <- bind_rows(
    berlin   = get_weather_data_for("berlin"),
    toronto  = get_weather_data_for("toronto"),
    tel_aviv = get_weather_data_for("tel_aviv"),
    zurich   = get_weather_data_for("zurich"),
    .id = "city_code"
  )

  # Return it (filtered)
  weather_data %>%
    filter(!(city_code == "zurich" & omit_zurich)) %>%
    filter(!(city_code == "toronto" & omit_toronto))
}

read_weather_data(omit_zurich = TRUE, omit_toronto = TRUE)

# Use the ellipsis (...) argument to provide the possibility of
# adding additional arguments to the function call.
weather_path <- function(...) {
  # All arguments are passed on to the next ....
  here("data/weather", ...)
}

weather_path("berlin.xlsx")

weather_path("some", "subdir", "with", "a", "file.csv")


## 2.6 Argument matching ----

# The ellipsis (...) can be used to force the user to fully
# name the function parameters when setting them:
only_names <- function(..., one = 1, two = 2) {
  list(one = one, two = two)
}

only_names(3, 4)

only_names(one = 3, 4)

only_names(one = 3, two = 4)

# Inside a function with an ellipsis argument you can capture
# the ellipsis with list(...):
ellipsis_test <- function(...) {
  args <- list(...)
  names(args)
}

ellipsis_test(a = 1, 2, c = 3:5)
# "a" "" "c"

# Arguments in ellipsis can be accessed with ..1, ..2:
ellipsis_direct_test <- function(...) {
  list(..1, ..2)
}

ellipsis_direct_test(a = 1, 2, c = 3:5)

## More on ellipses (...) ---
help("...")
# ..., ..1, etc used in Functions
# Description
# ... and ..1, ..2 etc are used to refer to arguments passed down from a
# calling function.
# These (and the following) can only be used inside a function which has
# ... among its formal arguments.

# ...elt(n) is a functional way to get ..<n> and basically the same as
# eval(paste0("..", n)), just more elegant and efficient.
# Note that switch(n, ...) is very close,
# differing by returning NULL invisibly instead of an error when n is zero
# or too large.


# ...length() returns the number of expressions in ...,
# and ...names() the names.
# These are the same as length(list(...)) or names(list(...)) but without
# evaluating the expressions in ... (which happens with list(...)).
# ...length()

# Evaluating elements of ... with ..1, ..2, ...elt(n), etc. propagates visibility.
# This is consistent with the evaluation of named arguments which also propagates visibility.

# Usage
# ...length()
# ...names()
# ...elt(n)

# Arguments: n, a positive integer, not larger than the number of expressions in ...,
# which is the same as ...length() which is the same as length(list(...)),
# but the latter evaluates all expressions in ....
# ... and ..1, ..2 are reserved words in R

tst <- function(n, ...) ...elt(n) # Return the n-th element in ...

tst(n = 1, pi = pi * 0:1, 2:4)
# Accessed the first element that is in the ellipsis, pi = pi * 0, pi * 1

tst(n = 2, pi = pi * 0:1, 2:4)
# Accesses the second element in the ellipsis, 2, 3, 5

try(tst(n = 1)) # Error : the ... list contains fewer than 1 element

tst.dl  <- function(x, ...) ...length() # Return the number of elements in ...
tst.dns <- function(x, ...) ...names() # Return the names of the elements in ...

tst.dl(x = 1:10) # No arguments passed to ...
tst.dl(x = 4, 5) # 1 (5)
tst.dl(x = 4, 5, 6) # 2 (5, 6)

tst.dl(x = 4, 5, 6, 7, sin(1:10), "foo" / "bar") # 5 elements, because
# "foo" / "bar" was not evaluated!

tst.dns(x = 4, foo = 5, 6, bar = 7, sini = sin(1:10), "foo" / "bar")

tst.dns(4, foo = 5, 6, bar = 7, sini = sin(1:10), "foo" / "bar")
# Three named and one unnamed argument ("")

# END