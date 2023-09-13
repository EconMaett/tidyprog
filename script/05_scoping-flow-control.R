# ****************************************************
# 05 - Scoping and flow control ----
# URL: https://krlmlr.github.io/tidyprog/index.html
# ****************************************************

library(tidyverse)
library(here)

## 5.1 Scope ----

# What happens if a function defines variables that have a variable by the same
# name in the global environment?

# We start with a variable defined in the global environment:
a <- 5
ls() # "a"
ls(envir = globalenv()) # "a"

# A function can access global variables:
f <- function() {
  return(a)
}

f() # 5

# On the other hand, a variable which is defined inside a function is contained
# in the function environment.

# It will not be known outside of that function.

# Respectively, it will not overwrite the value of global variables.
f <- function() {
  a <- 2
  a
}

f() # 2

a # 5

# Global variables are a (hidden) part of a function's interface.
# Ideally, functions are self-contained, i.e. independent of global varialbes.

# Notable exceptions are objects used across your entire analysis, such as
# your data set.
# Otherwise you would need to pass them across many layers.


### 5.1.1 Exercises ----

# 1. Double-check what happens if two functions declare / use a variable
# of the same name.

# Variables in different functions
f1 <- function() {
  a <- 3
  a + f2()
}

f2 <- function() {
  a
}

f1() # 8
f2() # 5
a # 5


## 5.2 Pure functions and side-effects ----

# Functions should do *one* thing, and do it *well*.

# A pure function is one that is called for its return value, and which
# has no side-effects:
pure_function <- function(x) {
  return(x + 1)
}

pure_function(x = 1) # 2

# For functions with side effect, it is good practice to return the input
# invisibly:
side_effect_function <- function(x) {
  file <- tempfile()
  writeLines(text = format(x), con = tempfile())
  print(x)
  message(x, " written to ", file)
  
  invisible(x)
}

side_effect_function(x = 2) # 2

# Separation helps isolate the side effects.
# If side effect functions return the input, they remain
# composable with pure functions:
5 |> 
  pure_function() |> 
  side_effect_function() |> 
  pure_function()


## 5.3 Control flow ----
weather_path <- function(filename) {
  # Returned value
  here("data/weather", filename)
}

read_weather_file <- function(filename) {
  readxl::read_excel(weather_path(filename))
}

# A way to regulate the control flow is by using if():
read_weather_data <- function(omit_zurich = FALSE, omit_toronto = FALSE) {
  # Create ensemble dataset from files on disk
  weather_data <- bind_rows(
    berlin   = read_weather_file(filename = "berlin.xlsx"),
    toronto  = read_weather_file(filename = "toronto.xlsx"),
    tel_aviv = read_weather_file(filename = "tel_aviv.xlsx"),
    zurich   = read_weather_file(filename = "zurich.xlsx"),
    .id = "city_code"
  )
  
  
  # Filter, conditionally
  if (omit_zurich) {
    weather_data <- weather_data |> 
      filter(city_code != "zurich")
  }
  
  if (omit_toronto) {
    weather_data <- weather_data |> 
      filter(city_code != "toronto")
  }
  
  # Return result
  return(weather_data)
}

read_weather_data(omit_zurich = TRUE, omit_toronto = TRUE) |> 
  count(city_code)

read_weather_data(omit_zurich = FALSE, omit_toronto = TRUE) |> 
  count(city_code)


# This can be useful if aiming at a possible early return:
read_weather_data <- function(omit_zurich = FALSE, omit_toronto = FALSE) {
  # Create ensemble dataset from files on disk
  weather_data <- bind_rows(
    berlin   = read_weather_file(filename = "berlin.xlsx"),
    toronto  = read_weather_file(filename = "toronto.xlsx"),
    tel_aviv = read_weather_file(filename = "tel_aviv.xlsx"),
    zurich   = read_weather_file(filename = "zurich.xlsx"),
    .id = "city_code"
  )
  
  
  # Filter, conditionally, and return
  if (!omit_zurich && !omit_toronto) {
    return(weather_data)
  } else if (omit_zurich && !omit_toronto) {
    weather_data |> 
      filter(city_code != "zurich")
  } else if (!omit_zurich && omit_toronto) {
    weather_data |> 
      filter(city_code != "toronto")
  } else {
    # Filter both
    weather_data |> 
      filter(city_code != "zurich") |> 
      filter(city_code != "toronto")
  }
}

read_weather_data(omit_toronto = TRUE) |> 
  count(city_code)

read_weather_data(omit_zurich = TRUE) |> 
  count(city_code)


## 5.4 Closures ----
weather_path <- function(filename) {
  # Returned value
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

# Here we creae a function that laods a particular dataset:
make_read_weather_file <- function(filename) {
  # Avoid odd effects due to lazy evaluation
  force(filename)
  
  # This function (closure) accesses the filename from the outer function
  f <- function() {
    read_weather_file(filename)
  }
  
  return(f)
}

read_berlin <- make_read_weather_file(filename = "berlin.xlsx")
read_toronto <- make_read_weather_file(filename = "toronto.xlsx")
read_tel_aviv <- make_read_weather_file(filename = "tel_aviv.xlsx")
read_zurich <- make_read_weather_file(filename = "zurich.xlsx")

read_toronto
# function() {
#   read_weather_file(filename)
# }

read_toronto()

read_berlin()

# Use closures as wrappers for other verbs / functions 
# (such functions are also called "adverbs")
loudly <- function(f) {
  force(f) 
  
  function(...) {
    args <- list(...)
    msg <- paste0(length(args), " argument(s)")
    message(msg)
    
    
    f(...)
  }
}

read_loudly <- loudly(read_weather_file)
read_loudly()
read_loudly("berlin.xlsx")

# The safely() funciton is another example from the purrr package:
cities <- list("berlin", "toronto", "milan", "tel_aviv")
try(map(.x = cities, .f = get_weather_data_for))

safely(get_weather_data_for)

map(.x = cities, .f = ~ safely(get_weather_data_for)(.))

safe_get_weather_data_for <- safely(.f = get_weather_data_for)
map(.x = cities, .f = ~ safe_get_weather_data_for(.))
