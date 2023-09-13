# ****************************************************
# 03 - Simple iteration ----
# URL: https://krlmlr.github.io/tidyprog/index.html
# ****************************************************

library(tidyverse)
library(here)

## 3.1 Vectors and columns ----

# Internally, tibbles are vectors of the same length.

# Get a vector with the files in a specific directory or our current project:
files <- dir(path = here("data/weather"), full.names = TRUE)
files

# Create a tibble with tibble::enframe():
files_df <- files |> 
  enframe()

files_df # name, value

# Or create a tibble using tibble::tibble() and providing 
# names for the columns:
tibble::tibble(filename = files)
# Now we have just one variable, "filename"

# Create a vector form a tibble column with dplyr::pull().
# By default, the rightmost column of the tibble is taken:
files_df |> 
  pull()

# Turn a specific column into a vector by providing the column
# name inside of pull():
files_df |> 
  pull(var = name)
# 1 2 3 4

### 3.1.1 Exercises ----

# 1. Investigate the output of fs::dir_ls() with tibble::enframe():
library(fs)
fs::dir_ls()
fs::dir_ls() |> 
  enframe()


## 3.2 Named vectors and two-column tibbles ----

# We want to load a table - here a dictionary detailing information to an
# id-like name - from an MS Excel file with readxl::read_excel():
dict <- readxl::read_excel(path = here("data/cities.xlsx"))
dict

# Use dplyr::pull():
dict |> 
  pull(var = weather_filename)

# Create absolute paths using here::here():
dict |> 
  pull(var = weather_filename) |> 
  here()

# Produce a named vector with tibble::deframe(), which is the
# inverse to tibble::enframe().

# Given an2-column tibble, tibble::deframe() will by default use
# the first column for the names and the second column for the
# values of the resulting vector.

# Given a 1-column tibble, deframe() creates an unnamed vector.

# Given a tibble with more than two columns, deframe() will
# use the first two columns for the name-vector pairs and 
# ignroe the rest, producing a warning.

weather_filenames <- dict |> 
  select(city_code, weather_filename) |> 
  deframe()

weather_filenames
# Now we have a named vector of paths to the excel files.

# Use names() to access the anmes of the named vector:
weather_filenames |> names()
# berlin, toronto, tel_aviv, zurich

# Some operations cause the names to be lost!
paste0("'", weather_filenames, "'")

weather_filenames |> 
  here()
# Produces the full paths

# A possible solution is to change the order of the transformation:
dict |> 
  mutate(weather_filename_here = here(weather_filename))

dict |> 
  mutate(weather_filename_here = here(weather_filename)) |> 
  select(city_code, weather_filename_here)

dict |> 
  mutate(weather_filename_here = here(weather_filename)) |> 
  select(city_code, weather_filename_here) |> 
  deframe()


## 3.3 Indexing/subsetting ----
input_files <- dict |> 
  select(city_code, weather_filename) |> 
  deframe()

input_files
names(input_files)
input_files[1]
input_files["berlin"]
input_files[1:2]
input_files[c(1, 2)]
input_files[c("berlin", "tel_aviv")]
input_files[c(TRUE, FALSE, TRUE, FALSE)]

# Consistent pipe-friendly access of single elements:
input_files |> 
  pluck(1)

input_files |> 
  pluck("berlin")


## 3.4 Construction ----

# Vectors and lists can have named elements:
rlang::set_names(x = 1:3, nm = letters[1:3])

# The vctrs package defines data types for lists where all elements have the same type.
# This is stricter than the base::list(), but more powerful than base::vector()
vctrs::list_of(1, 2, 3)
try(vctrs::list_of(1, 2, "3"))
# Error : Can't combine <double> and <character>

vctrs::list_of(letters[1:3], "e")


## 3.5 Processing multiple files ----

# In lists and vectors, we can use double brackets to 
# choose a single entry:
input_files[[1]]

here(input_files[[1]])

readxl::read_excel(path = here(input_files[[1]]))

# We cannot read in all files directly:
try(readxl::read_excel(path = here(input_files)))

# Unlike here(), the readxl::read_excel() function can only process
# one file at a time (it is not "vectorized").

# We use purrr::map() to work through each element in 
# the vector input_files.
# Note that purrr::map() returns a "list" object.
input_data <- map(.x = input_files, .f = ~ readxl::read_excel(path = here(.)))

# This is equivalent to:
input_data <- list(
  berlin   = readxl::read_excel(path = here(input_files[[1]])),
  toronto  = readxl::read_excel(path = here(input_files[[2]])),
  tel_aviv = readxl::read_excel(path = here(input_files[[3]])),
  zurich   = readxl::read_excel(path = here(input_files[[4]]))
)

# We can include purrr::map() in the pipe:
input_files |> 
  map(.f = ~ readxl::read_excel(path = here(.)))


## 3.6 Manipulating all datasets ----

# From each tibble in the named list of tibbles inside
# input_data, we only want the column "time" and all columns
# containing "emperature":
input_data[[1]] |> 
  select(time, contains("emperature"))

# Use purrr::map() to apply this to all entries in the list:
# We need an explicit dot (.) inside select() to indicate
# the position where each sub-dataset will be plugged in:
input_data |> 
  map(.f = ~ select(., time, contains("emperature")))

# Extend the above code to preseve only ovservations where
# tempeerature is larger or equal than 14 degrees celcius:
input_data |> 
  map(.f = ~ select(., time, contains("emperature"))) |> 
  map(.f = ~ filter(., temperature >= 14))

# Create a custom function for that specific purpose in a 
# call to purrr::map():
find_good_times <- function(data) {
  data |> 
    select(time, contains("emperature")) |> 
    filter(temperature >= 14)
}


find_good_times(input_data[[4]])

# Use purrr::map() to act on the entire dataset:
good_times <- map(.x = input_data, .f = ~ find_good_times(.))
good_times

# purrr::map() allows for the following shortcut notation for 
# functions with only one argument:
# map(input_data, function_with_one_argument)
map(input_data, find_good_times)


## 3.7 Typed output ----

dict <- readxl::read_excel(path = here("data/cities.xlsx"))

input_data <- dict %>%
  select(city_code, weather_filename) %>%
  deframe() %>%
  map(.f = ~ readxl::read_excel(path = here(.)))

# Get the number of rows in each tibble in input_data:
input_data |> 
  map(.f = ~ nrow(.))
# 49 observations per city

# Each time an integer is produced.
# Therefore we use purrr::map_int() to create a named
# integer vector instead of a named list as with purrr::map():
input_data |> 
  map_int(.f = ~ nrow(.))

# If the output is of type character, use purrr::map_chr():
input_data |> 
  map_chr(.f = ~ nrow(.))
# Automatic coercion from integer to character was deprecated
# in purrr 1.0.0.
# Instead use:
input_data |> 
  map_chr(.f = ~ as.character(nrow(.)))
# No warning message

# map_int(), map_dbl(), map_chr(), map_lgl(), map_raw()

# END