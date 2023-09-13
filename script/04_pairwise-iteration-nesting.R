# ****************************************************
# 03 - Pairwise iteration and nesting ----
# URL: https://krlmlr.github.io/tidyprog/index.html
# ****************************************************

library(tidyverse)
library(here)

dict <- readxl::read_excel(path = here("data/cities.xlsx"))

input_data <- dict |> 
  select(city_code, weather_filename) |> 
  deframe() |> 
  map(.f = ~ readxl::read_excel(path = here(.)))

find_good_times <- function(data) {
  data |> 
    select(time, contains("emperature")) |> 
    filter(temperature >= 14)
}

good_times <- input_data |> 
  map(.f = find_good_times)

good_times


## 4.1 Manipulating pairwise ----

# Prepare a list of future output file names:
output_filenames <- tempfile(pattern = names(good_times), fileext = ".csv")
output_filenames

# We want to use readr::write_csv() to write each tibble into the
# respective file.

# We implement a file-centric wrapper function that takes the file name
# as the first argument and also prints a message every time a file is written.
# We use purrr::map2() to handle this:
process_csv <- function(file, data) {
  readr::write_csv(x = data, file = file)
  message("Writing ", file)
  invisible(file)
}

# Map over 2 inputs with map2()
map2(
  .x = good_times, 
  .y = output_filenames, 
  .f = ~ process_csv(file = ..2, data = ..1)
  )

# Because process_csv() returns the file name, it is available as output.
# Since we are not interested in the side-effects of write_csv()
# and do not want to display them in the output, we can use
# walk2() instead of map2():
walk2(
  .x = good_times, 
  .y = output_filenames, 
  .f = ~ process_csv(file = ..2, data = ..1)
  )
# Now we only get the messages in the output.

print(walk2(
  .x = good_times, 
  .y = output_filenames, 
  .f = ~ process_csv(file = ..2, data = ..1)
  ))
# When explicitly printing the walk2() we also see the invisibly
# returned outputs.


### 4.1.1 Exercises ----
# 1. What does the following code display?
good_times |> 
  walk2(.y = output_filenames, .f = ~ readr::write_csv(..1, ..2)) |> 
  map_int(nrow)
# berlin toronto tel_aviv zurich
#     16       0      49       1


## 4.2 Moving to tibble-land ----

# How to combine the abilities of purrr::map() & co, which work on vectors and
# lists, with the commonly used data structure, the tibble?

# We start with the named list of tibbles called input_data
input_data
# and with dict
dict

# Calling tibble::enframe() to produce a data frame from the named list called
# "input_data" leads to a surprising but useful result:
nested_input_data <- input_data |> 
  enframe()

nested_input_data

# This is because lists are also vectors.
# In our case each list entry contains a tibble, which can be "nested" into
# each entry of the "value" column.

# Starting with the tibble "dict" we see how dplyr::mutate() and purrr::map()
# can work together to produce a similar result:
dict |> 
  select(city_code, weather_filename) |> 
  mutate(
    data = map(weather_filename, ~readxl::read_excel(here(.)))
  )
# This works because R interprets the columns of tibbles as vectors, which can be
# fed to map().
# To simplify the map() call, we create an intermediate column:
dict |> 
  select(city_code, weather_filename) |> 
  mutate(path = here(weather_filename)) |> 
  mutate(data = map(path, readxl::read_excel))

# Staying in "tibble-land" as long as possible helps retaining other important
# components of the data you are processing, so that you can keep using familiar
# data transformation tools.
dict_data <- dict |> 
  mutate(
    data = map(weather_filename, ~ readxl::read_excel(here(.))),
    rows = map_int(data, nrow)
  ) |> 
  select(-weather_filename)

dict_data

# This pattern can also be used with the map2() family of functions:
dict_dta_with_desc <- dict_data |> 
  mutate(
    desc = map2_chr(
      name, rows,
      ~ paste0(..2, " rows in data for ", ..1)
    )
  )

dict_dta_with_desc
# Because mutate() always appends to the end, the most recently added column
# can always be accessed with pull():
dict_dta_with_desc |> 
  pull()

# More generally, pmap() supports functions with an arbitrary number of
# arguments:
dict_data |> 
  mutate(
    cols = map_int(data, ncol),
    desc = pmap_chr(
      list(name, rows, cols),
      ~ paste0(..2, " rows and ", ..3, " cols in data for ", ..1)
    )
  )


## 4.3 Nesting and unnesting ----

# We start with the tibble "dict_data" which includes the nested tibbles
# in the "data" column
dict_data

# If we want to actually look at the data we can directly use tidyr::unnest()
# on the whole tibble, which by default acts on all list-columns.

# This expands our tibble by repeating the formerly unnested column entries
# as many times as each nested tibble has rows:
dict_data |> 
  unnest(cols = c("data"))

# This is very similar to bind_rows() of the "data" column:
dict_data |> 
  pull(data) |> 
  bind_rows()

check_columns_same <- function(x, y) {
  stopifnot(exprs = identical(colnames(x), colnames(y)))
}

bind_rows <- function(data_frames) {
  # Called for the side effect
  reduce(.x = data_frames, .f = check_columns_same)
  
  dplyr::bind_rows(data_frames)
}

try(expr = dict_data |> 
      pull(data) |> 
      bind_rows()
    )
# Error in fn(out, elt, ...) : 
#   identical(colnames(x), colnames(y)) is not TRUE

# Data flattened in this way is useful if the parts can be combined
# naturally into a larger dataset.

# Iterating over columns in the nested view corresponds to grouped operations
# in the flat view.

dict_data |> 
  mutate(n = map_int(data, nrow)) |> 
  select(-data)

dict_data |> 
  unnest(cols = c("data")) |> 
  count(name)

# Inversely, if  you want to have a more condensed view of your data,
# you can nest again.

# By default, the function tidyr::nest() will nest all data.

# Therefore it is useful to tell it which columns to ignore:
dict_data |> 
  unnest(cols = c("data")) |> 
  nest(data = c(-city_code, -name, -lng, -lat))

# Using this, we structure our data in new, customized ways.
# For processing of daily data over all cities, we create a new column "date":
dict_data |> 
  unnest(cols = c("data")) |> 
  mutate(date = as.Date(time)) |> 
  nest(data = c(-date))

# END