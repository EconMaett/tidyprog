# **********************************************************************
# 06 - Non-rectangular data ----
# URL: https://krlmlr.github.io/tidyprog/index.html
# **********************************************************************

library(tidyverse)
library(here)

# This chapter gives an example for processing deeply nested lists 
# and converting them to data frames.

# Such data occurs in JSON-files, raw data from web APIs.


## 6.1 Traversing ----

# We are now working with the results from downloading geo-location data from
# photon.komoot.de.
berlin <- readRDS(file = here("data/komoot-berlin.rds"))
berlin
str(berlin)

# The file has a complex list structure.
# We can access its components with the dollar-operator:
berlin$type
berlin$features
berlin$features[[1]]
berlin$features[[1]]$properties$city

# With the function purrr::pluck(), there is a more universal tool for accessing
# elements of more complex lists:

# Access the "type" element
berlin |> 
  pluck("type")
# "FeatureCollection"

# This is equivalent to the base command
berlin[["type"]]
# "FeatureCollection"


# Access the "feature" element
berlin |> 
  pluck("features")

# Because the command above returned a list with exactly one list-element,
# we want to access the list inside that entry directly
berlin |> 
  pluck("features", 1)

# This is equivalent to the following base command
berlin[["features"]][[1]]


# # From the list returned above we want to access the element "geometry"
berlin |> 
  pluck("features", 1, "geometry")

# This is equivalent to the base command
berlin[["features"]][[1]][["geometry"]]


# From the list above we want to access the list-entry "coordinates"
berlin |> 
  pluck("features", 1, "geometry", "coordinates")

# From the same list we want to access the entry "country"
berlin |> 
  pluck("features", 1, "properties", "country")
# "Germany"


# Note that the purrr::pluck() function is pipe-able:
berlin |> 
  pluck("features", 1) |> 
  pluck("properties", "country")
# "Germany"


## 6.2 Iterating and traversing ----
komoot <- readRDS(file = here("data/komoot.rds"))
komoot
# The new tibble is slightly different from the "berlin" object from before because
# the list-of-2 is stored for each city in the "content" column.

# By using pull() on the "content" column, we can produce a list containing
# information for all cities:
komoot_content <- komoot |> 
  pull(var = "content")

berlin <- komoot_content |> 
  pluck(1)

berlin |> 
  pluck("features", 1, "geometry", "coordinates")


toronto <- komoot_content |> 
  pluck(2)

toronto |> 
  pluck("features", 1, "geometry", "coordinates")

# With purrr::map() we can access the same element of the respective list
# for each city:
komoot_content |> 
  map(.f = ~ pluck(., "features", 1, "geometry", "coordinates"))

# With purrr::map() we can also use a shorthand notation for this, without the
# need to use pluck().

# We can just give it a list of the arguments which we would normally use
# as arguments for pluck():
komoot_content |> 
  map(list("features", 1, "geometry", "coordinates"))

# Note that the access path can also be stored in a variable:
accessor <- list("features", 1, "geometry", "coordinates")
coordinates <- komoot_content |> 
  map(accessor)
coordinates


## 6.3 Plucking multiple locations ----

# Define two locations in the city-lists:
accessor_coords <- list("features", 1, "geometry", "coordinates")
komoot_content |> 
  map(accessor_coords)

accessor_country <- list("features", 1, "properties", "country")
komoot_content |> 
  map(accessor_country)

# Combine them in a list of lists and hand it over to map(),
# inside of another map():
accessors <- list(coords = accessor_coords, country = accessor_country)
accessors |> 
  map(.f = ~ map(komoot_content, .))


## 6.4 Flattening ----

# It can happen that we end up with lists that are unnecessarily deep and we
# want to flatten them.

# An example for a deep list:
coordinates |> 
  pluck(1)

# Chop off a layer of a list and end up with a vector with one of the functions
# purrr::flatten_*():
coordinates |> 
  pluck(1) |> 
  flatten_dbl()

# Use map() to apply this to the entire list of city coordinates:
coordinates |> 
  map(.f = flatten_dbl)


## 6.5 Transposing ----
# Apply purrr::transpose() to the list "coordinates":
coordinates |> 
  transpose()

# What was previously a list with 4 elements of which each one was a list
# of 2 elements is now a list of 2 elements of which each one is a list
# of 4 elements.

# With flatten_dbl() we can simplify the structure so that we end up with
# a list of 2, where each element consists of a vector of 4 elements.

# The first vector contains the longitude and the second the latitude of the cities:
coordinates_transposed <- coordinates |> 
  transpose() |> 
  map(.f = ~ flatten_dbl(.))

coordinates_transposed


## 6.6 Rectangling ----

# A tibble is internally a list of vectors of equal length.
# We can make a tibble out of the unnamed list "coordinates_transposed":
coordinates_transposed |> 
  rlang::set_names(nm = c("lon", "lat"))

coordinates_transposed |> 
  rlang::set_names(nm = c("lon", "lat")) |> 
  as_tibble()

# If you want to keep the names open for now, but still get a tibble,
# set as_tibble()'s argument .name_repair = "universal"
coordinates_transposed |> 
  as_tibble(.name_repair = "universal")

coordinates_transposed |> 
  as_tibble(.name_repair = "universal") |> 
  rename(lon = ...1, lat = ...2)


## 6.7 Accessing APIs ----

# When dealing with web-APIs, query results come frequently in the 
# JavaScript Object Notation (JSON) format.

# A way to communicate with APIs is provided by the "httr" package.
# Use httr::GET() to execute a GET-query.
req <- httr::GET(url = "https://data.snb.ch/api/cube/conretail/dimensions/en")
req

# If you are using this command in a script, you need to wait until the query is finished
# processing: 
httr::stop_for_status(req)

# The result of the query can be accessed via httr::content()
content <- httr::content(req)
content

# As you can see, the result, as it is displayed in R, is already a 
# nested list at this point.

# The object did originally come as a JSON object though, which you can see
# if you want to look at the literal results of the query:
text_content <- httr::content(req, as = "text")
cat(text_content)


# The "jsonlite" package has the function jsonlite::prettify() to
# display a cone-line JSON-structure in a more clearly laid-out manner:
cat(jsonlite::prettify(text_content))

# END
