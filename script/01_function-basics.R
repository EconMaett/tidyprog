# *********************************************************
# 01 - Function basics ----
# URL: https://krlmlr.github.io/tidyprog/index.html
# *********************************************************

## Load packages ----
library(tidyverse)
library(readxl)
library(here)

## 1.2.1 Data ----
# Hourly weather measurements in four cities between 2019-04-28, 3pm
# and 2019-04-30, 3pm (49 observations per city).
berlin   <- read_excel(path = "data/weather/berlin.xlsx")
toronto  <- read_excel(path = "data/weather/toronto.xlsx")
tel_aviv <- read_excel(path = "data/weather/tel_aviv.xlsx")
zurich   <- read_excel(path = "data/weather/zurich.xlsx")

weather_data <- bind_rows(
  berlin = berlin,
  toronto = toronto,
  tel_aviv = tel_aviv,
  zurich = zurich,
  .id = "city_code"
)

### .2.2 Exploration
weather_data

# Plot humidity vs pressure
weather_data |> 
  ggplot(mapping = aes(x = pressure, y = humidity, color = city_code)) +
  geom_path() +
  theme_bw()

# Number of occurrences of types of weather per city
weather_data |> 
  ggplot(mapping = aes(x = city_code)) +
  geom_bar(mapping = aes(fill = summary)) +
  theme_bw()

weather_data |> 
  ggplot(mapping = aes(x = city_code)) +
  geom_bar(mapping = aes(fill = summary), position = position_dodge2("dodge", preserve = "single")) +
  theme_bw()

# Line plot with different line types and additional visualization
# of the line range (here, difference between apparent and
# actual temperature):
temperature_data <- weather_data |> 
  select(city_code, time, temperature, apparentTemperature) |> 
  gather(kind, temperature, -city_code, -time) |> 
  mutate(
    apparent = (kind == "apparentTemperature")
  ) |> 
  select(-kind)

temperature_data

temperature_data |> 
  ggplot(mapping = aes(x = time, color = city_code)) +
  geom_linerange(data = weather_data, mapping = aes(ymin = temperature, ymax = apparentTemperature)) +
  geom_line(mapping = aes(linetype = apparent, y = temperature)) +
  theme_bw()

# Relation of temperature difference between actual and apparent temperature
# (cf. line range in last plot) with wind speed, shown as scatter plot.
weather_data |> 
  mutate(
    apparentTemperatureReduction = temperature - apparentTemperature
  ) |> 
  filter(city_code != "tel_aviv") |> 
  ggplot(mapping = aes(x = windSpeed, y = apparentTemperatureReduction)) +
  geom_point(mapping = aes(color = city_code)) +
  theme_bw()

### 1.2.3 Further dplyr transformations ----

# Compare measurements of the same observable at two different
# points in time by adding a lag column:
weather_data |> 
  group_by(city_code) |> 
  mutate_at(.vars = vars(temperature, pressure, humidity), .funs = list(lag = lag)) |> 
  ungroup()

# Count observations per category or combinations of categories:
weather_data |> 
  count(city_code)
# 49 observations per city

weather_data |> 
  count(city_code, summary)

# Use summarize() to create a tibble with mean and maximum temperature
# for each city:
weather_data |> 
  group_by(city_code) |> 
  summarize(
    temperature_mean = mean(temperature),
    temperature_max = max(temperature)
  ) |> 
  ungroup()

# Compute and display summary data for all numeric variables
weather_data |> 
  group_by(city_code) |> 
  summarize_if(
    .predicate = is.numeric, 
    .funs = list(
      mean = mean,
      sd = sd,
      min = min,
      max = max
      )
    ) |> 
  ungroup() |> 
  gather(key, value, -city_code) |> 
  separate(col = key, into = c("indicator", "fun")) |> 
  xtabs(formula = value ~ city_code + indicator + fun) |> 
  ftable()

# END