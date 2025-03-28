# code from week 11 lab

library(tidyverse)
library(plotly)
library(widgetframe)
library(tidytext)

### load Starbucks and state-level data ###
sb_locs <- read_csv("https://raw.githubusercontent.com/JSC370/JSC370-2025/refs/heads/main/data/starbucks/starbucks-locations.csv")

sb_nutr <- read_csv("https://raw.githubusercontent.com/JSC370/JSC370-2025/refs/heads/main/data/starbucks/starbucks-menu-nutrition.csv")

usa_pop <- read_csv("https://raw.githubusercontent.com/JSC370/JSC370-2025/refs/heads/main/data/starbucks/us_state_pop.csv")

usa_states <- read_csv("https://raw.githubusercontent.com/JSC370/JSC370-2025/refs/heads/main/data/starbucks/states.csv")

### Merge data ###
starbucks <- sb_locs %>%
  filter(!is.na(`Longitude`), !is.na(`Latitude`)) %>%
  rename(
    State = `State/Province`,
    Longitude = `Longitude`,
    Latitude = `Latitude`
  )

starbucks_by_state <- starbucks %>%
  group_by(State) %>%
  summarize(n_stores = n())

# need state abbreviations
usa_pop_abbr <- 
  full_join(usa_pop, usa_states, by = c("state" = "State"))

# Fix the join - using State from starbucks_by_state to match with Abbreviation from usa_pop_abbr
starbucks_by_state <- full_join(starbucks_by_state, usa_pop_abbr,
                           by = c("State" = "Abbreviation"))


### Get topwords from menu items ###

topwords <- sb_nutr |>
  unnest_tokens(word, Item, token="words") |>
  group_by(word) |>
  summarise(word_frequency = n()) |>
  arrange(across(word_frequency, desc)) |>
  head(10)

