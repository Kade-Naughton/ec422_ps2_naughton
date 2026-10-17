library(tidyquant)
library(tidyverse)

# unemp_raw <- tq_get(
#   c("UNRATE", "ORUR", "CAUR", "TXUR", "NYUR"),
#   get = "economic.data",
#   from = "1976-01-01"
# )
# 
# head(unemp_raw)
# 
# dir.create("data", showWarnings = FALSE)
# 
# write_rds(unemp_raw, "data/unemp_raw.rds")

unemp_raw = read_rds("data/unemp_raw.rds")

unemp <- unemp_raw |>
  rename(unemployment_rate = price) |>
  mutate(
    region = case_when(
      symbol == "UNRATE" ~ "United States",
      symbol == "ORUR" ~ "Oregon",
      symbol == "CAUR" ~ "California",
      symbol == "TXUR" ~ "Texas",
      symbol == "NYUR" ~ "New York"
    )
  )


unemp <- unemp |>
  arrange(region, date) |>
  group_by(region) |>
  mutate(
    unemployment_rate = replace(
      unemployment_rate,
      date == as.Date("2025-10-01"),
      unemployment_rate[date == as.Date("2025-09-01")]
    )
  ) |>
  ungroup()

# Wrangling data
unemp <- unemp |>
      arrange(region, date) |>
      group_by(region) |>
      mutate(unemployment_rate_change = unemployment_rate - lag(unemployment_rate)) |>
      ungroup()    

unemp_recent <- unemp |>
  filter(date >= as.Date("2015-01-01"))

unemp_summary <- unemp |>
  group_by(region) |>
  summarize(
    mean_rate = mean(unemployment_rate, na.rm = TRUE),
    min_rate  = min(unemployment_rate,  na.rm = TRUE),
    max_rate  = max(unemployment_rate,  na.rm = TRUE)
  )

# Find extremes of data
unemp_top_increases <- unemp |>
  slice_max(unemployment_rate_change, n = 5, with_ties = TRUE) |>
  select(region, date, unemployment_rate, unemployment_rate_change)

unemp_top_decreases <- unemp |>
  slice_min(unemployment_rate_change, n = 5, with_ties = TRUE) |>
  select(region, date, unemployment_rate, unemployment_rate_change)

# Exclude 2020 due to abnormalities via Covid
unemp_no_covid <- unemp |>
  filter(year(date) != 2020)

unemp_top_increases_no_covid <- unemp_no_covid |>
  slice_max(unemployment_rate_change, n = 5, with_ties = TRUE) |>
  select(region, date, unemployment_rate, unemployment_rate_change)

unemp_top_decreases_no_covid <- unemp_no_covid |>
  slice_min(unemployment_rate_change, n = 5, with_ties = TRUE) |>
  select(region, date, unemployment_rate, unemployment_rate_change)

# Unemployment/time plot
ggplot(unemp, aes(x = date, y = unemployment_rate, color = region)) +
  geom_line() +
  labs(title = "Monthly Unemployment Rate by Region",
       x = "Date",
       y = "Unemployment Rate (%)",
       color = "Region") +
  theme_minimal()

# Separate plot for each region
ggplot(unemp, aes(x = date, y = unemployment_rate, color = region)) +
  geom_line() +
  facet_wrap(~ region) +
  labs(title = "Monthly Unemployment Rate by Region",
       x = "Date",
       y = "Unemployment Rate (%)") +
  theme_minimal() +
  theme(legend.position = "none")

# Mean unemployment by region bar chart, ordered lowest to highest
ggplot(unemp_summary, aes(x = fct_reorder(region, mean_rate), y = mean_rate)) +
  geom_col() +
  labs(title = "Average Unemployment Rate by Region, 1976 to Present",
       x = "Region",
       y = "Mean Unemployment Rate (%)") +
  theme_minimal()

# Oregon unemployment relative to US
# Chose because plots in previous questions make it hard to tell if Oregon
# is going better or worse compared to the US. Interest in oregon because 
# I've lived here my whole life and plan to stick around so I care for the 
# state.
oregon_gap <- unemp |>
  select(date, region, unemployment_rate) |>
  pivot_wider(names_from = region, values_from = unemployment_rate) |>
  mutate(oregon_minus_us = Oregon - `United States`)

ggplot(oregon_gap, aes(x = date, y = oregon_minus_us)) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Oregon Unemployment Rate Relative to the U.S.",
       x = "Date",
       y = "Oregon minus U.S. (percentage points)") +
  theme_minimal()


