# 2
## A:
library(tidyverse)

retail_raw <- read_csv("data/retail_sales_raw.csv")

head(retail_raw)

## The observation_date column has the <date> class so R recognized the dates instead of treating them as text (<chr>). The two retail sales columns are <dbl>, meaning they're stored as numeric values.

## B:
retail_raw <- read_csv(
  "data/retail_sales_raw.csv",
  na = ".",
  col_types = cols(observation_date = col_date(format = "%Y-%m-%d"), RSAFS = col_double(), RSAFSNA = col_double())
)

## 2.3
retail_raw_long <- retail_raw |>
  select(observation_date, RSAFS, RSAFSNA) |>
  pivot_longer(cols = c(RSAFS, RSAFSNA), names_to = "series_id", values_to = "sales")

head(retail_raw_long)

## 2.4a

library(fpp3)

retail_ts <- retail_raw_long |>
  mutate(Month = yearmonth(observation_date)) |>
  as_tsibble(index = Month, key = series_id)

head(retail_ts)
has_gaps(retail_ts)

##2.5
head(canadian_gas)

key_vars(canadian_gas)
index_var(canadian_gas)

can_gas <- canadian_gas |>
  select(Month, Volume)

has_gaps(can_gas)

head(can_gas)

## 2.6
can_gas_recent <- can_gas |>
  filter(Month >= yearmonth("1990 Jan"))

head(can_gas_recent)

can_gas_recent <- can_gas_recent |>
  mutate(year = year(Month), month = month(Month, label = TRUE), quarter = quarter(Month))

head(can_gas_recent)

can_gas_recent |>
  as_tibble() |>
  group_by(month) |>
  summarize(avg_volume = mean(Volume))

can_gas_recent |>
  mutate(next_month = Month + 1, same_month_last_year = Month - 12) |>
  head()

can_gas_q <- can_gas |>
  filter(Month <= yearmonth("2004 Dec")) |>
  index_by(Quarter = yearquarter(Month)) |>
  summarize(Volume = sum(Volume))

head(can_gas_q)

can_gas_q |> autoplot(Volume)

## Because we're measuring total gas production over a whole quarter. Adding the monthly values gives us the quarterly total, taking the mean would give us average monthly production.

## 2.7
retail_ts |> autoplot(sales)

retail_ts |>
  autoplot(sales) +
  labs(
    title = "U.S. Retail and Food Services Sales",
    subtitle = "Seasonally adjusted vs. not seasonally adjusted",
    x = NULL,
    y = "Millions of dollars",
    color = NULL
  ) +
  theme_minimal()

can_gas |>
  autoplot(Volume) +
  labs(
    title = "Canadian Natural Gas Production",
    x = "Year",
    y = "Billions of Cubic Meters"
  ) +
  theme_minimal()

can_gas |>
  autoplot(Volume) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(
    title = "Canadian Natural Gas Production with Trend",
    x = "Year",
    y = "Billions of Cubic Meters"
  ) +
  theme_minimal()
