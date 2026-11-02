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
  col_types = cols(
    observation_date = col_date(format = "%Y-%m-%d"),
    RSAFS = col_double(),
    RSAFSNA = col_double()
  )
)

## 2.3
retail_raw_long <- retail_raw |>
  select(observation_date, RSAFS, RSAFSNA) |>
  pivot_longer(
    cols = c(RSAFS, RSAFSNA),
    names_to = "series_id",
    values_to = "sales"
  )

head(retail_raw_long)

# 2.4a

library(fpp3)

retail_ts <- retail_raw_long |>
  mutate(Month = yearmonth(observation_date)) |>
  as_tsibble(index = Month, key = series_id)

head(retail_ts)
has_gaps(retail_ts)
