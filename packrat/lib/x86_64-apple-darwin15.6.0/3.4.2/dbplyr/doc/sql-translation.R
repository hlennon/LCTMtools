## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
options(tibble.print_min = 4L, tibble.print_max = 4L)

## ---- message = FALSE----------------------------------------------------
library(dbplyr)
library(dplyr)

## ------------------------------------------------------------------------
# In SQLite variable names are escaped by double quotes:
translate_sql(x)
# And strings are escaped by single quotes
translate_sql("x")

## ------------------------------------------------------------------------
translate_sql(x == 1 && (y < 2 || z > 3))
translate_sql(x ^ 2 < 10)
translate_sql(x %% 2 == 10)

## ------------------------------------------------------------------------
translate_sql(substr(x, 5, 10))
translate_sql(log(x, 10))

## ------------------------------------------------------------------------
translate_sql(1)
translate_sql(1L)

## ------------------------------------------------------------------------
translate_sql(if (x > 5) "big" else "small")

## ---- error = TRUE-------------------------------------------------------
translate_sql(mean(x, na.rm = TRUE))
translate_sql(mean(x, trim = 0.1))

## ------------------------------------------------------------------------
translate_sql(glob(x, y))
translate_sql(x %like% "ab%")

## ------------------------------------------------------------------------
knitr::include_graphics("windows.png", dpi = 200)

## ------------------------------------------------------------------------
translate_sql(mean(G))
translate_sql(rank(G))
translate_sql(ntile(G, 2))
translate_sql(lag(G))

## ------------------------------------------------------------------------
translate_sql(cummean(G), vars_order = "year")
translate_sql(rank(), vars_group = "ID")

## ---- eval = FALSE-------------------------------------------------------
#  mutate(players,
#    min_rank(yearID),
#    order_by(yearID, cumsum(G)),
#    lead(G, order_by = yearID)
#  )

## ------------------------------------------------------------------------
con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
flights <- copy_to(con, nycflights13::flights)
airports <- copy_to(con, nycflights13::airports)

## ------------------------------------------------------------------------
flights %>%
  select(contains("delay")) %>%
  show_query()

flights %>%
  select(distance, air_time) %>%  
  mutate(speed = distance / (air_time / 60)) %>%
  show_query()

## ------------------------------------------------------------------------
flights %>% 
  filter(month == 1, day == 1) %>%
  show_query()

## ------------------------------------------------------------------------
flights %>% 
  arrange(carrier, desc(arr_delay)) %>%
  show_query()

## ------------------------------------------------------------------------
flights %>%
  group_by(month, day) %>%
  summarise(delay = mean(dep_delay)) %>%
  show_query()

