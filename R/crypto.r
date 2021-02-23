
# Tim's crypto currency app

library(tidyverse)
library(janitor)
library(timetk)
library(rvest)

start_over <- "FALSE"
n_coin <- 100

# Import helper functions

source(here::here("R", "helper_functions.r"))

# List of coins, ranked by market cap

if (start_over == "TRUE") {
  
  start <- lubridate::now()
  master_coins_list <- get_marketcap_table(n = 100)
  lubridate::now() - start
  
} else {
  
  new_coins_list <- get_marketcap_table(n = n_coin)
  
  master_coins_list <-
    
    new_coins_list %>% 
    bind_rows(
      master_coins_list %>% 
        filter(currency %notin% new_coins_list$currency) %>% 
        mutate(rank = NA_integer_)
    )
  
}

# Get prices of historical prices back to November 2018 scraped from 

start <- lubridate::now()
coins_prices <-
  coins_list$currency %>% 
  map_df(
    safely_get_historical_prices,
    start_date = "20181101"
  ) %>%
  unnest()
lubridate::now() - start

saveRDS(coins_prices, here::here("R", "coins_prices.rds"))

# Tims trading strategy
