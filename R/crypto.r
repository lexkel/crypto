
# Tim's cryptocurrency app

library(tidyverse)
library(janitor)
library(timetk)
library(rvest)
library(googlesheets4)

top_n <- 100
start_over <- "TRUE"
sheet_url <- "https://docs.google.com/spreadsheets/d/17HoyJXunYyzzmc5IHbr8G_1xk0pMwXROo1f9fjpdVV0"

# Import helper functions

source(here::here("R", "helper_functions.r"))

if (start_over == "TRUE") {
  
  # Get latest marketcap coins list
  
  master_coins_list <- get_marketcap_table(n = top_n)
  
  get_coins_prices <- 
    master_coins_list %>% 
    mutate(
      start_date = "2018-11-01",
      end_date = as.character(Sys.Date())
    )
  
  master_coins_prices <-
    pmap_df(
      list(
        get_coins_prices$currency,
        get_coins_prices$start_date,
        get_coins_prices$end_date
      ),
      safely_get_historical_prices
    ) %>%
    unnest(cols = data) %>% 
    group_by(date) %>% 
    mutate(rank = rank(desc(market_cap)))
  
  # overwrite the existing googlesheets with new data?
  
  clear_data_googlesheets(sheet_url)
  
  save_data_googlesheets(
    data = master_coins_prices, 
    sheet = sheet_url
  )
  
} else {
  
  # Get latest marketcap coins list
  
  new_coins_list <- get_marketcap_table(n = top_n)
  
  # Load existing data from googlesheets
  
  existing_coins_prices <- read_sheet(sheet_url)
  existing_coins_list <- unique(existing_coins_prices$currency)
  
  # Create master list
  
  master_coins_list <-
    new_coins_list %>% 
    bind_rows(
      tibble(
        rank = NA_integer_,
        currency = existing_coins_list[existing_coins_list %notin% new_coins_list$currency]
      )
    )
  
  # Get new prices data append to existing prices
  
  get_coins_prices <-
    master_coins_list %>% 
      select(rank, currency) %>% 
      left_join(
        existing_coins_prices %>% 
          group_by(currency) %>% 
          summarise(
            start_date = max(date),
            .groups = "drop"
          ) %>% 
          mutate(start_date = as.character(as.Date(start_date) + 1)), 
        by = "currency" 
      ) %>% 
      mutate(
        start_date = ifelse(is.na(start_date), "2018-11-01", start_date),
        end_date = Sys.Date()
      ) %>%
      filter(as.Date(start_date) <= Sys.Date()) 
  
  new_coins_prices <-
    pmap_df(
      list(
        get_coins_prices$currency,
        get_coins_prices$start_date,
        get_coins_prices$end_date
      ),
      safely_get_historical_prices
    ) %>%
    unnest(cols = data)
  
  save_data_googlesheets(new_coins_prices, sheet = sheet_url)
  
  master_coin_prices <-
    existing_coins_prices %>% 
      bind_rows(new_coins_prices) %>%
      left_join(
        master_coins_list, 
        by = "currency"
      ) %>% 
      arrange(rank, desc(date)) %>% 
      select(-rank) %>% 
      group_by(date) %>% 
      mutate(rank = rank(desc(market_cap)))
  
}


# Tim's trading strategy

# My strategy is basically to hold 5 tokens/coins from the coin market cap rank by 
# capitalisation from the chosen level (ie. 10-15, 50-55, 100-105) and then rebalance 
# it every week (sell off the profitable ones and buy the ones that went backwards, 
# unless they went out of the range above which would mean I sell it completely and 
# buy a new one). I also rebalance out of session if something goes up by a significant 
# amount (ie sell off a portion of it and reinvest in others) It would be good to 
# benchmark it against BTC and against holding the top 10. But most interested to run 
# different rebalancing strategy scenarios (ie rebalancing monthly, not doing an out of 
# session rebalance etc.) and/or whether a weighted index would be better (ie. I hold 
# more % of the index in the higher capped coins). I have no idea if any of this is at 
# all possible or hard or easy so I'll leave it to you to tell me what is any isn't possible. 


