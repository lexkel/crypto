
`%notin%` <- Negate(`%in%`)

get_marketcap_table <- function(n = 100) {
  
  market_cap_table <-
    read_html("https://www.coingecko.com/en") %>% 
    html_node(xpath = '//table') %>%
    html_node(xpath = '//tbody') %>% 
    html_nodes(".center") %>% 
    html_nodes("a") %>% 
    html_attr("href") %>% 
    unique() %>% 
    str_remove("/en/coins/") %>% 
    as_tibble() %>%
    mutate(rank = row_number()) %>%
    select(rank, "currency" = value)
  
  closeAllConnections()
  gc()
  
  if (n > 100) {
  
    pages <- seq(2, ceiling(n / 100))
    
    market_cap_table <-
      market_cap_table %>%
      bind_rows(
        map_dfr(
          pages,
          function(x) {
            data <- 
              read_html(paste0("https://www.coingecko.com/en?page=", x)) %>% 
              html_node(xpath = '//table') %>%
              html_node(xpath = '//tbody') %>% 
              html_nodes(".center") %>% 
              html_nodes("a") %>% 
              html_attr("href") %>% 
              unique() %>% 
              str_remove("/en/coins/") %>% 
              as_tibble() %>%
              mutate(rank = seq(x*100-99, x*100)) %>%
              select(rank, "currency" = value)
            Sys.sleep(1)
            closeAllConnections()
            gc()
            return(data)
          }
        ) %>% 
        filter(rank <= n) 
      )
    
  }
  
  return(market_cap_table)
  
}

get_historical_prices <- function(currency, start_date, end_date) {
  
  # currency = "bitcoin"
  # start_date = Sys.Date() %-time% "1 year"
  # end_date = Sys.Date()
  
  cat("| ", tolower(currency), "\n")
  
  page <- function(...) {
    
    suppressPackageStartupMessages(library(timetk))
    
    page <- str_c(
      "https://www.coingecko.com/en/coins/",
      str_replace_all(tolower(currency), " ", "-"),
      "/historical_data/usd?",
      "start_date=",
      start_date,
      "&end_date=",
      end_date
    )
    
    return(page)
    
  }
  
  data <-
    page(currency) %>% 
    read_html()
  
  Sys.sleep(1.5)
  closeAllConnections()
  gc()
  
  data <- 
    data %>% 
    html_node(xpath = '//table') %>% 
    html_table() %>% 
    as_tibble() %>% 
    janitor::clean_names() %>% 
    mutate(
      currency = currency,
      close = ifelse(close == "N/A", NA_character_, close),
      across(market_cap:close, parse_number)
    ) %>% 
    select(currency, date, open, close) %>% 
    drop_na() %>% 
    nest(data = -currency)
  
  return(data)
  
}

safely_get_historical_prices <- possibly(get_historical_prices, otherwise = "Error")

save_data_googlesheets <- function(data, sheet) {
  data <- data %>% as.list() %>% data.frame()
  sheet_append(sheet, data)
}

load_data_googlesheets <- function(sheet) {
  read_sheet(sheet)
}
