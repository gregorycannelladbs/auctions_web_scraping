library(tidyverse)
library(rvest)
library(openxlsx)
library(rstudioapi)


setwd(dirname(getActiveDocumentContext()$path))

source("functions.R")


urls <- c(
  "Bushmills" = "https://irishwhiskeyauctions.ie/views/all-auctions.php?page=current-auction&ua-brand=4",
  "Dingle" = "https://irishwhiskeyauctions.ie/views/all-auctions.php?page=current-auction&ua-brand=7",
  "Jameson" = "https://irishwhiskeyauctions.ie/views/all-auctions.php?page=current-auction&ua-brand=14",
  "Midleton" = "https://irishwhiskeyauctions.ie/views/all-auctions.php?page=current-auction&ua-brand=19",
  "Redbreast" = "https://irishwhiskeyauctions.ie/views/all-auctions.php?page=current-auction&ua-brand=24",
  "Spot" = "https://irishwhiskeyauctions.ie/views/all-auctions.php?page=current-auction&ua-brand=26",
  "Teeling" = "https://irishwhiskeyauctions.ie/views/all-auctions.php?page=current-auction&ua-brand=28",
  "Tullamore" = "https://irishwhiskeyauctions.ie/views/all-auctions.php?page=current-auction&ua-brand=31",
  "Islay" = "https://irishwhiskeyauctions.ie/views/all-auctions.php?page=current-auction&ua-brand=11",
  "Macallan" = "https://irishwhiskeyauctions.ie/views/all-auctions.php?page=current-auction&ua-brand=81",
  "Speyside" = "https://irishwhiskeyauctions.ie/views/all-auctions.php?page=current-auction&ua-brand=25",
  "The Highlands" = "https://irishwhiskeyauctions.ie/views/all-auctions.php?page=current-auction&ua-brand=29",
  "The Lowlands" = "https://irishwhiskeyauctions.ie/views/all-auctions.php?page=current-auction&ua-brand=30",
  "Hibiki" = "https://irishwhiskeyauctions.ie/views/all-auctions.php?page=current-auction&ua-brand=9",
  "Nikka" = "https://irishwhiskeyauctions.ie/views/all-auctions.php?page=current-auction&ua-brand=20",
  "Suntory" = "https://irishwhiskeyauctions.ie/views/all-auctions.php?page=current-auction&ua-brand=27"
)


partial_auctions_lists <- map(urls, read_html)

full_urls <- map2_vec(partial_auctions_lists, urls, get_full_url)

full_auctions_lists <- map(full_urls, read_html)



products_url <- unlist(map(full_auctions_lists, get_products_url))

products_pages <- map(products_url, read_html)


products_attributes <- map(products_pages, get_product_attributes)



num_items <- map_vec(partial_auctions_lists, get_num_of_items)


df <- tibble(
  brand = unlist((map2(num_items, names(num_items), replicate))),
  product_name = sapply(products_attributes, "[[", 1),
  condition = sapply(products_attributes, "[[", 2),
  product_id = sapply(products_attributes, "[[", 3),
  last_bid_date = sapply(products_attributes, "[[", 4),
  bid_cnt = sapply(products_attributes, "[[", 5),
  last_bid_amount = sapply(products_attributes, "[[", 6),
  lower_price_guide = sapply(products_attributes, "[[", 7),
  upper_price_guide = sapply(products_attributes, "[[", 8),
  product_url = products_url
)


df <-
  df %>%
    mutate(
      condition_descritpion = case_when(
        condition %in% c("c1", "1") ~ "Poor Condition",
        condition %in% c("c2", "2")  ~ "Fair Condition",
        condition %in% c("c3", "3")  ~ "Very Good Condition",
        condition %in% c("c4", "4")  ~ "Great Condition",
        condition %in% c("c5", "5")  ~ "Mint Condition",
        TRUE ~ ""
      ),
      avg_savings = (lower_price_guide + upper_price_guide) / 2 - (last_bid_amount * 1.123),
      min_savings = lower_price_guide - (last_bid_amount * 1.123),
      max_savings  = upper_price_guide - (last_bid_amount * 1.123),
      prop_avg_savings = round(avg_savings / ((lower_price_guide + upper_price_guide) / 2), 2),
      prop_min_savings = round(min_savings / lower_price_guide, 2),
      prop_max_savings = round(max_savings / upper_price_guide, 2)
    ) %>%
    relocate(condition_descritpion, .after = condition) %>%
    relocate(product_url, .after = last_col()) %>%
    select(-avg_savings, -min_savings, -max_savings)
