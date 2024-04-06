get_num_of_items <- function(partial_auctions_list){
  
  num_items <-
    partial_auctions_list %>%
    html_elements(".head-note") %>%
    html_element("p") %>%
    html_text() %>%
    parse_number()
  
  return(num_items)
  
}

get_full_url <- function(partial_auctions_list, url){
  
  num_items <- get_num_of_items(partial_auctions_list)
  
  num_loads <- ceiling(num_items / 30)
  
  full_url <- str_c(url, "&lm=", num_loads)
  
  return(full_url)

}


get_products_url <-function(full_auctions_list){
  
  products_url <-
    full_auctions_list %>%
    html_elements(".item-image-box") %>%
    html_elements("a") %>%
    html_attrs() %>%
    sapply('[[', 2)
  
  
  products_url <- str_c("https://irishwhiskeyauctions.ie", products_url)
  
  return(products_url)
  
}


get_product_attributes <- function(product_page) {
  
  product_name <-
    product_page %>%
    html_element("h2") %>%
    html_text()
  
  
  bid_history <-
    product_page %>%
    html_element(".auction-right") %>%
    html_element("table") %>%
    html_table() %>%
    rename(bid_date = "Bid Date:", bid_amount = "Bid Amount:") 
  
  
  if(nrow(bid_history) > 0) {
    
    bid_cnt <- nrow(bid_history)
    
    last_bid <-
      bid_history %>%
      slice(1)
    
    last_bid_date <- pull(last_bid, bid_date)
    
    last_bid_amount <- parse_number(pull(last_bid, bid_amount))
    
  } else {
    
    bid_cnt <- 0
    
    last_bid_date <- NA
    
    last_bid_amount <- NA
    
  }
  
  
  
  product_info <-
    product_page %>%
    html_element(".tab-content") %>%
    html_elements("p") %>%
    html_text() %>%
    unlist() %>%
    str_to_lower()
  
  product_info <- product_info[str_detect(product_info, "iwa|price|condition info:")]
  
  
  product_id <- str_trim(gsub("\\D", " ", product_info[1]))
  
  amounts <- str_split(gsub("\\D", " ", product_info[2]), " ", simplify = TRUE)
  amounts <- as.numeric(amounts[amounts != ""])
  
  lower_price_guide <- min(amounts)
  upper_price_guide <- max(amounts)
  
  condition <- str_trim(str_sub(product_info[3], 16))
  
  
  
  product_attributes <- list(
    product_name, condition, product_id, last_bid_date, bid_cnt,
    last_bid_amount, lower_price_guide, upper_price_guide
    
  )
  
  return(product_attributes)
  
}
