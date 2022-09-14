name_checker <- function(asin){
  
  url_compositor <- paste0("https://www.amazon.it/dp/", asin)
  
  page <- read_html(url_compositor)
  
  name <- page %>%
    html_nodes(xpath= "//*[@id='productTitle']/text()") %>%
    html_text()
  
  name <- trimws(name)
  
  table <- page %>% html_table()
  
  data <- table[[3]]
  
  asin <- unlist(data[data$X1 == "ASIN", 2])
  
  asin_2 <- asin
  
  #real_name <- unlist(data[2,2])
  
  sku <- unlist(data[data$X1 == "Numero modello articolo", 2])
  
  correct_name <- ifelse(trimws(tolower(real_name)) == trimws(tolower(name)),"GOOD", "FIX")
  
  final <- cbind(asin,asin_2,name,real_name,correct_name,sku)
  
  final_df <<- as.data.frame(final)
  
  return(final_df)
  
  print("DONE")
  
}


asin <- c("B08X6QMR36", "B09SDHHKJZ","B0BCPB5TYQ")


name_raw <- purrr::map(asin, purrr::possibly(name_checker, NA))

name_df <- plyr::ldply(name_raw, data.frame)

write.csv(name_df, file = "data/Amazon Name.csv")
