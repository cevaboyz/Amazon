##AMAZON ASIN CHECKER 0.0
##
##
##


#Loading Libraries
library(svDialogs)
library(pivottabler)
library(priceR)
library(readxl)
library(rio)
library(tidyverse)
library(tibble)
library(openxlsx)
library(quantmod)
library(openxlsx)
library(gdata)
library(lubridate)
library(RSelenium)
library(rvest)
library(binman)
library(stringr)
library(dplyr)
library(lubridate)
library(rvest)
library(stringr)


url <- "https://www.amazon.it/dp/B07JVLQ38M?FALSE"


test <- read_html(url)


name <- test %>%
  html_nodes(xpath= "//*[@id='productTitle']/text()") %>%
  html_text()


name <- trimws(name)

real_name <- test %>%
  html_nodes(xpath="//*[@id='productDetails_techSpec_section_1']/tbody/tr[2]/td") %>%
  html_text()


table <- test %>% html_table()

data <- table[[3]]

asin <- unlist(data[6,2])

real_name <- unlist(data[2,2])

sku <- unlist(data[5,2])

correct_name <- ifelse(trimws(tolower(real_name)) == trimws(tolower(name)),"GOOD", "FIX")


final <- cbind(asin,name,real_name,correct_name,sku)

asin <- unlist(ASIN)


name_checker <- function(asin){
  
  url_compositor <- paste0("https://www.amazon.it/dp/", asin)
  
  page <- read_html(url_compositor)
  
  name <- page %>%
    html_nodes(xpath= "//*[@id='productTitle']/text()") %>%
    html_text()
  
  name <- trimws(name)
  
  table <- page %>% html_table()
  
  data <- table[[3]]
  
  asin <- asin
  
  real_name <- unlist(data[2,2])
  
  sku <- unlist(data[5,2])
  
  correct_name <- ifelse(trimws(tolower(real_name)) == trimws(tolower(name)),"GOOD", "FIX")
  
  final <- cbind(asin,name,real_name,correct_name,sku)
  
  final_df <<- as.data.frame(final)
  
  return(final_df)
  
  print("DONE")
  
}



name_raw <- purrr::map(asin, possibly(name_checker, NA))

name_df <- plyr::ldply(name_raw, data.frame)
