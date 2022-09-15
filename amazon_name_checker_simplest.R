library(tidyverse)
library(tibble)
library(openxlsx)
library(gdata)
library(lubridate)
library(rvest)
library(stringr)
library(dplyr)
library(purrr)
library(plyr)


ua <-
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/89.0.4389.90 Safari/537.36"




name_checker_2 <- function(asin) {
  url_compositor <- paste0("https://www.amazon.it/dp/", asin)
  
  system('sleep 15')
  
  page <- read_html(url_compositor, user_agent = ua)
  
  name <- page %>%
    html_nodes(xpath = "//*[@id='productTitle']/text()") %>%
    html_text()
  
  name <- trimws(name)
  
  asin_2 <- unlist(asin)
  
  final <- cbind(asin, asin_2, name)
  
  final_df <<- as.data.frame(final)
  
  return(final_df)
  
  print("DONE")
  
}



asin <- c(
  "B07JVLQ38M",
  "B08LVXG2V1",
  "B07Y8DPHCL",
  "B082WSYYCX",
  "B082WSZXHB",
  "B09CTY5P2L",
  "B09SGB55N4",
  "B015O6A6GM",
  "B00G3DNXIE",
  "B07Y8DHN3F",
  "B015O6AFPE",
  "B00O4QVS66",
  "B093C19CSD",
  "B079Z9GXQS",
  "B09ZJ9QXZY",
  "B07S5L9937",
  "B07FZ4PVRP",
  "B072MSQDQG",
  "B00RP0J7AW",
  "B00VYLJ4H4",
  "B015O6A68K",
  "B079FN8N4T",
  "B00HLUUNOK",
  "B075RGG4Y5",
  "B01N55NF5G",
  "B06WP9PPT6",
  "B09BJ7LNR6",
  "B07FYHM3QY",
  "B07S5L8VYG",
  "B06WGV3DBP",
  "B08NKXBJX1",
  "B08X7BYVZQ",
  "B07W3VJQZD",
  "B08J8XH5BH",
  "B08J8XNFNC",
  "B08J8XCSH4",
  "B08J8XCSHB",
  "B08J8XN21F",
  "B09CTZ6WKM",
  "B09CTZZGFY",
  "B09BHV4DWG",
  "B00N4K5PDK",
  "B00SKVYB6K",
  "B075RFR2CV",
  "B00VRGPXF8",
  "B0B9HC9HSR",
  "B0987B62CP",
  "B0987BTDT2",
  "B0987C6H2S",
  "B0987D9K2X",
  "B0987D6J4J",
  "B0987C1JFD",
  "B0987CNV93",
  "B0987BBB14",
  "B00FM266R2",
  "B076998CD8",
  "B08JVC7VB4",
  "B08R7NM2SF",
  "B08JV8L49M",
  "B08R7NSVL5",
  "B08NPR8QNN",
  "B08NPMMFWQ",
  "B00IRAFHB2",
  "B00A9269R2",
  "B07Y8GR1WS",
  "B07Y8GQMP5",
  "B08J8XMFQN",
  "B0056BZZDA",
  "B078RL1X78",
  "B0B8JFYZ34",
  "B07VC16GW8",
  "B07W4JVN8S",
  "B09N2JV9X3",
  "B07WBZC85N",
  "B07VGH4Z81",
  "B00GLSDCS2",
  "B07CKY9SNZ",
  "B07MZGRDJ4",
  "B07NYRRF1Y",
  "B07CKRCMB7",
  "B08V11MR92",
  "B08X6QMR36",
  "B09SDHHKJZ",
  "B0BCPB5TYQ",
  "B07S61V42S",
  "B0B1QV8TWC"
)

date <- Sys.time()

scrape_raw <- purrr::map(asin, purrr::possibly(name_checker_2, NA))

product_name_df_delay_ua <- plyr::ldply(scrape_raw, data.frame)

write.csv(name_df, file = paste0("data/Amazon_name_",date,".csv"))
