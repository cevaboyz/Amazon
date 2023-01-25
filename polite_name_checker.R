# required libraries ------------------------------------------------------
library(blastula)
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
library(polite)
library(xml2)
library(gt)


ua <-
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/89.0.4389.90 Safari/537.36"





polite_checker_1 <- function(asin) {
  url_composer <- paste0("https://www.amazon.it/dp/", asin)
  url  <-
    bow(
      url_composer,
      user_agent = ua,
      force = TRUE,
      delay = 10,
      times = 10,
      verbose = TRUE
    )
  info <-
    scrape(url) %>% html_elements(xpath = "//*[@id='productTitle']/text()") %>% html_text2()
  title <- trimws(info)
  title_lower <- tolower(title)
  asin_2 <- unlist(asin)
  chr_len_dynamic <- nchar(title_lower)
  real_name <- 0
  real_name_ws <- trimws(real_name)
  real_name_norm <- tolower(real_name_ws)
  real_name_chr_len <- nchar(real_name_norm)
  is_the_same <- title_lower == real_name_norm
  need_fix <-
    ifelse(is_the_same == TRUE, "No Problemo", "Fix Me Please My Lord")
  final <-
    cbind(
      asin,
      asin_2,
      title_lower,
      chr_len_dynamic,
      real_name_norm,
      real_name_chr_len,
      is_the_same,
      need_fix
    )
  final_df <<- as.data.frame(final)
  return(final_df)
  print("Thank you Jeff, Almighty God of the Interweb")
}


master_asin <- c(
  "B09BHV4DWG",
  "B082WSYYCX",
  "B082WSZXHB",
  "B072MSQDQG",
  "B00RP0J7AW",
  "B079FN8N4T",
  "B0BQC5XRMF",
  "B09CTY5P2L",
  "B00HLUUNOK",
  "B075RGG4Y5",
  "B00VYLJ4H4",
  "B015O6A68K",
  "B09ZJ9QXZY",
  "B01N55NF5G",
  "B0BFBDBJK2",
  "B09SGB55N4",
  "B00N4K5PDK",
  "B00SKVYB6K",
  "B00VRGPXF8",
  "B075RFR2CV",
  "B06WP9PPT6",
  "B0B9HC9HSR",
  "B07FYHM3QY",
  "B07S5L9937",
  "B08X7BYVZQ",
  "B015O6A6GM",
  "B00O4QVS66",
  "B07Y8DPHCL",
  "B00G3DNXIE",
  "B0BNL2X57R",
  "B09BJ7LNR6",
  "B06WGV3DBP",
  "B08NKXBJX1",
  "B07JVLQ38M",
  "B08LVXG2V1",
  "B07W3VJQZD",
  "B08J8XNFNC",
  "B08J8XCSH4",
  "B08J8XCSHB",
  "B08J8XN21F",
  "B09CTZ6WKM",
  "B09CTZZGFY",
  "B07S5L8VYG",
  "B07FZ4PVRP",
  "B08J8XH5BH",
  "B0BFGMJSV6",
  "B015O6AFPE",
  "B07Y8DHN3F",
  "B093C19CSD",
  "B079Z9GXQS",
  "B0987D6J4J",
  "B0987BBB14",
  "B0987C1JFD",
  "B0987CNV93",
  "B0987B62CP",
  "B0987BTDT2",
  "B0987D9K2X",
  "B0987C6H2S",
  "B08NPG5D5Y",
  "B0BDVBHWWD",
  "B08JVC7VB4",
  "B08HS77R9B",
  "B00FM266R2",
  "B08NP5M6D4",
  "B09K7ZS81N",
  "B08NK25BLP",
  "B08RYGFL21",
  "B08NPF5HWD",
  "B09CYDKRZ6",
  "B08NPR9BYM",
  "B09CYM6KR3",
  "B08JVCHZF3",
  "B08R7NSVL5",
  "B08XBZV29G",
  "B08JV8FFYH",
  "B01JOL22Y2",
  "B08JV9VVQT",
  "B08JV7QK54",
  "B08JV9LX42",
  "B01LXNKQOL",
  "B0711KBY99",
  "B071451JHC",
  "B0755CJFPL",
  "B076998CD8",
  "B08JV87TZW",
  "B08R7NM2SF",
  "B08JV8L49M",
  "B08NPR8QNN",
  "B08NK1L69Z",
  "B08NPMMFWQ",
  "B0848JZ6KP",
  "B009E7CBIY",
  "B00IRAFHB2",
  "B0BLCXY3J8",
  "B0BLCZ42BG",
  "B08J8ZLLDT",
  "B08J8Z15M1",
  "B08J8ZH957",
  "B08J8XJHNM",
  "B078RL1X78",
  "B09CTZV4TM",
  "B09CTZF711",
  "B09KV6G5KT",
  "B09CTZM9LJ",
  "B0B8JFYZ34",
  "B07Y8GCMLG",
  "B07Y8GH4K8",
  "B07Y8GQVND",
  "B08J8XMFQN",
  "B09GTTY863",
  "B08HR23S41",
  "B08HR1VGSR",
  "B07Y8FDGDQ",
  "B07Y8GQMP5",
  "B07Y8GR1WS",
  "B07Y8FZ6GJ",
  "B0056C02MI",
  "B00K6MR6JK",
  "B00M975Z7K",
  "B0056BZZDA",
  "B00A9269R2",
  "B07VGH4Z81",
  "B07VC16GW8",
  "B07W4JVN8S",
  "B07WBZC85N",
  "B09N2JV9X3",
  "B0BCPB5TYQ",
  "B08V11MR92",
  "B09SDHHKJZ",
  "B00GLSDCS2",
  "B08X6QMR36",
  "B08HS4ZSMH",
  "B08HS61FJ5",
  "B0B1QV8TWC",
  "B07S61V42S",
  "B07P35DTPJ",
  "B07CKY9SNZ",
  "B07PPC54RD",
  "B07CKRCMB7",
  "B07MZGRDJ4",
  "B07NYRRF1Y"
)




# master_asin <- c(
#   'B09BHV4DWG',
#   'B082WSYYCX',
#   'B082WSZXHB',
#   'B072MSQDQG',
#   'B00RP0J7AW',
#   'B079FN8N4T',
#   'B0BQC5XRMF',
#   'B09CTY5P2L',
#   'B00HLUUNOK',
#   'B075RGG4Y5',
#   'B00VYLJ4H4',
#   'B015O6A68K',
#   'B09ZJ9QXZY',
#   'B01N55NF5G',
#   'B0BFBDBJK2',
#   'B09SGB55N4',
#   'B00N4K5PDK',
#   'B00SKVYB6K',
#   'B00VRGPXF8',
#   'B075RFR2CV',
#   'B06WP9PPT6',
#   'B0B9HC9HSR',
#   'B07FYHM3QY',
#   'B07S5L9937',
#   'B08X7BYVZQ',
#   'B015O6A6GM',
#   'B00O4QVS66',
#   'B07Y8DPHCL',
#   'B00G3DNXIE',
#   'B0BNL2X57R',
#   'B09BJ7LNR6',
#   'B06WGV3DBP',
#   'B08NKXBJX1',
#   'B07JVLQ38M',
#   'B08LVXG2V1',
#   'B07W3VJQZD',
#   'B08J8XNFNC',
#   'B08J8XCSH4',
#   'B08J8XCSHB',
#   'B08J8XN21F',
#   'B09CTZ6WKM',
#   'B09CTZZGFY',
#   'B07S5L8VYG',
#   'B07FZ4PVRP',
#   'B08J8XH5BH',
#   'B0BFGMJSV6',
#   'B015O6AFPE',
#   'B07Y8DHN3F',
#   'B093C19CSD',
#   'B079Z9GXQS',
#   'B0987D6J4J',
#   'B0987BBB14',
#   'B0987C1JFD',
#   'B0987CNV93',
#   'B0987B62CP',
#   'B0987BTDT2',
#   'B0987D9K2X',
#   'B0987C6H2S',
#   'B08NPG5D5Y',
#   'B0BDVBHWWD',
#   'B08JVC7VB4',
#   'B08HS77R9B',
#   'B00FM266R2',
#   'B08NP5M6D4',
#   'B09K7ZS81N',
#   'B08NK25BLP',
#   'B08RYGFL21',
#   'B08NPF5HWD',
#   'B09CYDKRZ6',
#   'B08NPR9BYM',
#   'B09CYM6KR3',
#   'B08JVCHZF3',
#   'B08R7NSVL5',
#   'B08XBZV29G',
#   'B08JV8FFYH',
#   'B01JOL22Y2',
#   'B08JV9VVQT',
#   'B08JV7QK54',
#   'B08JV9LX42',
#   'B01LXNKQOL',
#   'B0711KBY99',
#   'B071451JHC',
#   'B0755CJFPL',
#   'B076998CD8',
#   'B08JV87TZW',
#   'B08R7NM2SF',
#   'B08JV8L49M',
#   'B08NPR8QNN',
#   'B08NK1L69Z',
#   'B08NPMMFWQ',
#   'B0848JZ6KP',
#   'B009E7CBIY',
#   'B00IRAFHB2',
#   'B0BLCXY3J8',
#   'B0BLCZ42BG',
#   'B08J8ZLLDT',
#   'B08J8Z15M1',
#   'B08J8ZH957',
#   'B08J8XJHNM',
#   'B078RL1X78',
#   'B09CTZV4TM',
#   'B09CTZF711',
#   'B09KV6G5KT',
#   'B09CTZM9LJ',
#   'B0B8JFYZ34',
#   'B07Y8GCMLG',
#   'B07Y8GH4K8',
#   'B07Y8GQVND',
#   'B08J8XMFQN',
#   'B09GTTY863',
#   'B08HR23S41',
#   'B08HR1VGSR',
#   'B07Y8FDGDQ',
#   'B07Y8GQMP5',
#   'B07Y8GR1WS',
#   'B07Y8FZ6GJ',
#   'B0056C02MI',
#   'B00K6MR6JK',
#   'B00M975Z7K',
#   'B0056BZZDA',
#   'B00A9269R2',
#   'B07VGH4Z81',
#   'B07VC16GW8',
#   'B07W4JVN8S',
#   'B07WBZC85N',
#   'B09N2JV9X3',
#   'B0BCPB5TYQ',
#   'B08V11MR92',
#   'B09SDHHKJZ',
#   'B00GLSDCS2',
#   'B08X6QMR36',
#   'B08HS4ZSMH',
#   'B08HS61FJ5',
#   'B0B1QV8TWC',
#   'B07S61V42S',
#   'B07P35DTPJ',
#   'B07CKY9SNZ',
#   'B07PPC54RD',
#   'B07CKRCMB7',
#   'B07MZGRDJ4',
#   'B07NYRRF1Y'
# )
#
#
#
#
#
# # master_asin <-
# #     c(
# #         "B08LVXG2V1",
# #         "B07Y8DPHCL",
# #         "B07FZ4PVRP",
# #         "B07FYHM3QY",
# #         "B07S5L8VYG",
# #         "B06WGV3DBP",
# #         "B08NKXBJX1",
# #         "B07W3VJQZD",
# #         "B08J8XH5BH",
# #         "B08J8XNFNC",
# #         "B08J8XCSH4",
# #         "B08J8XCSHB",
# #         "B08J8XN21F",
# #         "B09CTZ6WKM",
# #         "B09CTZZGFY",
# #         "B07JVLQ38M",
# #         "B082WSYYCX",
# #         "B082WSZXHB",
# #         "B09CTY5P2L",
# #         "B09SGB55N4",
# #         "B015O6A6GM",
# #         "B00G3DNXIE",
# #         "B07Y8DHN3F",
# #         "B015O6AFPE",
# #         "B00O4QVS66",
# #         "B093C19CSD",
# #         "B079Z9GXQS",
# #         "B09ZJ9QXZY",
# #         "B07S5L9937",
# #         "B072MSQDQG",
# #         "B00RP0J7AW",
# #         "B00VYLJ4H4",
# #         "B015O6A68K",
# #         "B079FN8N4T",
# #         "B00HLUUNOK",
# #         "B075RGG4Y5",
# #         "B01N55NF5G",
# #         "B06WP9PPT6",
# #         "B09BJ7LNR6",
# #         "B08X7BYVZQ",
# #         "B09BHV4DWG",
# #         "B00N4K5PDK",
# #         "B00SKVYB6K",
# #         "B075RFR2CV",
# #         "B00VRGPXF8",
# #         "B0B9HC9HSR",
# #         "B0BFBDBJK2",
# #         "B0BFGMJSV6",
# #         "B0BFBDBJK2",
# #         "B0987B62CP",
# #         "B0987BTDT2",
# #         "B0987C6H2S",
# #         "B0987D9K2X",
# #         "B0987D6J4J",
# #         "B0987C1JFD",
# #         "B0987CNV93",
# #         "B0987BBB14",
# #         "B00FM266R2",
# #         "B076998CD8",
# #         "B08JVC7VB4",
# #         "B08R7NM2SF",
# #         "B08JV8L49M",
# #         "B08R7NSVL5",
# #         "B08NPR8QNN",
# #         "B08NPMMFWQ",
# #         "B0BDVBHWWD",
# #         "B08JV8FFYH",
# #         "B08JV87TZW",
# #         "B09K7ZS81N",
# #         "B01JOL22Y2",
# #         "B08NK25BLP",
# #         "B08NPF5HWD",
# #         "B08NPR9BYM",
# #         "B09CYM6KR3",
# #         "B09CYDKRZ6",
# #         "B0711KBY99",
# #         "B071451JHC",
# #         "B08JVCHZF3",
# #         "B08NPG5D5Y",
# #         "B08RYGFL21",
# #         "B08JV9VVQT",
# #         "B08XBZV29G",
# #         "B01LXNKQOL",
# #         "B0848JZ6KP",
# #         "B08JV7QK54",
# #         "B08HS77R9B",
# #         "B08JV9LX42",
# #         "B0755CJFPL",
# #         "B08NK1L69Z",
# #         "B08NP5M6D4",
# #         "B00IRAFHB2",
# #         "B009E7CBIY",
# #         "B00M975Z7K",
# #         "B08J8Z15M1",
# #         "B08J8ZLLDT",
# #         "B08J8ZH957",
# #         "B08J8XJHNM",
# #         "B00A9269R2",
# #         "B00K6MR6JK",
# #         "B07Y8GR1WS",
# #         "B07Y8GH4K8",
# #         "B07Y8FDGDQ",
# #         "B07Y8GQMP5",
# #         "B07Y8GCMLG",
# #         "B07Y8FZ6GJ",
# #         "B07Y8GQVND",
# #         "B08HR1VGSR",
# #         "B08HR23S41",
# #         "B08J8XMFQN",
# #         "B0056BZZDA",
# #         "B0056C02MI",
# #         "B09GTTY863",
# #         "B078RL1X78",
# #         "B09KH2654V",
# #         "B09CTZF711",
# #         "B09KV6G5KT",
# #         "B09CTZM9LJ",
# #         "B09CTZV4TM",
# #         "B0B8JFYZ34",
# #         "B07VC16GW8",
# #         "B07W4JVN8S",
# #         "B09N2JV9X3",
# #         "B07WBZC85N",
# #         "B07VGH4Z81",
# #         "B00GLSDCS2",
# #         "B07CKY9SNZ",
# #         "B07MZGRDJ4",
# #         "B07NYRRF1Y",
# #         "B07CKRCMB7",
# #         "B08V11MR92",
# #         "B08X6QMR36",
# #         "B09SDHHKJZ",
# #         "B0BCPB5TYQ",
# #         "B07S61V42S",
# #         "B0B1QV8TWC",
# #         "B08HS4ZSMH",
# #         "B08HS61FJ5",
# #         "B07PPC54RD",
# #         "B07P35DTPJ",
# #         "B0BLCXY3J8",
# #         "B0BLCZ42BG",
# #         "B0BQC5XRMF",
# #         "B0BNL2X57R"
# #
# #     )
#

master_sku <- c(
  "11847",
  "11523",
  "11520",
  "11494X",
  "11140X",
  "11640",
  "11789",
  "11826",
  "11811X",
  "11504X",
  "11144X",
  "11249X",
  "11849",
  "11420X",
  "11735",
  "11824",
  "11234X",
  "11236X",
  "11225X",
  "11424X",
  "11493X",
  "11900",
  "11539",
  "11725",
  "11739",
  "11463X",
  "11312X",
  "11728",
  "11072X",
  "11758",
  "11808",
  "11619Y",
  "11738",
  "31002",
  "11799",
  "11726",
  "11768",
  "11769",
  "11770",
  "11772",
  "11837",
  "11838",
  "11737",
  "31001A",
  "11749",
  "11840",
  "11363X",
  "11657A",
  "11817",
  "11347A",
  "4032",
  "4035",
  "4033",
  "4034",
  "4028",
  "4030",
  "4029",
  "4031",
  "7471",
  "7503",
  "7485",
  "7494",
  "7786",
  "7479",
  "7502",
  "7599",
  "7472",
  "7870",
  "7500",
  "7478",
  "7912",
  "7499",
  "7477",
  "7480",
  "7481",
  "7808",
  "7492",
  "7489T",
  "7487",
  "7852",
  "7866",
  "7395",
  "7436",
  "7862",
  "7491",
  "7486",
  "7493",
  "7473",
  "7474",
  "7475",
  "7916T",
  "5057X",
  "5040",
  "16936",
  "16937",
  "16776",
  "16775B",
  "16777",
  "16715B",
  "16480",
  "16787B",
  "16783",
  "16785",
  "16784",
  "16788",
  "16629A",
  "16631A",
  "16653A",
  "16751B",
  "16719B",
  "16729B",
  "16728B",
  "16633A",
  "16628B",
  "16630A",
  "16652A",
  "6015E",
  "6221E",
  "16175",
  "6004E",
  "6113E",
  "5811",
  "5813",
  "5818",
  "5823",
  "5827",
  "9029",
  "9017",
  "9030",
  "9559Z",
  "9027",
  "9440",
  "9432",
  "9033",
  "9009",
  "9014",
  "9006",
  "9012",
  "9246",
  "9013",
  "9245"
)






#
#
#
#
# # asin <- c(
# #   "B07JVLQ38M",
# #   "B08LVXG2V1",
# #   "B07Y8DPHCL",
# #   "B082WSYYCX",
# #   "B082WSZXHB",
# #   "B09CTY5P2L",
# #   "B09SGB55N4",
# #   "B015O6A6GM",
# #   "B00G3DNXIE",
# #   "B07Y8DHN3F",
# #   "B015O6AFPE",
# #   "B00O4QVS66",
# #   "B093C19CSD",
# #   "B079Z9GXQS",
# #   "B09ZJ9QXZY",
# #   "B07S5L9937",
# #   "B07FZ4PVRP",
# #   "B072MSQDQG",
# #   "B00RP0J7AW",
# #   "B00VYLJ4H4",
# #   "B015O6A68K",
# #   "B079FN8N4T",
# #   "B00HLUUNOK",
# #   "B075RGG4Y5",
# #   "B01N55NF5G",
# #   "B06WP9PPT6",
# #   "B09BJ7LNR6",
# #   "B07FYHM3QY",
# #   "B07S5L8VYG",
# #   "B06WGV3DBP",
# #   "B08NKXBJX1",
# #   "B08X7BYVZQ",
# #   "B07W3VJQZD",
# #   "B08J8XH5BH",
# #   "B08J8XNFNC",
# #   "B08J8XCSH4",
# #   "B08J8XCSHB",
# #   "B08J8XN21F",
# #   "B09CTZ6WKM",
# #   "B09CTZZGFY",
# #   "B09BHV4DWG",
# #   "B00N4K5PDK",
# #   "B00SKVYB6K",
# #   "B075RFR2CV",
# #   "B00VRGPXF8",
# #   "B0B9HC9HSR",
# #   "B0987B62CP",
# #   "B0987BTDT2",
# #   "B0987C6H2S",
# #   "B0987D9K2X",
# #   "B0987D6J4J",
# #   "B0987C1JFD",
# #   "B0987CNV93",
# #   "B0987BBB14",
# #   "B00FM266R2",
# #   "B076998CD8",
# #   "B08JVC7VB4",
# #   "B08R7NM2SF",
# #   "B08JV8L49M",
# #   "B08R7NSVL5",
# #   "B08NPR8QNN",
# #   "B08NPMMFWQ",
# #   "B00IRAFHB2",
# #   "B00A9269R2",
# #   "B07Y8GR1WS",
# #   "B07Y8GQMP5",
# #   "B08J8XMFQN",
# #   "B0056BZZDA",
# #   "B078RL1X78",
# #   "B0B8JFYZ34",
# #   "B07VC16GW8",
# #   "B07W4JVN8S",
# #   "B09N2JV9X3",
# #   "B07WBZC85N",
# #   "B07VGH4Z81",
# #   "B00GLSDCS2",
# #   "B07CKY9SNZ",
# #   "B07MZGRDJ4",
# #   "B07NYRRF1Y",
# #   "B07CKRCMB7",
# #   "B08V11MR92",
# #   "B08X6QMR36",
# #   "B09SDHHKJZ",
# #   "B0BCPB5TYQ",
# #   "B07S61V42S",
# #   "B0B1QV8TWC"
# # )
#
# # master_sku <-
# #     c(
# #         "11799",
# #         "11728",
# #         "31001A",
# #         "11539",
# #         "11737",
# #         "11619Y",
# #         "11738",
# #         "11726",
# #         "11749",
# #         "11768",
# #         "11769",
# #         "11770",
# #         "11772",
# #         "11837",
# #         "11838",
# #         "31002",
# #         "11523",
# #         "11520",
# #         "11826",
# #         "11824",
# #         "11463X",
# #         "11072X",
# #         "11657A",
# #         "11363X",
# #         "11312X",
# #         "11817",
# #         "11347A",
# #         "11849",
# #         "11725",
# #         "11494X",
# #         "11140X",
# #         "11144X",
# #         "11249X",
# #         "11640",
# #         "11811X",
# #         "11504X",
# #         "11420X",
# #         "11493X",
# #         "11808",
# #         "11739",
# #         "11847",
# #         "11234X",
# #         "11236X",
# #         "11424X",
# #         "11225X",
# #         "11900",
# #         "11735",
# #         "11840",
# #         "11735",
# #         "4028",
# #         "4030",
# #         "4031",
# #         "4029",
# #         "4032",
# #         "4033",
# #         "4034",
# #         "4035",
# #         "7786",
# #         "7862",
# #         "7485",
# #         "7486",
# #         "7493",
# #         "7477",
# #         "7473",
# #         "7475",
# #         "7503",
# #         "7481",
# #         "7491",
# #         "7502",
# #         "7808",
# #         "7599",
# #         "7870",
# #         "7478",
# #         "7912",
# #         "7500",
# #         "7866",
# #         "7395",
# #         "7499",
# #         "7471",
# #         "7472",
# #         "7492",
# #         "7480",
# #         "7852",
# #         "7916T",
# #         "7489T",
# #         "7494",
# #         "7487",
# #         "7436",
# #         "7474",
# #         "7479",
# #         "5040",
# #         "5057X",
# #         "16175",
# #         "16775B",
# #         "16776",
# #         "16777",
# #         "16715B",
# #         "6113E",
# #         "6221E",
# #         "16630A",
# #         "16631A",
# #         "16633A",
# #         "16628B",
# #         "16629A",
# #         "16652A",
# #         "16653A",
# #         "16728B",
# #         "16729B",
# #         "16751B",
# #         "6004E",
# #         "6015E",
# #         "16719B",
# #         "16480",
# #         "16181",
# #         "16783",
# #         "16785",
# #         "16784",
# #         "16787B",
# #         "16788",
# #         "5813",
# #         "5818",
# #         "5827",
# #         "5823",
# #         "5811",
# #         "9559Z",
# #         "9006",
# #         "9013",
# #         "9245",
# #         "9246",
# #         "9017",
# #         "9027",
# #         "9030",
# #         "9029",
# #         "9009",
# #         "9033",
# #         "9440",
# #         "9432",
# #         "9012",
# #         "9014",
# #         "16936",
# #         "16937",
# #         "11789",
# #         "11758"
# #
# #     )
#
#
#
# master_sku <- c(
#   '11847',
#   '11523',
#   '11520',
#   '11494X',
#   '11140X',
#   '11640',
#   '11789',
#   '11826',
#   '11811X',
#   '11504X',
#   '11144X',
#   '11249X',
#   '11849',
#   '11420X',
#   '11735',
#   '11824',
#   '11234X',
#   '11236X',
#   '11225X',
#   '11424X',
#   '11493X',
#   '11900',
#   '11539',
#   '11725',
#   '11739',
#   '11463X',
#   '11312X',
#   '11728',
#   '11072X',
#   '11758',
#   '11808',
#   '11619Y',
#   '11738',
#   '31002',
#   '11799',
#   '11726',
#   '11768',
#   '11769',
#   '11770',
#   '11772',
#   '11837',
#   '11838',
#   '11737',
#   '31001A',
#   '11749',
#   '11840',
#   '11363X',
#   '11657A',
#   '11817',
#   '11347A',
#   '4032',
#   '4035',
#   '4033',
#   '4034',
#   '4028',
#   '4030',
#   '4029',
#   '4031',
#   '7471',
#   '7503',
#   '7485',
#   '7494',
#   '7786',
#   '7479',
#   '7502',
#   '7599',
#   '7472',
#   '7870',
#   '7500',
#   '7478',
#   '7912',
#   '7499',
#   '7477',
#   '7480',
#   '7481',
#   '7808',
#   '7492',
#   '7489T',
#   '7487',
#   '7852',
#   '7866',
#   '7395',
#   '7436',
#   '7862',
#   '7491',
#   '7486',
#   '7493',
#   '7473',
#   '7474',
#   '7475',
#   '7916T',
#   '5057X',
#   '5040',
#   '16936',
#   '16937',
#   '16776',
#   '16775B',
#   '16777',
#   '16715B',
#   '16480',
#   '16787B',
#   '16783',
#   '16785',
#   '16784',
#   '16788',
#   '16629A',
#   '16631A',
#   '16653A',
#   '16751B',
#   '16719B',
#   '16729B',
#   '16728B',
#   '16633A',
#   '16628B',
#   '16630A',
#   '16652A',
#   '6015E',
#   '6221E',
#   '16175',
#   '6004E',
#   '6113E',
#   '5811',
#   '5813',
#   '5818',
#   '5823',
#   '5827',
#   '9029',
#   '9017',
#   '9030',
#   '9559Z',
#   '9027',
#   '9440',
#   '9432',
#   '9033',
#   '9009',
#   '9014',
#   '9006',
#   '9012',
#   '9246',
#   '9013',
#   '9245'
# )

master_titles_correct <- c(
  "Bellissima Imetec Air Wonder, Spazzola ad Aria Calda, Tecnologia a ioni, Spazzole Rivestite in Ceramica e Cheratina, Asciuga, Dona Volume, 8 Accessori, 1000 W",
  "Bellissima Imetec Asciugacapelli K9 2300 Asciuga e Mantiene Idratati i Capelli senza Effetto Crespo, 2300 W, Tecnologia a Ioni, 8 Combinazioni Flusso d'Aria Temperatura, Diffusore capellli ricci",
  "Bellissima Imetec Asciugacapelli S9 2200 Asciuga e Modella con Precisione, 2200 W, 8 Combinazioni Flusso d'Aria Temperatura, Beccussio di precisione, Colpo aria fredda",
  "Bellissima Imetec B26 100 Piastra per Capelli Lunghi o Difficili da Disciplinare, Formato Extra Large, Rivestimento in Ceramica, 160° C - 230° C",
  "Bellissima Imetec B9 100 Piastra per Capelli, Piastra extralunghe, Riscaldamento Rapido, Multivoltaggio Automatico, Temperatura 210°, cavo giravole",
  "Bellissima Imetec BM 200 Piastra Mini per Capelli Lisci e Luminosi, Temperatura 200°, Multivoltaggio Automatico, Dimensioni Compatte da Viaggio",
  "Bellissima Imetec Ceramic & Argan Oil 6 in 1, Spazzola ad Aria Calda, 6 Accessori, Tecnologia a Ioni, Spazzole Rivestite in Ceramica e Olio di Argan, Asciuga e dona Volume, Colpo aria fredda, 1000 W",
  "Bellissima Imetec Creativity 4 You, Asciugacapelli Professionale, 4 Accessori, Diffusore, Controllo Intelligente della temperatura, Ion Technology, Beauty Bag, 2 velocità, 3 temperature, 1800 W",
  "Bellissima Imetec Creativity B15 50 Piastra per Capelli, Styling Liscio o Mosso, Rivestimento in Nanoceramica, Regolazione della Temperatura da 140°C a 230°C, Piastre Oscillanti",
  "Bellissima Imetec Creativity B27 100 Piastra Per Capelli Con Rivestimento In Ceramica E Controllo Della Temperatura, Temperatura regolabile 150°C / 230°C",
  "Bellissima Imetec Creativity B9 300 Piastra per Capelli, Styling Liscio o Mosso, Rivestimento in Ceramica, Regolazione Temperatura da 150°C a 230°C, Sistema Riscaldamento Rapido",
  "Bellissima Imetec Creativity B9 400 Piastra per Capelli, Styling Liscio o Mosso, Rivestimento in Ceramica e Cheratina, Piastre Arrotondate",
  "Bellissima Imetec Creativity Ceramic & Tourmaline, Piastra per Capelli, Liscio o Mosso, Rivestimento in Ceramica e tormalina, Regolazione Temperatura da 150-230°C, Riscaldamento Rapido",
  "Bellissima Imetec Creativity Color Shine B22 100 Piastra per Capelli Colorati, Rivestimento in Tessuto per la Protezione dei Capelli, temperatura regolabile da 150°C a 230°C",
  "Bellissima Imetec Diffon Ceramic & Argan Oil, Diffusore ad Aria Calda per Capelli Ricci, Tecnologia Ceramica e Olio di Argan, 2 Livelli Aria/Temperatura, Asciugatura Delicata, 700 W",
  "Bellissima Imetec Diffon Supreme, Diffusore ad Aria Calda per Capelli Ricci, Diffusore XL con 12 Dita, Tecnologia Ceramica&Argan Oil, 2 Velocità, 3 Temperature, Asciugatura Delicata",
  "Bellissima Imetec GH16 400 Spazzola ad Aria Modellante con 2 Accessori di Styling, Diametro 25 mm, Potenza 400 W, 2 Combinazioni Flusso d'Aria/Temperatura, Puntale Antiscottatura",
  "Bellissima Imetec GT13 50 Arricciacapelli, Rivestimento in Ceramica, 7 Diversi Livelli di Temperatura da 150°C a 210°C, Diametro di 25 mm, Riscaldamento Rapido, Pronto all'Uso",
  "Bellissima Imetec GT15 100 Arricciacapelli, Ricci Brillanti e Definiti, Diametro 19 mm, Rivestimento in Ceramica, Riscaldamento Rapido, Pronto all'Uso",
  "Bellissima Imetec GT15 200 Ferro Arricciacapelli con Rivestimento in Ceramica, Forma Conica, Controllo della Temperatura",
  "Bellissima imetec Intellisense B24 100 piastra intelligente, capelli lisci o mossi, protetti senza calore eccessivo, temperatura automatica su misura per tutti i capelli, rivestimento in ceramica",
  "Bellissima Imetec Kit Bellezza Capelli con Asciugacapelli Professionale, Tecnologia Ceramica e Cheratina e Piastra Lisciante a Vapore, Rivestimento in Ceramica e Cheratina, Idea Regalo",
  "Bellissima Imetec Magic Straight Brush PB11 100 Spazzola Elettrica Lisciante, 3 Lati Esterni Riscaldati, Spegnimento automatico, Tecnologia a Ioni, Ceramica, 160 C°- 210°C",
  "Bellissima Imetec My Pro Creativity Infrared B8 200, piastra lisciante, tecnologia raggi infrarossi, rivestimento in ceramica e cheratina, 11 livelli di temperatura da 130 °C a 230 °C, cavo 1,8 m",
  "Bellissima Imetec My Pro Revolution BHS4 1100, Spazzola ad Aria Calda, Asciuga e Modella, Rotazione Automatica a Doppio Verso, 2 Spazzole da 40 e 50 mm, Ionizzatore, 1000 W, Rivestimento in Ceramica",
  "Bellissima Imetec P11 2300 Asciugacapelli Professionale, 2300 W, Rivestimento in Ceramica e Tormalina, 8 Combinazioni Aria/Temperatura, Beccuccio Stretto, Diffusore capelli ricci",
  "Bellissima Imetec P2 2200 Asciugacapelli Professionale, Potenza 2200 W, Tecnologia a Ioni per Idratare i Capelli e Ridurre l'Effetto Crespo, 8 Combinazioni Aria/Temperatura, Beccuccio Stretto",
  "Bellissima Imetec P3 3400 Asciugacapelli con Motore AC professionale, Tecnologia a Ioni per Capelli morbidi, Idratati e Luminosi, Griglia Rivestita in Ceramica, 2400 W di Potenza",
  "Bellissima Imetec S5 2200 Asciugacapelli, 2200 W, Asciugatura Rapida e Styling a Lunga Durata, 8 Combinazioni Aria/Temperatura, Colpo d'Aria Fredda",
  "Bellissima Imetec Steam Ceramic & Argan Oil, Piastra lisciante a Vapore, Rivestimento in Ceramica e Olio di Argan, 3 Livelli di Temperatura, Riscaldamento Rapido, Piastre Oscillanti, Autospegnimento",
  "Bellissima Imetec Steam Elixir, Piastra a Vapore, Capelli lisci in Una Sola Passata Senza Danni, Rivestimento in Ceramica e Olio Argan, Rapido Riscaldamento, 4 Temperature, Tappetino Termoresistente",
  "Bellissima My Pro Imetec Beach Waves Gt20 100 Piastra Per Capelli A Onde Larghe E Strette, Rivestimento in ceramica, temperatura regolabile",
  "Bellissima My Pro Imetec Beach Waves Gt20 400 Piastra Per Capelli per onde larghe e strette, Rivestimento In Ceramica, temperatura regolabile 160 C°- 200°C",
  "Bellissima My Pro Imetec Ceramic P5 3800 Asciugacapelli Professionale Per Capelli Morbidi E Luminosi, Tecnologia Ceramica, 2300 W",
  "Bellissima My Pro Imetec Diffon Ceramic, Diffusore ad Aria Calda per Capelli Ricci, Tecnologia Ceramica, 700 W, 2 Combinazioni Aria/Temperatura, Asciugatura Delicata Senza Effetto Crespo",
  "Bellissima My Pro Imetec GH18 1100 Modellatore ad Aria, Rivestimento spazzole in Ceramica, 5 Accessori per Realizzare Capelli Lisci e Luminosi, a Onde morbide o Ricci Stretti, 1000 W",
  "Bellissima My Pro Imetec GT22 110, Accessorio Soft Curls per Styler Arricciacapelli Twist & Style, Ferro Riscaldato da 25 mm, Rivestimento in Ceramica, Temperatura 185°C",
  "Bellissima My Pro Imetec GT22 120, Accessorio Sculpted Curls per Styler Arricciacapelli Twist & Style, Ferro Riscaldato da 11 mm, Rivestimento in Ceramica, Temperatura 185°C",
  "Bellissima My Pro Imetec GT22 130, Accessorio Loose Waves per Styler Arricciacapelli Twist&Style, Ferro Riscaldato ad Ellisse, Diametro da 26 a 38 mm, Rivestimento Ceramica, Temperatura 190°C",
  "Bellissima My Pro Imetec GT22 140 Accessorio Glamour Waves per Styler Arricciacapelli Twist&Style, Ferro Riscaldato Forma Conica, Diametro 13/38 mm, Rivestimento in Ceramica, Temperatura 190°C",
  "Bellissima My Pro Imetec GT22 150 Accessorio Mermaid Waves per Styler Arricciacapelli Twist&Style, Ferro Riscaldato Forma Bubbles, Diametro 25 mm, Rivestimento in Ceramica, Temperatura 190°C",
  "Bellissima My Pro Imetec GT22 160, Accessorio Hollywood Waves per Styler Arricciacapelli Twist&Style, Rivestimento in ceramica, Temperatura 190°C, Ovale, Diametro 38 x 25 mm",
  "Bellissima My Pro Imetec Magic Style Brush P2 30 Spazzola Riscaldata, Capelli Liscio Effetto Naturale/Mosso Sostenuto, Diametro 30 mm, Tecnologia a Ioni, Rivestimento Ceramica",
  "Bellissima My Pro Imetec Steam B28 100 Piastra per Capelli Professionale a Vapore, Effetto Liscio a Lungo, Ceramica, Temperatura Regolabile 170°C - 200°C - 230°C, Riscaldamento Rapido",
  "Bellissima My Pro Imetec Twist & Style GT22 100, Manico per Styler Arricciacapelli per Onde e Ricci, Temperatura Automatica, Riscaldamento Rapido, Orientabile a 90°, Accessori non Inclusi",
  "Bellissima Imetec Twist&Style arricciacapelli, manico + 4 accessori per ricci e onde, temperatura automatica, riscaldamento rapido, rivestimento in ceramica, orientabile a 90°, idea regalo",
  "Imetec C20 2100 Asciugacapelli, 2100 W, Beccuccio Orientabile, Diffusore Professionale, Funzione Fast Drying per un'Asciugatura Rapida, 4 Combinazioni Aria/Temperatura, Colpo Aria Fredda, Cavo 1.8 m",
  "Imetec ECO SE9 1000 Asciugacapelli con Tecnologia Eco Technology 1400 W, Consumo Energetico Ridotto, 8 Combinazioni Aria/Temperatura, Diffusore per Capelli Ricci",
  "Imetec Salon Expert P3 3600 Asciugacapelli, Motore Professionale, 2200 W, Ionizzatore, Griglia Ceramica e tormalina, 8 combinazioni aria temperatura, Concentratore e Diffusore Professionali",
  "Imetec Salon Expert P5 3600 Asciugacapelli Professionale, 2300 W, Griglia con Rivestimento in Ceramica e Cheratina, Tecnologia a Ioni, 8 Combinazioni Aria/Temperatura, 2 Beccucci",
  "Imetec Compact Air, Termoventilatore piccolo e potente, Stufetta Elettrica, 2000 W, maniglia integrata, temperatura regolabile, funzione antigelo, dispositivo di protezione dai surriscaldamenti",
  "Imetec Eco Ceramic Diffusion, Stufetta elettrica, Termoventilatore, Corpo Oscillante, Tecnologia Ceramica, Basso Consumo Energetico, 6 Funzioni di Temperatura, Timer",
  "Imetec Eco Ceramic, Termoventilatore, Stufetta elettrica, Tecnologia Ceramica, Basso Consumo Energetico, Silenzioso, 3 Livelli di Temperatura, Termostato Ambiente",
  "Imetec Eco Rapid, Stufa Elettrica 2000 W, Tecnologia a Basso Consumo Energetico, Termoconvettore 4 Temperature, Termostato Ambiente, Silenzioso",
  "Imetec Silent Power Comfort, Termoventilatore silenzioso e compatto, Stufetta Elettrica, 2100 W, 4 funzioni, termostato ambiente e di sicurezza, funzione antigelo, maniglia",
  "Imetec Silent Power Eco, Termoventilatore Silenzioso, Stufetta Elettrica, Tecnologia ECO -35% Consumo Energetico, 2100 W, Funzione Antigelo, Temperatura Regolabile, 4 Funzioni, Termostato Ambiente",
  "Imetec Silent Power Protection, Termoventilatore silenzioso, 2100 W, Stufetta Elettrica, Timer spegnimento programmabile, temperatura regolabile, 4 funzioni, termostato ambiente, funzione antigelo",
  "Imetec Silent Power Pure, Termoventilatore silenzioso con ionizzatore e timer di autospegnimento, Stufetta Elettrica, 2100 W, funzione antigelo, temperatura regolabile, 4 funzioni",
  "Imetec BuonFrullato, Frullatore Compatto, Bicchiere in Plastica BPA Free, Capacità 700 ml, Coperchio Dosa Liquidi, 400 W",
  "Imetec CakeLover, Impastatrice Planetaria per Dolci, Creme e Impasti Salati, Accessoriata, Compatta, 1500W, Fruste, Pale e Ganci, Contenitore Inox, 5 Litri, 6 Velocità, Ricettario",
  "Imetec Crea&Crema Sbattitore Elettrico, Fruste Extralunghe per Impasti Dolci e Panna Montata, Ganci Acciaio Inox per Impastare, 5 Velocità, Funzione Turbo, Design Ergonomico, 500 W",
  "Imetec Cukò Maestro, Robot da cucina con cottura, 20 programmi, 10 funzioni, Impasta Pane e Pizza, capienza 2 L, Fino a 6 porzioni, 8 accessori, ricettario, 1000 W",
  "Imetec Dolcevita ES4 Bilancia Elettronica da Cucina, Pesa Solidi e Liquidi, Contenitore Estraibile, Scala Graduata, Portata 5 Kg, Divisione 1 g, Funzione Tara, Display Digitale, Spegnimento Automatico",
  "Imetec FrescoAroma Macina Caffè e Spezie, Pepe, Zucchero, Frutta Secca Elettrico con Lame in Acciaio Inox, Contenitore Salva-Freschezza, Funzionamento a Impulsi, 150 W",
  "Imetec FriggiLeggero - Friggitrice ad aria multifunzione, 12 programmi, Riscaldamento rapido, 18 L, 1550 W, Comandi digitali, Cestello, Griglia, Leccarda, Ricettario, 40x36x34,5 cm",
  "Imetec Frulla&Crea Frullatore a Immersione, 3 Accessori, Tritatutto, Frusta e Bicchiere BPA free, Piede Large, 2 Velocità, 1000 W, 15000 Giri Minuto",
  "Imetec Frulla&Gusta, Frullatore con 4 lame in acciaio inox, bicchiare in vetro, 2 velocità e funzionamento impulsi, 500 W, capienza 800 ml, lavabile lavastoviglie, piedini antiscivolo",
  "Imetec Frulla&Trita Frullatore a Immersione, Lame in Acciaio Inox, Piede Extra Large, Gambo Estraibile, Bicchiere graduato da 1 Litro, 800 Watt, 15.000 Giri Minuto",
  "Imetec FrullaFacile, Frullatore a Immersione, Gambo XL, Lame in Acciaio Inox, Funzionamento a Impulsi, Bicchiere Graduato da 700 ml BPA Free, 800 W, Impugnatura Ergonomica",
  "Imetec FrullaRapido, Frullatore a Immersione, Gambo XL in Acciaio, Lame in Acciaio Inox, Funzionamento a Impulsi, 450 W",
  "Imetec FrullaRapido+, Frullatore a Immersione, Gambo XL in Acciaio, Lame in Acciaio Inox, Funzionamento a Impulsi, Bicchiere graduato da 700 ml BPA Free, 800 W",
  "Imetec FrullaTutto Frullatore, Lame Tritaghiaccio in Acciaio Inox, Bicchiere da 800 ml in Vetro Antigraffio, 2 Velocità, Funzionamento Impulsi, 500 W, 2 Velocità, Lavabile Lavastoviglie",
  "Imetec GranToast, tostapane per toast extra farciti, fessure XL, pinze a fondo chiuso apribili a 360° e regolabili in larghezza, 600 W, 10 livelli doratura, Timer",
  "Imetec Grattugissima, grattuggia elettrica per formaggio, pane e frutta secca, Contenitore estraibile, coperchio salvafreschezza, 150 W, rullo in acciacio, accessori lavabili in lavastoviglie",
  "Imetec Griglia&Gusta Bistecchiera, Piastre Antiaderenti Removibili, Rivestimento Triplo Strato al Titanio Stone-Look, Grill con 7 Posizioni, 3 Funzioni di Cottura, Thermo Control, 2000 Watt",
  "Imetec HB3 Frullatore a Immersione, Gambo Large Estraibile, Lame in Acciaio Inox, Funzionamento a Impulsi, 450 W",
  "Imetec La Grattugia, grattugia elettrica, Rullo in Acciaio Inox, Tramoggia in Alluminio, Contenitore Estraibile con Coperchio Salvafreschezza, Sistema di Sicurezza, 150 W",
  "Imetec La Panetteria +Zeroglu, Macchina per Pane, Ciabatte, Panini, Dolci con Farine Naturali e Senza Glutine, 20 Programmi, 2 Pale Impastatrici, Temperatura di Lievitazione Controllata",
  "Imetec La TostaGriglia, tostiera, Piastre XL Rigate e Antiaderenti, Interruttore ON/OFF, Spia Riscaldamento Piastre, Gancio Chiusura, Avvolgicavo, Tostapane Compatto, 900 W",
  "Imetec PaneMio Macchina del Pane, Impasta Lievita e Cuoce, 12 Programmi, Cestello Pagnotta, Accessorio Panini, 3 Livelli di Doratura, Avvio Programmabile, 550 W, Ricettario",
  "Imetec Personal e Sport Blender PB 100 Mini Frullatore con 2 Bottiglie Take-Away in Tritan e 4 Lame in Acciaio Inox, 22,000 giri/min",
  "Imetec SM 1000 Soup Maker, Cuoce e Frulla, 3 Programmi Automatici, Vellutate, Zuppe e Frullati, 6 Porzioni, Lame Seghettate Acciaio Inox, con Ricettario, 900 W, 1.6 Litri",
  "Imetec SP 100 Spiralizer Elettrico, Tagliaverdure a Spirale, Affetta Verdure in 3 Forme Spaghetti, Tagliatelle, Pappardelle, Lame in Acciaio Inox, Contenitore BPA Free 500 ml, Sistema di Sicurezza",
  "Imetec Succovivo Pro 2000 Estrattore di Succo Professionale a Freddo, Spremitura Lenta 48 Giri/Min, 2 Filtri per Succhi, Accessorio per Granite e Sorbetti, Kit per Maschere Bellezza, con Ricettario",
  "Imetec Succovivo SJ4 1300 Estrattore di Succo Professionale a Freddo, Spremitura Lenta 50 Giri/Min, Filtro per Succhi, Accessorio per Granite e Sorbetti, pulizia in 2 minuti",
  "Imetec Tosta&Griglia, Tostiera elettrica, piastre XL per preparare 3 toast alla volta, compatta, riponibile in verticale, 800 W, spie di funzionamento",
  "Imetec TostaMaxi Tostapane, 2 Fessure Extralarge e Pinze Apribili per Toast Extrafarciti, 10 livelli di doratura, Timer con Autospegnimento, Cassetto Raccoglibriciole, 600 Watt",
  "Imetec TritaCompact Tritatutto, Lame in Acciaio Inox, Capienza Contenitore 400 ml, Funzionamento a Pressione, Compatto, 350 W",
  "Imetec TritaMax, Tritatutto, 4 Lame in Acciaio Inox, Capiente Contenitore 600 ml, Accessorio Frusta per Salse e Panna Montata, 2 velocità, 500 Watt",
  "Imetec Tritapiù Tritatutto, 4 Lame in Acciaio Inox, Capiente Contenitore 600 ml, 18.000 Giri/min, Funzionamento a Pressione, 1000 W, Materiali BPA Free",
  "Imetec Zero-Glu Pro, Macchina per Pane, Ciabatte e Panini Senza Glutine per Celiaci, Pasta Pizza, Dolci, Marmellate, 20 Programmi, 2 Pale Impastatrici, Temperatura levitazione controllata, Ricettario",
  "Bellissima Imetec Face Cleansing Spazzola Per Pulizia Viso, Deterge ed esfolia la pelle, Kit con 5 testine intercambiabili, Tecnologia a vibrazione sonica, Impermeabile, Beauty Bag inclusa",
  "Bellissima Imetec Kit Testine di Ricambio Face Cleansing per Pelli Normali e Pelli Sensibili, 4 Diverse Testine per Detergere, Massaggiare, Levigare e Idratare La Pelle",
  "Imetec Adapto Elegance, Plaid riscaldabile, Coperta elettrica, 180 x 140 cm, tessuto morbido Velvet Double Face, Risparmio energetico, Tecnologia adapto, Rapido riscaldamento, 6 temperature, Lavabile",
  "Imetec Adapto Grand Luxe, Plaid riscaldabile, Coperta elettrica, 180 x 140 cm, tessuto morbido Peluche Sherpa, Risparmio energetico, Tecnologia Adapto, Rapido riscaldamento, 6 temperature, Lavabile",
  "Imetec Adapto velvet jacquard plaid riscaldabile, coperta elettrica 140x180 cm tessuto morbido, basso consumo, tecnologia adapto, tecnologia di sicurezza, rapido riscaldamento, 6 temperature, lavabile",
  "Imetec adapto velvet square plaid riscaldabile, coperta elettrica 150x95 cm tessuto morbido, basso consumo, tecnologia adapto, dispositivo di sicurezza, rapido riscaldamento, 6 temperature, lavabile",
  "imetec adapto velvet tartan plaid riscaldabile con tasca mani e piedi, coperta elettrica 150x110 cm, basso consumo, tecnologia adapto, sicuro, rapido riscaldamento, 6 temperature, lavabile",
  "Imetec Adapto velvet tartan plaid riscaldabile, coperta elettrica 160x120 cm tessuto morbido, basso consumo, tecnologia adapto, dispositivo di sicurezza, rapido riscaldamento, 6 temperature, lavabile",
  "Imetec BW03 Boule Elettrica Cordless, Tecnologia Ceramica, Riscaldamento Ultrarapido, Termostato di Sicurezza, Spia di Ricarica",
  "Imetec Intellisense Cervical, Termoforo per Cervicale e Spalle, Cuscino Termico, 47x52 cm, Tessuto Anallergico, 5 Temperature, Electro Block di Sicurezza, Riscaldamento Rapido, Lavabile in Lavatrice",
  "Imetec Intellisense Comfort, Termoforo Multiuso, Cuscino Termico, per schiena e addome, Tasca per Mani, Rapido Riscaldamento, Tessuto Anallergico, 5 temperature, Electro Block di sicurezza, Lavabile",
  "Imetec Intellisense Essential, Termoforo Multiuso, Cuscino Termico, 40x35 cm, Tessuto Anallergico, 5 temperature, Electro Block di sicurezza, Lavabile, 40x35 cm",
  "Imetec Intellisense XL, Termoforo Extra Large Schiena e Gambe, Cuscino termico, 38x50 cm, Tessuto micro Peluche, 5 Temperature, Electro Block di sicurezza, Riscaldamento Rapido, lavabile in lavatrice",
  "Imetec Intellisensefeet scaldapiedi, tecnologia intellisense, tessuto morbido, 5 temperature, autospegnimento, dispositivo sicurezza electro block, rivestimento lavabile in lavatrice, 52x54 cm",
  "Imetec Scaldasonno Adapto matrimoniale 150 x 160 cm, basso consumo, riscaldamento rapido, temperatura personalizzata, 100% lana e merino, made in italy, 2 comandi separati, 6 temperature",
  "Imetec Scaldasonno Adapto matrimoniale 150 x 160 cm, basso consumo, tecnologia brevettata, riscaldamento rapido, temperatura personalizzata, 100% cotone, made in italy, 2 comandi con 6 temperature",
  "Imetec Scaldasonno Adapto matrimoniale, basso consumo, riscaldamento rapido, temperatura personalizzata, tessuto trapuntato anallergico, made in italy, 2 comandi con 6 temperature, 150 x 160 cm",
  "Imetec Scaldasonno Adapto maxi coprimaterasso matrimoniale 195 x 165 cm, basso consumo, riscaldamento rapido, temperatura personalizzata, 100% lana e merino, doppio comando, 6 temperature",
  "imetec scaldasonno adapto maxi coprimaterasso matrimoniale 195x165 cm, basso consumo, riscaldamento rapido, temperatura personalizzata, 100% cotone percalle trapuntato, doppio comando, 6 temperature",
  "Imetec Scaldasonno Adapto maxi coprimaterasso matrimoniale 195x165 cm, basso consumo, riscaldamento rapido, temperatura personalizzata, tessuto anallergico trapuntato, doppio comando, 6 temperature",
  "Imetec Scaldasonno Adapto maxi coprimaterasso singolo 195x90 cm, basso consumo, riscaldamento rapido, temperatura personalizzata, tessuto anallergico trapuntato, comando a 6 temperature",
  "Imetec Scaldasonno Adapto piazza e mezza 150 x 120 cm, basso consumo, tecnologia brevettata, riscaldamento rapido, temperatura personalizzata, 100 % cotone, made in italy, comando con 6 temperature",
  "Imetec Scaldasonno Adapto singolo 150 x 80 cm, basso consumo, riscaldamento rapido, temperatura personalizzata, 100% lana e merino, made in italy, tessuto antiscivolo, comando con 6 temperature",
  "Imetec Scaldasonno Adapto singolo 150 x 80 cm, basso consumo, tecnologia brevettata, riscaldamento rapido, temperatura personalizzata, 100% cotone, made in italy, comando con 6 temperature",
  "Imetec Scaldasonno Adapto singolo 150 x 80 cm, riscaldamento rapido, temperatura costante e personalizzata, tessuto trapuntato anallergico, made in Italy, comando con 6 temperature",
  "Imetec Scaldasonno Matrimoniale 150x160 cm, 50% Lana e Merino, 2 Temperature, Dispositivo Sicurezza Electro Block, Made in Italy",
  "Imetec Scaldasonno Matrimoniale, 150 x 140 cm, 2 Comandi con 2 Temperature, Tessuto Trapuntato, Lavabile a Mano e in Lavatrice a 40°C",
  "Imetec Scaldasonno Sensitive Singolo, si Adatta ai Cambi di Temperatura, Morbido Peluche, Coprimaterasso Maxi 190 x 90, Electro Block, 6 Temperature, Timer, Risparmio Energetico, Lavabile",
  "Imetec Scaldasonno Singolo 150x80 cm, Basso Consumo, 50% Lana e Merino, 2 Temperature, Dispositivo Sicurezza Electro Block, Made in Italy",
  "Imetec Scaldasonno Singolo, 150 x 80 cm, Comando 2 Temperature, Tessuto Trapuntato, Lavabile a Mano e in Lavatrice a 40°C",
  "Imetec Body Analizer ES7 400 Bilancia Pesapersone Diagnostica, Misura Acqua Corporea, Massa Muscolare e Grassa",
  "Imetec Compact ES1 100 Bilancia pesapersone elettronica compatta, Design ultrasottile, Ampio LCD display, Portata Max 150 kg",
  "Imetec Monitoring ES9 300 Bilancia Pesapersone Elettronica, Monitoraggio Trend Grafico Peso, 4 Utenti, fino a 180 Kg, LCD Display, Vetro Temperato",
  "Imetec Precision ES13 200 Bilancia pesapersone elettronica, rivela anche le minime variazioni di peso, fino a 180 Kg, LCD Display, vetro temperato, batterie incluse",
  "Imetec Pro, Bilancia pesapersone meccanica, Rileva il peso in modo chiaro e affidabile, Pedana antiscivolo, Portata max 150 kg, Divisione 1 kg, Funzionamento analogico",
  "Imetec Activation Ferro da Stiro a Vapore, Piastra Scorrevole in Ceramica e Tormalina, Colpo Vapore 200 g, Tecnologia Anticalcare ZeroCalc, Antigoccia, Eco, 2400 W",
  "Imetec Eco Perfect Ferro da Stiro a Vapore, Risultati Ottimi con -35% di Consumo di Acqua e -25% di Consumo Energetico, Piastra con Rivestimento Pro Ceramic, Tripla Protezione Anticalcare, 2400 W",
  "Imetec Intellifast, ferro da stiro a vapore, Tecnologia Intelli System, Impostazione automatica della temperatura, Piastra Ceramic Diamond, Protezione anticalcare, 2400 W, Colpo Vapore 200 g",
  "Imetec Nuvola Ferro da Stiro da Viaggio a Vapore, Piastra in Acciaio Inossidabile da 1000 W, Custodia da Viaggio, Doppio Voltaggio",
  "Imetec Onda F1, Ferro da Stiro a Vapore con Piastra a Onde Scorrevole Brevettata, 2400 W, Colpo di Vapore 200g, Serbatoio 300 ml, Tripla Protezione Anticalcare",
  "Imetec Onda F2, Ferro da Stiro a Vapore con Piastra a Onde Scorrevole Brevettata, 2400 W, Colpo di Vapore 200g, Serbatoio 300 ml, Tripla Protezione Anticalcare",
  "Imetec Onda P2 Ferro da Stiro Generatore di Vapore, Piastra a Onde Scorrevole Brevettata, 2400W, Colpo Vapore 300 g, Serbatoio 2,5L, 3 Filtri con Ioni di Argento, Tecnologia No Stop Vapor",
  "Imetec Rapidvapor Ferro da Stiro Generatore di Vapore, Piastra Fluid Ceramic a Rapido Riscaldamento, Leggero, Sistema di Pulizia Calc Clean, Colpo Vapore 240 g, 2400 W, 5 Bar Pump",
  "Imetec ZeroCalc KF1 100 Kit 3 filtri anticalcare per Ferri da Stiro Imetec ZeroCalc Pro Ceramic PS2 2400, PS2 2000, PS2 2200, Intellivapor PS3 3000, Imetec Onda, Ricambi Originali",
  "Imetec ZeroCalc PS1 2000 Ferro da Stiro Compatto, 3.8 BAR, Tecnologia Anticalcare e a Risparmio Energetico, Ricarica Continua, Vapore Pronto in 1 Minuto, 2100 W, Serbatoio XL da 1 L",
  "Imetec ZeroCalc Z1 2500 Ferro da Stiro, Tecnologia Anticalcare, Piastra Inox 2200 W, Colpo Vapore 120 g",
  "Imetec ZeroCalc Z1 2800 Ferro da Stiro con Tecnologia Anticalcare, Piastra Ceramica ad Alta Scorrevolezza, Tecnologia a Risparmio Energetico, 2200 W, Colpo Vapore 130 g",
  "Imetec Zerocalc Z3 3500 Ferro Da Stiro, Tecnologia Anticalcare, Piastra Inox 2400 W, Colpo Vapore 150 g",
  "Imetec Zerocalc Z3 3700 Ferro da Stiro, Tecnologia Anticalcare, Piastra Ceramica ad Alta Scorrevolezza, 2400W, Colpo Vapore 160 G, 0,3 litri, Dispositivo Antigoccia Tecnologia a Risparmio Energetico",
  "Imetec ZeroCalc Z3 3900 Ferro da Stiro a Vapore, Tecnologia Anticalcare, Tecnologia Intellivapor (temperatura unica ideale per tutti i tessuti), Piastra Ceramica, 2400 W, Colpo Vapore 170 g"
)




# master_titles_correct <- c(
#   "Bellissima Imetec Air Wonder, Spazzola ad Aria Calda, Tecnologia a ioni, Spazzole Rivestite in Ceramica e Cheratina, Asciuga, Dona Volume, 8 Accessori, 1000 W",
#   "Bellissima Imetec Asciugacapelli K9 2300 Asciuga e Mantiene Idratati i Capelli senza Effetto Crespo, 2300 W, Tecnologia a Ioni, 8 Combinazioni Flusso d'Aria Temperatura, Diffusore capellli ricci",
#   "Bellissima Imetec Asciugacapelli S9 2200 Asciuga e Modella con Precisione, 2200 W, 8 Combinazioni Flusso d'Aria Temperatura, Beccussio di precisione, Colpo aria fredda",
#   "Bellissima Imetec B26 100 Piastra per Capelli Lunghi o Difficili da Disciplinare, Formato Extra Large, Rivestimento in Ceramica, 160° C - 230° C",
#   "Bellissima Imetec B9 100 Piastra per Capelli, Piastra extralunghe, Riscaldamento Rapido, Multivoltaggio Automatico, Temperatura 210°, cavo giravole",
#   "Bellissima Imetec BM 200 Piastra Mini per Capelli Lisci e Luminosi, Temperatura 200°, Multivoltaggio Automatico, Dimensioni Compatte da Viaggio",
#   "Bellissima Imetec Ceramic & Argan Oil 6 in 1, Spazzola ad Aria Calda, 6 Accessori, Tecnologia a Ioni, Spazzole Rivestite in Ceramica e Olio di Argan, Asciuga e dona Volume, Colpo aria fredda, 1000 W",
#   "Bellissima Imetec Creativity 4 You, Asciugacapelli Professionale, 4 Accessori, Diffusore, Controllo Intelligente della temperatura, Ion Technology, Beauty Bag, 2 velocità, 3 temperature, 1800 W",
#   "Bellissima Imetec Creativity B15 50 Piastra per Capelli, Styling Liscio o Mosso, Rivestimento in Nanoceramica, Regolazione della Temperatura da 140°C a 230°C, Piastre Oscillanti",
#   "Bellissima Imetec Creativity B27 100 Piastra Per Capelli Con Rivestimento In Ceramica E Controllo Della Temperatura, Temperatura regolabile 150°C / 230°C",
#   "Bellissima Imetec Creativity B9 300 Piastra per Capelli, Styling Liscio o Mosso, Rivestimento in Ceramica, Regolazione Temperatura da 150°C a 230°C, Sistema Riscaldamento Rapido",
#   "Bellissima Imetec Creativity B9 400 Piastra per Capelli, Styling Liscio o Mosso, Rivestimento in Ceramica e Cheratina, Piastre Arrotondate",
#   "Bellissima Imetec Creativity Ceramic & Tourmaline, Piastra per Capelli, Liscio o Mosso, Rivestimento in Ceramica e tormalina, Regolazione Temperatura da 150-230°C, Riscaldamento Rapido",
#   "Bellissima Imetec Creativity Color Shine B22 100 Piastra per Capelli Colorati, Rivestimento in Tessuto per la Protezione dei Capelli, temperatura regolabile da 150°C a 230°C",
#   "Bellissima Imetec Diffon Ceramic & Argan Oil, Diffusore ad Aria Calda per Capelli Ricci, Tecnologia Ceramica e Olio di Argan, 2 Livelli Aria/Temperatura, Asciugatura Delicata, 700 W",
#   "Bellissima Imetec Diffon Supreme, Diffusore ad Aria Calda per Capelli Ricci, Diffusore XL con 12 Dita, Tecnologia Ceramica&Argan Oil, 2 Velocità, 3 Temperature, Asciugatura Delicata",
#   "Bellissima Imetec GH16 400 Spazzola ad Aria Modellante con 2 Accessori di Styling, Diametro 25 mm, Potenza 400 W, 2 Combinazioni Flusso d'Aria/Temperatura, Puntale Antiscottatura",
#   "Bellissima Imetec GT13 50 Arricciacapelli, Rivestimento in Ceramica, 7 Diversi Livelli di Temperatura da 150°C a 210°C, Diametro di 25 mm, Riscaldamento Rapido, Pronto all'Uso",
#   "Bellissima Imetec GT15 100 Arricciacapelli, Ricci Brillanti e Definiti, Diametro 19 mm, Rivestimento in Ceramica, Riscaldamento Rapido, Pronto all'Uso",
#   "Bellissima Imetec GT15 200 Ferro Arricciacapelli con Rivestimento in Ceramica, Forma Conica, Controllo della Temperatura",
#   "Bellissima imetec Intellisense B24 100 piastra intelligente, capelli lisci o mossi, protetti senza calore eccessivo, temperatura automatica su misura per tutti i capelli, rivestimento in ceramica",
#   "Bellissima Imetec Kit Bellezza Capelli con Asciugacapelli Professionale, Tecnologia Ceramica e Cheratina e Piastra Lisciante a Vapore, Rivestimento in Ceramica e Cheratina",
#   "Bellissima Imetec Magic Straight Brush PB11 100 Spazzola Elettrica Lisciante, 3 Lati Esterni Riscaldati, Spegnimento automatico, Tecnologia a Ioni, Ceramica, 160 C°- 210°C",
#   "Bellissima Imetec My Pro Creativity Infrared B8 200, piastra lisciante, tecnologia raggi infrarossi, rivestimento in ceramica e cheratina, 11 livelli di temperatura da 130 °C a 230 °C, cavo 1,8 m",
#   "Bellissima Imetec My Pro Revolution BHS4 1100, Spazzola ad Aria Calda, Asciuga e Modella, Rotazione Automatica a Doppio Verso, 2 Spazzole da 40 e 50 mm, Ionizzatore, 1000 W, Rivestimento in Ceramica",
#   "Bellissima Imetec P11 2300 Asciugacapelli Professionale, 2300 W, Rivestimento in Ceramica e Tormalina, 8 Combinazioni Aria/Temperatura, Beccuccio Stretto, Diffusore capelli ricci",
#   "Bellissima Imetec P2 2200 Asciugacapelli Professionale, Potenza 2200 W, Tecnologia a Ioni per Idratare i Capelli e Ridurre l'Effetto Crespo, 8 Combinazioni Aria/Temperatura, Beccuccio Stretto",
#   "Bellissima Imetec P3 3400 Asciugacapelli con Motore AC professionale, Tecnologia a Ioni per Capelli morbidi, Idratati e Luminosi, Griglia Rivestita in Ceramica, 2400 W di Potenza",
#   "Bellissima Imetec S5 2200 Asciugacapelli, 2200 W, Asciugatura Rapida e Styling a Lunga Durata, 8 Combinazioni Aria/Temperatura, Colpo d'Aria Fredda",
#   "Bellissima Imetec Steam Ceramic & Argan Oil, Piastra lisciante a Vapore, Rivestimento in Ceramica e Olio di Argan, 3 Livelli di Temperatura, Riscaldamento Rapido, Piastre Oscillanti, Autospegnimento",
#   "Bellissima Imetec Steam Elixir, Piastra a Vapore, Capelli lisci in Una Sola Passata Senza Danni, Rivestimento in Ceramica e Olio Argan, Rapido Riscaldamento, 4 Temperature, Tappetino Termoresistente",
#   "Bellissima My Pro Imetec Beach Waves Gt20 100 Piastra Per Capelli A Onde Larghe E Strette, Rivestimento in ceramica, temperatura regolabile",
#   "Bellissima My Pro Imetec Beach Waves Gt20 400 Piastra Per Capelli per onde larghe e strette, Rivestimento In Ceramica, temperatura regolabile 160 C°- 200°C",
#   "Bellissima My Pro Imetec Ceramic P5 3800 Asciugacapelli Professionale Per Capelli Morbidi E Luminosi, Tecnologia Ceramica, 2300 W",
#   "Bellissima My Pro Imetec Diffon Ceramic, Diffusore ad Aria Calda per Capelli Ricci, Tecnologia Ceramica, 700 W, 2 Combinazioni Aria/Temperatura, Asciugatura Delicata Senza Effetto Crespo",
#   "Bellissima My Pro Imetec GH18 1100 Modellatore ad Aria, Rivestimento spazzole in Ceramica, 5 Accessori per Realizzare Capelli Lisci e Luminosi, a Onde morbide o Ricci Stretti, 1000 W",
#   "Bellissima My Pro Imetec GT22 110, Accessorio Soft Curls per Styler Arricciacapelli Twist & Style, Ferro Riscaldato da 25 mm, Rivestimento in Ceramica, Temperatura 185°C",
#   "Bellissima My Pro Imetec GT22 120, Accessorio Sculpted Curls per Styler Arricciacapelli Twist & Style, Ferro Riscaldato da 11 mm, Rivestimento in Ceramica, Temperatura 185°C",
#   "Bellissima My Pro Imetec GT22 130, Accessorio Loose Waves per Styler Arricciacapelli Twist&Style, Ferro Riscaldato ad Ellisse, Diametro da 26 a 38 mm, Rivestimento Ceramica, Temperatura 190°C",
#   "Bellissima My Pro Imetec GT22 140 Accessorio Glamour Waves per Styler Arricciacapelli Twist&Style, Ferro Riscaldato Forma Conica, Diametro 13/38 mm, Rivestimento in Ceramica, Temperatura 190°C",
#   "Bellissima My Pro Imetec GT22 150 Accessorio Mermaid Waves per Styler Arricciacapelli Twist&Style, Ferro Riscaldato Forma Bubbles, Diametro 25 mm, Rivestimento in Ceramica, Temperatura 190°C",
#   "Bellissima My Pro Imetec GT22 160, Accessorio Hollywood Waves per Styler Arricciacapelli Twist&Style, Rivestimento in ceramica, Temperatura 190°C, Ovale, Diametro 38 x 25 mm",
#   "Bellissima My Pro Imetec Magic Style Brush P2 30 Spazzola Riscaldata, Capelli Liscio Effetto Naturale/Mosso Sostenuto, Diametro 30 mm, Tecnologia a Ioni, Rivestimento Ceramica",
#   "Bellissima My Pro Imetec Steam B28 100 Piastra per Capelli Professionale a Vapore, Effetto Liscio a Lungo, Ceramica, Temperatura Regolabile 170°C - 200°C - 230°C, Riscaldamento Rapido",
#   "Bellissima My Pro Imetec Twist & Style GT22 100, Manico per Styler Arricciacapelli per Onde e Ricci, Temperatura Automatica, Riscaldamento Rapido, Orientabile a 90°, Accessori non Inclusi",
#   "Bellissima Imetec Twist&Style arricciacapelli, manico + 4 accessori per ricci e onde, temperatura automatica, riscaldamento rapido, rivestimento in ceramica, orientabile a 90°, idea regalo",
#   "Imetec C20 2100 Asciugacapelli, 2100 W, Beccuccio Orientabile, Diffusore Professionale, Funzione Fast Drying per un'Asciugatura Rapida, 4 Combinazioni Aria/Temperatura, Colpo Aria Fredda, Cavo 1.8 m",
#   "Imetec ECO SE9 1000 Asciugacapelli con Tecnologia Eco Technology 1400 W, Consumo Energetico Ridotto, 8 Combinazioni Aria/Temperatura, Diffusore per Capelli Ricci",
#   "Imetec Salon Expert P3 3600 Asciugacapelli, Motore Professionale, 2200 W, Ionizzatore, Griglia Ceramica e tormalina, 8 combinazioni aria temperatura, Concentratore e Diffusore Professionali",
#   "Imetec Salon Expert P5 3600 Asciugacapelli Professionale, 2300 W, Griglia con Rivestimento in Ceramica e Cheratina, Tecnologia a Ioni, 8 Combinazioni Aria/Temperatura, 2 Beccucci",
#   "Imetec Compact Air, Termoventilatore piccolo e potente, Stufetta Elettrica, 2000 W, maniglia integrata, temperatura regolabile, funzione antigelo, dispositivo di protezione dai surriscaldamenti",
#   "Imetec Eco Ceramic Diffusion, Stufetta elettrica, Termoventilatore, Corpo Oscillante, Tecnologia Ceramica, Basso Consumo Energetico, 6 Funzioni di Temperatura, Timer",
#   "Imetec Eco Ceramic, Termoventilatore, Stufetta elettrica, Tecnologia Ceramica, Basso Consumo Energetico, Silenzioso, 3 Livelli di Temperatura, Termostato Ambiente",
#   "Imetec Eco Rapid, Stufa Elettrica 2000 W, Tecnologia a Basso Consumo Energetico, Termoconvettore 4 Temperature, Termostato Ambiente, Silenzioso",
#   "Imetec Silent Power Comfort, Termoventilatore silenzioso e compatto, Stufetta Elettrica, 2100 W, 4 funzioni, termostato ambiente e di sicurezza, funzione antigelo, maniglia",
#   "Imetec Silent Power Eco, Termoventilatore Silenzioso, Stufetta Elettrica, Tecnologia ECO -35% Consumo Energetico, 2100 W, Funzione Antigelo, Temperatura Regolabile, 4 Funzioni, Termostato Ambiente",
#   "Imetec Silent Power Protection, Termoventilatore silenzioso, 2100 W, Stufetta Elettrica, Timer spegnimento programmabile, temperatura regolabile, 4 funzioni, termostato ambiente, funzione antigelo",
#   "Imetec Silent Power Pure, Termoventilatore silenzioso con ionizzatore e timer di autospegnimento, Stufetta Elettrica, 2100 W, funzione antigelo, temperatura regolabile, 4 funzioni",
#   "Imetec BuonFrullato, Frullatore Compatto, Bicchiere in Plastica BPA Free, Capacità 700 ml, Coperchio Dosa Liquidi, 400 W",
#   "Imetec CakeLover, Impastatrice Planetaria per Dolci, Creme e Impasti Salati, Accessoriata, Compatta, 1500W, Fruste, Pale e Ganci, Contenitore Inox, 5 Litri, 6 Velocità, Ricettario",
#   "Imetec Crea&Crema Sbattitore Elettrico, Fruste Extralunghe per Impasti Dolci e Panna Montata, Ganci Acciaio Inox per Impastare, 5 Velocità, Funzione Turbo, Design Ergonomico, 500 W",
#   "Imetec Cukò Maestro, Robot da cucina con cottura, 20 programmi, 10 funzioni, Impasta Pane e Pizza, capienza 2 L, Fino a 6 porzioni, 8 accessori, ricettario, 1000 W",
#   "Imetec Dolcevita ES4 Bilancia Elettronica da Cucina, Pesa Solidi e Liquidi, Contenitore Estraibile, Scala Graduata, Portata 5 Kg, Divisione 1 g, Funzione Tara, Display Digitale, Spegnimento Automatico",
#   "Imetec FrescoAroma Macina Caffè e Spezie, Pepe, Zucchero, Frutta Secca Elettrico con Lame in Acciaio Inox, Contenitore Salva-Freschezza, Funzionamento a Impulsi, 150 W",
#   "Imetec FriggiLeggero - Friggitrice ad aria multifunzione, 12 programmi, Riscaldamento rapido, 18 L, 1550 W, Comandi digitali, Cestello, Griglia, Leccarda, Ricettario, 40x36x34,5 cm",
#   "Imetec Frulla&Crea Frullatore a Immersione, 3 Accessori, Tritatutto, Frusta e Bicchiere BPA free, Piede Large, 2 Velocità, 1000 W, 15000 Giri Minuto",
#   "Imetec Frulla&Gusta, Frullatore con 4 lame in acciaio inox, bicchiare in vetro, 2 velocità e funzionamento impulsi, 500 W, capienza 800 ml, lavabile lavastoviglie, piedini antiscivolo",
#   "Imetec Frulla&Trita Frullatore a Immersione, Lame in Acciaio Inox, Piede Extra Large, Gambo Estraibile, Bicchiere graduato da 1 Litro, 800 Watt, 15.000 Giri Minuto",
#   "Imetec FrullaFacile, Frullatore a Immersione, Gambo XL, Lame in Acciaio Inox, Funzionamento a Impulsi, Bicchiere Graduato da 700 ml BPA Free, 800 W, Impugnatura Ergonomica",
#   "Imetec FrullaRapido, Frullatore a Immersione, Gambo XL in Acciaio, Lame in Acciaio Inox, Funzionamento a Impulsi, 450 W",
#   """Imetec FrullaRapido+, Frullatore a Immersione, Gambo XL in Acciaio, Lame in Acciaio Inox, Funzionamento a Impulsi, Bicchiere graduato da 700 ml BPA Free, 800 W"","
# """Imetec FrullaTutto Frullatore, Lame Tritaghiaccio in Acciaio Inox, Bicchiere da 800 ml in Vetro Antigraffio, 2 Velocità, Funzionamento Impulsi, 500 W, 2 Velocità, Lavabile Lavastoviglie
# "","
# "Imetec GranToast, tostapane per toast extra farciti, fessure XL, pinze a fondo chiuso apribili a 360° e regolabili in larghezza, 600 W, 10 livelli doratura, Timer",
# "Imetec Grattugissima, grattuggia elettrica per formaggio, pane e frutta secca, Contenitore estraibile, coperchio salvafreschezza, 150 W, rullo in acciacio, accessori lavabili in lavastoviglie",
# "Imetec Griglia&Gusta Bistecchiera, Piastre Antiaderenti Removibili, Rivestimento Triplo Strato al Titanio Stone-Look, Grill con 7 Posizioni, 3 Funzioni di Cottura, Thermo Control, 2000 Watt",
# "Imetec HB3 Frullatore a Immersione, Gambo Large Estraibile, Lame in Acciaio Inox, Funzionamento a Impulsi, 450 W",
# "Imetec La Grattugia, grattugia elettrica, Rullo in Acciaio Inox, Tramoggia in Alluminio, Contenitore Estraibile con Coperchio Salvafreschezza, Sistema di Sicurezza, 150 W",
# "Imetec La Panetteria +Zeroglu, Macchina per Pane, Ciabatte, Panini, Dolci con Farine Naturali e Senza Glutine, 20 Programmi, 2 Pale Impastatrici, Temperatura di Lievitazione Controllata",
# "Imetec La TostaGriglia, tostiera, Piastre XL Rigate e Antiaderenti, Interruttore ON/OFF, Spia Riscaldamento Piastre, Gancio Chiusura, Avvolgicavo, Tostapane Compatto, 900 W",
# "Imetec PaneMio Macchina del Pane, Impasta Lievita e Cuoce, 12 Programmi, Cestello Pagnotta, Accessorio Panini, 3 Livelli di Doratura, Avvio Programmabile, 550 W, Ricettario",
# "Imetec Personal e Sport Blender PB 100 Mini Frullatore con 2 Bottiglie Take-Away in Tritan e 4 Lame in Acciaio Inox, 22,000 giri/min",
# "Imetec SM 1000 Soup Maker, Cuoce e Frulla, 3 Programmi Automatici, Vellutate, Zuppe e Frullati, 6 Porzioni, Lame Seghettate Acciaio Inox, con Ricettario, 900 W, 1.6 Litri",
# "Imetec SP 100 Spiralizer Elettrico, Tagliaverdure a Spirale, Affetta Verdure in 3 Forme Spaghetti, Tagliatelle, Pappardelle, Lame in Acciaio Inox, Contenitore BPA Free 500 ml, Sistema di Sicurezza",
# "Imetec Succovivo Pro 2000 Estrattore di Succo Professionale a Freddo, Spremitura Lenta 48 Giri/Min, 2 Filtri per Succhi, Accessorio per Granite e Sorbetti, Kit per Maschere Bellezza, con Ricettario",
# "Imetec Succovivo SJ4 1300 Estrattore di Succo Professionale a Freddo, Spremitura Lenta 50 Giri/Min, Filtro per Succhi, Accessorio per Granite e Sorbetti, pulizia in 2 minuti",
# "Imetec Tosta&Griglia, Tostiera elettrica, piastre XL per preparare 3 toast alla volta, compatta, riponibile in verticale, 800 W, spie di funzionamento",
# "Imetec TostaMaxi Tostapane, 2 Fessure Extralarge e Pinze Apribili per Toast Extrafarciti, 10 livelli di doratura, Timer con Autospegnimento, Cassetto Raccoglibriciole, 600 W",
# "Imetec TritaCompact Tritatutto, Lame in Acciaio Inox, Capienza Contenitore 400 ml, Funzionamento a Pressione, Compatto, 350 W",
# "Imetec TritaMax, Tritatutto, 4 Lame in Acciaio Inox, Capiente Contenitore 600 ml, Accessorio Frusta per Salse e Panna Montata, 2 velocità, 500 Watt",
# "Imetec Tritapiù Tritatutto, 4 Lame in Acciaio Inox, Capiente Contenitore 600 ml, 18.000 Giri/min, Funzionamento a Pressione, 1000 W, Materiali BPA Free",
# "Imetec Zero-Glu Pro, Macchina per Pane, Ciabatte e Panini Senza Glutine per Celiaci, Pasta Pizza, Dolci, Marmellate, 20 Programmi, 2 Pale Impastatrici, Temperatura levitazione controllata, Ricettario",
# "Bellissima Imetec Face Cleansing Spazzola Per Pulizia Viso, Deterge ed esfolia la pelle, Kit con 5 testine intercambiabili, Tecnologia a vibrazione sonica, Impermeabile, Beauty Bag inclusa",
# "Bellissima Imetec Kit Testine di Ricambio Face Cleansing per Pelli Normali e Pelli Sensibili, 4 Diverse Testine per Detergere, Massaggiare, Levigare e Idratare La Pelle",
# "Imetec Adapto Elegance, Plaid riscaldabile, Coperta elettrica, 180 x 140 cm, tessuto morbido Velvet Double Face, Risparmio energetico, Tecnologia adapto, Rapido riscaldamento, 6 temperature, Lavabile",
# "Imetec Adapto Grand Luxe, Plaid riscaldabile, Coperta elettrica, 180 x 140 cm, tessuto morbido Peluche Sherpa, Risparmio energetico, Tecnologia Adapto, Rapido riscaldamento, 6 temperature, Lavabile",
# "Imetec Adapto velvet jacquard plaid riscaldabile, coperta elettrica 140x180 cm tessuto morbido, basso consumo, tecnologia adapto, dispositivo di sicurezza, rapido riscaldamento, 6 temperature, lavabile",
# "Imetec adapto velvet square plaid riscaldabile, coperta elettrica 150x95 cm tessuto morbido, basso consumo, tecnologia adapto, dispositivo di sicurezza, rapido riscaldamento, 6 temperature, lavabile",
# """imetec adapto velvet tartan plaid riscaldabile con tasca mani e piedi, coperta elettrica 150x110 cm, basso consumo, tecnologia adapto, sicuro, rapido riscaldamento, 6 temperature, lavabile
# "","
# """Imetec Adapto velvet tartan plaid riscaldabile, coperta elettrica 160x120 cm tessuto morbido, basso consumo, tecnologia adapto, dispositivo di sicurezza, rapido riscaldamento, 6 temperature, lavabile
# "","
# "Imetec BW03 Boule Elettrica Cordless, Tecnologia Ceramica, Riscaldamento Ultrarapido, Termostato di Sicurezza, Spia di Ricarica",
# "Imetec Intellisense Cervical, Termoforo per Cervicale e Spalle, Cuscino Termico, 47x52 cm, Tessuto Anallergico, 5 Temperature, Electro Block di Sicurezza, Riscaldamento Rapido, Lavabile in Lavatrice",
# "Imetec Intellisense Comfort, Termoforo Multiuso, Cuscino Termico, per schiena e addome, Tasca per Mani, Rapido Riscaldamento, Tessuto Anallergico, 5 temperature, Electro Block di sicurezza, Lavabile",
# "Imetec Intellisense Essential, Termoforo Multiuso, Cuscino Termico, 40x35 cm, Tessuto Anallergico, 5 temperature, Electro Block di sicurezza, Lavabile, 40x35 cm",
# "Imetec Intellisense XL, Termoforo Extra Large Schiena e Gambe, Cuscino termico, 38x50 cm, Tessuto micro Peluche, 5 Temperature, Electro Block di sicurezza, Riscaldamento Rapido, lavabile in lavatrice",
# "Imetec Intellisensefeet scaldapiedi, tecnologia intellisense, tessuto morbido, 5 temperature, autospegnimento, dispositivo sicurezza electro block, rivestimento lavabile in lavatrice, 52x54 cm",
# "Imetec Scaldasonno Adapto matrimoniale 150 x 160 cm, basso consumo, riscaldamento rapido, temperatura personalizzata, 100% lana e merino, made in italy, 2 comandi separati, 6 temperature",
# """Imetec Scaldasonno Adapto matrimoniale 150 x 160 cm, basso consumo, tecnologia brevettata, riscaldamento rapido, temperatura personalizzata, 100% cotone, made in italy, 2 comandi con 6 temperature
# "","
# "Imetec Scaldasonno Adapto matrimoniale, basso consumo, riscaldamento rapido, temperatura personalizzata, tessuto trapuntato anallergico, made in italy, 2 comandi con 6 temperature, 150 x 160 cm",
# "Imetec Scaldasonno Adapto maxi coprimaterasso matrimoniale 195 x 165 cm, basso consumo, riscaldamento rapido, temperatura personalizzata, 100% lana e merino, doppio comando, 6 temperature",
# "imetec scaldasonno adapto maxi coprimaterasso matrimoniale 195x165 cm, basso consumo, riscaldamento rapido, temperatura personalizzata, 100% cotone percalle trapuntato, doppio comando, 6 temperature",
# "Imetec Scaldasonno Adapto maxi coprimaterasso matrimoniale 195x165 cm, basso consumo, riscaldamento rapido, temperatura personalizzata, tessuto anallergico trapuntato, doppio comando, 6 temperature",
# "Imetec Scaldasonno Adapto maxi coprimaterasso singolo 195x90 cm, basso consumo, riscaldamento rapido, temperatura personalizzata, tessuto anallergico trapuntato, comando a 6 temperature",
# "Imetec Scaldasonno Adapto piazza e mezza 150 x 120 cm, basso consumo, tecnologia brevettata, riscaldamento rapido, temperatura personalizzata, 100 % cotone, made in italy, comando con 6 temperature",
# "Imetec Scaldasonno Adapto singolo 150 x 80 cm, basso consumo, riscaldamento rapido, temperatura personalizzata, 100% lana e merino, made in italy, tessuto antiscivolo, comando con 6 temperature",
# "Imetec Scaldasonno Adapto singolo 150 x 80 cm, basso consumo, tecnologia brevettata, riscaldamento rapido, temperatura personalizzata, 100% cotone, made in italy, comando con 6 temperature",
# "Imetec Scaldasonno Adapto singolo 150 x 80 cm, riscaldamento rapido, temperatura costante e personalizzata, tessuto trapuntato anallergico, made in Italy, comando con 6 temperature",
# "Imetec Scaldasonno Matrimoniale 150x160 cm, 50% Lana e Merino, 2 Temperature, Dispositivo Sicurezza Electro Block, Made in Italy",
# "Imetec Scaldasonno Matrimoniale, 150 x 140 cm, 2 Comandi con 2 Temperature, Tessuto Trapuntato, Lavabile a Mano e in Lavatrice a 40°C",
# "Imetec Scaldasonno Sensitive Singolo, si Adatta ai Cambi di Temperatura, Morbido Peluche, Coprimaterasso Maxi 190 x 90, Electro Block, 6 Temperature, Timer, Risparmio Energetico, Lavabile",
# "Imetec Scaldasonno Singolo 150x80 cm, Basso Consumo, 50% Lana e Merino, 2 Temperature, Dispositivo Sicurezza Electro Block, Made in Italy",
# "Imetec Scaldasonno Singolo, 150 x 80 cm, Comando 2 Temperature, Tessuto Trapuntato, Lavabile a Mano e in Lavatrice a 40°C",
# "Imetec Body Analizer ES7 400 Bilancia Pesapersone Diagnostica, Misura Acqua Corporea, Massa Muscolare e Grassa",
# "Imetec Compact ES1 100 Bilancia pesapersone elettronica compatta, Design ultrasottile, Ampio LCD display, Portata Max 150 kg",
# "Imetec Monitoring ES9 300 Bilancia Pesapersone Elettronica, Monitoraggio Trend Grafico Peso, 4 Utenti, fino a 180 Kg, LCD Display, Vetro Temperato",
# "Imetec Precision ES13 200 Bilancia pesapersone elettronica, rivela anche le minime variazioni di peso, fino a 180 Kg, LCD Display, vetro temperato, batterie incluse",
# "Imetec Pro, Bilancia pesapersone meccanica, Rileva il peso in modo chiaro e affidabile, Pedana antiscivolo, Portata max 150 kg, Divisione 1 kg, Funzionamento analogico",
# "Imetec Activation Ferro da Stiro a Vapore, Piastra Scorrevole in Ceramica e Tormalina, Colpo Vapore 200 g, Tecnologia Anticalcare ZeroCalc, Antigoccia, Eco, 2400 W",
# """Imetec Eco Perfect Ferro da Stiro a Vapore, Risultati Ottimi con -35% di Consumo di Acqua e -25% di Consumo Energetico, Piastra con Rivestimento Pro Ceramic, Tripla Protezione Anticalcare, 2400 W
# "","
# "Imetec Intellifast, ferro da stiro a vapore, Tecnologia Intelli System, Impostazione automatica della temperatura, Piastra Ceramic Diamond, Protezione anticalcare, 2400 W, Colpo Vapore 200 g",
# "Imetec Nuvola Ferro da Stiro da Viaggio a Vapore, Piastra in Acciaio Inossidabile da 1000 W, Custodia da Viaggio, Doppio Voltaggio",
# "Imetec Onda F1, Ferro da Stiro a Vapore con Piastra a Onde Scorrevole Brevettata, 2400 W, Colpo di Vapore 200g, Serbatoio 300 ml, Tripla Protezione Anticalcare",
# "Imetec Onda F2, Ferro da Stiro a Vapore con Piastra a Onde Scorrevole Brevettata, 2400 W, Colpo di Vapore 200g, Serbatoio 300 ml, Tripla Protezione Anticalcare",
# "Imetec Onda P2 Ferro da Stiro Generatore di Vapore, Piastra a Onde Scorrevole Brevettata, 2400W, Colpo Vapore 300 g, Serbatoio 2,5L, 3 Filtri con Ioni di Argento, Tecnologia No Stop Vapor",
# "Imetec Rapidvapor Ferro da Stiro Generatore di Vapore, Piastra Fluid Ceramic a Rapido Riscaldamento, Leggero, Sistema di Pulizia Calc Clean, Colpo Vapore 240 g, 2400 W, 5 Bar Pump",
# "Imetec ZeroCalc KF1 100 Kit 3 filtri anticalcare per Ferri da Stiro Imetec ZeroCalc Pro Ceramic PS2 2400, PS2 2000, PS2 2200, Intellivapor PS3 3000, Imetec Onda, Ricambi Originali",
# "Imetec ZeroCalc PS1 2000 Ferro da Stiro Compatto, 3.8 BAR, Tecnologia Anticalcare e a Risparmio Energetico, Ricarica Continua, Vapore Pronto in 1 Minuto, 2100 W, Serbatoio XL da 1 L",
# "Imetec ZeroCalc Z1 2500 Ferro da Stiro, Tecnologia Anticalcare, Piastra Inox 2200 W, Colpo Vapore 120 g",
# "Imetec ZeroCalc Z1 2800 Ferro da Stiro con Tecnologia Anticalcare, Piastra Ceramica ad Alta Scorrevolezza, Tecnologia a Risparmio Energetico, 2200 W, Colpo Vapore 130 g",
# "Imetec Zerocalc Z3 3500 Ferro Da Stiro, Tecnologia Anticalcare, Piastra Inox 2400 W, Colpo Vapore 150 g",
# "Imetec Zerocalc Z3 3700 Ferro da Stiro, Tecnologia Anticalcare, Piastra Ceramica ad Alta Scorrevolezza, 2400W, Colpo Vapore 160 G, 0,3 litri, Dispositivo Antigoccia Tecnologia a Risparmio Energetico",
# "Imetec ZeroCalc Z3 3900 Ferro da Stiro a Vapore, Tecnologia Anticalcare, Tecnologia Intellivapor (temperatura unica ideale per tutti i tessuti), Piastra Ceramica, 2400 W, Colpo Vapore 170 g")
#



# master_titles_correct  <-
#   c(
#     "Bellissima Imetec Air Wonder, Spazzola ad Aria Calda, Tecnologia a ioni, Spazzole Rivestite in Ceramica e Cheratina, Asciuga, Dona Volume, 8 Accessori, 1000 W",
#     "Bellissima Imetec Asciugacapelli K9 2300 Asciuga e Mantiene Idratati i Capelli senza Effetto Crespo, 2300 W, Tecnologia a Ioni, 8 Combinazioni Flusso d'Aria Temperatura, Diffusore capellli ricci",
#     "Bellissima Imetec Asciugacapelli S9 2200 Asciuga e Modella con Precisione, 2200 W, 8 Combinazioni Flusso d'Aria Temperatura, Beccussio di precisione, Colpo aria fredda",
#     "Bellissima Imetec B26 100 Piastra per Capelli Lunghi o Difficili da Disciplinare, Formato Extra Large, Rivestimento in Ceramica, 160° C - 230° C",
#     "Bellissima Imetec B9 100 Piastra per Capelli, Piastra extralunghe, Riscaldamento Rapido, Multivoltaggio Automatico, Temperatura 210°, cavo giravole",
#     "Bellissima Imetec BM 200 Piastra Mini per Capelli Lisci e Luminosi, Temperatura 200°, Multivoltaggio Automatico, Dimensioni Compatte da Viaggio",
#     "Bellissima Imetec Ceramic & Argan Oil 6 in 1, Spazzola ad Aria Calda, 8 Accessori, Tecnologia a Ioni, Spazzole Rivestite in Ceramica e Olio di Argan, Asciuga e dona Volume, Colpo aria fredda, 1000 W",
#     "Bellissima Imetec Creativity 4 You, Asciugacapelli Professionale, 4 Accessori, Diffusore, Controllo Intelligente della temperatura, Ion Technology, Beauty Bag, 2 velocità, 3 temperature, 1800 W",
#     "Bellissima Imetec Creativity B15 50 Piastra per Capelli, Styling Liscio o Mosso, Rivestimento in Nanoceramica, Regolazione della Temperatura da 140°C a 230°C, Piastre Oscillanti",
#     "Bellissima Imetec Creativity B27 100 Piastra Per Capelli Con Rivestimento In Ceramica E Controllo Della Temperatura, Temperatura regolabile 150°C / 230°C",
#     "Bellissima Imetec Creativity B9 300 Piastra per Capelli, Styling Liscio o Mosso, Rivestimento in Ceramica, Regolazione Temperatura da 150°C a 230°C, Sistema Riscaldamento Rapido",
#     "Bellissima Imetec Creativity B9 400 Piastra per Capelli, Styling Liscio o Mosso, Rivestimento in Ceramica e Cheratina, Piastre Arrotondate",
#     "Bellissima Imetec Creativity Ceramic & Tourmaline, Piastra per Capelli, Liscio o Mosso, Rivestimento in Ceramica e tormalina, Regolazione Temperatura da 150-230°C, Riscaldamento Rapido",
#     "Bellissima Imetec Creativity Color Shine B22 100 Piastra per Capelli Colorati, Rivestimento in Tessuto per la Protezione dei Capelli, temperatura regolabile da 150°C a 230°C",
#     "Bellissima Imetec Diffon Ceramic & Argan Oil, Diffusore ad Aria Calda per Capelli Ricci, Tecnologia Ceramica e Olio di Argan, 2 Livelli Aria/Temperatura, Asciugatura Delicata, 700 W",
#     "Bellissima Imetec Diffon Supreme, Diffusore ad Aria Calda per Capelli Ricci, Diffusore XL con 12 Dita, Tecnologia Ceramica&Argan Oil, 2 Velocità, 3 Temperature, Ionizzatore, Asciugatura Delicata",
#     "Bellissima Imetec GH16 400 Spazzola ad Aria Modellante con 2 Accessori di Styling, Diametro 25 mm, Potenza 400 W, 2 Combinazioni Flusso d'Aria/Temperatura, Puntale Antiscottatura",
#     "Bellissima Imetec GT13 50 Arricciacapelli, Rivestimento in Ceramica, 7 Diversi Livelli di Temperatura da 150°C a 210°C, Diametro di 25 mm, Riscaldamento Rapido, Pronto all'Uso",
#     "Bellissima Imetec GT15 100 Arricciacapelli, Ricci Brillanti e Definiti, Diametro 19 mm, Rivestimento in Ceramica, Riscaldamento Rapido, Pronto all'Uso",
#     "Bellissima Imetec GT15 200 Ferro Arricciacapelli con Rivestimento in Ceramica, Forma Conica, Controllo della Temperatura",
#     "Bellissima imetec Intellisense B24 100 piastra intelligente, capelli lisci o mossi, protetti senza calore eccessivo, temperatura automatica su misura per tutti i capelli, rivestimento in ceramica",
#     "Bellissima Imetec Kit Bellezza Capelli con Asciugacapelli Professionale, Tecnologia Ceramica e Cheratina e Piastra Lisciante a Vapore, Rivestimento in Ceramica e Cheratina",
#     "Bellissima Imetec Magic Straight Brush PB11 100 Spazzola Elettrica Lisciante, 3 Lati Esterni Riscaldati, Spegnimento automatico, Tecnologia a Ioni, Ceramica, 160 C°- 210°C",
#     "Bellissima Imetec My Pro Creativity Infrared B8 200, piastra lisciante, tecnologia raggi infrarossi, rivestimento in ceramica e cheratina, 11 livelli di temperatura da 130 °C a 230 °C, cavo 1,8 m",
#     "Bellissima Imetec My Pro Revolution BHS4 1100, Spazzola ad Aria Calda, Asciuga e Modella, Rotazione Automatica a Doppio Verso, 2 Spazzole da 40 e 50 mm, Ionizzatore, 1000 W, Rivestimento in Ceramica",
#     "Bellissima Imetec P11 2300 Asciugacapelli Professionale, 2300 W, Rivestimento in Ceramica e Tormalina, 8 Combinazioni Aria/Temperatura, Beccuccio Stretto, Diffusore capelli ricci",
#     "Bellissima Imetec P2 2200 Asciugacapelli Professionale, Potenza 2200 W, Tecnologia a Ioni per Idratare i Capelli e Ridurre l'Effetto Crespo, 8 Combinazioni Aria/Temperatura, Beccuccio Stretto",
#     "Bellissima Imetec P3 3400 Asciugacapelli con Motore AC professionale, Tecnologia a Ioni per Capelli morbidi, Idratati e Luminosi, Griglia Rivestita in Ceramica, 2400 W di Potenza",
#     "Bellissima Imetec S5 2200 Asciugacapelli, 2200 W, Asciugatura Rapida e Styling a Lunga Durata, 8 Combinazioni Aria/Temperatura, Colpo d'Aria Fredda",
#     "Bellissima Imetec Steam Ceramic & Argan Oil, Piastra lisciante a Vapore, Rivestimento in Ceramica e Olio di Argan, 3 Livelli di Temperatura, Riscaldamento Rapido, Piastre Oscillanti, Autospegnimento",
#     "Bellissima Imetec Steam Elixir, Piastra a Vapore, Capelli lisci in Una Sola Passata Senza Danni, Rivestimento in Ceramica e Olio Argan, Rapido Riscaldamento, 4 Temperature, Tappetino Termoresistente",
#     "Bellissima My Pro Imetec Beach Waves Gt20 100 Piastra Per Capelli A Onde Larghe E Strette, Rivestimento in ceramica, temperatura regolabile",
#     "Bellissima My Pro Imetec Beach Waves Gt20 400 Piastra Per Capelli per onde larghe e strette, Rivestimento In Ceramica, temperatura regolabile 160 C°- 200°C",
#     "Bellissima My Pro Imetec Ceramic P5 3800 Asciugacapelli Professionale Per Capelli Morbidi E Luminosi, Tecnologia Ceramica, 2300 W",
#     "Bellissima My Pro Imetec Diffon Ceramic, Diffusore ad Aria Calda per Capelli Ricci, Tecnologia Ceramica, 700 W, 2 Combinazioni Aria/Temperatura, Asciugatura Delicata Senza Effetto Crespo",
#     "Bellissima My Pro Imetec GH18 1100 Modellatore ad Aria, Rivestimento spazzole in Ceramica, 5 Accessori per Realizzare Capelli Lisci e Luminosi, a Onde morbide o Ricci Stretti, 1000 W",
#     "Bellissima My Pro Imetec GT22 110, Accessorio Soft Curls per Styler Arricciacapelli Twist & Style, Ferro Riscaldato da 25 mm, Rivestimento in Ceramica, Temperatura 185°C",
#     "Bellissima My Pro Imetec GT22 120, Accessorio Sculpted Curls per Styler Arricciacapelli Twist & Style, Ferro Riscaldato da 11 mm, Rivestimento in Ceramica, Temperatura 185°C",
#     "Bellissima My Pro Imetec GT22 130, Accessorio Loose Waves per Styler Arricciacapelli Twist&Style, Ferro Riscaldato ad Ellisse, Diametro da 26 a 38 mm, Rivestimento Ceramica, Temperatura 190°C",
#     "Bellissima My Pro Imetec GT22 140 Accessorio Glamour Waves per Styler Arricciacapelli Twist&Style, Ferro Riscaldato Forma Conica, Diametro 13/38 mm, Rivestimento in Ceramica, Temperatura 190°C",
#     "Bellissima My Pro Imetec GT22 150 Accessorio Mermaid Waves per Styler Arricciacapelli Twist&Style, Ferro Riscaldato Forma Bubbles, Diametro 25 mm, Rivestimento in Ceramica, Temperatura 190°C",
#     "Bellissima My Pro Imetec GT22 160, Accessorio Hollywood Waves per Styler Arricciacapelli Twist&Style, Rivestimento in ceramica, Temperatura 190°C, Ovale, Diametro 38 x 25 mm",
#     "Bellissima My Pro Imetec Magic Style Brush P2 30 Spazzola Riscaldata, Capelli Liscio Effetto Naturale/Mosso Sostenuto, Diametro 30 mm, Tecnologia a Ioni, Rivestimento Ceramica",
#     "Bellissima My Pro Imetec Steam B28 100 Piastra per Capelli Professionale a Vapore, Effetto Liscio a Lungo, Ceramica, Temperatura Regolabile 170°C - 200°C - 230°C, Riscaldamento Rapido",
#     "Bellissima My Pro Imetec Twist & Style GT22 100, Manico per Styler Arricciacapelli per Onde e Ricci, Temperatura Automatica, Riscaldamento Rapido, Orientabile a 90°, Accessori non Inclusi",
#     "Bellissima Twist&Style arricciacapelli, manico + 4 accessori per ricci e onde, temperatura automatica, riscaldamento rapido, rivestimento in ceramica, orientabile a 90°, idea regalo",
#     "Imetec C20 2100 Asciugacapelli, 2100 W, Beccuccio Orientabile, Diffusore Professionale, Funzione Fast Drying per un'Asciugatura Rapida, 4 Combinazioni Aria/Temperatura, Colpo Aria Fredda, Cavo 1.8 m",
#     "Imetec ECO SE9 1000 Asciugacapelli con Tecnologia Eco Technology 1400 W, Consumo Energetico Ridotto, 8 Combinazioni Aria/Temperatura, Diffusore per Capelli Ricci",
#     "Imetec Salon Expert P3 3600 Asciugacapelli, Motore Professionale, 2200 W, Ionizzatore, Griglia Ceramica e tormalina, 8 combinazioni aria temperatura, Concentratore e Diffusore Professionali",
#     "Imetec Salon Expert P5 3600 Asciugacapelli Professionale, 2300 W, Griglia con Rivestimento in Ceramica e Cheratina, Tecnologia a Ioni, 8 Combinazioni Aria/Temperatura, 2 Beccucci",
#     "Imetec Compact Air, Termoventilatore piccolo e potente, Stufetta Elettrica, 2000 W, maniglia integrata, temperatura regolabile, funzione antigelo, dispositivo di protezione dai surriscaldamenti",
#     "Imetec Eco Ceramic Diffusion, Stufetta elettrica, Termoventilatore, Corpo Oscillante, Tecnologia Ceramica, Basso Consumo Energetico, 6 Funzioni di Temperatura, Timer",
#     "Imetec Eco Ceramic, Termoventilatore, Stufetta elettrica, Tecnologia Ceramica, Basso Consumo Energetico, Silenzioso, 3 Livelli di Temperatura, Termostato Ambiente",
#     "Imetec Eco Rapid, Stufa Elettrica 2000 W, Tecnologia a Basso Consumo Energetico, Termoconvettore 4 Temperature, Termostato Ambiente, Silenzioso",
#     "Imetec Silent Power Comfort, Termoventilatore silenzioso e compatto, Stufetta Elettrica, 2100 W, 4 funzioni, termostato ambiente e di sicurezza, funzione antigelo, maniglia",
#     "Imetec Silent Power Eco, Termoventilatore Silenzioso, Stufetta Elettrica, Tecnologia ECO -35% Consumo Energetico, 2100 W, Funzione Antigelo, Temperatura Regolabile, 4 Funzioni, Termostato Ambiente",
#     "Imetec Silent Power Protection, Termoventilatore silenzioso, 2100 W, Stufetta Elettrica, Timer spegnimento programmabile, temperatura regolabile, 4 funzioni, termostato ambiente, funzione antigelo",
#     "Imetec Silent Power Pure, Termoventilatore silenzioso con ionizzatore e timer di autospegnimento, Stufetta Elettrica, 2100 W, funzione antigelo, temperatura regolabile, 4 funzioni",
#     "Imetec BuonFrullato, Frullatore Compatto, Bicchiere in Plastica BPA Free, Capacità 700 ml, Coperchio Dosa Liquidi, 400 W",
#     "Imetec CakeLover, Impastatrice Planetaria per Dolci, Creme e Impasti Salati, Accessoriata, Compatta, 1500W, Fruste, Pale e Ganci, Contenitore Inox, 5 Litri, 6 Velocità, Ricettario",
#     "Imetec Crea&Crema Sbattitore Elettrico, Fruste Extralunghe per Impasti Dolci e Panna Montata, Ganci Acciaio Inox per Impastare, 5 Velocità, Funzione Turbo, Design Ergonomico, 500 W",
#     "Imetec Cukò Maestro, Robot da cucina con cottura, 20 programmi, 10 funzioni, Impasta Pane e Pizza, capienza 2 L, Fino a 6 porzioni, 8 accessori, ricettario, 1000 W",
#     "Imetec Dolcevita ES4 Bilancia Elettronica da Cucina, Pesa Solidi e Liquidi, Contenitore Estraibile, Scala Graduata, Portata 5 Kg, Divisione 1 g, Funzione Tara, Display Digitale, Spegnimento Automatico",
#     "Imetec FrescoAroma Macina Caffè e Spezie, Pepe, Zucchero, Frutta Secca Elettrico con Lame in Acciaio Inox, Contenitore Salva-Freschezza, Funzionamento a Impulsi, 150 W",
#     "Imetec FriggiLeggero - Friggitrice ad aria multifunzione, 12 programmi, Riscaldamento rapido, 18 L, 1550 W, Comandi digitali, Cestello, Griglia, Leccarda, Ricettario, 40x36x34,5 cm",
#     "Imetec Frulla&Crea Frullatore a Immersione, 3 Accessori, Tritatutto, Frusta e Bicchiere BPA free, Piede Large, 2 Velocità, 1000 W, 15000 Giri Minuto",
#     "Imetec Frulla&Gusta, Frullatore con 4 lame in acciaio inox, bicchiare in vetro, 2 velocità e funzionamento impulsi, 500 W, capienza 800 ml, lavabile lavastoviglie, piedini antiscivolo",
#     "Imetec Frulla&Trita Frullatore a Immersione, Lame in Acciaio Inox, Piede Extra Large, Gambo Estraibile, Bicchiere graduato da 1 Litro, 800 Watt, 15.000 Giri Minuto",
#     "Imetec FrullaFacile, Frullatore a Immersione, Gambo XL, Lame in Acciaio Inox, Funzionamento a Impulsi, Bicchiere Graduato da 700 ml BPA Free, 800 W, Impugnatura Ergonomica",
#     "Imetec FrullaRapido, Frullatore a Immersione, Gambo XL in Acciaio, Lame in Acciaio Inox, Funzionamento a Impulsi, 450 W",
#     "Imetec FrullaRapido+, Frullatore a Immersione, Gambo XL in Acciaio, Lame in Acciaio Inox, Funzionamento a Impulsi, Bicchiere graduato da 700 ml BPA Free, 800 W",
#     "Imetec FrullaTutto Frullatore, Lame Tritaghiaccio in Acciaio Inox, Bicchiere da 800 ml in Vetro Antigraffio, 2 Velocità, Funzionamento Impulsi, 500 W, 2 Velocità, Lavabile Lavastoviglie",
#     "Imetec GranToast, tostapane per toast extra farciti, fessure XL, pinze a fondo chiuso apribili a 360° e regolabili in larghezza, 600 W, 10 livelli doratura, Timer",
#     "Imetec Grattugissima, grattuggia elettrica per formaggio, pane e frutta secca, Contenitore estraibile, coperchio salvafreschezza, 150 W, rullo in acciacio, accessori lavabili in lavastoviglie",
#     "Imetec Griglia&Gusta Bistecchiera, Piastre Antiaderenti Removibili, Rivestimento Triplo Strato al Titanio Stone-Look, Grill con 7 Posizioni, 3 Funzioni di Cottura, Thermo Control, 2000 Watt",
#     "Imetec HB3 Frullatore a Immersione, Gambo Large Estraibile, Lame in Acciaio Inox, Funzionamento a Impulsi, 450 W",
#     "Imetec La Grattugia, grattugia elettrica, Rullo in Acciaio Inox, Tramoggia in Alluminio, Contenitore Estraibile con Coperchio Salvafreschezza, Sistema di Sicurezza, 150 W",
#     "Imetec La Panetteria +Zeroglu, Macchina per Pane, Ciabatte, Panini, Dolci con Farine Naturali e Senza Glutine, 20 Programmi, 2 Pale Impastatrici, Temperatura di Lievitazione Controllata",
#     "Imetec La TostaGriglia, tostiera, Piastre XL Rigate e Antiaderenti, Interruttore ON/OFF, Spia Riscaldamento Piastre, Gancio Chiusura, Avvolgicavo, Tostapane Compatto, 900 W",
#     "Imetec PaneMio Macchina del Pane, Impasta Lievita e Cuoce, 12 Programmi, Cestello Pagnotta, Accessorio Panini, 3 Livelli di Doratura, Avvio Programmabile, 550 W, Ricettario",
#     "Imetec Personal e Sport Blender PB 100 Mini Frullatore con 2 Bottiglie Take-Away in Tritan e 4 Lame in Acciaio Inox, 22,000 giri/min",
#     "Imetec SM 1000 Soup Maker, Cuoce e Frulla, 3 Programmi Automatici, Vellutate, Zuppe e Frullati, 6 Porzioni, Lame Seghettate Acciaio Inox, con Ricettario, 900 W, 1.6 Litri",
#     "Imetec SP 100 Spiralizer Elettrico, Tagliaverdure a Spirale, Affetta Verdure in 3 Forme Spaghetti, Tagliatelle, Pappardelle, Lame in Acciaio Inox, Contenitore BPA Free 500 ml, Sistema di Sicurezza",
#     "Imetec Succovivo Pro 2000 Estrattore di Succo Professionale a Freddo, Spremitura Lenta 48 Giri/Min, 2 Filtri per Succhi, Accessorio per Granite e Sorbetti, Kit per Maschere Bellezza, con Ricettario",
#     "Imetec Succovivo SJ4 1300 Estrattore di Succo Professionale a Freddo, Spremitura Lenta 50 Giri/Min, Filtro per Succhi, Accessorio per Granite e Sorbetti, pulizia in 2 minuti",
#     "Imetec Tosta&Griglia, Tostiera elettrica, piastre XL per preparare 3 toast alla volta, compatta, riponibile in verticale, 800 W, spie di funzionamento",
#     "Imetec TostaMaxi Tostapane, 2 Fessure Extralarge e Pinze Apribili per Toast Extrafarciti, 10 livelli di doratura, Timer con Autospegnimento, Cassetto Raccoglibriciole, 600 W",
#     "Imetec TritaCompact Tritatutto, Lame in Acciaio Inox, Capienza Contenitore 400 ml, Funzionamento a Pressione, Compatto, 350 W",
#     "Imetec TritaMax, Tritatutto, 4 Lame in Acciaio Inox, Capiente Contenitore 600 ml, Accessorio Frusta per Salse e Panna Montata, 2 velocità, 500 Watt",
#     "Imetec Tritapiù Tritatutto, 4 Lame in Acciaio Inox, Capiente Contenitore 600 ml, 18.000 Giri/min, Funzionamento a Pressione, 1000 W, Materiali BPA Free",
#     "Imetec Zero-Glu Pro, Macchina per Pane, Ciabatte e Panini Senza Glutine per Celiaci, Pasta Pizza, Dolci, Marmellate, 20 Programmi, 2 Pale Impastatrici, Temperatura levitazione controllata, Ricettario",
#     "Bellissima Imetec Face Cleansing Spazzola Per Pulizia Viso, Deterge ed esfolia la pelle, Kit con 5 testine intercambiabili, Tecnologia a vibrazione sonica, Impermeabile, Beauty Bag inclusa",
#     "Bellissima Imetec Kit Testine di Ricambio Face Cleansing per Pelli Normali e Pelli Sensibili, 4 Diverse Testine per Detergere, Massaggiare, Levigare e Idratare La Pelle",
#     "Imetec Adapto Elegance, Plaid riscaldabile, Coperta elettrica, 180x140 cm, tessuto morbido Velvet Double Face, Risparmio energetico, Tecnologia adapto, Rapido riscaldamento, 6 temperature, Lavabile",
#     "Imetec Adapto Grand Luxe, Plaid riscaldabile, Coperta elettrica, 180x140 cm, tessuto morbido Peluche Sherpa, Risparmio energetico, Tecnologia Adapto, Rapido riscaldamento, 6 temperature, Lavabile",
#     "Imetec Adapto velvet jacquard plaid riscaldabile, coperta elettrica 140x180 cm tessuto morbido, basso consumo, tecnologia adapto, dispositivo di sicurezza, rapido riscaldamento, 6 temperature, lavabile",
#     "Imetec adapto velvet square plaid riscaldabile, coperta elettrica 150x95 cm tessuto morbido, basso consumo, tecnologia adapto, dispositivo di sicurezza, rapido riscaldamento, 6 temperature, lavabile",
#     "imetec adapto velvet tartan plaid riscaldabile con tasca mani e piedi, coperta elettrica 150x110 cm, basso consumo, tecnologia adapto, sicuro, rapido riscaldamento, 6 temperature, lavabile",
#     "Imetec Adapto velvet tartan plaid riscaldabile, coperta elettrica 160x120 cm tessuto morbido, basso consumo, tecnologia adapto, dispositivo di sicurezza, rapido riscaldamento, 6 temperature, lavabile",
#     "Imetec BW03 Boule Elettrica Cordless, Tecnologia Ceramica, Riscaldamento Ultrarapido, Termostato di Sicurezza, Spia di Ricarica",
#     "Imetec Intellisense Cervical, Termoforo per Cervicale e Spalle, Cuscino Termico, 47x52 cm, Tessuto Anallergico, 5 Temperature, Electro Block di Sicurezza, Riscaldamento Rapido, Lavabile in Lavatrice",
#     "Imetec Intellisense Comfort, Termoforo Multiuso, Cuscino Termico, per schiena e addome, Tasca per Mani, Rapido Riscaldamento, Tessuto Anallergico, 5 temperature, Electro Block di sicurezza, Lavabile",
#     "Imetec Intellisense Essential, Termoforo Multiuso, Cuscino Termico, 40x35 cm, Tessuto Anallergico, 5 temperature, Electro Block di sicurezza, Lavabile, 40x35 cm",
#     "Imetec Intellisense XL, Termoforo Extra Large Schiena e Gambe, Cuscino termico, 38x50 cm, Tessuto micro Peluche, 5 Temperature, Electro Block di sicurezza, Riscaldamento Rapido, lavabile in lavatrice",
#     "Imetec Intellisensefeet scaldapiedi, tecnologia intellisense, tessuto morbido, 5 temperature, autospegnimento, dispositivo sicurezza electro block, rivestimento lavabile in lavatrice, 52x54 cm",
#     "Imetec Scaldasonno Adapto matrimoniale 150 x 160 cm, basso consumo, riscaldamento rapido, temperatura personalizzata, 100% lana e merino, made in italy, 2 comandi separati, 6 temperature",
#     "Imetec Scaldasonno Adapto matrimoniale 150 x 160 cm, basso consumo, tecnologia brevettata, riscaldamento rapido, temperatura personalizzata, 100% cotone, made in italy, 2 comandi con 6 temperature",
#     "Imetec Scaldasonno Adapto matrimoniale, basso consumo, riscaldamento rapido, temperatura personalizzata, tessuto trapuntato anallergico, made in italy, 2 comandi con 6 temperature, 150 x 160 cm",
#     "Imetec Scaldasonno Adapto maxi coprimaterasso matrimoniale 195 x 165 cm, basso consumo, riscaldamento rapido, temperatura personalizzata, 100% lana e merino, doppio comando, 6 temperature",
#     "imetec scaldasonno adapto maxi coprimaterasso matrimoniale 195x165 cm, basso consumo, riscaldamento rapido, temperatura personalizzata, 100% cotone percalle trapuntato, doppio comando, 6 temperature",
#     "Imetec Scaldasonno Adapto maxi coprimaterasso matrimoniale 195x165 cm, basso consumo, riscaldamento rapido, temperatura personalizzata, tessuto anallergico trapuntato, doppio comando, 6 temperature",
#     "Imetec Scaldasonno Adapto maxi coprimaterasso singolo 195x90 cm, basso consumo, riscaldamento rapido, temperatura personalizzata, tessuto anallergico trapuntato, comando a 6 temperature",
#     "Imetec Scaldasonno Adapto piazza e mezza 150 x 120 cm, basso consumo, tecnologia brevettata, riscaldamento rapido, temperatura personalizzata, 100 % cotone, made in italy, comando con 6 temperature",
#     "Imetec Scaldasonno Adapto singolo 150 x 80 cm, basso consumo, riscaldamento rapido, temperatura personalizzata, 100% lana e merino, made in italy, tessuto antiscivolo, comando con 6 temperature",
#     "Imetec Scaldasonno Adapto singolo 150 x 80 cm, basso consumo, tecnologia brevettata, riscaldamento rapido, temperatura personalizzata, 100% cotone, made in italy, comando con 6 temperature",
#     "Imetec Scaldasonno Adapto singolo 150 x 80 cm, riscaldamento rapido, temperatura costante e personalizzata, tessuto trapuntato anallergico, made in Italy, comando con 6 temperature",
#     "Imetec Scaldasonno Matrimoniale 150x160 cm, 50% Lana e Merino, 2 Temperature, Dispositivo Sicurezza Electro Block, Made in Italy",
#     "Imetec Scaldasonno Matrimoniale, 150 x 140 cm, 2 Comandi con 2 Temperature, Tessuto Trapuntato, Lavabile a Mano e in Lavatrice a 40°C",
#     "Imetec Scaldasonno Sensitive Singolo, si Adatta ai Cambi di Temperatura, Morbido Peluche, Coprimaterasso Maxi 190 x 90, Electro Block, 6 Temperature, Timer, Risparmio Energetico, Lavabile",
#     "Imetec Scaldasonno Singolo 150x80 cm, Basso Consumo, 50% Lana e Merino, 2 Temperature, Dispositivo Sicurezza Electro Block, Made in Italy",
#     "Imetec Scaldasonno Singolo, 150 x 80 cm, Comando 2 Temperature, Tessuto Trapuntato, Lavabile a Mano e in Lavatrice a 40°C",
#     "Imetec Body Analizer ES7 400 Bilancia Pesapersone Diagnostica, Misura Acqua Corporea, Massa Muscolare e Grassa",
#     "Imetec Compact ES1 100 Bilancia pesapersone elettronica compatta, Design ultrasottile, Ampio LCD display, Portata Max 150 kg",
#     "Imetec Monitoring ES9 300 Bilancia Pesapersone Elettronica, Monitoraggio Trend Grafico Peso, 4 Utenti, fino a 180 Kg, LCD Display, Vetro Temperato",
#     "Imetec Precision ES13 200 Bilancia pesapersone elettronica, rivela anche le minime variazioni di peso, fino a 180 Kg, LCD Display, vetro temperato, batterie incluse",
#     "Imetec Pro, Bilancia pesapersone meccanica, Rileva il peso in modo chiaro e affidabile, Pedana antiscivolo, Portata max 150 kg, Divisione 1 kg, Funzionamento analogico",
#     "Imetec Activation Ferro da Stiro a Vapore, Piastra Scorrevole in Ceramica e Tormalina, Colpo Vapore 200 g, Tecnologia Anticalcare ZeroCalc, Antigoccia, Eco, 2400 W",
#     "Imetec Eco Perfect Ferro da Stiro a Vapore, Risultati Ottimi con -35% di Consumo di Acqua e -25% di Consumo Energetico, Piastra con Rivestimento Pro Ceramic, Tripla Protezione Anticalcare, 2400 W",
#     "Imetec Intellifast, ferro da stiro a vapore, Tecnologia Intelli System, Impostazione automatica della temperatura, Piastra Ceramic Diamond, Protezione anticalcare, 2400 W, Colpo Vapore 200 g",
#     "Imetec Nuvola Ferro da Stiro da Viaggio a Vapore, Piastra in Acciaio Inossidabile da 1000 W, Custodia da Viaggio, Doppio Voltaggio",
#     "Imetec Onda F1, Ferro da Stiro a Vapore con Piastra a Onde Scorrevole Brevettata, 2400 W, Colpo di Vapore 200g, Serbatoio 300 ml, Tripla Protezione Anticalcare",
#     "Imetec Onda F2, Ferro da Stiro a Vapore con Piastra a Onde Scorrevole Brevettata, 2400 W, Colpo di Vapore 200g, Serbatoio 300 ml, Tripla Protezione Anticalcare",
#     "Imetec Onda P2 Ferro da Stiro Generatore di Vapore, Piastra a Onde Scorrevole Brevettata, 2400W, Colpo Vapore 300 g, Serbatoio 2,5L, 3 Filtri con Ioni di Argento, Tecnologia No Stop Vapor",
#     "Imetec Rapidvapor Ferro da Stiro Generatore di Vapore, Piastra Fluid Ceramic a Rapido Riscaldamento, Leggero, Sistema di Pulizia Calc Clean, Colpo Vapore 240 g, 2400 W, 5 Bar Pump",
#     "Imetec ZeroCalc KF1 100 Kit 3 filtri anticalcare per Ferri da Stiro Imetec ZeroCalc Pro Ceramic PS2 2400, PS2 2000, PS2 2200, Intellivapor PS3 3000, Imetec Onda, Ricambi Originali",
#     "Imetec ZeroCalc PS1 2000 Ferro da Stiro Compatto, 3.8 BAR, Tecnologia Anticalcare e a Risparmio Energetico, Ricarica Continua, Vapore Pronto in 1 Minuto, 2100 W, Serbatoio XL da 1 L",
#     "Imetec ZeroCalc Z1 2500 Ferro da Stiro, Tecnologia Anticalcare, Piastra Inox 2200 W, Colpo Vapore 120 g",
#     "Imetec ZeroCalc Z1 2800 Ferro da Stiro con Tecnologia Anticalcare, Piastra Ceramica ad Alta Scorrevolezza, Tecnologia a Risparmio Energetico, 2200 W, Colpo Vapore 130 g",
#     "Imetec Zerocalc Z3 3500 Ferro Da Stiro, Tecnologia Anticalcare, Piastra Inox 2400 W, Colpo Vapore 150 g",
#     "Imetec Zerocalc Z3 3700 Ferro da Stiro, Tecnologia Anticalcare, Piastra Ceramica ad Alta Scorrevolezza, 2400W, Colpo Vapore 160 G, 0,3 litri, Dispositivo Antigoccia Tecnologia a Risparmio Energetico",
#     "Imetec ZeroCalc Z3 3900 Ferro da Stiro a Vapore, Tecnologia Anticalcare, Tecnologia Intellivapor (temperatura unica ideale per tutti i tessuti), Piastra Ceramica, 2400 W, Colpo Vapore 170 g"
#   )















# master_title <-
#     c(
#         'Bellissima My Pro Imetec Diffon Ceramic, Diffusore ad Aria Calda per Capelli Ricci, Tecnologia Ceramica, 700 W, 2 Combinazioni Aria/Temperatura, Asciugatura Delicata Senza Effetto Crespo',
#         'Bellissima Imetec P3 3400 Asciugacapelli con Motore AC professionale, Tecnologia a Ioni per Capelli morbidi, Idratati e Luminosi, Griglia Rivestita in Ceramica, 2400 W di Potenza',
#         'Bellissima My Pro Imetec Steam B28 100 Piastra per Capelli Professionale a Vapore, Effetto Liscio a Lungo, Ceramica, Temperatura Regolabile 170°C - 200°C - 230°C, Riscaldamento Rapido',
#         'Bellissima Imetec Magic Straight Brush PB11 100 Spazzola Elettrica Lisciante, 3 Lati Esterni Riscaldati, Spegnimento automatico, Tecnologia a Ioni, Ceramica, 160 C°- 210°C',
#         'Bellissima My Pro Imetec Magic Style Brush P2 30 Spazzola Riscaldata, Capelli Liscio Effetto Naturale/Mosso Sostenuto, Diametro 30 mm, Tecnologia a Ioni, Rivestimento Ceramica',
#         'Bellissima My Pro Imetec Beach Waves Gt20 100 Piastra Per Capelli A Onde Larghe E Strette, Rivestimento in ceramica, temperatura regolabile',
#         'Bellissima My Pro Imetec Beach Waves Gt20 400 Piastra Per Capelli per onde larghe e strette, Rivestimento In Ceramica, temperatura regolabile 160 C°- 200°C',
#         'Bellissima My Pro Imetec GH18 1100 Modellatore ad Aria, Rivestimento spazzole in Ceramica, 5 Accessori per Realizzare Capelli Lisci e Luminosi, a Onde morbide o Ricci Stretti, 1000 W',
#         'Bellissima My Pro Imetec Twist & Style GT22 100, Manico per Styler Arricciacapelli per Onde e Ricci, Temperatura Automatica, Riscaldamento Rapido, Orientabile a 90°, Accessori non Inclusi',
#         'Bellissima My Pro Imetec GT22 110, Accessorio Soft Curls per Styler Arricciacapelli Twist & Style, Ferro Riscaldato da 25 mm, Rivestimento in Ceramica, Temperatura 185°C',
#         'Bellissima My Pro Imetec GT22 120, Accessorio Sculpted Curls per Styler Arricciacapelli Twist & Style, Ferro Riscaldato da 11 mm, Rivestimento in Ceramica, Temperatura 185°C',
#         'Bellissima My Pro Imetec GT22 130, Accessorio Loose Waves per Styler Arricciacapelli Twist&Style, Ferro Riscaldato ad Ellisse, Diametro da 26 a 38 mm, Rivestimento Ceramica, Temperatura 190°C',
#         'Bellissima My Pro Imetec GT22 140 Accessorio Glamour Waves per Styler Arricciacapelli Twist&Style, Ferro Riscaldato Forma Conica, Diametro 13/38 mm, Rivestimento in Ceramica, Temperatura 190°C',
#         'Bellissima My Pro Imetec GT22 150 Accessorio Mermaid Waves per Styler Arricciacapelli Twist&Style, Ferro Riscaldato Forma Bubbles, Diametro 25 mm, Rivestimento in Ceramica, Temperatura 190°C',
#         'Bellissima My Pro Imetec GT22 160, Accessorio Hollywood Waves per Styler Arricciacapelli Twist&Style, Rivestimento in ceramica, Temperatura 190°C, Ovale, Diametro 38 x 25 mm',
#         'Bellissima My Pro Imetec Ceramic P5 3800 Asciugacapelli Professionale Per Capelli Morbidi E Luminosi, Tecnologia Ceramica, 2300 W',
#         'Bellissima Imetec Asciugacapelli K9 2300 Asciuga e Mantiene Idratati i Capelli senza Effetto Crespo, 2300 W, Tecnologia a Ioni, 8 Combinazioni Flusso dAria Temperatura, Diffusore capellli ricci',
#         'Bellissima Imetec Asciugacapelli S9 2200 Asciuga e Modella con Precisione, 2200 W, 8 Combinazioni Flusso dAria Temperatura, Beccussio di precisione, Colpo aria fredda',
#         'Bellissima Imetec Creativity 4 You, Asciugacapelli Professionale, 4 Accessori, Diffusore, Controllo Intelligente della temperatura, Ion Technology, Beauty Bag, 2 velocità, 3 temperature, 1800 W',
#         'Bellissima Imetec Diffon Supreme, Diffusore ad Aria Calda per Capelli Ricci, Diffusore XL con 12 Dita, Tecnologia Ceramica&Argan Oil, 2 Velocità, 3 Temperature, Ionizzatore, Asciugatura Delicata',
#         'Bellissima Imetec P11 2300 Asciugacapelli Professionale, 2300 W, Rivestimento in Ceramica e Tormalina, 8 Combinazioni Aria/Temperatura, Beccuccio Stretto, Diffusore capelli ricci',
#         'Bellissima Imetec S5 2200 Asciugacapelli, 2200 W, Asciugatura Rapida e Styling a Lunga Durata, 8 Combinazioni Aria/Temperatura, Colpo dAria Fredda',
#         'Imetec ECO SE9 1000 Asciugacapelli con Tecnologia Eco Technology 1400 W, Consumo Energetico Ridotto, 8 Combinazioni Aria/Temperatura, Diffusore per Capelli Ricci',
#         'Imetec C20 2100 Asciugacapelli, 2100 W, Beccuccio Orientabile, Diffusore Professionale, Funzione Fast Drying per unAsciugatura Rapida, 4 Combinazioni Aria/Temperatura, Colpo Aria Fredda, Cavo 1.8 m',
#         'Bellissima Imetec P2 2200 Asciugacapelli Professionale, Potenza 2200 W, Tecnologia a Ioni per Idratare i Capelli e Ridurre lEffetto Crespo, 8 Combinazioni Aria/Temperatura, Beccuccio Stretto',
#         'Imetec Salon Expert P3 3600 Asciugacapelli, Motore Professionale, 2200 W, Ionizzatore, Griglia Ceramica e tormalina, 8 combinazioni aria temperatura, Concentratore e Diffusore Professionali',
#         'Imetec Salon Expert P5 3600 Asciugacapelli Professionale, 2300 W, Griglia con Rivestimento in Ceramica e Cheratina, Tecnologia a Ioni, 8 Combinazioni Aria/Temperatura, 2 Beccucci',
#         'Bellissima Imetec Creativity Ceramic & Tourmaline, Piastra per Capelli, Liscio o Mosso, Rivestimento in Ceramica e tormalina, Regolazione Temperatura da 150-230°C, Riscaldamento Rapido',
#         'Bellissima Imetec My Pro Creativity Infrared B8 200, piastra lisciante, tecnologia raggi infrarossi, rivestimento in ceramica e cheratina, 11 livelli di temperatura da 130 °C a 230 °C, cavo 1,8 m',
#         'Bellissima Imetec B26 100 Piastra per Capelli Lunghi o Difficili da Disciplinare, Formato Extra Large, Rivestimento in Ceramica, 160° C - 230° C',
#         'Bellissima Imetec B9 100 Piastra per Capelli, Piastra extralunghe, Riscaldamento Rapido, Multivoltaggio Automatico, Temperatura 210°, cavo giravole',
#         'Bellissima Imetec Creativity B9 300 Piastra per Capelli, Styling Liscio o Mosso, Rivestimento in Ceramica, Regolazione Temperatura da 150°C a 230°C, Sistema Riscaldamento Rapido',
#         'Bellissima Imetec Creativity B9 400 Piastra per Capelli, Styling Liscio o Mosso, Rivestimento in Ceramica e Cheratina, Piastre Arrotondate',
#         'Bellissima Imetec BM 200 Piastra Mini per Capelli Lisci e Luminosi, Temperatura 200°, Multivoltaggio Automatico, Dimensioni Compatte da Viaggio',
#         'Bellissima Imetec Creativity B15 50 Piastra per Capelli, Styling Liscio o Mosso, Rivestimento in Nanoceramica, Regolazione della Temperatura da 140°C a 230°C, Piastre Oscillanti',
#         'Bellissima Imetec Creativity B27 100 Piastra Per Capelli Con Rivestimento In Ceramica E Controllo Della Temperatura, Temperatura regolabile 150°C / 230°C',
#         'Bellissima Imetec Creativity Color Shine B22 100 Piastra per Capelli Colorati, Rivestimento in Tessuto per la Protezione dei Capelli, temperatura regolabile da 150°C a 230°C',
#         'Bellissima Imetec Intellisense B24 100 Piastra per Capelli Lisci e Mossi a 60° C in Meno con Tecnologia Intellistyle, Rivestimento in Ceramica, 5 Livelli di Temperatura',
#         'Bellissima Imetec Steam Elixir, Piastra a Vapore, Capelli lisci in Una Sola Passata Senza Danni, Rivestimento in Ceramica e Olio Argan, Rapido Riscaldamento, 4 Temperature, Tappetino Termoresistente',
#         'Bellissima Imetec My Pro Revolution BHS4 1100, Spazzola ad Aria Calda, Asciuga e Modella, Rotazione Automatica a Doppio Verso, 2 Spazzole da 40 e 50 mm, Ionizzatore, 1000 W, Rivestimento in Ceramica',
#         'Bellissima Imetec Air Wonder, Spazzola ad Aria Calda, Tecnologia a ioni, Spazzole Rivestite in Ceramica e Cheratina, Asciuga, Dona Volume, 8 Accessori, 1000 W',
#         'Bellissima Imetec GH16 400 Spazzola ad Aria Modellante con 2 Accessori di Styling, Diametro 25 mm, Potenza 400 W, 2 Combinazioni Flusso dAria/Temperatura, Puntale Antiscottatura',
#         'Bellissima Imetec GT13 50 Arricciacapelli, Rivestimento in Ceramica, 7 Diversi Livelli di Temperatura da 150°C a 210°C, Diametro di 25 mm, Riscaldamento Rapido, Pronto allUso',
#         'Bellissima Imetec GT15 200 Ferro Arricciacapelli con Rivestimento in Ceramica, Forma Conica, Controllo della Temperatura',
#         'Bellissima Imetec GT15 100 Arricciacapelli, Ricci Brillanti e Definiti, Diametro 19 mm, Rivestimento in Ceramica, Riscaldamento Rapido, Pronto allUso',
#         'Bellissima Imetec Kit Bellezza Capelli con Asciugacapelli Professionale, Tecnologia Ceramica e Cheratina e Piastra Lisciante a Vapore, Rivestimento in Ceramica e Cheratina',
#
#
#         'Bellissima Imetec Diffon Ceramic & Argan Oil, Diffusore ad Aria Calda per Capelli Ricci, Tecnologia Ceramica e Olio di Argan, 2 Livelli Aria/Temperatura, Asciugatura Delicata, 700 W',
#
#
#         "Bellissima Twist&Style Arricciacapelli, Manico + 4 Accessori per Ricci e Onde, Temperatura Automatica, Riscaldamento Rapido, Rivestimento in Ceramica, Orientabile a 90°",
#
#         'Bellissima Imetec Diffon Ceramic & Argan Oil, Diffusore ad Aria Calda per Capelli Ricci, Tecnologia Ceramica e Olio di Argan, 2 Livelli Aria/Temperatura, Asciugatura Delicata, 700 W',
#
#         'Imetec Silent Power Comfort, Termoventilatore silenzioso e compatto, Stufetta Elettrica, 2100 W, 4 funzioni, termostato ambiente e di sicurezza, funzione antigelo, maniglia',
#         'Imetec Silent Power Eco, Termoventilatore Silenzioso, Stufetta Elettrica, Tecnologia ECO -35% Consumo Energetico, 2100 W, Funzione Antigelo, Temperatura Regolabile, 4 Funzioni, Termostato Ambiente',
#         'Imetec Silent Power Pure, Termoventilatore silenzioso con ionizzatore e timer di autospegnimento, Stufetta Elettrica, 2100 W, funzione antigelo, temperatura regolabile, 4 funzioni',
#         'Imetec Silent Power Protection, Termoventilatore silenzioso, 2100 W, Stufetta Elettrica, Timer spegnimento programmabile, temperatura regolabile, 4 funzioni, termostato ambiente, funzione antigelo',
#         'Imetec Compact Air, Termoventilatore piccolo e potente, Stufetta Elettrica, 2000 W, maniglia integrata, temperatura regolabile, funzione antigelo, dispositivo di protezione dai surriscaldamenti',
#         'Imetec Eco Ceramic, Termoventilatore, Stufetta elettrica, Tecnologia Ceramica, Basso Consumo Energetico, Silenzioso, 3 Livelli di Temperatura, Termostato Ambiente',
#         'Imetec Eco Rapid, Stufa Elettrica 2000 W, Tecnologia a Basso Consumo Energetico, Termoconvettore 4 Temperature, Termostato Ambiente, Silenzioso',
#         'Imetec Eco Ceramic Diffusion, Stufetta elettrica, Termoventilatore, Corpo Oscillante, Tecnologia Ceramica, Basso Consumo Energetico, 6 Funzioni di Temperatura, Timer',
#         'Imetec Dolcevita ES4 Bilancia Elettronica da Cucina, Pesa Solidi e Liquidi, Contenitore Estraibile, Scala Graduata, Portata 5 Kg, Divisione 1 g, Funzione Tara, Display Digitale, Spegnimento Automatico',
#         'Imetec Succovivo Pro 2000 Estrattore di Succo Professionale a Freddo, Spremitura Lenta 48 Giri/Min, 2 Filtri per Succhi, Accessorio per Granite e Sorbetti, Kit per Maschere Bellezza, con Ricettario',
#         'Imetec Crea&Crema Sbattitore Elettrico, Fruste Extralunghe per Impasti Dolci e Panna Montata, Ganci Acciaio Inox per Impastare, 5 Velocità, Funzione Turbo, Design Ergonomico, 500 W',
#         'Imetec Tosta&Griglia, Tostiera elettrica, piastre XL per preparare 3 toast alla volta, compatta, riponibile in verticale, 800 W, spie di funzionamento',
#         'Imetec TostaMaxi Tostapane, 2 Fessure Extralarge e Pinze Apribili per Toast Extrafarciti, 10 livelli di doratura, Timer con Autospegnimento, Cassetto Raccoglibriciole, 600 W',
#         'Imetec GranToast, tostapane per toast extra farciti, fessure XL, pinze a fondo chiuso apribili a 360° e regolabili in larghezza, 600 W, 10 livelli doratura, Timer',
#         'Imetec TritaCompact Tritatutto, Lame in Acciaio Inox, Capienza Contenitore 400 ml, Funzionamento a Pressione, Compatto, 350 W',
#         'Imetec Tritapiù Tritatutto, 4 Lame in Acciaio Inox, Capiente Contenitore 600 ml, 18.000 Giri/min, Funzionamento a Pressione, 1000 W, Materiali BPA Free',
#         'Imetec CakeLover, Impastatrice Planetaria per Dolci, Creme e Impasti Salati, Accessoriata, Compatta, 1500W, Fruste, Pale e Ganci, Contenitore Inox, 5 Litri, 6 Velocità, Ricettario',
#         'Imetec Griglia&Gusta Bistecchiera, Piastre Antiaderenti Removibili, Rivestimento Triplo Strato al Titanio Stone-Look, Grill con 7 Posizioni, 3 Funzioni di Cottura, Thermo Control, 2000 Watt',
#         'Imetec Succovivo SJ4 1300 Estrattore di Succo Professionale a Freddo, Spremitura Lenta 50 Giri/Min, Filtro per Succhi, Accessorio per Granite e Sorbetti, pulizia in 2 minuti',
#         'Imetec FriggiLeggero - Friggitrice ad aria multifunzione, 12 programmi, Riscaldamento rapido, 18 L, 1550 W, Comandi digitali, Cestello, Griglia, Leccarda, Ricettario, 40x36x34,5 cm',
#         'Imetec HB3 Frullatore a Immersione, Gambo Large Estraibile, Lame in Acciaio Inox, Funzionamento a Impulsi, 450 W',
#         'Imetec Frulla&Crea Frullatore a Immersione, 3 Accessori, Tritatutto, Frusta e Bicchiere BPA free, Piede Large, 2 Velocità, 1000 W, 15000 Giri Minuto',
#         'Imetec Frulla&Trita Frullatore a Immersione, Lame in Acciaio Inox, Piede Extra Large, Gambo Estraibile, Bicchiere graduato da 1 Litro, 800 Watt, 15.000 Giri Minuto',
#         'metec FrullaRapido, Frullatore a Immersione, Gambo XL in Acciaio, Lame in Acciaio Inox, Funzionamento a Impulsi, 450 W',
#         "Imetec FrullaRapido+, Frullatore a Immersione, Gambo XL in Acciaio, Lame in Acciaio Inox, Funzionamento a Impulsi, Bicchiere graduato da 700 ml BPA Free, 800 W"	,
#         'Imetec FrullaFacile, Frullatore a Immersione, Gambo XL, Lame in Acciaio Inox, Funzionamento a Impulsi, Bicchiere Graduato da 700 ml BPA Free, 800 W, Impugnatura Ergonomica',
#         'Imetec Personal e Sport Blender PB 100 Mini Frullatore con 2 Bottiglie Take-Away in Tritan e 4 Lame in Acciaio Inox, 22,000 giri/min',
#         'Imetec SM 1000 Soup Maker, Cuoce e Frulla, 3 Programmi Automatici, Vellutate, Zuppe e Frullati, 6 Porzioni, Lame Seghettate Acciaio Inox, con Ricettario, 900 W, 1.6 Litri',
#         'Imetec FrullaTutto Frullatore, Lame Tritaghiaccio in Acciaio Inox, Bicchiere da 800 ml in Vetro Antigraffio, 2 Velocità, Funzionamento Impulsi, 500 W, 2 Velocità, Lavabile Lavastoviglie',
#         'Imetec BuonFrullato, Frullatore Compatto, Bicchiere in Plastica BPA Free, Capacità 700 ml, Coperchio Dosa Liquidi, 400 W',
#         'Imetec Frulla&Gusta, Frullatore con 4 lame in acciaio inox, bicchiare in vetro, 2 velocità e funzionamento impulsi, 500 W, capienza 800 ml, lavabile lavastoviglie, piedini antiscivolo',
#         'Imetec La Grattugia, grattugia elettrica, Rullo in Acciaio Inox, Tramoggia in Alluminio, Contenitore Estraibile con Coperchio Salvafreschezza, Sistema di Sicurezza, 150 W',
#         'Imetec Grattugissima, grattuggia elettrica per formaggio, pane e frutta secca, Contenitore estraibile, coperchio salvafreschezza, 150 W, rullo in acciacio, accessori lavabili in lavastoviglie',
#         'Imetec PaneMio Macchina del Pane, Impasta Lievita e Cuoce, 12 Programmi, Cestello Pagnotta, Accessorio Panini, 3 Livelli di Doratura, Avvio Programmabile, 550 W, Ricettario',
#         'Imetec Zero-Glu Pro, Macchina per Pane, Ciabatte e Panini Senza Glutine per Celiaci, Pasta Pizza, Dolci, Marmellate, 20 Programmi, 2 Pale Impastatrici, Temperatura levitazione controllata, Ricettario',
#         'Imetec La Panetteria +Zeroglu, Macchina per Pane, Ciabatte, Panini, Dolci con Farine Naturali e Senza Glutine, 20 Programmi, 2 Pale Impastatrici, Temperatura di Lievitazione Controllata',
#         'Imetec Cukò Maestro, Robot da cucina con cottura, 20 programmi, 10 funzioni, Impasta Pane e Pizza, capienza 2 L, Fino a 6 porzioni, 8 accessori, ricettario, 1000 W',
#         'Imetec La TostaGriglia, tostiera, Piastre XL Rigate e Antiaderenti, Interruttore ON/OFF, Spia Riscaldamento Piastre, Gancio Chiusura, Avvolgicavo, Tostapane Compatto, 900 W',
#         'Imetec SP 100 Spiralizer Elettrico, Tagliaverdure a Spirale, Affetta Verdure in 3 Forme Spaghetti, Tagliatelle, Pappardelle, Lame in Acciaio Inox, Contenitore BPA Free 500 ml, Sistema di Sicurezza',
#         'Imetec TritaMax, Tritatutto, 4 Lame in Acciaio Inox, Capiente Contenitore 600 ml, Accessorio Frusta per Salse e Panna Montata, 2 velocità, 500 Watt',
#         'Imetec FrescoAroma Macina Caffè e Spezie, Pepe, Zucchero, Frutta Secca Elettrico con Lame in Acciaio Inox, Contenitore Salva-Freschezza, Funzionamento a Impulsi, 150 W',
#         'Bellissima Imetec Kit Testine di Ricambio Face Cleansing per Pelli Normali e Pelli Sensibili, 4 Diverse Testine per Detergere, Massaggiare, Levigare e Idratare La Pelle',
#         'Bellissima Imetec Face Cleansing Spazzola Per Pulizia Viso, Deterge ed esfolia la pelle, Kit con 5 testine intercambiabili, Tecnologia a vibrazione sonica, Impermeabile, Beauty Bag inclusa
# '
#     )
#
#
#
#
#
#
#
#
#
#
#
# part_2 <-
#     c(
#         'Imetec Scaldasonno Sensitive Singolo, si Adatta ai Cambi di Temperatura, Morbido Peluche, Coprimaterasso Maxi 190 x 90, Electro Block, 6 Temperature, Timer, Risparmio Energetico, Lavabile',
#
#         'Imetec Adapto Velvet Square plaid riscaldabile, coperta elettrica 150x95 cm tessuto morbido, Tecnologia Adapto, Dispositivo di sicurezza, rapido riscaldamento, 6 temperature, lavabile in lavatrice',
#
#
#         'Imetec Adapto Velvet Jacquard plaid riscaldabile, coperta elettrica 140x180 cm tessuto morbido, Tecnologia Adapto, Dispositivo di sicurezza, rapido riscaldamento, 6 temperature, lavabile in lavatrice',
#
#
#         'Imetec Adapto Velvet Tartan plaid riscaldabile con tasca mani e piedi, coperta elettrica 150x110 cm, Tecnologia Adapto, Dispositivo di sicurezza, rapido riscaldamento, 6 temperature, lavabile',
#
#
#         'Imetec Adapto Velvet Tartan plaid riscaldabile, coperta elettrica 160x120 cm tessuto morbido, Tecnologia Adapto, Dispositivo di sicurezza, rapido riscaldamento, 6 temperature, lavabile in lavatrice'
#
#
#
#
#
#
#
#     )
#
#
#
#
#
#
#
#
#
#
#
# part_3 <-
#     c(
#         'Imetec Scaldaletto Singolo, 150 x 80 cm, Comando 2 Temperature, Tessuto Trapuntato, Lavabile a Mano e in Lavatrice a 40° C',
#         'Imetec Scaldaletto Matrimoniale, 150 x 140 cm, 2 Comandi con 2 Temperature, Tessuto Trapuntato, Lavabile a Mano e in Lavatrice a 40° C',
#
#         'Imetec Scaldasonno Adapto Singolo 150 x 80 Cm, Tecnologia Brevettata, Riscaldamento Rapido, Temperatura Costante e Personalizzata, 100% Cotone, Comando con 6 Temperature',
#
#
#         'Imetec Scaldasonno Adapto Matrimoniale 150 x 160 cm, Tecnologia Brevettata, Riscaldamento Rapido, Temperatura Costante e Personalizzata, 100% Cotone, 2 Comandi con 6 Temperature
# ',
#
# 'Imetec Scaldasonno Adapto Piazza e Mezza 150 X 120 cm, Tecnologia Brevettata, Riscaldamento Rapido, Temperatura Costante e Personalizzata, 100 % Cotone, Comando con 6 Temperature',
#
#
# 'Imetec Scaldasonno Adapto Singolo 150 x 80 Cm, Riscaldamento Rapido, Temperatura Costante e Personalizzata, 100% Lana e Merino, tessuto antiscivolo, Comando con 6 Temperature
# ',
#
#
# 'Imetec Scaldasonno Adapto Matrimoniale 150 x 160 cm, Riscaldamento rapido, Temperatura costante e personalizzata, 100% lana e merino, tessuto antiscivolo, 2 comandi separati, 6 Temperature',
#
#
#
# 'Imetec Scaldasonno Adapto Singolo 150 x 80 cm, Riscaldamento Rapido, Temperatura Costante e Personalizzata, Tessuto Trapuntato Anallergico, Comando con 6 Temperature
# '
#
#     )
#
#
#
#
#
#
#
#
#
#
# part_4 <-
#     c(
#         "Imetec Scaldasonno Adapto Matrimoniale, 150x160, Riscaldamento Rapido, Temperatura Costante e Personalizzata, Tessuto Trapuntato Anallergico, Comando con 6 Temperature",
#
#
#
#         'Imetec Scaldasonno Adapto Maxi Coprimaterasso Singolo 195x90 cm, Riscaldamento Rapido, Temperatura Costante e Personalizzata, Tessuto Anallergico Trapuntato, Comando con 6 Temperature',
#
#
#
#         'Imetec Scaldasonno Adapto Maxi Coprimaterasso Matrimoniale 195x165 cm, Riscaldamento Rapido, Temperatura Costante e Personalizzata, Tessuto Anallergico Trapuntato, Doppio Comando, 6 Temperature',
#
#
#
#         'Imetec Scaldasonno Adapto Maxi Coprimaterasso Matrimoniale 195 x 165 cm, Riscaldamento Rapido, Temperatura Costante e Personalizzata, 100% Lana e Merino, Doppio Comando, 6 Temperature',
#
#
#
#         'Imetec Scaldasonno Singolo 150x80 cm, Basso Consumo, 50% Lana e Merino, 2 Temperature, Dispositivo Sicurezza Electro Block, Made in Italy',
#
#
#         'Imetec Scaldasonno Matrimoniale 150x160 cm, 50% Lana e Merino, 2 Temperature, Dispositivo Sicurezza Electro Block, Made in Italy',
#
#
#         " Imetec Scaldasonno Adapto Maxi Coprimaterasso Matrimoniale 195x165 cm, Riscaldamento Rapido, Temperatura Costante e Personalizzata, 100% cotone percalle trapuntato, Doppio Comando, 6 Temperature"
#         ,
#
#
#         "Imetec BW03 Boule Elettrica Cordless, Tecnologia Ceramica, Riscaldamento Ultrarapido, Termostato di Sicurezza, Spia di Ricarica"
#         ,
#
#
#         'no Italia'
#     )
#
#
#
#
#
#
#
#
#
#
# part_5 <-
#     c(
#         'Imetec Intellisense Comfort, Termoforo Multiuso, Cuscino Termico, per schiena e addome, Tasca per Mani, Rapido Riscaldamento, Tessuto Anallergico, 5 temperature, Electro Block di sicurezza, Lavabile
# ',
#
#
#
#
# 'Imetec Intellisense Comfort, Termoforo Multiuso, Cuscino Termico, 40x35 cm, Tessuto Anallergico, 5 temperature, Electro Block di sicurezza, Lavabile, grigio, 40x35 cm
# ',
#
#
# 'Imetec Intellisense XL, Termoforo Extra Large Schiena e Gambe, Cuscino termico, 38x50 cm, Tessuto micro Peluche, 5 Temperature, Electro Block di sicurezza, Riscaldamento Rapido, lavabile in lavatrice
# ',
#
#
# 'Imetec Intellisense Cervical, Termoforo per Cervicale e Spalle, Cuscino Termico, 47x52 cm, Tessuto Anallergico, 5 Temperature, Electro Block di Sicurezza, Riscaldamento Rapido, Lavabile in Lavatrice
# ',
#
#
# 'Imetec Scaldapiedi, Tecnologia Intellisense, Tessuto Morbido, Comando Digitale a 5 Temperature, Autospegnimento, Dispositivo di Sicurezza Electro Block, Rivestimento Lavabile in Lavatrice, 52x54 cm'
#
# ,
# 'Imetec Compact ES1 100 Bilancia pesapersone elettronica compatta, Design ultrasottile, Ampio LCD display, Portata Max 150 kg
# ',
#
#
# 'Imetec Monitoring ES9 300 Bilancia Pesapersone Elettronica, Monitoraggio Trend Grafico Peso, 4 Utenti, fino a 180 Kg, LCD Display, Vetro Temperato
# ',
#
# 'Imetec Pro, Bilancia pesapersone meccanica, Rileva il peso in modo chiaro e affidabile, Pedana antiscivolo, Portata max 150 kg, Divisione 1 kg, Funzionamento analogico',
#
# 'Imetec Precision ES13 200 Bilancia pesapersone elettronica, rivela anche le minime variazioni di peso, fino a 180 Kg, LCD Display, vetro temperato, batterie incluse
# ',
# 'Imetec Body Analizer ES7 400 Bilancia Pesapersone Diagnostica, Misura Acqua Corporea, Massa Muscolare e Grassa
# ',
# 'Imetec Nuvola Ferro da Stiro da Viaggio a Vapore, Piastra in Acciaio Inossidabile da 1000 W, Custodia da Viaggio, Doppio Voltaggio',
#
#
# 'Imetec ZeroCalc Z1 2500 Ferro da Stiro, Tecnologia Anticalcare, Piastra Inox 2200 W, Colpo Vapore 120 g
# ',
#
#
#
# 'Imetec Zerocalc Z3 3700 Ferro da Stiro, Tecnologia Anticalcare, Piastra Ceramica ad Alta Scorrevolezza, 2400W, Colpo Vapore 160 G, 0,3 litri, Dispositivo Antigoccia Tecnologia a Risparmio Energetico',
#
#
# 'Imetec ZeroCalc Z3 3900 Ferro da Stiro a Vapore, Tecnologia Anticalcare, Tecnologia Intellivapor (temperatura unica ideale per tutti i tessuti), Piastra Ceramica, 2400 W, Colpo Vapore 170 g',
#
#
# 'Imetec Zerocalc Z3 3500 Ferro Da Stiro, Tecnologia Anticalcare, Piastra Inox 2400 W, Colpo Vapore 150 g
# ',
#
#
# 'Imetec Eco Perfect Ferro da Stiro a Vapore, Risultati Ottimi con -35% di Consumo di Acqua e -25% di Consumo Energetico, Piastra con Rivestimento Pro Ceramic, Tripla Protezione Anticalcare, 2400 W
# ',
#
#
# 'Imetec Onda F1, Ferro da Stiro a Vapore con Piastra a Onde Scorrevole Brevettata, 2400 W, Colpo di Vapore 200g, Serbatoio 300 ml, Tripla Protezione Anticalcare
# '
#
#
#     )
#
#
#
#
#
#
# part_6 <- c(
#     "Imetec Intellifast, ferro da stiro a vapore, Tecnologia Intelli System, Impostazione automatica della temperatura, Piastra Ceramic Diamond, Protezione anticalcare, 2400 W, Colpo Vapore 200 g",
#
#     "Imetec Activation Ferro da Stiro a Vapore, Piastra Scorrevole in Ceramica e Tormalina, Colpo Vapore 200 g, Tecnologia Anticalcare ZeroCalc, Antigoccia, Eco, 2400 W"
# )
#
# part_7 <- c(
#     "Imetec ZeroCalc KF1 100 Kit 3 filtri anticalcare per Ferri da Stiro Imetec ZeroCalc Pro Ceramic PS2 2400, PS2 2000, PS2 2200, Intellivapor PS3 3000, Imetec Onda, Ricambi Originali",
#
#     "Imetec Rapidvapor Ferro da Stiro Generatore di Vapore, Piastra Fluid Ceramic a Rapido Riscaldamento, Leggero, Sistema di Pulizia Calc Clean, Colpo Vapore 240 g, 2400 W, 5 Bar Pump"
# )
#
#
# part_8 <- c(
#     "Imetec Onda F2, Ferro da Stiro a Vapore con Piastra a Onde Scorrevole Brevettata, 2400 W, Colpo di Vapore 200g, Serbatoio 300 ml, Tripla Protezione Anticalcare",
#     "Imetec Onda P2 Ferro da Stiro Generatore di Vapore, Piastra a Onde Scorrevole Brevettata, 2400W, Colpo Vapore 300 g, Serbatoio 2,5L, 3 Filtri con Ioni di Argento, Tecnologia No Stop Vapor"
# )
#
#
# part_9 <-
#     c(
#         "Imetec ZeroCalc Z1 2800 Ferro da Stiro con Tecnologia Anticalcare, Piastra Ceramica ad Alta Scorrevolezza, Tecnologia a Risparmio Energetico, 2200 W, Colpo Vapore 130 g",
#         "Imetec ZeroCalc PS1 2000 Ferro da Stiro Compatto, 3.8 BAR, Tecnologia Anticalcare e a Risparmio Energetico, Ricarica Continua, Vapore Pronto in 1 Minuto, 2100 W, Serbatoio XL da 1 L"
#     )
#
#
# part_10 <-
#     c(
#         "Imetec Adapto Elegance, Plaid riscaldabile, Coperta elettrica, 180x140 cm, tessuto morbido Velvet Double Face, Risparmio energetico, Tecnologia adapto, Rapido riscaldamento, 6 temperature, Lavabile",
#         "Imetec Adapto Grand Luxe, Plaid riscaldabile, Coperta elettrica, 180x140 cm, tessuto morbido Peluche Sherpa, Risparmio energetico, Tecnologia Adapto, Rapido riscaldamento, 6 temperature, Lavabile",
#         "Bellissima Imetec Ceramic & Argan Oil 6 in 1, Spazzola ad Aria Calda, 8 Accessori, Tecnologia a Ioni, Spazzole Rivestite in Ceramica e Olio di Argan, Asciuga e dona Volume, Colpo aria fredda, 1000 W",
#         "Bellissima Imetec Steam Ceramic & Argan Oil, Piastra lisciante a Vapore, Rivestimento in Ceramica e Olio di Argan, 3 Livelli di Temperatura, Riscaldamento Rapido, Piastre Oscillanti, Autospegnimento"
#
#
#     )





master_data_sheet <-
  data.frame(master_asin, master_sku, master_titles_correct)











date <- Sys.time()

scrape_raw <-
  purrr::map(master_asin, purrr::possibly(polite_checker_1, NA))

product_name_df_delay_ua <- plyr::ldply(scrape_raw, data.frame)


write.csv(product_name_df_delay_ua,
          file = paste0("polite_data/Amazon_name_", date, ".csv"))


## MERGING THE LIVE VERSION AND THE LOCAL HARDCORDED VERSION


##rename the columns in order to make possible the left join
colnames(product_name_df_delay_ua)[1] <- "master_asin"

##left join
results <-
  merge(x = product_name_df_delay_ua,
        y = master_data_sheet,
        by = "master_asin",
        all.x = TRUE)

##normalizing the live product titles removing accents, trimming whitespaces and make all the strings lowercase
results$title_lower <- gsub("'", "", results$title_lower)

results$title_lower <- tolower(trimws(results$title_lower))


##performing the same steps for the hardcorded data
results$master_titles_correct <-
  gsub("'", "", results$master_titles_correct)

results$master_titles_correct <-
  tolower(trimws(results$master_titles_correct))

#generate the column with the test if the live version and the hardcorded version are the same or they need a fix
results$test_names <-
  ifelse(results$title_lower == results$master_titles_correct,
         "TRUE",
         "NEED FIX")

##reordering df

#results <- results[, c(1, 2, 3, 4, 11, 12)]

#results <- na.omit(results)

results$master_title_correct_length <-
  nchar(results$master_titles_correct)

results$chr_len_dynamic <- nchar(results$title_lower)

results$test_chr_length <-
  ifelse(results$master_title_correct_length == results$chr_len_dynamic,
         "TRUE",
         "NEED FIX")

results$test_asin <-
  ifelse(results$master_asin == results$asin_2, "TRUE", "NEED FIX")

results$test_asin_logical <- as.logical(results$test_asin)

results$test_chr_length_logical <-
  as.logical(results$test_chr_length)

results$test_names_logical <- as.logical(results$test_names)

results <- results[, c(9, 2, 1, 14,   3, 4, 10, 12, 11, 13)]


#results <- na.omit(results)

##Renaming the Columns

colnames(results)[1] <- "SKU PRODOTTO"
colnames(results)[2] <- "ASIN LIVE AMAZON"
colnames(results)[3] <- "ASIN FORNITO TENACTA"
colnames(results)[4] <-
  "L'ASIN LIVE SU AMAZON è UGUALE ALL'ASIN FORNITO TENACTA?"
colnames(results)[5] <- "NOME PRODOTTO LIVE AMAZON"
colnames(results)[6] <-
  "LUNGHEZZA NOME PRODOTTO LIVE SU AMAZON"
colnames(results)[7] <- "NOME PRODOTTO UFFICIALE FORNITO DA TENACTA"
colnames(results)[8] <- "LUNGHEZZA NOME PRODOTTO FORNITO DA TENACTA"
colnames(results)[9] <-
  "IL NOME PRODOTTO LIVE AMAZON è UGUALE AL NOME PRODOTTO UFFICIALE TENACTA?"
colnames(results)[10] <-
  "LA LUNGHEZZA DEL NOME PRODOTTO LIVE AMAZON è UGUALE ALLA LUNGHEZZA DEL NOME UFFICIALE TENACTA?"

##FINAL CHECK
results$results <-
  ifelse(
    results$`L'ASIN LIVE SU AMAZON è UGUALE ALL'ASIN FORNITO TENACTA?` == "TRUE" &
      results$`LA LUNGHEZZA DEL NOME PRODOTTO LIVE AMAZON è UGUALE ALLA LUNGHEZZA DEL NOME UFFICIALE TENACTA?` == "TRUE" &
      results$`IL NOME PRODOTTO LIVE AMAZON è UGUALE AL NOME PRODOTTO UFFICIALE TENACTA?` == "TRUE",
    "TRUE",
    "NEED FIX"
  )

colnames(results)[11] <- "QUESTO ASIN RICHIEDE UNA CORREZIONE"

#results$LINK <-
#paste0("https://www.amazon.it/dp/", results$`ASIN FORNITO TENACTA`)

#test_duplicates <- results[!duplicated(results$master_asin), ]

duplicates_row <- results[duplicated(results$`ASIN LIVE AMAZON`),]

result_raw <- results


final_results <-
  results %>% filter(results$`QUESTO ASIN RICHIEDE UNA CORREZIONE` == "NEED FIX")



n_row_results <- as.numeric(nrow(results))

n_row_final_results <- as.numeric(nrow(final_results))




tbl_html <-
  final_results %>% gt() %>% as_raw_html()





##Add the temp file to attach with the mail

#write.csv(final_results, paste0("Titoli_da_Correggere", format(date, "%d-%b-%Y %H.%M"), ".csv"))
write.csv(final_results, "titoli_da_correggere.csv")
file_path <- "titoli_da_corregere.csv"
#file_path <- paste0("Titoli_da_Correggere", format(date, "%d-%b-%Y %H.%M"), ".csv")



#############################
#############################
#############################Blastula / GMAIL


# read environment variables ----------------------------------------------
EMAIL_SENDER <- Sys.getenv("EMAIL_SENDER")
EMAIL_PASSWORD <- Sys.getenv("EMAIL_PASSWORD")
EMAIL_RECIPIENT <- Sys.getenv("EMAIL_RECIPIENT")
EMAIL_CC_1 <- Sys.getenv("EMAIL_CC_1")
EMAIL_CC_2  <- Sys.getenv("EMAIL_CC_2")
EMAIL_CC_3 <- Sys.getenv("EMAIL_CC_3")

# set gmail credentials ---------------------------------------------------
credentials <- creds_envvar(
  user = EMAIL_SENDER,
  pass_envvar = "EMAIL_PASSWORD",
  provider = "gmail",
  host = NULL,
  port = NULL,
  use_ssl = TRUE
)

# compose email -----------------------------------------------------------

if (n_row_final_results == 0) {
    write.csv(
        as.data.frame(n_row_final_results),
        file = paste0("logs/Amazon_name_log_corretti", date, ".csv")
    )
    
} else{
    email <-
        compose_email(body = blocks(tbl_html),
                      footer = "Questo è un elenco generato automaticamente degli ASIN che richiedono una verifica del Product Title, si ricorda che questo sistema non assicura il 100% di sicurezza ed affidabilità, si richiede quindi un controllo manuale periodico che effettivamente gli ASIN ed i loro Product Titles siano corretti. Per Ulteriori informazioni contattare lo sviluppatore.",
                      title = "AMAZON ITALIA: TITOLI PRODOTTO CHE RICHIEDONO UNA VERIFICA E CORREZIONE")
    
    email <-
        email %>% add_attachment(
            file = file_path
             )
}



# email <- compose_email(
#   body = blocks(tbl_html),
#   footer = "This is a Scheduled Workflow to notify about the Amazon's Product Names that need manual intervention.
#   Contact me for any support",
#   title = "This is a dataframe with all the ASINs provided for the monitoring"
# )
#


# send email --------------------------------------------------------------
smtp_send(
  email,
  from = EMAIL_SENDER,
  to = EMAIL_RECIPIENT,
  cc = c(EMAIL_CC_1, EMAIL_CC_2, EMAIL_CC_3),
  subject = "REPORT PERIODICO AMAZON ITALIA > VERIFICA TITOLI PRODOTTI",
  credentials = credentials,
  verbose = TRUE
)

write.csv(results,
          file = paste0("polite_data/Amazon_name_results_logs_", date, ".csv"))
