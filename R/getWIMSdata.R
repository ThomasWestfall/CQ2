

# install.packages("rvest")
# library(rvest)
# library(dplyr)
# library(stringr)
#
# # retrieving the target web page
# url = "https://data.water.vic.gov.au/WMIS/#/overview/stations/topic/Surface%20Water/site/BET%20BET%20CREEK%20%40%20BET%20BET/graph?ww-station-table-_hiddenColumns=%5B%22cma_decode%22%2C%22monitortype%22%2C%22telem%22%2C%22zone%22%2C%22grdatum_decode%22%2C%22station_latitude%22%2C%22station_longitude%22%2C%22lldatum_decode%22%2C%22commence_decode%22%2C%22cease_decode%22%5D&ww-station-table-sort=%5B%7B%22field%22%3A%22station_name%22%2C%22ascending%22%3Atrue%7D%5D&wmis-station-map-zoom=%7B%22center%22%3A%5B16043668.809444923%2C-4370675.223601683%5D%2C%22zoom%22%3A8%7D&graphPeriod=01%2F11%2F2014&graphPeriodIndex=14&graphPeriod=01%2F11%2F2024&search=407211&graph=%5B%22407211%2Csalinityaselectricalconductivityec%22%5D"
# document <- read_html(url)
#
# summaries_xpath <- document %>%
#   html_elements(xpath = '//*[@id="lv_407211_salinityaselectricalconductivityec"]/div[3]')
#
#
# res <- str_match(document, 'new Dygraph\\(document.getElementById\\(\"container\\"\\),\\s*(.*?)\\s*, \\{labels')
# res[,2]
#
# head(summaries_xpath)
#
# head(summaries_xpath)
#
# html_products <- document %>% html_elements(data = "?lit$904474124$")
#
#
# link <- "https://en.wikipedia.org/wiki/List_of_Formula_One_drivers"
# page <- read_html(link)
# drivers_F1 <- html_element(page, "table.sortable") %>%
#   html_table()

