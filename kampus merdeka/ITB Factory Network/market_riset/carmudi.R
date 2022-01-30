library(rvest)
library(dplyr)

rm(list=ls())

url = "https://www.carmudi.co.id/en/used-cars-for-sale/indonesia"

extractorizer = function(url){
  links = url %>% read_html() %>% html_nodes("a") %>% html_attr("href")
  df = 
    data.frame(id = 1:length(links),links = links) %>% 
    filter(grepl("/for-sale/",links,ignore.case = T))
  return(df)
}

extractorizer(url)