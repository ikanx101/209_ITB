library(rvest)
library(dplyr)
library(tidyr)
library(reshape2)

rm(list=ls())

# urls
urls = paste0("https://www.carmudi.co.id/en/used-cars-for-sale/indonesia?page_number=",1:5,"&page_size=50")

# extractorizer
extractorizer = function(url){
  data = 
    url %>% 
    read_html() %>% 
    {tibble(
      nama = html_nodes(.,".js-ellipsize-text") %>% html_text(trim = T),
      harga = html_nodes(.,".delta.weight--bold") %>% html_text(trim = T),
      detail = html_nodes(.,".soft-half--sides.milli") %>% html_text(trim = T)
    )}
  data = 
    data %>% 
    mutate(harga = gsub("Rp ","",harga),
           harga = gsub("\\.","",harga),
           harga = gsub(" ","",harga),
           harga = as.numeric(harga)) %>% 
    mutate(tahun = substr(nama,1,4),
           tahun = as.numeric(tahun)) %>% 
    separate(detail,
             into = c("jarak","transmisi","kota","dummy1","dummy2"),
             sep = "\n\n") %>% 
    select(-dummy1,-dummy2) %>%
    rowwise() %>% 
    mutate(nama = gsub(tahun,"",nama,ignore.case = T),
         nama = trimws(nama)) %>% 
    ungroup()
  return(data)
}

data_all = data.frame()

for (i in 1:length(urls)) {
  temp = extractorizer(urls[i])
  data_all = rbind(temp,data_all)
  print(i)
}


save(data_all,file = "data_carmudi.rda")
