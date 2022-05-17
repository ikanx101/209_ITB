setwd("/cloud/project/Semester II/Research Method/UAS")

# sucikan hati dan diri
rm(list=ls())

# libraries
library(dplyr)
library(tidyr)
library(readxl)

# import data
df = 
  read_excel("20201207 Product list.xlsx") %>% 
  janitor::clean_names() %>% 
  select(product_code,brand,burn_3m,cost_benefit) %>% 
  filter(cost_benefit > 0)
df$id = 1:nrow(df)

partisi = sample(nrow(df),1000)

df1 = df %>% filter(id %in% partisi[1:100]) %>% arrange(id) %>% select(-id)
df2 = df %>% filter(id %in% partisi[101:200]) %>% arrange(id) %>% select(-id)
df3 = df %>% filter(id %in% partisi[201:300]) %>% arrange(id) %>% select(-id)
df4 = df %>% filter(id %in% partisi[301:400]) %>% arrange(id) %>% select(-id)
df5 = df %>% filter(id %in% partisi[401:500]) %>% arrange(id) %>% select(-id)
df6 = df %>% filter(id %in% partisi[501:600]) %>% arrange(id) %>% select(-id)
df7 = df %>% filter(id %in% partisi[601:700]) %>% arrange(id) %>% select(-id)
df8 = df %>% filter(id %in% partisi[701:800]) %>% arrange(id) %>% select(-id)
df9 = df %>% filter(id %in% partisi[801:900]) %>% arrange(id) %>% select(-id)
df10 = df %>% filter(id %in% partisi[901:1000]) %>% arrange(id) %>% select(-id)

save(df1,df2,df3,df4,df5,
     df6,df7,df8,df9,df10,
     file = "bahan.rda")
