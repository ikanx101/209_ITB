rm(list=ls())

library(dplyr)
library(tidyr)
library(readxl)

path = "data_thesis.xlsx"
sh = excel_sheets(path)

sh

# =============================================================================
df_1 = read_excel(path,sheet = "df1")
df_2 = read_excel(path,sheet = "df2")
df_3 = read_excel(path,sheet = "df3")

colnames(df_2)[3:8] = paste0("Gula_",1:6)

for(i in 1:nrow(df_3)){
  persen = runif(4,0,1)
  persen = persen / sum(persen) 
  persen = round(persen,1)
  persen2 = runif(4,0,1)
  persen2 = persen2 / sum(persen2) 
  persen2 = round(persen2,1)
  persen2 = persen2[1:2]
  
  df_3$w1[i] = df_3$kebutuhan_gula[i] * persen2[1]
  df_3$w2[i] = df_3$kebutuhan_gula[i] * persen2[2]
  
  df_3$w3[i] = df_3$kebutuhan_gula[i] * persen[1]
  df_3$w4[i] = df_3$kebutuhan_gula[i] * persen[2]
  df_3$w5[i] = df_3$kebutuhan_gula[i] * persen[3]
  df_3$w6[i] = df_3$kebutuhan_gula[i] * persen[4]
  
}

df_3 = 
  df_3 %>% 
  rename(kebutuhan_gula_w3_w6 = kebutuhan_gula)

save(df_1,df_2,df_3,
     file = "raw.rda")

