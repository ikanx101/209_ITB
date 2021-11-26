rm(list=ls())
library(dplyr)

options(scipen = 99)

a1 = sample(1:10^2,10) 
a2 = sample(10^2:10^4,10)
a3 = sample(10^4:10^6,10)
a4 = sample(10^6:10^8,20)
a5 = sample(10^8:10^9,5)

a = c(1,a1,a2,a3,a4,a5,10^9) %>% sort() %>% unique()

a
urut = paste(a,collapse = ",")
piton = paste0("[",urut,"]")
piton

save(piton,file = "n.rda")