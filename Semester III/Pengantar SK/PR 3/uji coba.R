rm(list=ls())

library(dplyr)
library(ggplot2)


path = list.files(pattern = "*.dat")

for(i in 1:length(path)){
  df = read.table(path[i]) %>% rename(x = V1,y = V2)
  
  plt = 
    df %>% 
    ggplot(aes(x,y)) +
    geom_point()
  
  ggsave(plt,filename = paste0(path[i],".png"))
  
}
