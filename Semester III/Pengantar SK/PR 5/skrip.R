# ======================================================
# bebersih global environment
rm(list=ls())
# libraries yang dibutuhkan
library(dplyr)
library(tidyr)
library(ggplot2)

# ======================================================
# banyak benda
n_benda = 3

# kita generate posisi masing-masing benda
r_1 = sample(100,2,replace = T) 
r_2 = sample(100,2,replace = T) 
r_3 = sample(100,2,replace = T) 

r_1
r_2
r_3

# ======================================================
# bikin grafik posisi awal
x = c(r_1[1],r_2[1],r_3[1])
y = c(r_1[2],r_2[2],r_3[2])

data.frame(x,y) %>%
  ggplot(aes(x = x,y = y)) +
  geom_point() +
  theme_void()

