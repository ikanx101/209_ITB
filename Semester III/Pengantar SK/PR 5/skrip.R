rm(list=ls())

library(dplyr)
library(ggplot2)
library(gganimate)

# misal ada 3 benda
n_benda = 3

# parameter yang masuk
m = sample(100,n_benda) # sebagai massa dari n body

# posisi benda
x = sample(10,n_benda) # sebagai posisi di sumbu x
y = sample(10,n_benda) # sebagai posisi di sumbu y
pos = data.frame(id = 1:n_benda,x,y,
                 iter = 1)

# konstanta gravitasi
G = 6.67 * 10^(-11)

# hitung waktu dan banyak iterasi
t = 10
dt = .5
iter_max = t/dt

# initial velocity dan acceleration
v = rep(0,n_benda)
a = rep(0,n_benda)

# kita mulai
i = 1

# update kecepatan half kick
v = v + a * dt/2

# ambil data frame yang dibutuhkan
temp_pos = pos %>% filter(iter == i)

# update ppsisi sementara
temp_pos$x = temp_pos$x * dt
temp_pos$y = temp_pos$y * dt

# kita update acceleration
for(j in 1:n_benda){
  for(k in 1:n_benda){
    
  }
}


