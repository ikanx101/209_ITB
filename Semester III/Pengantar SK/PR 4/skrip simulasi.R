# mulai dari hati yang bersih
rm(list = ls())

# panggil libraries
library(dplyr)
library(ggplot2)

# gambar lingkaran
xc = 1
yc = 1
r = 1
ggplot() + annotate("path",
                    x=xc+r*cos(seq(0,2*pi,length.out=100)),
                    y=yc+r*sin(seq(0,2*pi,length.out=100)))


# sekarang kita hitung dengan simulasi montecarlo
# kita tentukan terlebih dahulu berapa banyak simulasinya
# ini fungsi utk menghitungnya
# kita pakai metode jatuh yah
set.seed(10104074)
hitung_pi = function(n){
  x = runif(n)
  y = runif(n)
  data = data.frame(x,y)
  data =
    data %>%
    mutate(jatuh = x^2 + y^2,
           ket = ifelse(jatuh <= 1, 1,0))
  return(4 * sum(data$ket)/n)
}

# saatnya simulasi
simulasi = data.frame(
  n = c(10,20,50,75,100,400,600,999,1100,2000,
        3000,5000,7500,10000,20000,30000,500000,
        1000000,2000000,4000000),
  pi = 0
)

simulasi$pi = sapply(simulasi$n,hitung_pi)
simulasi$beda = pi - simulasi$pi
simulasi


simulasi %>%
  ggplot(aes(x = n,
             y = pi)) +
  geom_line(color = "darkred") +
  labs(title = "Berapa nilai Pi?\nDihitung dengan pendekatan luas lingkaran yang memiliki radius = 1",
       subtitle = "Perhitungan menggunakan simulasi Monte Carlo",
       caption = "Simulasi dan visualisasi\nmenggunakan R\nikanx101.com",
       x = "Banyak Simulasi",
       y = "Nilai aproksimasi") +
  annotate("text",
           x = 500000, 
           y = 3.22, 
           label = paste0("Pi ~ ",simulasi$pi[18],
                          "\nselisih dengan Pi aslinya = ",round(simulasi$beda[18],7))
  )

# ======================
# Ini bagian awal
# rm(list=ls())

# beda pi dengan 22/7
pi - (22/7)

# beda pi dengan 3.14
pi - 3.14

# pendekatan saya jauh lebih akurat dibandingkan 22/7 dan 3.14

# bikin fungsi utk generate data
# lalu kita rbind
generate_sim = function(n){
  data = data.frame(
    x = runif(n),
    y = runif(n),
    iterasi = n
  ) %>%
    mutate(penanda = ifelse(x^2 + y^2 <= 1, 1, 0)) %>% distinct()
  return(data)
}

# sekarang kita bikin datasetnya
n_sim = c(10,20,50,100,400,1000,5000)

# kita mulai iterasinya
i = 1
data_sim = generate_sim(n_sim[i])

for(i in 2:length(n_sim)){
  temp = generate_sim(n_sim[i])
  data_sim = rbind(data_sim,temp)
}

# yuk kita bikin dulu titiknya
data_sim %>%
  ggplot(aes(x = x, y = y)) +
  geom_point(aes(color = penanda)) +
  facet_wrap(~iterasi) +
  theme_minimal() +
  labs(x = "x",
       y = "y",
       title = "Proses Menghitung L1\nDengan cara generating random dots",
       subtitle = "Berapa banyak random dots yang dibuat:") +
  theme(legend.position = "none")