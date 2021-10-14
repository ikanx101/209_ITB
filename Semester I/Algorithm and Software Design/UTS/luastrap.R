rm(list=ls())

# library
library(dplyr)

# pendefinisian function
luas_trap = function(x0, # titik awal
                     xn, # titik akhir
                     n,  # banyak selang
                     f){ # fungsi y = f(x)
  # menghitung delta x
  h = (xn - x0) / n
  # menghitung f di x0
  f0 = f(x0)
  # selang pertama
  i = 1
  k = x0 + i*h
  fn = f(k)
  integration = (f0+fn)/2
  # iterasi untuk selang berikutnya hingga selesai
  for(i in 2:n){
    f0 = fn
    k = x0 + i*h
    fn = f(k)
    temp = (f0+fn)/2
    integration = integration + temp
  }
  # menghitung hampiran luas
  integration = integration * h
  return(integration)
}

# definisi fungsi dari soal
g = function(x){
  isi = 8 - (((x+1)^2)/2)
  1 + sqrt(isi)
}

# kita coba dengan berbagai macam N
N = c(10,50,100,200,1000,2500,5000,100000,250000,500000,750000,1000000)
Luas = c()
for(i in 1:length(N)){
  Luas[i] = luas_trap(0,
                      sqrt(14)-1,
                      N[i],
                      g)
}

# hasil akhir
hasil_final = 
  data.frame(N,Luas) %>% 
  rename("n banyak selang" = N,
         "Luas aproksimasi" = Luas)

hasil_final