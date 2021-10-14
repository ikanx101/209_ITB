rm(list=ls())

# library
library(dplyr)

luas_mc = function(f,x1,x2,y1,y2,N){
  # membuat template vector luas
  luas = c()
  # lakukan 100 x pengulangan
  for(ikanx in 1:100){
    # generating random number
    x = runif(N,x1,x2)
    y = runif(N,y1,y2)
    
    # pengecekan y <= f(x)
    rekap = 
      data.frame(x,y) %>% 
      rowwise() %>% 
      mutate(f_x = f(x),
             on_target = ifelse(y <= f_x,1,0)) %>% 
      ungroup()
    
    # hitung rasio on target vs all dots
    rasio = sum(rekap$on_target) / N
    # hitung luas
    luas_temp = (x2-x1)*(y2-y1)*rasio
    # memasukkan ke dalam template
    luas = c(luas,luas_temp)
  }
  
  # menghitung rata-rata luas
  return(mean(luas))
}

# definisi fungsi dari soal
g = function(x){
  isi = 8 - (((x+1)^2)/2)
  1 + sqrt(isi)
}


# kita coba dengan berbagai macam N
N = c(10,50,100,200,500,1000,2500,5000,10000,20000)
Luas = c()
for(i in 1:length(N)){
  Luas[i] = luas_mc(g,0,sqrt(14)-1,0,4,N[i])
}

# hasil akhir
hasil_final = 
  data.frame(N,Luas) %>% 
  rename("n banyak titik" = N,
         "Luas aproksimasi" = Luas)

hasil_final