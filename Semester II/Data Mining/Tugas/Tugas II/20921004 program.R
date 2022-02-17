rm(list=ls())

library(dplyr)
library(tidyr)
library(tictoc)

tic("all process")

# membuat functions
# standarisasi data numerik
z_numeric = function(var){
  n = length(var)           # menghitung n: berapa banyak data
  m = sum(var) / n          # hitung variabel m
  s = sum(abs(var - m))/n   # hitung variabel s
  z = (var - m)/s           # hitung z
  return(z)                 # output nilai z
}

# standarisasi data ordinal
z_ordinal = function(var){
  var = as.factor(sort(var))  # mengubah var menjadi factor
  var = as.numeric(var)       # mengubah jadi angka numerik
  M = max(var)                # cari nilai max dari var
  z = (var - 1) / (M - 1)     # hitung z
  return(z)                   # output nilai z
}

# hitung jarak data biner
dist_nominal = function(a,b){
  jarak = sum(a != b)     # beri nilai 1 jika berbeda lalu dijumlahkan
  return(jarak)           # output function
}

# hitung jarak data numerik dengan formula euclid
dist_euclid = function(a,b){
  d = abs(a-b)
  d = sum(d^2)
  d = sqrt(d)
  d = round(d,3)
}

# memanggil data
data = 
  read.csv("train.csv")

# pre-processing data
df = 
  data %>%                                              # memanggil data
  mutate(price_range = z_ordinal(price_range),          # menghitung z ordinal dari price_range
         battery_power = z_numeric(battery_power),      # menghitung z numerik dari battery_power
         clock_speed = z_numeric(clock_speed),          # menghitung z numerik dari clock_speed
         fc = z_numeric(fc),                            # menghitung z numerik dari fc
         int_memory = z_numeric(int_memory),            # menghitung z numerik dari int_memory
         m_dep = z_numeric(m_dep),                      # menghitung z numerik dari m_dep
         mobile_wt = z_numeric(mobile_wt),              # menghitung z numerik dari mobile_wt
         n_cores = z_numeric(n_cores),                  # menghitung z numerik dari n_cores
         pc = z_numeric(pc),                            # menghitung z numerik dari pc
         px_height = z_numeric(px_height),              # menghitung z numerik dari px_height
         px_width = z_numeric(px_width),                # menghitung z numerik dari px_width
         ram = z_numeric(ram),                          # menghitung z numerik dari ram
         sc_h = z_numeric(sc_h),                        # menghitung z numerik dari sc_h
         sc_w = z_numeric(sc_w),                        # menghitung z numerik dari sc_w
         talk_time = z_numeric(talk_time))              # menghitung z numerik dari talk_time


# save dulu atribut binary
att_binary = c("blue","dual_sim","four_g","three_g","touch_screen","wifi")

# mengubah menjadi matriks dan transpose
# matriks pertama untuk variabel binary
mat_1 = df %>% select(att_binary) %>% as.matrix() %>% t()   
# matriks kedua untuk variabel numerik
mat_2 = df %>% select(-att_binary) %>% as.matrix() %>% t()   


# menghitung matriks jarak 
# membuat matriks N x N
N = ncol(mat_1)
mat_dist_1 = matrix(NA,nrow = N,ncol = N)
mat_dist_2 = matrix(NA,nrow = N,ncol = N)

# menghitung matriks jarak dari atribut binary
for(i in 1:N){                                         
  for(j in 1:N){
    mat_dist_1[i,j] = dist_nominal(mat_1[,i],mat_1[,j])   # menghitung jarak pasangan per baris data object
  }
}

# menghitung matriks jarak dari atribut numerik -- jarak yang saya pakai adalah euclid
for(i in 1:N){
  for(j in 1:N){
    mat_dist_2[i,j] = dist_euclid(mat_2[,i],mat_2[,j])  # menghitung jarak pasangan per baris data object
  }
}

# menghitung matriks jarak final
matriks = (mat_dist_1 + mat_dist_2) / (nrow(mat_1) + nrow(mat_2))    # delta yang saya gunakan per atribut adalah 1
colnames(matriks) = data$id             # kasih column names
rownames(matriks) = data$id             # kasih row names

# hasil akhir
matriks[lower.tri(matriks)] = NA

# export ke bentuk csv
# write.csv(matriks,"dist_matriks.csv")

toc()