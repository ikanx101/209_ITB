# skrip untuk mengambil parameter-parameter

setwd("~/209_ITB/Thesis/Penelitian Mandiri V dan VII")

rm(list=ls())

library(readxl)
library(dplyr)

nama = "data input.xlsx"
sheets = excel_sheets(nama)

# ambilin data
df_1 = read_excel(nama,sheet = sheets[1]) # ambil spek
df_2 = read_excel(nama,sheet = sheets[2]) # ambil matriks gula x produk
df_3 = read_excel(nama,sheet = sheets[3]) # ambil matriks produk x minggu

df_4 = merge(df_2,df_3)  # gabung menjadi matriks besar gula x produk x minggu
df_4[is.na(df_4)] = 0

maxcap = read_excel(nama,sheet = sheets[4]) # ambil max capacity gudang 
maxcap = maxcap$maxcap

temporary = read_excel(nama,sheet = sheets[5])

# bikin f_ik
# produk i bisa diproduksi dengan gula k
f_ik = 
  df_4 %>% 
  select(Gula_1,Gula_2,Gula_3,Gula_4,Gula_5,Gula_6) %>% 
  as.matrix()
colnames(f_ik) = NULL

# mengambil himpunan produk yang diproduksi per minggu
df_4 = 
  df_4 %>% 
  mutate(pakai_gula = Gula_1 + Gula_2 + Gula_3 + Gula_4 + Gula_5 + Gula_6)

# mengambil list produk dengan pemakaian gula >= 2
P_j_2 = df_4$code_product[df_4$pakai_gula >= 2]
  # bikin perminggu
  P_3_2 = df_4$code_product[df_4$pakai_gula >= 2 & df_4$w3 > 0]
  P_4_2 = df_4$code_product[df_4$pakai_gula >= 2 & df_4$w4 > 0]
  P_5_2 = df_4$code_product[df_4$pakai_gula >= 2 & df_4$w5 > 0]
  P_6_2 = df_4$code_product[df_4$pakai_gula >= 2 & df_4$w6 > 0]

# mengambil list produk dengan pemakaian gula == 1
P_j_1 = df_4$code_product[df_4$pakai_gula == 1]

# list produk per minggu
P1 = df_4$code_product[df_4$w1 > 0]
P2 = df_4$code_product[df_4$w2 > 0]
P3 = df_4$code_product[df_4$w3 > 0]
P4 = df_4$code_product[df_4$w4 > 0]
P5 = df_4$code_product[df_4$w5 > 0]
P6 = df_4$code_product[df_4$w6 > 0]

# Dj sebagai kebutuhan gula di minggu perencanaan
temp = 
  df_3 %>% 
  select(w3,w4,w5,w6) %>% 
  reshape2::melt() %>% 
  group_by(variable) %>% 
  summarise(Dj = sum(value)) %>% 
  ungroup()
Dj = temp$Dj
Dj = c(0,0,Dj)

# total semua kebutuhan gula di bulan perencanaan
D = sum(Dj[3:6])

# harga gula per kg
c_k = df_1$harga_gula

# min order quantity
o_k = df_1$min_order


# kebutuhan bahan baku gula k (dalam ton) dari produk i pada week j
g_ijk = function(i,j,k){
  temp = 
    df_4 %>% 
    filter(code_product == i) %>% 
    select(k,j)
  hasil = as.numeric(temp[1]) * as.numeric(temp[2])
  return(hasil)
}

# contoh
matt_g_ijk = array(NA,dim = c(51,6,6))
for(i in 1:51){
  for(j in 1:6){
    for(k in 1:6){
      matt_g_ijk[i,j,k] = g_ijk(i,
                                paste0("w",j),
                                paste0("Gula_",k)
      )
    }
  }
}  

# total bahan baku k yang dibutuhkan pada week 1 dan 2
d_2k = temporary$d2k

# stok level bahan baku k di gudang pada akhir week 1
Z_1k = df_1$stok_akhir_bulan


# x_hat_1k untuk k di G
# banyak gula yang dikirim pada minggu 1
x_hat_1k = temporary$x_hat_1k
# banyak gula yang dikirim pada minggu 2
x_hat_2k = temporary$x_hat_2k