# memanggil libraries
library(readxl)             # untuk mengimport data excel
library(dplyr)              # untuk data carpentry
library(ompr)               # untuk optimisasi
library(ompr.roi)           # untuk optimisasi
library(ROI.plugin.glpk)    # untuk optimisasi

rm(list=ls())

# ambil data yang dibutuhkan
load("raw.rda")

# kita gabung dulu antara df_2 dan df_3
df_4 = merge(df_2,df_3) 
df_4[is.na(df_4)] = 0

# kita ubah cde product menjadi bilangan 1-51
df_4$code_product = 1:nrow(df_4)

# ==============================================================================
# kita ambil parameter-parameter

# himpunan semua minggu yang terlibat
M = 1:6

# himpunan produk yang diproduksi per minggu
P1 = df_4$code_product[df_4$w1>0]
P2 = df_4$code_product[df_4$w2>0]
P3 = df_4$code_product[df_4$w3>0]
P4 = df_4$code_product[df_4$w4>0]
P5 = df_4$code_product[df_4$w5>0]
P6 = df_4$code_product[df_4$w6>0]

# himpunan semua gula
G = df_1$nama_gula

# kebutuhan gula di bulan perencanaan w3-s6
D = sum(df_3$kebutuhan_gula_w3_w6)

# kebutuhan bahan baku gula k (dalam ton) dari produk i pada week j
g_kij = function(k,i,j){
  temp = 
    df_4 %>% 
    filter(code_product == i) %>% 
    select(k,j)
  hasil = as.numeric(temp[1]) * as.numeric(temp[2])
  return(hasil)
}

# contoh
matt_g_kij = array(NA,dim = c(6,51,6))

for(k in 1:6){
  for(i in 1:51){
    for(j in 1:6){
      matt_g_kij[k,i,j] = g_kij(paste0("Gula_",k),
                                i,
                                paste0("w",1:6))
    }
  }
}  

# max kapasitas
maxcap = 1427 * 1000

# total proporsi portofolio bahan baku gula k yang ditetapkan dalam setahun
Qk = df_1$proporsi /100 * 2100000 * 12

# harga gula per kg
c_k = df_1$harga_gula

# min order quantity
o_k = df_1$min_order

# total bahan baku k yang dibutuhkan pada week 2
d_rujukan = 
  df_4 %>% 
  mutate(d_12 = Gula_1 * w2,
         d_22 = Gula_2 * w2,
         d_32 = Gula_3 * w2,
         d_42 = Gula_4 * w2,
         d_52 = Gula_5 * w2,
         d_62 = Gula_6 * w2) %>% 
  summarise(d_12 = sum(d_12),
            d_22 = sum(d_22),
            d_32 = sum(d_32),
            d_42 = sum(d_42),
            d_52 = sum(d_52),
            d_62 = sum(d_62)
            ) %>% 
  reshape2::melt()
d_k2 = d_rujukan$value

# stok level bahan baku k di gudang pada akhir week 1
Z_k1 = df_1$stok_akhir_bulan

# eps suatu bilangan yang kecil
eps = 10^(-5)
miu = 10^(-5)

# bikin function yk penghubung nilai diskontinu dari variabel xk
yk = function(xk,k){
  yk = 
    if(xk == 0){1} else
    if(o_k[k] <= xk & xk <= maxcap){0}
  return(yk)
}

# ==============================================================================

library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)

milp_new = 
  # model
  MIPModel() %>%

  # variabel keputusan pertama
  # banyaknya bahan baku k yang dibeli
  add_variable(x[k],type = "integer",lb = 0,
               k = 1:6) %>%
  # dengan constraint minimal sebesar min order 
  add_constraint(x[k] >= o_k[k],
                 k = 1:6) %>% 
  # constraints 2
  add_constraint(sum_expr(x[k]) >= D,
                 k = 1:6) %>%  
  # set objective
  set_objective(sum_expr(c_k[k] * x[k], k = 1:6),"min")

result = milp_new %>% solve_model(with_ROI("glpk", verbose = TRUE))
get_solution(result, x[k]) 



  # variabel keputusan kedua
  # banyaknya pengiriman bahan baku gula jenis k pada awal week j
  add_variable(x_hat[k,j],type = "integer",lb = 0,
               k = 1:6,
               j = 1:6) %>% 
  
  # variabel keputusan ketiga
  # gula k digunakan pada produk i pada week j
  add_variable(a[k,i,j],type = "binary",lb = 0,
               k = 1:6,
               i = 1:51,
               j = 1:6) %>% 
  
  # variabel keputusan keempat
  # proporsi penggunaan bahan baku k dari seluruh kebutuhan bahan baku untuk produk i di week j
  add_variable(b[k,i,j],type = "continuous",lb = 0,
               k = 1:6,
               i = 1:51,
               j = 2:6) %>% 
  
  # variabel keputusan kelima
  #  stok level bahan baku k di akhir week j
  add_variable(z[k,j],type = "continuous",lb = 0,
               k = 1:6,
               j = 1:6) %>% 
  
  # variabel penghubung
  add_variable(y[k],type = 'binary',
               k = 1:6) %>% 
  
  # constraints 1
  # kendala I menjadi penghubung antara variabel keputusan biner, integer, atau kontinu yang berkaitan
  # untuk setiap k in gula
  add_constraint(x[k] <= D * y[k],
                 k = 1:6) %>% 
  add_constraint(x[k] >= eps * y[k],
                 k = 1:6) %>% 
  # untuk setiap j selain 1 dan 2
  # untuk semua i dan semua k
  add_constraint(b[k,i,j] <= a[k,i,j],
                 j = 3:6,
                 i = 1:51,
                 k = 1:6) %>% 
  add_constraint(b[k,i,j] >= miu * a[k,i,j],
                 j = 3:6,
                 i = 1:51,
                 k = 1:6) %>% 
  
  




