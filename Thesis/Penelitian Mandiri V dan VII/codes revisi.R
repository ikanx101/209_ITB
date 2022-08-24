# ==============================================================================
# 
# supplier selection in R
# optimization modelling
#
# ==============================================================================

setwd("~/209_ITB/Thesis/Penelitian Mandiri V dan VII")

# memanggil libraries
library(readxl)             # untuk mengimport data excel
library(dplyr)              # untuk data carpentry
library(ompr)               # untuk optimisasi
library(ompr.roi)           # untuk optimisasi
library(ROI.plugin.glpk)    # untuk optimisasi

rm(list=ls())

options(scipen = 99)

# ambil data yang dibutuhkan
load("raw.rda")

# kita ganti stok akhir bulan dengan data ini:
df_1$stok_akhir_bulan = c(467470,466910,375400,470148,373600,404600)

# menghitung kebutuhan gula w3 hingga w6
df_3 = 
  df_3 %>% 
  mutate(kebutuhan_gula_w3_w6 = w3 + w4 + w5 + w6)


# kita gabung dulu antara df_2 dan df_3
df_4 = merge(df_2,df_3) 
df_4[is.na(df_4)] = 0

# kita ubah cde product menjadi bilangan 1-51
df_4$code_product = 1:nrow(df_4)

# ==============================================================================
# kita ambil parameter-parameter

# M as the set of weeks on the planning horizon
M = 1:6

# M_hat as the set of raw-material delivery weeks
M_hat = 3:6

# N as the number of raw-material types
N = 1:6

# I as the number of items
I = 1:nrow(df_4)

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

# himpunan semua gula
G = 1:6

# Dj sebagai kebutuhan gula di minggu perencanaan
temp = df_3 %>% 
  select(w1,w2,w3,w4,w5,w6) %>% 
  reshape2::melt() %>% 
  group_by(variable) %>% 
  summarise(Dj = sum(value)) %>% 
  ungroup()
Dj = temp$Dj
Dj[1:2] = c(0,0)

# total kebutuhan gula di bulan perencanaan w3-s6
D = sum(df_3$kebutuhan_gula_w3_w6)

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
                                paste0("w",1:6),
                                paste0("Gula_",k)
                                )
    }
  }
}  

# harga gula per kg
c_k = df_1$harga_gula

# min order quantity
o_k = df_1$min_order

# max kapasitas
maxcap = 1427 * 1000 * 10

# total bahan baku k yang dibutuhkan pada week 1 dan 2
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
d_2k = d_rujukan$value/2

# total proporsi portofolio bahan baku gula k yang ditetapkan dalam setahun
Prk = df_1$proporsi /100 * 3000000 * 12

# stok level bahan baku k di gudang pada akhir week 1
Z_1k = df_1$stok_akhir_bulan


# miu suatu bilangan yang kecil
miu = 10^(-5)

# x_hat_1k untuk k di G
# banyak gula yang dikirim pada minggu 1
x_hat_1k = sample(10:40,6) * 200
# banyak gula yang dikirim pada minggu 2
x_hat_2k = sample(10:40,6) * 200


# ==============================================================================

library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)

milp_new = 
  # model
  MIPModel() %>%
  
  # ============================================================================
  # variabel keputusan I
  # banyaknya bahan baku k yang dibeli
  add_variable(x[k],type = "integer",lb = 0,
               k = G) %>%
  # dengan constraint minimal sebesar min order 
  add_constraint(x[k] >= o_k[k],
                 k = G) %>% 
  
  # variabel penghubung
  add_variable(y[k],type = 'binary',
               k = G) %>% 
  
  # constraints (1)
  add_constraint(x[k] <= D * y[k],
                 k = G) %>% 
  # constraints (2)
  add_constraint(x[k] >= o_k[k] * y[k],
                 k = G) %>% 
  
  # ============================================================================
  # variabel keputusan II
  # banyaknya pengiriman bahan baku gula jenis k pada awal week j
  add_variable(x_hat[j,k],type = "integer",lb = 0,
               j = M,
               k = G) %>% 
  
  # kita buat constraint agar x_hat[1,k] jadi x_hat_1k
  add_constraint(x_hat[j,k] == x_hat_1k[k],
                 j = 1,
                 k = G) %>% 
  # kita buat constraint agar x_hat[2,k] jadi x_hat_2k
  add_constraint(x_hat[j,k] == x_hat_2k[k],
                 j = 2,
                 k = G) %>% 
  
  # ============================================================================
  # variabel keputusan III
  # jika item i diproduksi dengan gula k pada week j
  add_variable(a[i,j,k],type = "binary",lb = 0,
               i = I,
               j = M_hat,
               k = G) %>% 
  
  # ============================================================================
  # variabel keputusan IV
  # proporsi item i diproduksi dengan gula k pada week j
  add_variable(b[i,j,k],type = "continuous",lb = 0,
               i = I,
               j = M_hat,
               k = G) %>% 
  
  # ============================================================================
  #  stok level bahan baku k di akhir week j
  add_variable(z[j,k],type = "continuous",lb = 0,
               j = M,
               k = G) %>% 
  
  # kita buat constraint agar z_1k sama dengan yang distate di parameter
  add_constraint(z[j,k] == Z_1k[k],
                 j = 1,
                 k = G) %>% 
  
  # ============================================================================
  # constraint (3)
  add_constraint(sum_expr(x_hat[j,k],
                          j = M_hat) == x[k],
                 k = G) %>% 
  
  # ============================================================================
  # constraint (4) berlaku pada j = M_hat
  # week 3
  #add_constraint(sum_expr(x_hat[j,k],
  #                        k = G) <= Dj[3],
  #               j = 3) %>% 
  # week 4
  #add_constraint(sum_expr(x_hat[j,k],
  #                        k = G) <= Dj[4],
  #               j = 4) %>% 
  # week 5
  #add_constraint(sum_expr(x_hat[j,k],
  #                        k = G) <= Dj[5],
  #               j = 5) %>%
  # week 6
  #add_constraint(sum_expr(x_hat[j,k],
  #                        k = G) <= Dj[6],
  #               j = 6) %>% 
  
  
  # ============================================================================
  # constraint (5) berlaku pada j = M_hat
  # week 3
  add_constraint(a[i,j,k] <= f_ik[i,k],
                 k = G,
                 j = 3,
                 i = P3) %>% 
  # week 4
  add_constraint(a[i,j,k] <= f_ik[i,k],
                 k = G,
                 j = 4,
                 i = P4) %>% 
  # week 5
  add_constraint(a[i,j,k] <= f_ik[i,k],
                 k = G,
                 j = 5,
                 i = P5) %>%
  # week 6
  add_constraint(a[i,j,k] <= f_ik[i,k],
                 k = G,
                 j = 6,
                 i = P6) %>% 
  
  
  # ============================================================================
  # constraint (6) berlaku pada j = M_hat
  # week 3
  add_constraint(b[i,j,k] <= f_ik[i,k] * a[i,j,k],
                 k = G,
                 j = 3,
                 i = P3) %>% 
  # week 4
  add_constraint(b[i,j,k] <= f_ik[i,k] * a[i,j,k],
                 k = G,
                 j = 4,
                 i = P4) %>%
  # week 5
  add_constraint(b[i,j,k] <= f_ik[i,k] * a[i,j,k],
                 k = G,
                 j = 5,
                 i = P5) %>% 
  # week 6
  add_constraint(b[i,j,k] <= f_ik[i,k] * a[i,j,k],
                 k = G,
                 j = 6,
                 i = P6) %>% 

  # ============================================================================
  # constraint (7) berlaku pada j = M_hat
  # week 3
  add_constraint(miu * a[i,j,k] <= b[i,j,k],
                 k = G,
                 i = P3,
                 j = 3) %>% 
  # week 4
  add_constraint(miu * a[i,j,k] <= b[i,j,k],
                 k = G,
                 i = P4,
                 j = 4) %>% 
  # week 5
  add_constraint(miu * a[i,j,k] <= b[i,j,k],
                 k = G,
                 i = P5,
                 j = 5) %>% 
  # week 6
  add_constraint(miu * a[i,j,k] <= b[i,j,k],
                 k = G,
                 i = P6,
                 j = 6) %>% 
  
  # ============================================================================
  # constraint (8) berlaku pada j = M_hat
  # week 3
  add_constraint(x_hat[j,k] >= sum_expr(a[i,j,k],
                                        i = P3),
                 k = G,
                 j = 3) %>% 
  # week 4
  add_constraint(x_hat[j,k] >= sum_expr(a[i,j,k],
                                        i = P4),
                 k = G,
                 j = 4) %>% 
  # week 5
  add_constraint(x_hat[j,k] >= sum_expr(a[i,j,k],
                                        i = P5),
                 k = G,
                 j = 5) %>% 
  # week 6
  add_constraint(x_hat[j,k] >= sum_expr(a[i,j,k],
                                        i = P6),
                 k = G,
                 j = 6) %>% 

  # ============================================================================
  # constraint (9) berlaku pada j = M_hat
  # week 3
  add_constraint(sum_expr(a[i,j,k],
                          k = G) >= 2,
                 j = 3,
                 i = P_3_2) %>% 
  # week 4
  add_constraint(sum_expr(a[i,j,k],
                          k = G) >= 2,
                 j = 4,
                 i = P_4_2) %>% 
  # week 5
  add_constraint(sum_expr(a[i,j,k],
                          k = G) >= 2,
                 j = 5,
                 i = P_5_2) %>% 
  # week 6
  add_constraint(sum_expr(a[i,j,k],
                          k = G) >= 2,
                 j = 6,
                 i = P_6_2) %>% 

  # ============================================================================
  # constraint (10) berlaku pada j = M_hat
  # week 3
  add_constraint(sum_expr(b[i,j,k],
                          k = G) == 1,
                 j = 3,
                 i = P3) %>% 
  # week 4
  add_constraint(sum_expr(b[i,j,k],
                          k = G) == 1,
                 j = 4,
                 i = P4) %>% 
  # week 5
  add_constraint(sum_expr(b[i,j,k],
                          k = G) == 1,
                 j = 5,
                 i = P5) %>% 
  # week 6
  add_constraint(sum_expr(b[i,j,k],
                          k = G) == 1,
                 j = 6,
                 i = P6) %>% 
  
  # ============================================================================
  # constraint (11) pada bagian 1
  # ini untuk menghitung saldo pada week 2 
  add_constraint(z[j,k] == z[1,k] + x_hat[1,k] - d_2k[k],
                 j = 2,
                 k = G) %>% 
  # ini agar si z[2,k] gak lebih dari maxcap
  add_constraint(sum_expr(z[j,k],
                          k = G) <= maxcap,
                 j = 2) %>% 
  
  # constraint (11) pada bagian 2
  # week 3
  # ini untuk menghitung saldo pada week 3
  add_constraint(z[3,k] == z[2,k] + x_hat[2,k] - sum_expr(b[i,3,k] * matt_g_ijk[i,3,k],
                                                          i = P3),
                 k = G) %>% 

  # ini agar si z[3,k] gak lebih dari maxcap
  add_constraint(sum_expr(z[j,k],
                          k = G) <= maxcap,
                 j = 3) %>% 
  
  # week 4
  # ini untuk menghitung saldo pada week 4
  add_constraint(z[4,k] == z[3,k] + x_hat[3,k] - sum_expr(b[i,4,k] * matt_g_ijk[i,4,k],
                                                          i = P4),
                 k = G) %>% 
  
  # ini agar si z[4,k] gak lebih dari maxcap
  add_constraint(sum_expr(z[j,k],
                          k = G) <= maxcap,
                 j = 4) %>% 
  
  # week 5
  # ini untuk menghitung saldo pada week 5
  add_constraint(z[5,k] == z[4,k] + x_hat[4,k] - sum_expr(b[i,5,k] * matt_g_ijk[i,5,k],
                                                          i = P5),
                 k = G) %>% 
  
  # ini agar si z[5,k] gak lebih dari maxcap
  add_constraint(sum_expr(z[j,k],
                          k = G) <= maxcap,
                 j = 5) %>% 
  
  # ============================================================================
  # set objective
  set_objective(sum_expr(c_k[k] * x[k], k = G),"min")
  

# solver
result = milp_new %>% solve_model(with_ROI("glpk", verbose = TRUE))
solusi_1 = get_solution(result, x[k]) %>% as.data.frame() 
solusi_2 = get_solution(result, x_hat[j,k]) %>% as.data.frame() 
solusi_3 = get_solution(result, z[j,k]) %>% as.data.frame() 

solusi_1
solusi_2 %>% filter(j %in% M_hat) %>%  group_by(k) %>% summarise(sum(value)) %>% ungroup()
solusi_3

  
get_solution(result, y[k]) 
get_solution(result, a[i,j,k]) 
get_solution(result, b[i,j,k]) 




solusi_2 %>% 
  group_by(j) %>% 
  summarise(total = sum(value)) %>% 
  ungroup()

Dj

# ==============================================================================
# save ke sini dulu
library(expss)
library(openxlsx)

# kita bikin workbook-nya
wb = createWorkbook()

# lalu saya buat variabel bernama tabel_all
# berisi list dari semua tabel yang telah kita buat bersama-sama
tabel_all = list("Data Spek Bahan Baku",
                 df_1,
                 "Data Kebutuhan Bahan Baku Pada Bulan May",
                 df_4,
                 "kebutuhan gula di bulan perencanaan w3-w6",
                 D,
                 "max kapasitas",
                 maxcap,
                 "total proporsi portofolio bahan baku gula k yang ditetapkan dalam setahun",
                 Prk,
                 "Demand gula pada w1-w2",
                 d_2k,
                 "stok level bahan baku k di gudang pada akhir week 1",
                 Z_1k
)

# bikin sheet
nama_sheet = paste0("Raw Data")
sh = addWorksheet(wb, nama_sheet)

# masukin semua tabel ke sheet tersebut
xl_write(tabel_all, wb, sh)

# berisi list dari semua tabel yang telah kita buat bersama-sama
tabel_all = list("Hasil x_k",
                 solusi_1,
                 "Hasil x_cap",
                 solusi_2,
                 "Hasil z_k",
                 solusi_3)

# bikin sheet
nama_sheet = paste0("Hasil")
sh = addWorksheet(wb, nama_sheet)

# masukin semua tabel ke sheet tersebut
xl_write(tabel_all, wb, sh)


# export ke Excel
saveWorkbook(wb, "data mentah dan hasil.xlsx", overwrite = TRUE)


