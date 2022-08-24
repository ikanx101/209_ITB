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
source("1 code preparation parameter.R")

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

# himpunan semua gula
G = 1:6

# total proporsi portofolio bahan baku gula k yang ditetapkan dalam setahun
Prk = df_1$proporsi /100 * 3000000 * 12

# miu suatu bilangan yang kecil
miu = 10^(-6)




# ==============================================================================
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
  add_constraint(z[j,k] == z[(j-1),k] + x_hat[(j-1),k] - d_2k[k],
                 j = 2,
                 k = G) %>% 
  
  # ini untuk menghitung saldo pada week 3 
  add_constraint(z[j,k] == z[(j-1),k] + x_hat[(j-1),k] - sum_expr(b[i,j,k] * matt_g_ijk[i,j,k],
                                                                  i = P3),
                 j = 3,
                 k = G) %>% 
  
  # ini untuk menghitung saldo pada week 4 
  add_constraint(z[j,k] == z[(j-1),k] + x_hat[(j-1),k] - sum_expr(b[i,j,k] * matt_g_ijk[i,j,k],
                                                                  i = P4),
                 j = 4,
                 k = G) %>% 
  
  # ini untuk menghitung saldo pada week 5 
  add_constraint(z[j,k] == z[(j-1),k] + x_hat[(j-1),k] - sum_expr(b[i,j,k] * matt_g_ijk[i,j,k],
                                                                  i = P5),
                 j = 5,
                 k = G) %>% 
  
  # ini untuk menghitung saldo pada week 6 
  add_constraint(z[j,k] >= z[(j-1),k] + x_hat[(j-1),k] - sum_expr(b[i,j,k] * matt_g_ijk[i,j,k],
                                                                  i = P6),
                 j = 6,
                 k = G) %>% 
  
  
  # ini agar si z[j,k] gak lebih dari maxcap
  add_constraint(sum_expr(z[j,k],
                          k = G) <= maxcap,
                 j = M) %>% 
  
  # ============================================================================
  add_constraint(z[(j-1),k] + x_hat[(j-1),k] >= sum_expr(b[i,j,k] * matt_g_ijk[i,j,k],
                                                        i = P3),
                 j = 3) %>% 
  
  add_constraint(z[(j-1),k] + x_hat[(j-1),k] >= sum_expr(b[i,j,k] * matt_g_ijk[i,j,k],
                                                         i = P4),
                 j = 4) %>% 
  
  add_constraint(z[(j-1),k] + x_hat[(j-1),k] >= sum_expr(b[i,j,k] * matt_g_ijk[i,j,k],
                                                         i = P5),
                 j = 5) %>% 
  
  add_constraint(z[(j-1),k] + x_hat[(j-1),k] >= sum_expr(b[i,j,k] * matt_g_ijk[i,j,k],
                                                        i = P6),
                 j = 6) %>% 
  
  
  set_objective(sum_expr(c_k[k] * x[k], k = G),"min")



  
  
  


  
  

# solver
result = milp_new %>% solve_model(with_ROI("glpk", verbose = TRUE))
solusi_1 = get_solution(result, x[k]) %>% as.data.frame() 
solusi_2 = get_solution(result, x_hat[j,k]) %>% as.data.frame() 
solusi_3 = get_solution(result, z[j,k]) %>% as.data.frame() 

# semua solusi
solusi_1

# liat x_hat di minggu perencanaan harus sama dengan solusi 1
solusi_2 %>% filter(j %in% M_hat) %>%  group_by(k) %>% summarise(sum(value)) %>% ungroup()

# x hat
solusi_2


# lihat stok total
solusi_3 

# lihat stok pada week 1
solusi_3 %>% filter(j == 1)

# lihat stok pada week 2
solusi_3 %>% filter(j == 2) 

# lihat stok pada week 3
solusi_3 %>% filter(j == 3)

# lihat stok pada week 4
solusi_3 %>% filter(j == 4)

# lihat stok pada week 5
solusi_3 %>% filter(j == 5)

# lihat stok pada week 6
solusi_3 %>% filter(j == 6)

  
b = get_solution(result, b[i,j,k]) 

# ===========================================================================
# kita lihat pada week 3
b_ =
  b %>% 
  filter(j == 3) %>% 
  filter(i %in% P3) %>% 
  mutate(g_ijk = NA)

for(x in 1:nrow(b_)){
  b_$g_ijk[x] = matt_g_ijk[b_$i[x],
                           b_$j[x],
                           b_$k[x]]
}

# kebutuhan pada week 3
b_ %>% mutate(total = value * g_ijk) %>% group_by(k) %>% summarise(demand = sum(total)) %>% .$demand

# lihat stok pada week 3
(solusi_3 %>% filter(j == 2) %>% .$value ) +   # stok level pada week 2 
  (solusi_2 %>% filter(j == 2) %>% .$value) -  # barang yang dikirim di week 2
  (b_ %>% mutate(total = value * g_ijk) %>% group_by(k) %>% summarise(demand = sum(total)) %>% .$demand)

# bandingkan dengan stok pada week 3 hasil model
solusi_3 %>% filter(j == 3) %>% .$value


# ===========================================================================
# kita lihat pada week 4
b_ =
  b %>% 
  filter(j == 4) %>% 
  filter(i %in% P4) %>% 
  mutate(g_ijk = NA)

for(x in 1:nrow(b_)){
  b_$g_ijk[x] = matt_g_ijk[b_$i[x],
                           b_$j[x],
                           b_$k[x]]
}

# kebutuhan pada week 4
b_ %>% mutate(total = value * g_ijk) %>% group_by(k) %>% summarise(demand = sum(total)) %>% .$demand

# lihat stok pada week 4
(solusi_3 %>% filter(j == 3) %>% .$value ) +   # stok level pada week 3 
  (solusi_2 %>% filter(j == 3) %>% .$value) -  # barang yang dikirim di week 3
  (b_ %>% mutate(total = value * g_ijk) %>% group_by(k) %>% summarise(demand = sum(total)) %>% .$demand)

# bandingkan dengan stok pada week 4 hasil model
solusi_3 %>% filter(j == 4) %>% .$value


# ===========================================================================
# kita lihat pada week 5
b_ =
  b %>% 
  filter(j == 5) %>% 
  filter(i %in% P5) %>% 
  mutate(g_ijk = NA)

for(x in 1:nrow(b_)){
  b_$g_ijk[x] = matt_g_ijk[b_$i[x],
                           b_$j[x],
                           b_$k[x]]
}

# kebutuhan pada week 5
b_ %>% mutate(total = value * g_ijk) %>% group_by(k) %>% summarise(demand = sum(total)) %>% .$demand

# lihat stok pada week 5
(solusi_3 %>% filter(j == 4) %>% .$value ) +   # stok level pada week 4 
  (solusi_2 %>% filter(j == 4) %>% .$value) -  # barang yang dikirim di week 4
  (b_ %>% mutate(total = value * g_ijk) %>% group_by(k) %>% summarise(demand = sum(total)) %>% .$demand)

# bandingkan dengan stok pada week 4 hasil model
solusi_3 %>% filter(j == 5) %>% .$value


# ===========================================================================
# kita lihat pada week 6
b_ =
  b %>% 
  filter(j == 6) %>% 
  filter(i %in% P6) %>% 
  mutate(g_ijk = NA)

for(x in 1:nrow(b_)){
  b_$g_ijk[x] = matt_g_ijk[b_$i[x],
                           b_$j[x],
                           b_$k[x]]
}

# kebutuhan pada week 6
b_ %>% mutate(total = value * g_ijk) %>% group_by(k) %>% summarise(demand = sum(total)) %>% .$demand

# lihat stok pada week 6
(solusi_3 %>% filter(j == 5) %>% .$value ) +   # stok level pada week 5 
  (solusi_2 %>% filter(j == 5) %>% .$value) -  # barang yang dikirim di week 5
  (b_ %>% mutate(total = value * g_ijk) %>% group_by(k) %>% summarise(demand = sum(total)) %>% .$demand)

# bandingkan dengan stok pada week 4 hasil model
solusi_3 %>% filter(j == 6) %>% .$value
