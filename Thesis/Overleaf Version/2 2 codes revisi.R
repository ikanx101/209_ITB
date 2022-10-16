# ==============================================================================
# 
# supplier selection in R
# optimization modelling
#
# ==============================================================================

# memanggil libraries
library(readxl)             # untuk mengimport data excel
library(dplyr)              # untuk data carpentry
library(ompr)               # untuk optimisasi
library(ompr.roi)           # untuk optimisasi
library(ROI.plugin.glpk)    # untuk optimisasi
library(tictoc)             # untuk menghitung runtime

tic("Keseluruhan Proses: ")

rm(list=ls())

options(scipen = 99)

# ambil data yang dibutuhkan
source("1 code preparation parameter.R")

# ==============================================================================
# kita ambil parameter-parameter

# M as the set of weeks on the supply cycle
M = 1:4

# N as the number of raw-material types
N = 1:6

# I as the number of items
I = 1:nrow(df_4)

# himpunan semua gula
G = 1:6

# total proporsi portofolio bahan baku gula k yang ditetapkan dalam setahun
Prk = df_1$proporsi /100 * 3000000 * 12

# miu suatu bilangan yang kecil
#miu = 10^(-6)
miu = 10^(-10)

# ==============================================================================
# model utama
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
  
  # constraints I - 1
  add_constraint(x[k] <= D * y[k],
                 k = G) %>% 
  # constraints I - 2
  add_constraint(x[k] >= o_k[k] * y[k],
                 k = G) %>% 
                 
  # ============================================================================
  # variabel keputusan II
  # banyaknya pengiriman bahan baku gula jenis k pada awal week j
  add_variable(x_hat[j,k],type = "integer",lb = 0,
               j = M,
               k = G) %>% 
               
  # constraint III
  add_constraint(sum_expr(x_hat[j,k],
                          j = M) == x[k],
                 k = G) %>% 
                 
  # ============================================================================
  # variabel keputusan III
  # jika item i diproduksi dengan gula k pada week j
  add_variable(a[i,j,k],type = "binary",lb = 0,
               i = I,
               j = M,
               k = G) %>% 
  
  # ============================================================================
  # variabel keputusan IV
  # proporsi item i diproduksi dengan gula k pada week j
  add_variable(b[i,j,k],type = "continuous",lb = 0,
               i = I,
               j = M,
               k = G) %>% 
               
  # ============================================================================
  # variabel keputusan V
  # stok level bahan baku k di akhir week j
  add_variable(z[j,k],type = "continuous",lb = 0,
               j = M,
               k = G) %>% 
               
  # ============================================================================
  # constrain II
  # modifikasi dari Bu Rieske
  # kita pecah dua karena ada pengaruh dari Z_0k
    # untuk j = 1
    add_constraint(sum_expr(x_hat[1,k],k = G) + sum_expr(Z_0k[k],k = G) >= Dj[1]) %>%
    
    # untuk j >= 1
    add_constraint(sum_expr(x_hat[j,k],k = G) + sum_expr(z[j-1,k],k = G) >= Dj[j],
                   j = 2:4) %>%
                   
  # ============================================================================
  # constrain V
  # memastikan bahwa semua produk yg bisa diproduksi dengan dua gula bisa dibeli
  # minggu 1 - untuk setiap produk yg bisa diproduksi dengan min 2 gula
  add_constraint(sum_expr(a[i,j,k],k = G) >= 2,
                 j = 1,
                 i = P_1_2) %>%
                 
  # minggu 2 - untuk setiap produk yg bisa diproduksi dengan min 2 gula
  add_constraint(sum_expr(a[i,j,k],k = G) >= 2,
                 j = 2,
                 i = P_2_2) %>%
  
  # minggu 3 - untuk setiap produk yg bisa diproduksi dengan min 2 gula
  add_constraint(sum_expr(a[i,j,k],k = G) >= 2,
                 j = 3,
                 i = P_3_2) %>%
                 
  # minggu 4 - untuk setiap produk yg bisa diproduksi dengan min 2 gula
  add_constraint(sum_expr(a[i,j,k],k = G) >= 2,
                 j = 4,
                 i = P_4_2) %>%
  
  # ============================================================================
  # constraint IV bagian a_ijk
  # week 1
  add_constraint(a[i,j,k] <= f_ik[i,k],
               k = G,
               j = 1,
               i = P1) %>% 
  # week 2
  add_constraint(a[i,j,k] <= f_ik[i,k],
                 k = G,
                 j = 2,
                 i = P2) %>% 
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
  
  # constraint IV bagian b_ijk
  # week 1
  add_constraint(b[i,j,k] <= f_ik[i,k] * a[i,j,k],
                 k = G,
                 j = 1,
                 i = P1) %>% 
  # week 2
  add_constraint(b[i,j,k] <= f_ik[i,k] * a[i,j,k],
                 k = G,
                 j = 2,
                 i = P2) %>%
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
  
  # constraint IV bagian miu * a_ijk
  # week 1
  add_constraint(miu * a[i,j,k] <= b[i,j,k],
                k = G,
                i = P1,
                j = 1) %>% 
  # week 2
  add_constraint(miu * a[i,j,k] <= b[i,j,k],
                 k = G,
                 i = P2,
                 j = 2) %>% 
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
  
  # objective function
  set_objective(sum_expr(c_k[k] * x[k], k = G),"min")
  
# solver
result = milp_new %>% solve_model(with_ROI("glpk", verbose = TRUE))
source("3 export hasil ke excel.R")  

toc()
