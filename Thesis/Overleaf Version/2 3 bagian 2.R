# ==============================================================================
# 
# supplier selection in R
# optimization modelling
#
# ==============================================================================

# set working directory dulu
setwd("~/209_ITB/Thesis/Overleaf Version")


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
mo_k = df_1$proporsi /100 * 3000000 * 12
alfa = 500
mo_k = (1 / alfa) * mo_k

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
  # constraint II
  # modifikasi dari Bu Rieske
  # kita pecah dua karena ada pengaruh dari Z_0k
    # untuk j = 1
    #add_constraint(sum_expr(x_hat[1,k],k = G) + sum_expr(Z_0k[k],k = G) >= Dj[1]) %>%
    
    # untuk j >= 1
    #add_constraint(sum_expr(x_hat[j,k],k = G) + sum_expr(z[j-1,k],k = G) >= Dj[j],j = 2:4) %>%
                   
  # ============================================================================
  # constraint V
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
  
  # ============================================================================
  # constraint persamaan 12
  # week 1
  add_constraint((1 - a[i,j,1]) + (1 - a[i,j,2]) >= b[i,j,1] - b[i,j,2],
                 i = P1,
                 j = 1) %>%
  # week 2
  add_constraint((1 - a[i,j,1]) + (1 - a[i,j,2]) >= b[i,j,1] - b[i,j,2],
                 i = P2,
                 j = 2) %>%
  # week 3
  add_constraint((1 - a[i,j,1]) + (1 - a[i,j,2]) >= b[i,j,1] - b[i,j,2],
                 i = P3,
                 j = 3) %>%
  # week 4
  add_constraint((1 - a[i,j,1]) + (1 - a[i,j,2]) >= b[i,j,1] - b[i,j,2],
                 i = P4,
                 j = 4) %>%
  
  
  # constraint persamaan 13
  # week 1
  add_constraint((1 - a[i,j,1]) + (1 - a[i,j,2]) >= b[i,j,2] - b[i,j,1],
                 i = P1,
                 j = 1) %>%
  # week 2
  add_constraint((1 - a[i,j,1]) + (1 - a[i,j,2]) >= b[i,j,2] - b[i,j,1],
                 i = P2,
                 j = 2) %>%
  # week 3
  add_constraint((1 - a[i,j,1]) + (1 - a[i,j,2]) >= b[i,j,2] - b[i,j,1],
                 i = P3,
                 j = 3) %>%
  # week 4
  add_constraint((1 - a[i,j,1]) + (1 - a[i,j,2]) >= b[i,j,2] - b[i,j,1],
                 i = P4,
                 j = 4) %>%
  
  
  # constraint persamaan 14
  # week 1
  add_constraint((1 - a[i,j,1]) + (1 - a[i,j,3]) >= b[i,j,1] - b[i,j,3],
                 i = P1,
                 j = 1) %>%
  # week 2
  add_constraint((1 - a[i,j,1]) + (1 - a[i,j,3]) >= b[i,j,1] - b[i,j,3],
                 i = P2,
                 j = 2) %>%
  # week 3
  add_constraint((1 - a[i,j,1]) + (1 - a[i,j,3]) >= b[i,j,1] - b[i,j,3],
                 i = P3,
                 j = 3) %>%
  # week 4
  add_constraint((1 - a[i,j,1]) + (1 - a[i,j,3]) >= b[i,j,1] - b[i,j,3],
                 i = P4,
                 j = 4) %>%
  
  
  # constraint persamaan 15
  # week 1
  add_constraint((1 - a[i,j,1]) + (1 - a[i,j,3]) >= b[i,j,3] - b[i,j,1],
                 i = P1,
                 j = 1) %>%
  # week 2
  add_constraint((1 - a[i,j,1]) + (1 - a[i,j,3]) >= b[i,j,3] - b[i,j,1],
                 i = P2,
                 j = 2) %>%
  # week 3
  add_constraint((1 - a[i,j,1]) + (1 - a[i,j,3]) >= b[i,j,3] - b[i,j,1],
                 i = P3,
                 j = 3) %>%
  # week 4
  add_constraint((1 - a[i,j,1]) + (1 - a[i,j,3]) >= b[i,j,3] - b[i,j,1],
                 i = P4,
                 j = 4) %>%
  
  
  # constraint persamaan 16
  # week 1
  add_constraint((1 - a[i,j,1]) + (1 - a[i,j,4]) >= b[i,j,1] - b[i,j,4],
                 i = P1,
                 j = 1) %>%
  # week 2
  add_constraint((1 - a[i,j,1]) + (1 - a[i,j,4]) >= b[i,j,1] - b[i,j,4],
                 i = P2,
                 j = 2) %>%
  # week 3
  add_constraint((1 - a[i,j,1]) + (1 - a[i,j,4]) >= b[i,j,1] - b[i,j,4],
                 i = P3,
                 j = 3) %>%
  # week 4
  add_constraint((1 - a[i,j,1]) + (1 - a[i,j,4]) >= b[i,j,1] - b[i,j,4],
                 i = P4,
                 j = 4) %>%
  
  
  # constraint persamaan 17
  # week 1
  add_constraint((1 - a[i,j,1]) + (1 - a[i,j,4]) >= b[i,j,4] - b[i,j,1],
                 i = P1,
                 j = 1) %>%
  # week 2
  add_constraint((1 - a[i,j,1]) + (1 - a[i,j,4]) >= b[i,j,4] - b[i,j,1],
                 i = P2,
                 j = 2) %>%
  # week 3
  add_constraint((1 - a[i,j,1]) + (1 - a[i,j,4]) >= b[i,j,4] - b[i,j,1],
                 i = P3,
                 j = 3) %>%
  # week 4
  add_constraint((1 - a[i,j,1]) + (1 - a[i,j,4]) >= b[i,j,4] - b[i,j,1],
                 i = P4,
                 j = 4) %>%
  
  
  # constraint persamaan 18
  # week 1
  add_constraint((1 - a[i,j,1]) + (1 - a[i,j,5]) >= b[i,j,1] - b[i,j,5],
                 i = P1,
                 j = 1) %>%
  # week 2
  add_constraint((1 - a[i,j,1]) + (1 - a[i,j,5]) >= b[i,j,1] - b[i,j,5],
                 i = P2,
                 j = 2) %>%
  # week 3
  add_constraint((1 - a[i,j,1]) + (1 - a[i,j,5]) >= b[i,j,1] - b[i,j,5],
                 i = P3,
                 j = 3) %>%
  # week 4
  add_constraint((1 - a[i,j,1]) + (1 - a[i,j,5]) >= b[i,j,1] - b[i,j,5],
                 i = P4,
                 j = 4) %>%
  
  
  # constraint persamaan 19
  # week 1
  add_constraint((1 - a[i,j,1]) + (1 - a[i,j,5]) >= b[i,j,5] - b[i,j,1],
                 i = P1,
                 j = 1) %>%
  # week 2
  add_constraint((1 - a[i,j,1]) + (1 - a[i,j,5]) >= b[i,j,5] - b[i,j,1],
                 i = P2,
                 j = 2) %>%
  # week 3
  add_constraint((1 - a[i,j,1]) + (1 - a[i,j,5]) >= b[i,j,5] - b[i,j,1],
                 i = P3,
                 j = 3) %>%
  # week 4
  add_constraint((1 - a[i,j,1]) + (1 - a[i,j,5]) >= b[i,j,5] - b[i,j,1],
                 i = P4,
                 j = 4) %>%
  
  
  # constraint persamaan 20
  # week 1
  add_constraint((1 - a[i,j,1]) + (1 - a[i,j,6]) >= b[i,j,1] - b[i,j,6],
                 i = P1,
                 j = 1) %>%
  # week 2
  add_constraint((1 - a[i,j,1]) + (1 - a[i,j,6]) >= b[i,j,1] - b[i,j,6],
                 i = P2,
                 j = 2) %>%
  # week 3
  add_constraint((1 - a[i,j,1]) + (1 - a[i,j,6]) >= b[i,j,1] - b[i,j,6],
                 i = P3,
                 j = 3) %>%
  # week 4
  add_constraint((1 - a[i,j,1]) + (1 - a[i,j,6]) >= b[i,j,1] - b[i,j,6],
                 i = P4,
                 j = 4) %>%
  
  
  # constraint persamaan 21
  # week 1
  add_constraint((1 - a[i,j,1]) + (1 - a[i,j,6]) >= b[i,j,6] - b[i,j,1],
                 i = P1,
                 j = 1) %>%
  # week 2
  add_constraint((1 - a[i,j,1]) + (1 - a[i,j,6]) >= b[i,j,6] - b[i,j,1],
                 i = P2,
                 j = 2) %>%
  # week 3
  add_constraint((1 - a[i,j,1]) + (1 - a[i,j,6]) >= b[i,j,6] - b[i,j,1],
                 i = P3,
                 j = 3) %>%
  # week 4
  add_constraint((1 - a[i,j,1]) + (1 - a[i,j,6]) >= b[i,j,6] - b[i,j,1],
                 i = P4,
                 j = 4) %>%
                 
                 
  # constraint persamaan 22
  # week 1
  add_constraint((1 - a[i,j,2]) + (1 - a[i,j,3]) >= b[i,j,2] - b[i,j,3],
                 i = P1,
                 j = 1) %>%
  # week 2
  add_constraint((1 - a[i,j,2]) + (1 - a[i,j,3]) >= b[i,j,2] - b[i,j,3],
                 i = P2,
                 j = 2) %>%
  # week 3
  add_constraint((1 - a[i,j,2]) + (1 - a[i,j,3]) >= b[i,j,2] - b[i,j,3],
                 i = P3,
                 j = 3) %>%
  # week 4
  add_constraint((1 - a[i,j,2]) + (1 - a[i,j,3]) >= b[i,j,2] - b[i,j,3],
                 i = P4,
                 j = 4) %>%
  
  
  # constraint persamaan 23
  # week 1
  add_constraint((1 - a[i,j,2]) + (1 - a[i,j,3]) >= b[i,j,3] - b[i,j,2],
                 i = P1,
                 j = 1) %>%
  # week 2
  add_constraint((1 - a[i,j,2]) + (1 - a[i,j,3]) >= b[i,j,3] - b[i,j,2],
                 i = P2,
                 j = 2) %>%
  # week 3
  add_constraint((1 - a[i,j,2]) + (1 - a[i,j,3]) >= b[i,j,3] - b[i,j,2],
                 i = P3,
                 j = 3) %>%
  # week 4
  add_constraint((1 - a[i,j,2]) + (1 - a[i,j,3]) >= b[i,j,3] - b[i,j,2],
                 i = P4,
                 j = 4) %>%
  
                 
  # constraint persamaan 24
  # week 1
  add_constraint((1 - a[i,j,2]) + (1 - a[i,j,4]) >= b[i,j,2] - b[i,j,4],
                 i = P1,
                 j = 1) %>%
  # week 2
  add_constraint((1 - a[i,j,2]) + (1 - a[i,j,4]) >= b[i,j,2] - b[i,j,4],
                 i = P2,
                 j = 2) %>%
  # week 3
  add_constraint((1 - a[i,j,2]) + (1 - a[i,j,4]) >= b[i,j,2] - b[i,j,4],
                 i = P3,
                 j = 3) %>%
  # week 4
  add_constraint((1 - a[i,j,2]) + (1 - a[i,j,4]) >= b[i,j,2] - b[i,j,4],
                 i = P4,
                 j = 4) %>%
  
  
  # constraint persamaan 25
  # week 1
  add_constraint((1 - a[i,j,2]) + (1 - a[i,j,4]) >= b[i,j,4] - b[i,j,2],
                 i = P1,
                 j = 1) %>%
  # week 2
  add_constraint((1 - a[i,j,2]) + (1 - a[i,j,4]) >= b[i,j,4] - b[i,j,2],
                 i = P2,
                 j = 2) %>%
  # week 3
  add_constraint((1 - a[i,j,2]) + (1 - a[i,j,4]) >= b[i,j,4] - b[i,j,2],
                 i = P3,
                 j = 3) %>%
  # week 4
  add_constraint((1 - a[i,j,2]) + (1 - a[i,j,4]) >= b[i,j,4] - b[i,j,2],
                 i = P4,
                 j = 4) %>%
  
  
  # constraint persamaan 26
  # week 1
  add_constraint((1 - a[i,j,2]) + (1 - a[i,j,5]) >= b[i,j,2] - b[i,j,5],
                 i = P1,
                 j = 1) %>%
  # week 2
  add_constraint((1 - a[i,j,2]) + (1 - a[i,j,5]) >= b[i,j,2] - b[i,j,5],
                 i = P2,
                 j = 2) %>%
  # week 3
  add_constraint((1 - a[i,j,2]) + (1 - a[i,j,5]) >= b[i,j,2] - b[i,j,5],
                 i = P3,
                 j = 3) %>%
  # week 4
  add_constraint((1 - a[i,j,2]) + (1 - a[i,j,5]) >= b[i,j,2] - b[i,j,5],
                 i = P4,
                 j = 4) %>%
  
  
  # constraint persamaan 27
  # week 1
  add_constraint((1 - a[i,j,2]) + (1 - a[i,j,5]) >= b[i,j,5] - b[i,j,2],
                 i = P1,
                 j = 1) %>%
  # week 2
  add_constraint((1 - a[i,j,2]) + (1 - a[i,j,5]) >= b[i,j,5] - b[i,j,2],
                 i = P2,
                 j = 2) %>%
  # week 3
  add_constraint((1 - a[i,j,2]) + (1 - a[i,j,5]) >= b[i,j,5] - b[i,j,2],
                 i = P3,
                 j = 3) %>%
  # week 4
  add_constraint((1 - a[i,j,2]) + (1 - a[i,j,5]) >= b[i,j,5] - b[i,j,2],
                 i = P4,
                 j = 4) %>%
                 
                 
  # constraint persamaan 28
  # week 1
  add_constraint((1 - a[i,j,2]) + (1 - a[i,j,6]) >= b[i,j,2] - b[i,j,6],
                 i = P1,
                 j = 1) %>%
  # week 2
  add_constraint((1 - a[i,j,2]) + (1 - a[i,j,6]) >= b[i,j,2] - b[i,j,6],
                 i = P2,
                 j = 2) %>%
  # week 3
  add_constraint((1 - a[i,j,2]) + (1 - a[i,j,6]) >= b[i,j,2] - b[i,j,6],
                 i = P3,
                 j = 3) %>%
  # week 4
  add_constraint((1 - a[i,j,2]) + (1 - a[i,j,6]) >= b[i,j,2] - b[i,j,6],
                 i = P4,
                 j = 4) %>%
                 
                 
  # constraint persamaan 29
  # week 1
  add_constraint((1 - a[i,j,2]) + (1 - a[i,j,6]) >= b[i,j,6] - b[i,j,2],
                 i = P1,
                 j = 1) %>%
  # week 2
  add_constraint((1 - a[i,j,2]) + (1 - a[i,j,6]) >= b[i,j,6] - b[i,j,2],
                 i = P2,
                 j = 2) %>%
  # week 3
  add_constraint((1 - a[i,j,2]) + (1 - a[i,j,6]) >= b[i,j,6] - b[i,j,2],
                 i = P3,
                 j = 3) %>%
  # week 4
  add_constraint((1 - a[i,j,2]) + (1 - a[i,j,6]) >= b[i,j,6] - b[i,j,2],
                 i = P4,
                 j = 4) %>%               
  
  
  # constraint persamaan 30
  # week 1
  add_constraint((1 - a[i,j,3]) + (1 - a[i,j,4]) >= b[i,j,3] - b[i,j,4],
                 i = P1,
                 j = 1) %>%
  # week 2
  add_constraint((1 - a[i,j,3]) + (1 - a[i,j,4]) >= b[i,j,3] - b[i,j,4],
                 i = P2,
                 j = 2) %>%
  # week 3
  add_constraint((1 - a[i,j,3]) + (1 - a[i,j,4]) >= b[i,j,3] - b[i,j,4],
                 i = P3,
                 j = 3) %>%
  # week 4
  add_constraint((1 - a[i,j,3]) + (1 - a[i,j,4]) >= b[i,j,3] - b[i,j,4],
                 i = P4,
                 j = 4) %>%        
  
  
  # constraint persamaan 31
  # week 1
  add_constraint((1 - a[i,j,3]) + (1 - a[i,j,4]) >= b[i,j,4] - b[i,j,3],
                 i = P1,
                 j = 1) %>%
  # week 2
  add_constraint((1 - a[i,j,3]) + (1 - a[i,j,4]) >= b[i,j,4] - b[i,j,3],
                 i = P2,
                 j = 2) %>%
  # week 3
  add_constraint((1 - a[i,j,3]) + (1 - a[i,j,4]) >= b[i,j,4] - b[i,j,3],
                 i = P3,
                 j = 3) %>%
  # week 4
  add_constraint((1 - a[i,j,3]) + (1 - a[i,j,4]) >= b[i,j,4] - b[i,j,3],
                 i = P4,
                 j = 4) %>%    
  
  
  
  # constraint persamaan 32
  # week 1
  add_constraint((1 - a[i,j,3]) + (1 - a[i,j,5]) >= b[i,j,3] - b[i,j,5],
                 i = P1,
                 j = 1) %>%
  # week 2
  add_constraint((1 - a[i,j,3]) + (1 - a[i,j,5]) >= b[i,j,3] - b[i,j,5],
                 i = P2,
                 j = 2) %>%
  # week 3
  add_constraint((1 - a[i,j,3]) + (1 - a[i,j,5]) >= b[i,j,3] - b[i,j,5],
                 i = P3,
                 j = 3) %>%
  # week 4
  add_constraint((1 - a[i,j,3]) + (1 - a[i,j,5]) >= b[i,j,3] - b[i,j,5],
                 i = P4,
                 j = 4) %>%    
  
  
  # constraint persamaan 33
  # week 1
  add_constraint((1 - a[i,j,3]) + (1 - a[i,j,5]) >= b[i,j,5] - b[i,j,3],
                 i = P1,
                 j = 1) %>%
  # week 2
  add_constraint((1 - a[i,j,3]) + (1 - a[i,j,5]) >= b[i,j,5] - b[i,j,3],
                 i = P2,
                 j = 2) %>%
  # week 3
  add_constraint((1 - a[i,j,3]) + (1 - a[i,j,5]) >= b[i,j,5] - b[i,j,3],
                 i = P3,
                 j = 3) %>%
  # week 4
  add_constraint((1 - a[i,j,3]) + (1 - a[i,j,5]) >= b[i,j,5] - b[i,j,3],
                 i = P4,
                 j = 4) %>%    
  
  
  # constraint persamaan 34
  # week 1
  add_constraint((1 - a[i,j,3]) + (1 - a[i,j,6]) >= b[i,j,3] - b[i,j,6],
                 i = P1,
                 j = 1) %>%
  # week 2
  add_constraint((1 - a[i,j,3]) + (1 - a[i,j,6]) >= b[i,j,3] - b[i,j,6],
                 i = P2,
                 j = 2) %>%
  # week 3
  add_constraint((1 - a[i,j,3]) + (1 - a[i,j,6]) >= b[i,j,3] - b[i,j,6],
                 i = P3,
                 j = 3) %>%
  # week 4
  add_constraint((1 - a[i,j,3]) + (1 - a[i,j,6]) >= b[i,j,3] - b[i,j,6],
                 i = P4,
                 j = 4) %>%    
  
  
  # constraint persamaan 35
  # week 1
  add_constraint((1 - a[i,j,3]) + (1 - a[i,j,6]) >= b[i,j,6] - b[i,j,3],
                 i = P1,
                 j = 1) %>%
  # week 2
  add_constraint((1 - a[i,j,3]) + (1 - a[i,j,6]) >= b[i,j,6] - b[i,j,3],
                 i = P2,
                 j = 2) %>%
  # week 3
  add_constraint((1 - a[i,j,3]) + (1 - a[i,j,6]) >= b[i,j,6] - b[i,j,3],
                 i = P3,
                 j = 3) %>%
  # week 4
  add_constraint((1 - a[i,j,3]) + (1 - a[i,j,6]) >= b[i,j,6] - b[i,j,3],
                 i = P4,
                 j = 4) %>% 
  
  
  # constraint persamaan 36
  # week 1
  add_constraint((1 - a[i,j,4]) + (1 - a[i,j,5]) >= b[i,j,4] - b[i,j,5],
                 i = P1,
                 j = 1) %>%
  # week 2
  add_constraint((1 - a[i,j,4]) + (1 - a[i,j,5]) >= b[i,j,4] - b[i,j,5],
                 i = P2,
                 j = 2) %>%
  # week 3
  add_constraint((1 - a[i,j,4]) + (1 - a[i,j,5]) >= b[i,j,4] - b[i,j,5],
                 i = P3,
                 j = 3) %>%
  # week 4
  add_constraint((1 - a[i,j,4]) + (1 - a[i,j,5]) >= b[i,j,4] - b[i,j,5],
                 i = P4,
                 j = 4) %>% 
                 
                 
  # constraint persamaan 37
  # week 1
  add_constraint((1 - a[i,j,4]) + (1 - a[i,j,5]) >= b[i,j,5] - b[i,j,4],
                 i = P1,
                 j = 1) %>%
  # week 2
  add_constraint((1 - a[i,j,4]) + (1 - a[i,j,5]) >= b[i,j,5] - b[i,j,4],
                 i = P2,
                 j = 2) %>%
  # week 3
  add_constraint((1 - a[i,j,4]) + (1 - a[i,j,5]) >= b[i,j,5] - b[i,j,4],
                 i = P3,
                 j = 3) %>%
  # week 4
  add_constraint((1 - a[i,j,4]) + (1 - a[i,j,5]) >= b[i,j,5] - b[i,j,4],
                 i = P4,
                 j = 4) %>%               
                 
                 
  # constraint persamaan 38
  # week 1
  add_constraint((1 - a[i,j,4]) + (1 - a[i,j,6]) >= b[i,j,4] - b[i,j,6],
                 i = P1,
                 j = 1) %>%
  # week 2
  add_constraint((1 - a[i,j,4]) + (1 - a[i,j,6]) >= b[i,j,4] - b[i,j,6],
                 i = P2,
                 j = 2) %>%
  # week 3
  add_constraint((1 - a[i,j,4]) + (1 - a[i,j,6]) >= b[i,j,4] - b[i,j,6],
                 i = P3,
                 j = 3) %>%
  # week 4
  add_constraint((1 - a[i,j,4]) + (1 - a[i,j,6]) >= b[i,j,4] - b[i,j,6],
                 i = P4,
                 j = 4) %>%  
  
  
  # constraint persamaan 39
  # week 1
  add_constraint((1 - a[i,j,4]) + (1 - a[i,j,6]) >= b[i,j,6] - b[i,j,4],
                 i = P1,
                 j = 1) %>%
  # week 2
  add_constraint((1 - a[i,j,4]) + (1 - a[i,j,6]) >= b[i,j,6] - b[i,j,4],
                 i = P2,
                 j = 2) %>%
  # week 3
  add_constraint((1 - a[i,j,4]) + (1 - a[i,j,6]) >= b[i,j,6] - b[i,j,4],
                 i = P3,
                 j = 3) %>%
  # week 4
  add_constraint((1 - a[i,j,4]) + (1 - a[i,j,6]) >= b[i,j,6] - b[i,j,4],
                 i = P4,
                 j = 4) %>%  
  
  
  # constraint persamaan 40
  # week 1
  add_constraint((1 - a[i,j,5]) + (1 - a[i,j,6]) >= b[i,j,5] - b[i,j,6],
                 i = P1,
                 j = 1) %>%
  # week 2
  add_constraint((1 - a[i,j,5]) + (1 - a[i,j,6]) >= b[i,j,5] - b[i,j,6],
                 i = P2,
                 j = 2) %>%
  # week 3
  add_constraint((1 - a[i,j,5]) + (1 - a[i,j,6]) >= b[i,j,5] - b[i,j,6],
                 i = P3,
                 j = 3) %>%
  # week 4
  add_constraint((1 - a[i,j,5]) + (1 - a[i,j,6]) >= b[i,j,5] - b[i,j,6],
                 i = P4,
                 j = 4) %>%  
  
  
  # constraint persamaan 41
  # week 1
  add_constraint((1 - a[i,j,5]) + (1 - a[i,j,6]) >= b[i,j,6] - b[i,j,5],
                 i = P1,
                 j = 1) %>%
  # week 2
  add_constraint((1 - a[i,j,5]) + (1 - a[i,j,6]) >= b[i,j,6] - b[i,j,5],
                 i = P2,
                 j = 2) %>%
  # week 3
  add_constraint((1 - a[i,j,5]) + (1 - a[i,j,6]) >= b[i,j,6] - b[i,j,5],
                 i = P3,
                 j = 3) %>%
  # week 4
  add_constraint((1 - a[i,j,5]) + (1 - a[i,j,6]) >= b[i,j,6] - b[i,j,5],
                 i = P4,
                 j = 4) %>% 
  
  
  # ============================================================================
  # constraint VI 
  # week 1
  add_constraint(sum_expr(b[i,j,k],
                          k = G) == 1,
                 j = 1,
                 i = P1) %>% 
  # week 2
  add_constraint(sum_expr(b[i,j,k],
                          k = G) == 1,
                 j = 2,
                 i = P2) %>% 
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
               
  
  # ============================================================================
  # kita keluarkan angka kebutuhan
  add_variable(butuh[j,k],type = "continuous",lb = 0,
             j = M,
             k = G) %>% 
  
  # kebutuhan pada week 1
  add_constraint(butuh[j,k] == sum_expr(b[i,j,k] * matt_g_ijk[i,j,k],
                                        i = P1),
                 j = 1,
                 k = G) %>% 
  # kebutuhan pada week 2
  add_constraint(butuh[j,k] == sum_expr(b[i,j,k] * matt_g_ijk[i,j,k],
                                        i = P2),
                 j = 2,
                 k = G) %>% 
  # kebutuhan pada week 3
  add_constraint(butuh[j,k] == sum_expr(b[i,j,k] * matt_g_ijk[i,j,k],
                                        i = P3),
                 j = 3,
                 k = G) %>% 
  # kebutuhan pada week 4
  add_constraint(butuh[j,k] == sum_expr(b[i,j,k] * matt_g_ijk[i,j,k],
                                        i = P4),
                 j = 4,
                 k = G) %>% 
  
  # ============================================================================
  # constraint VII
  # kita akan modifikasi constraint 7 dari Bu Rieske ke bentuk lain
   # yakni mengubah menjadi definisi z_jk lalu jumlahnya gak boleh lebih dari maxcap
  
  # bagian definisi z_jk
  # ini untuk menghitung saldo pada week 1 
  add_constraint(z[j,k] >= Z_0k[k] + x_hat[j,k] - butuh[j,k],
                 j = 1,
                 k = G) %>% 
  
  # ini untuk menghitung saldo pada week 2 - 4
  add_constraint(z[j,k] == z[(j-1),k] + x_hat[j,k] - butuh[j,k],
                 j = 2:4,
                 k = G) %>% 
  
  # ============================================================================
  # yang dikirim harus lebih banyak dari yang dibutuhkan
  add_variable(x_hat_stb[j,k],type = "continuous",lb = 0,
               j = M,
               k = G) %>%
  
  add_variable(x_stb[k],type = "continuous",lb = 0,
               k = G) %>%
  
  # week 1
  add_constraint(x_hat_stb[j,k] == z[j,k] - Z_0k[k] + butuh[j,k],
                 j = 1,
                 k = G) %>% 
  
  # week 2:4
  add_constraint(x_hat_stb[j,k] == z[j,k] - z[(j-1),k] + butuh[j,k],
                 j = 2:4,
                 k = G) %>% 
  # total
  add_constraint(sum_expr(x_hat_stb[j,k],
                          j = M) == x_stb[k],
                 k = G) %>% 
  
  # bagian constraint max cap
  # ini agar si z[j,k] gak lebih dari maxcap
  add_constraint(sum_expr(z[j,k],
                          k = G) <= maxcap,
                 j = M) %>% 
  
  # bagian constraint safety stok
  # ini agar si z[j,k] gak kurang dari safety stok
  add_constraint(z[j,k] >= ss,
                 k = G,
                 j = M) %>% 
  
  # ============================================================================
  # modifikasi untuk menambahkan cost ke dalam objective function
  add_variable(cost_w1[k],type = "continuous",lb = 0,k = G) %>%
  add_constraint(cost_w1[k] >= z[j,k] * c_k[k],j = 1,k = G) %>% 
  
  
  # week 2-4
  add_variable(tot_w24[k],type = "continuous",lb = 0,k = G) %>%
  add_constraint(tot_w24[k] == 0.5 * ic * sum_expr(z[j,k] + z[j-1,k] + x_hat[j,k],
                                              j = 2:4),
                 k = G) %>% 
  
  # objective function
  set_objective(sum_expr((c_k[k] * x[k]) + cost_w1[k] + tot_w24[k],# + tot_w1[k]  - yearly[k]
                         k = G),
                "min")
               
  
# solver
result = milp_new %>% solve_model(with_ROI("glpk", verbose = TRUE))
nama_file_output = "output fungsi ketiga demo video.xlsx"
source("3 export hasil ke excel.R")  

toc()
