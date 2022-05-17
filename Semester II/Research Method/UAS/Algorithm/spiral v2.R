setwd("~/209_ITB/Semester II/Research Method/UAS")

# sucikan hati dan diri
rm(list=ls())

# libraries
library(dplyr)
library(tidyr)
library(readxl)

# import data
load("bahan.rda")

# kita buat utk df1
df_hit = df10
n = nrow(df_hit)

# function matriks rotasi
buat_rot_mat = function(kali,n){
  # buat template sebuah matriks identitas
  temp_mat = matrix(0,ncol = n,nrow = n)
  diag(temp_mat) = 1
  
  theta = kali / 360
  # buat matriks identitas terlebih dahulu
  mat_rot = temp_mat
  
  for(i in 1:(n-1)){
    for(j in 1:i){
      temp = temp_mat
      idx = n-i
      idy = n+1-j
      # print(paste0("Matriks rotasi untuk ",idx," - ",idy,": DONE"))
      temp[idx,idx] = cos(theta)
      temp[idx,idy] = -sin(theta)
      temp[idy,idx] = sin(theta)
      temp[idy,idy] = cos(theta)
      # assign(paste0("M",idx,idy),temp)
      mat_rot = mat_rot %*% temp
      mat_rot = mat_rot 
    }
  }
  return(mat_rot)
}

# membuat matriks rotasi
mat_rot = buat_rot_mat(50,n)

# sekarang kita buat function big bang nya terlebih dahulu
big_bang = function(){
  stars = sample(c(0,1),n,replace = T)
  return(stars)
}


# sekarang bikin objective function nya terlebih dahulu
evaluation_func = function(randomin){
  df_hit$pilih = randomin
  finalista = 
    df_hit %>% 
    filter(pilih == 1) %>% 
    summarise(budget = sum(burn_3m),
              omset = sum(cost_benefit))
  
  beta = 10^25
  constraint = finalista$budget - 5000000
  obj = -finalista$omset
  output = obj + beta * (max(0,constraint))^2
  return(output)
}


n_stars = 200 # berapa banyak calon solusi
stars_home = vector("list",n_stars)
for(i in 1:n_stars){
  stars_home[[i]] = big_bang()
}

# =========================
# initial condition
f_hit = rep(NA,n_stars)

for(i in 1:n_stars){
  f_hit[i] = evaluation_func(stars_home[[i]])
}


# sekarang kita hitung dengan rotasi pertama

for(ikanx in 1:70){# melakukan SDOA sebanyak xx kali rotasi dan konstraksi
  # penentuan calon dengan nilai paling minimum
  n_bhole = which.min(f_hit)
  bhole = stars_home[[n_bhole]]
  
  # melakukan rotasi dan konstraksi
  for(i in 1:n_stars){
    Xt = stars_home[[i]]
    X = mat_rot %*% (Xt - bhole)
    X = bhole + (.85 * X) %>% round() %>% abs()
    X = ifelse(X>=1,1,0)
    stars_home[[i]] = X
  }
  
  # perhitungan obj function kembali
  for (i in 1:n_stars){
    temp = evaluation_func(stars_home[[i]])
    f_hit[i] = temp
  }
}


hasil = which.min(f_hit)
pilihan = stars_home[[hasil]] %>% round() %>% as.numeric()

# nilai R squared terbaik
df_hit$pilih_spiral = pilihan
finalista = 
  df_hit %>% 
  filter(pilih_spiral == 1) %>% 
  summarise(budget = sum(burn_3m),
            omset = sum(cost_benefit))


# ============================================================================
library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)

model = 
  MIPModel() %>%
  add_variable(x[i],
               i = 1:n, 
               type = "binary",
               lb = 0) %>%
  set_objective(sum_over(x[i] * df_hit$cost_benefit[i], i = 1:n),
                "max") %>%
  add_constraint(sum_over(x[i] * df_hit$burn_3m[i], i = 1:n) <= 5000000)
result = model %>% solve_model(with_ROI(solver = "glpk"))
hasil_ompr = get_solution(result, x[i])
df_hit$pilih_ompr = hasil_ompr$value

# perhitungan eksak
df_hit %>% 
  filter(pilih_ompr == 1) %>% 
  summarise(budget = sum(burn_3m),
            omset = sum(cost_benefit))

# perhitungan spiral
finalista


# =========================================
# kita akan gunakan simulasi
n_sim = 100
budget_simu = rep(NA,n_sim)
revenue_simu = rep(NA,n_sim)
pilihan_simu = vector("list",n_sim)

for(ikanx in 1:n_sim){
  pilih_temp = big_bang()
  simu_liu = 
    df_hit %>% 
    mutate(pilih_sim = pilih_temp) %>% 
    filter(pilih_sim == 1) %>% 
    summarise(budget = sum(burn_3m),
              omset = sum(cost_benefit))
  if(simu_liu$budget <= 5000000){
    budget_simu[ikanx] = simu_liu$budget
    revenue_simu[ikanx] = simu_liu$omset
    pilihan_simu[[ikanx]] = pilih_temp
  }
}

n_max = which.max(revenue_simu)
budget_simu[n_max]
revenue_simu[n_max]

df_hit$pilih_simulasi = pilihan_simu[[n_max]]

table(df_hit$pilih_spiral)
table(df_hit$pilih_ompr)
table(df_hit$pilih_simulasi)
table(df_hit$pilih_spiral,
      df_hit$pilih_ompr)

# save hasilnya
save(df_hit,file = "dataset_10.rda")