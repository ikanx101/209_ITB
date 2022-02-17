rm(list=ls())
setwd("~/209_ITB/Meta Heuristics/Spiral Optimization/tulisan 3")

library(dplyr)
library(ggplot2)

data = read.csv("crime.csv") %>% janitor::clean_names()
colnames(data) = c("id","percent_m", "is_south", "mean_education", "police_exp60", 
                   "police_exp59", "labour_participation", "m_per1000f", "state_pop", 
                   "nonwhites_per1000", "unemploy_m24", "unemploy_m39", "gdp", "inequality", 
                   "prob_prison", "time_prison", "crime_rate")

data

# =================================================================
target = data$inequality
data = data %>% select(-id,-inequality)
nama_var = colnames(data)


# =================================================================
# star randomizer
N = 800
star = vector("list",N)

for(i in 1:N){
  calon = runif(15,0,1)
  star[[i]] = calon
}

# =================================================================
# set obj function
obj_funct = function(list){
  bound = round(list,0)
  predictor = nama_var[bound == 1]
  df_reg = data[predictor]
  df_reg$target = target
  
  model = lm(target ~ .,df_reg)
  hasil = summary(model)
  r_sq = hasil$adj.r.squared
  return(r_sq)
}

# set constraint 1
batas1 = function(list){
  bound = round(list,0)
  sqrt(sum(bound^2)) - sqrt(15)
}

# set constraint 2
batas2 = function(list){
  bound = round(list,0)
  1 - sqrt(sum(bound^2))
}

beta = 9999999

# function keseluruhan
f_tot = function(list){
  - obj_funct(list) + max((beta * batas1(list)),0) + max((beta * batas2(list)),0)
}

# function matriks rotasi
buat_rot_mat = function(theta,n){
  # buat template sebuah matriks identitas
  temp_mat = matrix(0,ncol = n,nrow = n)
  diag(temp_mat) = 1
  
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

mat_rotasi = buat_rot_mat(2*pi / 60,15)

# initial condition
f_hit = c()

# perhitungan obj function
for (i in 1:N){
  temp = f_tot(star[[i]])
  f_hit[i] = temp
}


# mulai part serunya
for(ikanx in 1:60){
  # penentuan black hole
  n_bhole = which.min(f_hit)
  bhole = star[[n_bhole]]
  
  for(i in 1:N){
    Xt = star[[i]]
    X = mat_rotasi %*% (Xt - bhole)
    X = bhole + (.7 * X)
    star[[i]] = X
  }
  
  # perhitungan obj function
  for (i in 1:N){
    temp = f_tot(star[[i]])
    f_hit[i] = temp
  }
  
  print(min(f_hit))

}

print(which.min(f_hit))
hasil = which.min(f_hit)
pilihan = star[[hasil]] %>% round()
obj_funct(pilihan)
nama_var[pilihan == 1]
