setwd("~/209_ITB/Meta Heuristics/Spiral Optimization/tulisan 2")

rm(list=ls())

library(dplyr)

# kita akan buat fuction untuk R
# matriks rotasi

# hitung theta
rot = 10
theta = 2*pi / rot
# definisi matriks rotasi
R12 = matrix(c(cos(theta),-sin(theta),0,
               sin(theta),cos(theta),0,
               0,0,1),
             ncol = 3,byrow = T)
R13 = matrix(c(cos(theta),0,-sin(theta),
               0,1,0,
               sin(theta),0,cos(theta)),
             ncol = 3,byrow = T)
R23 = matrix(c(1,0,0,
               0,cos(theta),-sin(theta),
               0,sin(theta),cos(theta)),
             ncol = 3,byrow = T)


mat_rotasi = R23 %*% R13 %*% R12 
mat_rotasi =  mat_rotasi %>% round(4)

# input banyak variabel
n = 3
temp_mat = matrix(0,ncol = n,nrow = n)
diag(temp_mat) = 1
mat_rot = temp_mat

for(i in 1:(n-1)){
  for(j in 1:i){
    temp = temp_mat
    idx = n-i
    idy = n+1-j
    #print(paste0("Matriks rotasi untuk ",idx," - ",idy,": DONE"))
    temp[idx,idx] = cos(theta)
    temp[idx,idy] = -sin(theta)
    temp[idy,idx] = sin(theta)
    temp[idy,idy] = cos(theta)
    # assign(paste0("M",idx,idy),temp)
    mat_rot = mat_rot %*% temp
    mat_rot = mat_rot %>% round(4)
  }
}


mat_rot == mat_rotasi

v = c(3,4,5)

norma_vec = function(x){
  x = x^2
  x = sum(x)
  return(sqrt(x))
}

norma_vec(v)

x1 = mat_rot %*% v
norma_vec(x1)

x2 = mat_rotasi %*% v
norma_vec(x2)
