rm(list=ls())

library(dplyr)
set.seed(10104074)

# berapa banyak variabel
n_var = 2

# function
f = function(vec){
  x = vec[1]
  y = vec[2]
  f1 = x^2 + 2 * y^2 - 54
  f2 = 2*x - y + 9
  F_ = 1 + f1^2 + f2^2
  return(1 / F_)
}

# big bang
big_bang = function(){
  runif(n_var,-10,10)
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

# bikin matriks rotasinya
A_rot = buat_rot_mat(2*pi/7,n_var)

# kita mulai spiralnya
N_spiral = 1200
id_calon = 1:N_spiral
# siapin rumahnya dulu
calon = vector("list",N_spiral)
f_hit = c()
# perhitungan pertama
for(i in 1:N_spiral){
  calon[[i]] = big_bang()
  f_hit[i] = f(calon[[i]])
}

iterasi_lanjut = 1

while(iterasi_lanjut <= 200){
  # penentuan calon paling minimum
  id_min = id_calon[which(f_hit == max(f_hit))] %>% min()
  iterasi_lanjut = id_calon[which(f_hit == max(f_hit))] %>% length()
  pusat = calon[[id_min]]
  
  # proses rotasi semua calon
  for(i in 1:N_spiral){
    Xt = calon[[i]]
    X = A_rot %*% (Xt - pusat)
    X = pusat + (.7 * X)
    calon[[i]] = X 
    f_hit[i] = f(X)
  }
  
  print(iterasi_lanjut)
}

id_min = id_calon[which(f_hit == max(f_hit))] %>% min()
hasil_final = calon[[id_min]]
hasil_final
f(hasil_final)
#save(hasil_final,file = "hasil_spiral.rda")