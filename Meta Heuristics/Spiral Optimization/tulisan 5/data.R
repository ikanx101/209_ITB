rm(list=ls())

library(dplyr)
library(TSP)
library(leaflet)

set.seed(10104074)

# save file dulu
load("~/S2 Sains Komputasi ITB/CASES/Potensi Semarang/Tahap II/super_cleaned.rda")
data = data_bank %>% filter(kota_kab == "Kota Semarang")
write.csv(data,"bank.csv",row.names = F)

# ========================================
# bikin matriks jarak euclidean
# function hitung jarak
jarak = function(a,b){
  x = data$lat[a] - data$lat[b]
  x = x^2
  y = data$lng[a] - data$lng[b]
  y = y^2
  r = sqrt(x + y)
  return(r)
}
# bikin matriks jarak
n_bank = nrow(data)
mat_jarak = matrix(0,nrow = n_bank,ncol = n_bank)
for(i in 1:n_bank){
  for(j in 1:n_bank){
    mat_jarak[i,j] = jarak(i,j)
  }
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
A_rot = buat_rot_mat(2*pi/50,n_bank)

# objective function
obj_fun = function(tes){
  tes = round(tes)
  # untuk keperluan looping
  unique_cluster = sort(unique(tes))
  n_unique = length(unique_cluster)
  # menyiapkan rumah
  hasil_tour = rep(0,n_unique)
  id_hasil = vector("list",n_unique)
  for(cl_i in 1:n_unique){
    id = which(tes == unique_cluster[cl_i])
    if(length(id) > 4){
      temp = mat_jarak[id,id]
      problem = as.TSP(temp)
      hasil = solve_TSP(problem)
      tour = tour_length(hasil)
      # agar tidak jauh2
      tour = ifelse(tour > .20,9000,tour)
      hasil_tour[cl_i] = tour
      id_hasil[[cl_i]] = id
    }
    else if(length(id) <= 4){ # kalau ada cluster yang hanya berisi 1, kita hukum
      hasil_tour[cl_i] = 10000    # hukuman
      id_hasil[[cl_i]] = id
    }
  }
  output = list("id_cluster" = id_hasil,
                "tour_total" = sum(hasil_tour),
                "tour" = hasil_tour)
  return(output)
}

# berapa banyak cluster
n_cluster = 5

# kita mulai spiralnya
N_spiral = 1200
id_calon = 1:N_spiral
# siapin rumahnya dulu
calon = vector("list",N_spiral)
save = vector("list",N_spiral)
f_hit = c()
# perhitungan pertama
for(i in 1:N_spiral){
  calon[[i]] = runif(n_bank,0.55,n_cluster)
  save[[i]] = obj_fun(calon[[i]])
  f_hit[i] = save[[i]]$tour_total
}

iterasi_lanjut = 1

while(iterasi_lanjut <= 200){
  # penentuan calon paling minimum
  id_min = id_calon[which(f_hit == min(f_hit))] %>% min()
  iterasi_lanjut = id_calon[which(f_hit == min(f_hit))] %>% length()
  pusat = calon[[id_min]]
  
  # proses rotasi semua calon
  for(i in 1:N_spiral){
    Xt = calon[[i]]
    X = A_rot %*% (Xt - pusat)
    X = pusat + (.9 * X)
    calon[[i]] = abs(X) 
    save[[i]] = obj_fun(calon[[i]] %>% round())
    f_hit[i] = save[[i]]$tour_total
  }
  
  print(iterasi_lanjut)
}

id_min = id_calon[which(f_hit == min(f_hit))] %>% min()
hasil_final = save[[id_min]]
hasil_final
save(hasil_final,file = "hasil_spiral.rda")