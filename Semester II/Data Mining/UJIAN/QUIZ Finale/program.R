rm(list=ls())

library(dplyr)

# data dari soal
df = data.frame(
  titik = paste0("p",1:10),
  x = c(4,2.1,3.4,2.7,.8,4.6,4.3,2.2,4.1,1.5),
  y = c(5.2,3.9,3.1,2,4.1,2.9,1.2,1,4.1,3)
)

# program untuk membuat titik sentroid secara random
random_titik = function(){
  list(
    sentroid_1 = runif(2,0,6),
    sentroid_2 = runif(2,0,6)
  )
}

# program untuk menghitung jarak
jarak = function(x1,x2){
  sb_1 = (x1[1] - x2[1])^2
  sb_2 = (x1[2] - x2[2])^2
  sqrt(sb_1 + sb_2)
}

# program untuk menghitung sentroid baru
new_sentroid = function(data){
  hit = 
    data %>% 
    group_by(cluster_no) %>% 
    summarise(x = mean(x),
              y = mean(y)) %>% 
    ungroup()
  output = list(sentroid_1 = c(hit$x[1],hit$y[1]),
                sentroid_2 = c(hit$x[2],hit$y[2]))
  return(output)
}

# program untuk menghitung selisih sentroid baru dengan sentroid lama
konvergen_yn = function(){
  part1 = sentroid_baru$sentroid_1 - sentroid_1
  part2 = sentroid_baru$sentroid_2 - sentroid_2
  sqrt(sum(part1^2) + sum(part2^2))
}

# program untuk menghitung SSE
hitung_SSE = function(df){
  # hitung jarak terhadap sentroid
  for(i in 1:nrow(df)){
    titik = c(df$x[i],df$y[i])
    df$jarak_sentroid1[i] = jarak(titik,sentroid_1)
    df$jarak_sentroid2[i] = jarak(titik,sentroid_2)
  }
  SSE_final = 
    df %>% 
    mutate(jarak_thd_centroid = ifelse(cluster_no == 1,
                                       jarak_sentroid1,
                                       jarak_sentroid2))
  SSE_final$jarak_thd_centroid^2 %>% sum() %>% round(4)
}

# program untuk menghitung silhouette coefficient
sil_coeff = function(df){
  # menghitung distance matrix
  tes = 
    df %>% 
    select(x,y)
  mat_dist = dist(tes,upper = T) %>% as.matrix()
  # mengambil id titik per cluster
  id_cl_1 = which(df_1$cluster_no == 1)
  id_cl_2 = which(df_1$cluster_no == 2)
  # menghitung nilai a
  a1 = mat_dist[id_cl_1,id_cl_1] %>% mean()
  a2 = mat_dist[id_cl_2,id_cl_2] %>% mean()
  a = mean(a1,a2)
  # menghitung nilai b
  b = rep(NA,10)
  for(i in 1:10){
    if(i %in% id_cl_1){
      b[i] = mat_dist[i,id_cl_2] %>% mean()
    } else
      if(i %in% id_cl_2){
        b[i] = mat_dist[i,id_cl_1] %>% mean()
      }
  }
  b = min(b)
  # menghitung silhouette coefficient
  s_coeff = (b-a)/max(a,b)
  return(s_coeff)
}


# =============================================================================
# k means cluster di mulai
random = random_titik()

# initial sentroid
sentroid_1 = random$sentroid_1
sentroid_2 = random$sentroid_2
# menyiapkan template untuk menghitung jarak
df$jarak_sentroid1 = NA
df$jarak_sentroid2 = NA
# initial konvergensi
konvergensi = 1000
# proses iterasi k-means hingga konvergensi tercapai
while(konvergensi > 10^(-7)){
  # hitung jarak terhadap sentroid
  for(i in 1:nrow(df)){
    titik = c(df$x[i],df$y[i])
    df$jarak_sentroid1[i] = jarak(titik,sentroid_1)
    df$jarak_sentroid2[i] = jarak(titik,sentroid_2)
  }
  # memasukkan masing-masing titik ke cluster terdekat
  df = 
    df %>% 
    mutate(cluster_no = ifelse(jarak_sentroid1 < jarak_sentroid2,1,2))
  # menghitung sentroid baru
  sentroid_baru = new_sentroid(df)
  # menghitung konvergensi
  konvergensi = konvergen_yn()
  # update sentroid baru
  sentroid_1 = sentroid_baru$sentroid_1
  sentroid_2 = sentroid_baru$sentroid_2
}

# print hasil akhir
df
# sentroid 1
sentroid_1
# sentroid 2
sentroid_2