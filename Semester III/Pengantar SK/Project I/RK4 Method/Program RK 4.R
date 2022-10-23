# ==============================================================================
# Program Runge Kutta 4th Order
#
# Project I - Pengantar Sains Komputasi
#
# Mohammad Rizka Fadhli - 20921004
#
# ==============================================================================

# membersihkan global environment
rm(list=ls())

# INPUT dari user:
  # rate peluruhan A dan pertumbuhan B
  r_a = readline(prompt = "Rate peluruhan A: ") %>% as.numeric()
  # rate peluruhan B dan pertumbuhan C
  r_b = readline(prompt = "Rate peluruhan B: ") %>% as.numeric()
  # rate peluruhan C
  r_c = readline(prompt = "Rate peluruhan C: ") %>% as.numeric()
  # massa awal zat radioaktif A
  q_a = readline(prompt = "massa awal zat radioaktif A: ") %>% as.numeric()
  # massa awal zat radioaktif B
  q_b = readline(prompt = "massa awal zat radioaktif B: ") %>% as.numeric()
  # massa awal zat radioaktif C
  q_c = readline(prompt = "massa awal zat radioaktif C: ") %>% as.numeric()
  # panjang iterasi
  iter_length = readline(prompt = "seberapa panjang iterasi dilakukan: ") %>% 
                as.numeric()
  # h
  h = readline(prompt = "selang h: ") %>% as.numeric()
  
# initial condition
t   = 0   # waktu awal t = 0

# kalkulasi banyak iterasi yang dilakukan
num_iter = (iter_length / h) + 1

# definisi fungsi persamaan diferensial
  # zat radio aktif A
  d_a = function(t,q_a){
    (-r_a * q_a) 
  }
  # zat radio aktif B
  d_b = function(t,q_a,q_b){
    ((r_a * q_a) - (r_b * q_b)) 
  }
  # zat radio aktif C
  d_c = function(t,q_b,q_c){
    ((r_b * q_b) - (r_c * q_c)) 
  }

# persiapan array utk iterasi
A = c(q_a)
B = c(q_b)
C = c(q_c)
t_ = c(t)


# proses iterasi
for(i in 2:num_iter){
  # kita akan hitung dulu zat a
  k1 = d_a(t,A[i-1]) 
  k2 = d_a(t + 0.5*h,A[i-1] + 0.5*k1*h) 
  k3 = d_a(t + 0.5*h,A[i-1] + 0.5*k2*h) 
  k4 = d_a(t + h,A[i-1] + k3*h) 
  
  A[i] = A[i-1] + (1/6)*(k1 + 2*k2 + 2*k3 + k4) * h
  
  # kita hitung zat b
  k1 = d_b(t,
           A[i-1],
           B[i-1]) 
  k2 = d_b(t + 0.5*h,
           A[i-1] + 0.5*k1*h,
           B[i-1] + 0.5*k1*h) 
  k3 = d_b(t + 0.5*h,
           A[i-1] + 0.5*k2*h,
           B[i-1] + 0.5*k2*h) 
  k4 = d_b(t + h,
           A[i-1] + k3*h,
           B[i-1] + k3*h) 
  
  B[i] = B[i-1] + (1/6)*(k1 + 2*k2 + 2*k3 + k4) * h
  
  # kita hitung zat c
  k1 = d_c(t,
           B[i-1],
           C[i-1]) 
  k2 = d_c(t + 0.5*h,
           B[i-1] + 0.5*k1*h,
           C[i-1] + 0.5*k1*h) 
  k3 = d_c(t + 0.5*h,
           B[i-1] + 0.5*k2*h,
           C[i-1] + 0.5*k2*h) 
  k4 = d_c(t + h,
           B[i-1] + k3*h,
           C[i-1] + k3*h) 
  
  C[i] = C[i-1] + (1/6)*(k1 + 2*k2 + 2*k3 + k4) * h
  
  t_[i] = t_[i-1] + h
  
}

# output
output = data.frame(t_,A,B,C)

print(output)
