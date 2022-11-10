# definisi dan initial condition
r_a = .5   # rate peluruhan A dan pertumbuhan B
r_b = .4   # rate peluruhan B dan pertumbuhan C
r_c = .3   # rate peluruhan C

q_a = 10  # massa awal zat radioaktif A
q_b = 0   # massa awal zat radioaktif B
q_c = 0   # massa awal zat radioaktif C
t   = 0   # waktu awal t = 0

iter_length = 5    # panjang iterasi
h = .01
num_iter = (iter_length / h) + 1 # berapa banyak iterasi 


d_a = function(t,q_a){
  (-r_a * q_a) 
  }
d_b = function(t,q_a,q_b){
  ((r_a * q_a) - (r_b * q_b)) 
  }
d_c = function(t,q_b,q_c){
  ((r_b * q_b) - (r_c * q_c)) 
  }


A = c(q_a)
B = c(q_b)
C = c(q_c)
t_ = c(t)


for(i in 2:num_iter){
  # kita akan hitung dulu zat a
  k1 = d_a(t,A[i-1]) 
  k2 = d_a(t + 0.5*h,A[i-1] + 0.5*k1*h) 
  k3 = d_a(t + 0.5*h,A[i-1] + 0.5*k2*h) 
  k4 = d_a(t + h,A[i-1] + k3*h) 
  
  A[i] = A[i-1] + (1/6)*(k1 + 2*k2 + 2*k3 + k4) * h
  
  # kita akan hitung zat b
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
  
  # kita akan hitung zat c
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

data.frame(t_,A,B,C) %>% tail()
