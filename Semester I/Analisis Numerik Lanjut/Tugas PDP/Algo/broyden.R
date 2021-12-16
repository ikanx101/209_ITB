rm(list=ls())

library(dplyr)
norm_vec_inf = function(x)max(abs(x))

x0 = c(runif(1,3,8),
       runif(1,-2,2),
       runif(1,-6,0)) %>% round(1)

# bikin fungsi F(x1,x2,x3)
F_x_k = function(x){
  f1 = x[1]^2 + x[2] - 37
  f2 = x[1] - x[2]^2 - 5 
  f3 = x[1] + x[2] + x[3] - 3
  xk = c(f1,f2,f3)
  return(xk)
}

# set toleransi max yang diinginkan
tol_max = 0.00001

# set max iterasi yang diperbolehkan
iter_max = 70

# kita mulai metode Broyden-nya sesuai dengan algoritma 10.2
# step 1
v = F_x_k(x0)

# step 2
# bikin matriks jacobi
jax = function(x){
  a11 = 2*x[1]
  a12 = 1
  a13 = 0
  a21 = 1
  a22 = -2*x[2]
  a23 = 0
  a31 = 1
  a32 = 1
  a33 = 1
  J = matrix(c(a11,a12,a13,a21,a22,a23,a31,a32,a33),ncol = 3,byrow = T)
  J_inv = matlib::inv(J)
  return(J_inv)
}
A = jax(x0)

# step 3
s = -A %*% v
x = x0 + s
iter = 2

# step 4  
while(norm_vec_inf(F_x_k(x)) > tol_max && iter <= iter_max){
  # step 5
  w = v
  v = F_x_k(x)
  y = v - w
  # step 6
  z = -A %*% y
  # step 7
  p = t(-s) %*% z
  p = as.numeric(p)
  # step 8
  ut = t(s) %*% A
  # step 9
  A = A + ((s+z)/p) %*% ut
  # step 10
  s = -A %*% v
  # step 11
  x = x + s
  iter = iter + 1
  # output
  #  pesan = paste(x,collapse = ",")
  #  pesan = paste0("Iterasi ke- ",iter,": (",pesan,")")
  #  print(pesan)
}

if(iter <= iter_max){
  list("Titik initial: " = x0,
       "Solusi Final: " = x,
       "Banyak iterasi: " = iter)
} else if(iter > iter_max){
  list("Titik initial: " = x0,
       "Solusi Final: " = "Tidak Konvergen atau melebihi batas iterasi")
}
