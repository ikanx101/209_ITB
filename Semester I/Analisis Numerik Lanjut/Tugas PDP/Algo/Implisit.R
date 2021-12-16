rm(list=ls())
L = 1
Nx = 10
h = L/Nx
Te = 0.5
Nt = 50
k = Te/Nt
lambda = k/h^2

x = seq(0,1,by = h)

u_x0 = function(x){sin(pi*x)}
w = u_x0(x)
w = w[-1]
w = w[-length(w)]

A = matrix(0,
           ncol = length(w),
           nrow = length(w))

for(i in 1:length(w)){
  A[i,i] = 1+(2*lambda)
}

for(i in 1:(length(w)-1)){
  A[i+1,i] = -lambda
  A[i,i+1] = -lambda
}

A_inv = solve(A)
# iterasi sesuai dengan NT
for(i in 1:Nt){
  w = A_inv %*% w
}

solusi_numerik = data.frame(x,y = c(0,w,0)) 
solusi_numerik