rm(list=ls())

norm_new = function(x){
  sum(x^2) %>% sqrt()
}

theta = 30

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

Rn_1 = (R23 %*% (R13 %*% R12)) %>% round(4)
Rn_2 = (R23 %*% R13 %*% R12) %>% round(4)

Rn_1 == Rn_2

x0 = c(1,1,1)

x0
norm_new(x0)

x1 = Rn_1 %*% x0 
x1
x1 %>% norm_new()

x2 = Rn_1 %*% x1 
x2
x2 %>% norm_new()
