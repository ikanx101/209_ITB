rm(list=ls())

# library
library(dplyr)

# pendefinisian function RK 4 order
rk_4order = function(f,      # dy/dx
                     x0, y0, # init condition
                     h,      # selang
                     xmax){  # x max
  # initial condition
  x = x0
  y = y0
  n = (xmax-x0)/h
  # proses iterasi
  for(i in 1:n){
    k1 = f(x0,y0)
    k2 = f(x0 + 0.5*h,y0 + 0.5*k1*h)
    k3 = f(x0 + 0.5*h,y0 + 0.5*k2*h)
    k4 = f(x0 + h,y0 + k3*h)
    y0 = y0 + (1/6)*(k1 + 2*k2 + 2*k3 + k4) * h
    x0 = x0 + h
    x = c(x, x0)
    y = c(y, y0)
  }
  # output
  output = data.frame(x = x,
                      y = y)
  return(output)
}

# fungsi dy/dx dari soal
dydx = function(x,y){(x^2 + y)*sin((x^2) * y)}

# initial condition
x0 = 0
y0 = 5
xmax = 2
h = 0.2
solusi = rk_4order(dydx,x0,y0,h,xmax)
solusi

# saat kita ubah nilai h nya
solusi_h1 = rk_4order(dydx,x0,y0,0.1,xmax)
solusi_h1

solusi_h2 = rk_4order(dydx,x0,y0,0.01,xmax)
solusi_h2

