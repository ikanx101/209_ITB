rm(list=ls())

# memanggil libraries
library(dplyr)
library(ggplot2)

# input
# rate of change
r_A = .05
r_B = .04
r_C = .03

# kondisi awal
Q_A_0 = 10
Q_B_0 = 0
Q_C_0 = 0

# initial parameter untuk iterasi
max_iter = 50
dt = .01
