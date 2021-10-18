# mulai dari nol
rm(list=ls())

# libraries
library(dplyr)
library(ggplot2)

# kita belajar dari dasar dulu ya

# ini data contoh
x <- c(32,64,96,118,126,144,152.5,158)
y <- c(99.5,104.8,108.5,100,86,64,35.3,15)

# we will make y the response variable and x the predictor
# the response variable is usually on the y-axis
plot(x,y,pch=19)

