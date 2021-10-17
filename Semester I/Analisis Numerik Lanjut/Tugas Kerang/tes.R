rm(list=ls())

library(dplyr)
library(ggplot2)

f1 = function(x1,x2){cos(2* x1) - cos(2*x2) - 0.4}

ikanx = function(f,a,b){
  x = seq(a,b,by = .25)

  temp_plot = 
    expand.grid(x,x) %>%
    rename(x = Var1,
	   y = Var2) %>%
    mutate(z = f(x,y))

  temp_plot %>%
     ggplot(aes(x = x,y = y,z = z)) +
     geom_contour() +
     geom_contour_filled()
}


