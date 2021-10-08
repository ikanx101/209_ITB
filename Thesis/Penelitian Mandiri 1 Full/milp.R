rm(list=ls())

library(dplyr)
library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)

# data yang dibutuhkan
profit = c(5,7,3)
sales = c(7,5,9)
M = 99999

# membuat model
mil_prog = 
  MIPModel() %>%
  # menambah variabel
  # xi
  add_variable(x[i],
	       i = 1:3,
	       type = "continuous",
	       lb = 0) %>%
  # yi
  add_variable(y[i],
	       i = 1:3,
	       type = "binary",
	       lb = 0) %>%
  # z
  add_variable(z,type = "binary",lb = 0) %>%
  # membuat objective function
  set_objective(sum_expr(x[i] * profit[i],
			 i = 1:3),
		"max") %>%
  # menambah constraints
  # max tonase
  add_constraint(x[i] <= sales[i],
		 i = 1:3) %>%
  # memilih 2 produk
  add_constraint(x[i] - y[i] * M <= 0,
		 i = 1:3) %>%
  add_constraint(sum_expr(y[i],
		 i = 1:3) <= 2) %>%
  # memilih 1 plant
  add_constraint(3*x[1] + 4*x[2] + 2*x[3] - M * z <= 30) %>%
  add_constraint(4*x[1] + 6*x[2] + 2*x[3] + M * z <= 40 + M) 

mil_prog

hasil = 
  mil_prog %>%
  solve_model(with_ROI(solver = "glpk",
		       verbose = T))

xi = 
  hasil %>% 
  get_solution(x[i])

yi = 
  hasil %>%
  get_solution(y[i])

zi = 
  hasil %>%
  get_solution(z)
