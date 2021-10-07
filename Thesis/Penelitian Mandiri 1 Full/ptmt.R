rm(list=ls())

library(dplyr)
library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)

bin_prog = 
  MIPModel() %>%
  # menambah variabel
  add_variable(x[i,j],
	       i = 1:20,
	       j = 1:5,
	       type = "binary",
	       lb = 0) %>%
  # membuat objective function
  set_objective(sum_expr(x[i,j],
			 i = 1:20,
			 j = 1:5),
		"max") %>%
  # menambah constraints
  # max kapasitas kelas
  add_constraint(sum_expr(x[i,j],i = 1:20) >= 5,
		 j = 1:5) %>%
  add_constraint(sum_expr(x[i,j],i = 1:20) <= 8,
		 j = 1:5) %>%
  # frek kunjungan siswa
  add_constraint(sum_expr(x[i,j],j = 1:5) >= 2,
		 i = 1:20) %>%
  add_constraint(sum_expr(x[i,j],j = 1:5) <= 3,
		 i = 1:20) %>%
  # jeda sehari
  add_constraint(x[i,j] + x[i,j+1] <= 1,
		 i = 1:20,
		 j = 1:4)

bin_prog

hasil = 
  bin_prog %>%
  solve_model(with_ROI(solver = "glpk",
		       verbose = T))

presensi = 
  hasil %>% 
  get_solution(x[i,j]) %>%
  filter(value == 1) %>%
  rename(siswa = i,
         hari = j) %>%
  group_by(hari) %>%
  summarise(presensi = paste(siswa,collapse = ",")) %>%
  ungroup()
