rm(list=ls())
library(dplyr)
for(i in 1:30){cat("\n")}

# masalah optimisasi
# min 7x1 + 8x2
# terhadap:
# 3x1 + 4x2 = 9
# 2x1 + x2 >= 3
# -100 <=  x1, x2 <= 100

# ================================
library(ROI)

# set obj function
obj_f = L_objective(c(7,8),
		    names = c("x1","x2")
		   )

# mendefinisikan constraints
A = rbind(c(3,4),
          c(2,1))
dir = c("==",">=")
b = c(9,3)

const = L_constraint(L = A, dir = dir, rhs = b)

# mendefinisikan boundaries
bou = V_bound(li = 1:2, # x1 dan x2
	      ui = 1:2, # x1 dan x2
	      lb = c(-100,-100),
	      ub = c(100,100))

start = Sys.time()
# penyelesaian dengan ROI
linear_roi = OP(objective = obj_f,
		constraints = const,
		bounds = bou)
solusi_roi = ROI_solve(linear_roi)
print(solusi_roi)
print(solution(solusi_roi))
end = Sys.time()
waktu = end - start
print("Waktu komputasi ROI: ")
print(waktu) 

print(" =================================")
print(" =================================")

# ============================
library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)

# penyelesaian dengan OMPR
# prinsip tidy dari tidyverse
# pipe operator %>%

model = 
  MIPModel() %>%
  add_variable(x[i],
	       i = 1:2,
               tipe = "continuous", # tinggal diubah menjadi binary atau integer
               lb = -100, 
               ub = 100) %>%
  set_objective(7*x[1] + 8*x[2],"min") %>%
  add_constraint(3*x[1] + 4*x[2] == 9) %>%
  add_constraint(2*x[1] + x[2] >= 3)

start = Sys.time()
hasil_ompr = solve_model(model, with_ROI(solver = "glpk", verbose = T))
solusi = hasil_ompr %>% get_solution(x[i])
print(hasil_ompr)
print(solusi)
end = Sys.time()
waktu = end - start
print("Waktu komputasi OMPR: ")
print(waktu)
