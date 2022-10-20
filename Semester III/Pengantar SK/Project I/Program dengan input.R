# membersihkan global environment
rm(list=ls())

# set working directory
setwd("/mnt/chromeos/removable/Workstation/209_ITB/Semester III/Pengantar SK/Project I")

# memanggil library yang dibutuhkan
library(dplyr)

# definisi dan initial condition
r_a = .05  # rate peluruhan A dan pertumbuhan B
r_b = .05  # rate peluruhan B dan pertumbuhan C
r_c = .05  # rate peluruhan C

q_a = c(10) # massa awal zat radioaktif A
q_b = c(0)  # massa awal zat radioaktif B
q_c = c(0)  # massa awal zat radioaktif C
t = c(0)    # waktu awal t = 0

dt = c(.05)
iter_length = 3
num_iter = iter_length / dt[1]

for(i in 2:num_iter){
    # peluruhan A dan pertumbuhan B
    rate_1 = r_a * q_a[i-1] * dt
    # peluruhan B dan pertumbuhan C
    rate_2 = r_b * q_b[i-1] * dt
    # peluruhan C
    rate_3 = r_c * q_c[i-1] * dt
    
    # perhitungan massa zat A
    q_a[i] = q_a[i-1] - rate_1
    # perhitungan massa zat B
    q_b[i] = q_b[i-1] + rate_1 - rate_2
    # perhitungan massa zat C
    q_c[i] = q_c[i-1] + rate_2 - rate_3
    # perhitungan massa waktu
    t[i] = t[i-1] + dt
}

df = data.frame(t,q_a,q_b,q_c)

df