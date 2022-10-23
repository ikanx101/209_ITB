# ==============================================================================
# Program Euler
#
# Project I - Pengantar Sains Komputasi
#
# Mohammad Rizka Fadhli - 20921004
#
# ==============================================================================


# membersihkan global environment
rm(list=ls())

# INPUT dari user:
  # rate peluruhan A dan pertumbuhan B
  r_a = readline(prompt = "Rate peluruhan A: ") %>% as.numeric()
  # rate peluruhan B dan pertumbuhan C
  r_b = readline(prompt = "Rate peluruhan B: ") %>% as.numeric()
  # massa awal zat radioaktif A
  qa0 = readline(prompt = "massa awal zat radioaktif A: ") %>% as.numeric()
  # massa awal zat radioaktif B
  qb0 = readline(prompt = "massa awal zat radioaktif B: ") %>% as.numeric()
  # massa awal zat radioaktif C
  qc0 = readline(prompt = "massa awal zat radioaktif C: ") %>% as.numeric()
  # delta t
  dt0 = readline(prompt = "nilai delta t: ") %>% as.numeric()
  # panjang iterasi
  iter_length = readline(prompt = "seberapa panjang iterasi dilakukan: ") %>% as.numeric()


# proses perhitungan dengan metode Euler
q_a = c(qa0)  # array massa zat radioaktif A
q_b = c(qb0)  # array massa zat radioaktif B
q_c = c(qc0)  # array massa zat radioaktif C
t = c(0)      # waktu awal t = 0
dt = dt0
num_iter = (iter_length / dt) + 1

# proses iterasi
for(i in 2:num_iter){
    # peluruhan A dan pertumbuhan B
    rate_1 = r_a * q_a[i-1] * dt
    # peluruhan B dan pertumbuhan C
    rate_2 = r_b * q_b[i-1] * dt
    
    # perhitungan massa zat A
    q_a[i] = q_a[i-1] - rate_1
    # perhitungan massa zat B
    q_b[i] = q_b[i-1] + rate_1 - rate_2
    # perhitungan massa zat C
    q_c[i] = q_c[i-1] + rate_2 
    # perhitungan massa waktu
    t[i] = t[i-1] + dt
}

df = data.frame(t,q_a,q_b,q_c)

print(df)