# ======================================================
# tugas pengantar sains komputasi
# program n-body simulation
# by:
# 20921004 - mohammad rizka fadhli
# ======================================================

setwd("/root/209_ITB/Semester III/Pengantar SK/PR 5/skrip.R")

# ======================================================
# bebersih global environment
rm(list=ls())
# libraries yang dibutuhkan
library(dplyr) # sebagai data manipulation
library(ggplot2) # sebagai data visualization
library(gganimate) # sebagai animator grafik
library(av) # untuk render animasi ke dalam mp4

# ======================================================
# Simulation parameters
N         = 5     # Number of particles
t         = 0     # current time of the simulation
tEnd      = 10    # time at which simulation ends
dt        = 0.01  # timestep
max_iter  = (tEnd - t)/dt # berapa banyak iterasi dilakukan
softening = 0.1   # softening length
G         = 1     # Newton's Gravitational Constant

# kita buat dulu rumahnya
final = vector("list",max_iter)

# Generate Initial Conditions
set.seed(20921004)

mass = rep(20/N,N)  # total mass of particles is 20
pos = runif(N*2)    # randomly selected positions
pos = matrix(pos,ncol = 2)
vel = runif(N*2)    # randomly selected velocities
vel = matrix(vel,ncol = 2)

# Convert to Center-of-Mass Frame
vel = vel - mean(mass * vel) / mean(mass)

# Initial condition for acceleration
acc = matrix(rep(0,N*2),ncol = 2)

# kita mulai iterasinya dari sini
for(iter in 1:max_iter){
    # update velocity 
    vel = vel + acc * dt

    # update position
    pos = pos + vel * dt

    # saya akan simpan masing-masing iterasinya
    df = 
        as.data.frame(pos) %>%
        rename(x = V1,y = V2) %>%
        mutate(iter = iter,
               t_iter = t)
    df$id_bintang = 1:N
    # simpan ke dalam list
    final[[iter]] = df

    # kita akan buat pairwise dari semua kemungkinan yang ada
    pair_wise = expand.grid(i = 1:N,
                            j = 1:N) %>% filter(i != j) %>%
                            arrange(i,j) %>%
                            # menghitung rj - ri
                            mutate(x_i = pos[i,1],
                                   x_j = pos[j,1],
                                   y_i = pos[i,2],
                                   y_j = pos[j,2]) %>%
                            mutate(dx = x_j - x_i,
                                   dy = y_j - y_i) %>%
                            # menghitung norm dari r_j - r_i
                            mutate(inv_r3 = (dx^2 + dy^2 + softening^2)^(-3/2)) %>%
                            mutate(m_j = mass[j]) %>%
                            mutate(a_i_x = G * m_j * dx * inv_r3,
                                   a_i_y = G * m_j * dy * inv_r3) %>%
                            group_by(i) %>%
                            summarise(a_i_x = sum(a_i_x),
                                      a_i_y = sum(a_i_y)) %>%
                            ungroup()
    
    # menghitung acceleration
    acc = pair_wise %>% select(-i) %>% rename(V1 = a_i_x,V2 = a_i_y)
    acc = data.matrix(acc)

    # update time
    t = t + dt
}

# kita gabung kembali datanya
df_all = do.call(rbind,final)
df_all

# bikin grafiknya
plt = 
  df_all %>%
  ggplot(aes(x = x,y = y)) +
  geom_point(aes(color = as.factor(id_bintang))) +
  #scale_color_manual(values = c("yellow","red","purple")) +
  transition_time(t_iter) +
  shadow_wake(wake_length = 0.1, alpha = FALSE) +
  labs(title = "Pergerakan Benda Angkasa",
       caption = "Dibuat dengan R\nikanx101.com") +
  theme_minimal()

# animasikan
a = animate(plt, duration = 30, fps = 20, renderer = av_renderer())

# save ke local
anim_save("animation bintang.mp4", a)

