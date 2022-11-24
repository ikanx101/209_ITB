# ======================================================
# tugas pengantar sains komputasi
# program n-body simulation
# by:
# 20921004 - mohammad rizka fadhli
# ======================================================

# ======================================================
# bebersih global environment
rm(list=ls())
# libraries yang dibutuhkan
library(dplyr) # sebagai data manipulation
library(ggplot2) # sebagai data visualization
library(gganimate) # sebagai animator grafik

# ======================================================
# banyak benda
n_benda = 25

# kita generate random posisi masing-masing benda
r_mat = sample(1000,n_benda * 2,replace = T)
# diisimpan dalam bentuk matrix
r_mat = matrix(r_mat,ncol = 2)
r_mat

# definisi dan initial condition
# massa benda
# sengaja dibuat besar dan n-1 benda
m_i = sample(1000,n_benda - 1) * 10^13
# benda ke n akan dibuat sangat besar sebagai pusat gravitasi 
m_i = c(m_i, 10^18)

# velocity benda i
# v_i saat t=0 adalah nol
v_i = rep(0,n_benda * 2)
v_i = matrix(v_i,ncol = 2)
v_i

# acceleration benda i
# a_i saat t = 0 adalah nol
a_i = rep(0,n_benda * 2)
a_i = matrix(a_i,ncol = 2)
a_i

# waktu simulasi
t = 300
t0 = 0
dt = 0.05

# banyak iterasi akan dilakukan
max_iter = t/dt
max_iter

# softening length
# digunakan agar lebih soft a_i yang dihasilkan
softening = 10^4

# konstanta gravitasi
G = 6.67 * 10^(-11)

# =====================================================
# bikin grafik posisi awal
as.data.frame(r_mat) %>%
  rename(x = V1,y = V2) %>%
  ggplot(aes(x = x,y = y)) +
  geom_point(size = 3) +
  theme_void() +
  coord_equal() 

# =====================================================
# kita buat dulu template datanya
final = vector("list",max_iter)

# kita buat iterasi untuk perhitungan semuanya
for(iter in 1:max_iter){
  
  # kita update nilai v_i dengan half kick
  # v_i = v_i + a_i * dt/2

  # kita update nilai r_mat dengan half kick
  r_mat = r_mat + v_i * dt

  # saya akan simpan masing-masing iterasinya
  df = 
    as.data.frame(r_mat) %>%
    rename(x = V1,y = V2) %>%
    mutate(iter = iter,
           t_iter = t0)
  df$id_bintang = 1:n_benda
  # simpan ke dalam list
  final[[iter]] = df

  # kita update nilai acceleration
  for(i in 1:n_benda){
    for(j in 1:n_benda){
      if(i != j){
        dx = r_mat[j,1] - r_mat[i,1]
        dy = r_mat[j,2] - r_mat[i,2]
        inv_r3 = (dx^2 + dy^2 + softening^2)^(-1.5)
        a_i[i,1] = a_i[i,1] + G * (dx * inv_r3) * m_i[j]
        a_i[i,2] = a_i[i,2] + G * (dy * inv_r3) * m_i[j]
        }
    }
  }

  # kita update kembali nilai v_i dari acceleration terbaru
  v_i = v_i + a_i * dt/2
  
  # update nilai waktu
  t0 = t0 + dt
}

a_i
v_i
# kita gabung kembali datanya
df_all = do.call(rbind,final)

# ======================================================
# kita akan buat animasi terlebih dahulu

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
a = animate(plt, renderer = ffmpeg_renderer())

# save ke local
anim_save("animation.mp4", a)