# ======================================================
# tugas pengantar sains komputasi
# program n-body simulation
# menggunakan pendekatan metode leapfrog
# oleh :
# 20921004 - mohammad rizka fadhli
# ======================================================

# catatan terkait skrip ini
# saya menggunakan bahasa R versi > 4.0 dengan memanfaatkan prinsip tidy
 # untuk menyelesaikan masalah n-body simulation secara paralel pada perhitungan
 # acceleration di tiap-tiap iterasi dt.
# libraries yang digunakan adalah:
 # dplyr, ggplot, gganimate, dan av.

# set working directory 
# catatan: harap diubah sesuai dengan working directory masing-masing
setwd("/root/209_ITB/Semester III/Pengantar SK/PR 5")

# ======================================================
# membersihkan global environment
rm(list=ls())

# libraries yang dibutuhkan
library(dplyr)     # sebagai data manipulation
library(ggplot2)   # sebagai data visualization
library(gganimate) # sebagai animator grafik
library(av)        # untuk render animasi ke dalam mp4

# seed untuk randomisasi angka
set.seed(20921004)

# ======================================================
# parameter yang digunakan dalam simulasi
N         = 3     # banyak benda
t         = 0     # t initial
tEnd      = 2     # t akhir
dt        = 10^(-2) # delta t
max_iter  = (tEnd - t)/dt # berapa banyak iterasi dilakukan
softening = 0.1   # softening agar jarak r_j - r_i tidak nol
G         = 1     # Newton's Gravitational Constant
mass      = rep(20/N,N)   # massa per benda (dibuat total sama dengan 20)
pos       = runif(N*2)    # randomisasi initial posisi setiap benda
pos       = matrix(pos,ncol = 2) # dibuat dalam bentuk matriks
vel       = rep(0,N*2)    # randomisasi initial velocity setiap benda
vel       = matrix(vel,ncol = 2) # dibuat dalam bentuk matriks


# Initial condition untuk acceleration
acc = matrix(rep(0,N*2),ncol = 2) # yakni dibuat sama dengan nol

# saya gunakan vector berisi list untuk menampung hasil komputasi
 # setiap iterasi
final = vector("list",max_iter)

# ======================================================
# kita hitung terlebih dahulu energinya
 # e_kin = 1/2 * v^2
 # e_pot = - 1/r
e_kin = 0.5 * sum(vel^2 )
e_pot = -1 / sum(sqrt(pos^2))
e_tot = e_kin + e_pot
E0 = e_tot

# ======================================================
# kita mulai iterasinya dari sini
for(iter in 1:max_iter){
    # update velocity (half kick)
    vel = vel + acc * dt/2

    # update position
    pos = pos + vel * dt

    # perhitungan energi
    e_kin = 0.5 * sum(vel^2 )
    e_pot = -1 / sum(sqrt(pos^2))
    e_tot = e_kin + e_pot

    # saya akan simpan masing-masing iterasinya
    df = 
        as.data.frame(pos) %>%
        rename(x = V1,y = V2) %>%
        mutate(iter = iter,
               t_iter = t)
    df$e_kin = e_kin # menyimpan energi kinetik
    df$e_pot = e_pot # menyimpan energi potensial
    df$e_tot = e_tot # menghitung total energi
    df$id_bintang = 1:N
    # simpan ke dalam vector list final
    final[[iter]] = df

    # proses perhitungan acceleration dengan memanfaatkan
     # paralelisasi dalam bentuk data frame dan tidy
    # kita akan buat pairwise dari semua kemungkinan yang ada
    pair_wise = expand.grid(i = 1:N,
                            j = 1:N) %>% filter(i != j) %>%
                            arrange(i,j) %>%
                            # menghitung rj - ri
                            mutate(x_i = pos[i,1],
                                   x_j = pos[j,1],
                                   y_i = pos[i,2],
                                   y_j = pos[j,2]) %>%
                            # menghitung dx dan dy
                            mutate(dx = x_j - x_i,
                                   dy = y_j - y_i) %>%
                            # menghitung norm dari r_j - r_i
                            mutate(inv_r3 = (dx^2 + dy^2 + softening^2)^(-3/2)) %>%
                            # menambahkan variabel mass
                            mutate(m_j = mass[j]) %>%
                            # menghitung acceleration masing-masing titik
                            mutate(a_i_x = G * m_j * dx * inv_r3,
                                   a_i_y = G * m_j * dy * inv_r3) %>%
                            # melakukan summary untuk setiap benda ke i
                            group_by(i) %>%
                            summarise(a_i_x = sum(a_i_x),
                                      a_i_y = sum(a_i_y)) %>%
                            ungroup()
    
    # menghitung acceleration
    acc = pair_wise %>% select(-i) %>% rename(V1 = a_i_x,V2 = a_i_y)
    acc = data.matrix(acc) # mengubah accalaration dalam bentuk matriks

    # update velocity (half kick)
    vel = vel + acc * dt/2

    # update time
    t = t + dt
}

# kita gabung kembali data dari masing-masing iterasi
df_all = do.call(rbind,final)

# kita buat grafiknya
plt = 
  df_all %>%
  ggplot(aes(x = x,y = y)) +
  geom_point(aes(color = as.factor(id_bintang))) +
  #scale_color_manual(values = c("yellow","red","purple")) +
  transition_time(t_iter) +
  shadow_wake(wake_length = 0.1, alpha = FALSE) +
  labs(title = "Project Pengantar Sains Komputasi\nPergerakan Benda Luar Angkasa",
       subtitle = 'Posisi dan velocity awal benda dibuat random\nsedangkan massa setiap benda sama',
       caption = "Dibuat dengan R\n20921004 Mohammad Rizka Fadhli",
       color = 'Benda ke-') +
  theme_minimal()

# animasikan
a = animate(plt, duration = 30, fps = 20, renderer = av_renderer())

# save animasi ke local
anim_save("animation Leapfrog 20921004.mp4", a)

# kita buat grafik dari energinya sekarang
plt = 
  df_all %>% 
  mutate(t_iter = round(t_iter,2)) %>%
  group_by(t_iter) %>%
  summarise(E = mean(e_tot)) %>%
  ungroup() %>%
  mutate(graf = (E - E0)/E0,
         graf = round(graf,1))

# membuat grafik
plt %>%
  ggplot(aes(x = t_iter,
             y = graf)) +
  geom_line(color = "darkred") +
  theme_minimal() +
  labs(title = "Grafik (E-E0)/E0 untuk Setiap t",
       subtitle = "Metode Leapfrog dengan dt = 10^-2",
       caption = "Dibuat dengan R\n20921004 Mohammad Rizka Fadhli")
# save ke local
ggsave("Plot Energi Metode Leapfrog 20921004.png")

# terima kasih
# visit ikanx101.com