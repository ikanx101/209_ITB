# ==============================================================================
# save ke sini dulu
library(expss)
library(openxlsx)

# kita bikin workbook-nya
wb = createWorkbook()

solusi_1 = get_solution(result, x[k]) %>% as.data.frame() 
solusi_2 = get_solution(result, x_hat[j,k]) %>% as.data.frame()  
solusi_3 = get_solution(result, z[j,k]) %>% as.data.frame() 
solusi_4 = get_solution(result, a[i,j,k]) %>% as.data.frame()
solusi_5 = get_solution(result, b[i,j,k]) %>% as.data.frame()
solusi_6 = get_solution(result, tot[k]) %>% as.data.frame()

# lalu saya buat variabel bernama tabel_all
# berisi list dari semua tabel yang telah kita buat bersama-sama
tabel_all = list("Hasil x_k -- total gula k yang dibeli",
                 solusi_1,
                 "Hasil x_hat_jk -- total gula k yang dikirim pada minggu j. Notes: x_hat_1k dan x_hat_2k didapat dari parameter",
                 solusi_2,
                 "Hasil z_jk -- stok gula k pada akhir minggu j. Notes: z_1k adalah parameter",
                 solusi_3,
                 "Hasil a_ijk -- terisi 1 jika produk i menggunakan gula k pada minggu j",
                 solusi_4,
                 "Hasil b_ijk -- proporsi gula k yang digunakan pada produk i di minggu j",
                 solusi_5
                )

# bikin sheet
nama_sheet = paste0("Hasil Run Codes")
sh = addWorksheet(wb, nama_sheet)

# masukin semua tabel ke sheet tersebut
xl_write(tabel_all, wb, sh)

# ==============================================================================
# bikin sheet
nama_sheet = paste0("hasil obj function")
sh = addWorksheet(wb, nama_sheet)

# kita buat dulu tabel fungsi objective-nya
temp_obj_1 = solusi_1 %>% select(k,value) %>% rename(x_k = value) %>% mutate(c_k = c_k,
                                                                             `x_k * c_k` = x_k * c_k)
temp_obj_2 = solusi_6 %>% select(k,value) %>% rename(total_z_k = value)
temp_obj = merge(temp_obj_1,temp_obj_2) 

tot_xk_ck = sum(temp_obj$`x_k * c_k`)
tot_zk = sum(temp_obj$total_z_k)
hasil = tot_xk_ck + tot_zk

tabel_all = list(
  "Berikut adalah summary tabel hasil fungsi objective:",
  temp_obj,
  "Total nilai Fungsi Objective:",
  paste0("F = ",tot_xk_ck," + ",tot_zk," = ",hasil))

# masukin semua tabel ke sheet tersebut
xl_write(tabel_all, wb, sh)

# ==============================================================================
# bikin sheet
nama_sheet = paste0("x_hat_jk")
sh = addWorksheet(wb, nama_sheet)

tabel = 
  solusi_2 %>% 
  select(j,k,value) %>% 
  reshape2::dcast(k~j,
                  value.var = "value") %>% 
  rename(jenis_gula = k,
         w1 = `1`,
         w2 = `2`,
         w3 = `3`,
         w4 = `4`,
         w5 = `5`,
         w6 = `6`) %>% 
  janitor::adorn_totals()

# masukin semua tabel ke sheet tersebut
xl_write(tabel, wb, sh)


# ==============================================================================
# bikin sheet
nama_sheet = paste0("z_jk")
sh = addWorksheet(wb, nama_sheet)

tabel = 
  solusi_3 %>% 
  select(j,k,value) %>% 
  reshape2::dcast(k~j,
                  value.var = "value") %>% 
  rename(jenis_gula = k,
         w1 = `1`,
         w2 = `2`,
         w3 = `3`,
         w4 = `4`,
         w5 = `5`,
         w6 = `6`) %>% 
  janitor::adorn_totals()

tabel_all = list(
  "Tabel z_jk",
  tabel,
  paste0("Perhatikan bahwa maxcap adalah sebesar: ",maxcap)
)

# masukin semua tabel ke sheet tersebut
xl_write(tabel_all, wb, sh)

# ==============================================================================
# bikin sheet
nama_sheet = paste0("a_ijk")
sh = addWorksheet(wb, nama_sheet)

temporary = 
  solusi_4 %>% 
  select(i,j,k,value) %>% 
  group_split(j)

tabel_all = list(
  "Penggunaan pada w3",
  temporary[[1]] %>% reshape2::dcast(i ~ k,value.var = "value") %>% rename(produk = i,
                                                                           gula_1 = `1`,gula_2 = `2`,
                                                                           gula_3 = `3`,gula_4 = `4`,
                                                                           gula_5 = `5`,gula_6 = `6`),
  
  "Penggunaan pada w4",
  temporary[[2]] %>% reshape2::dcast(i ~ k,value.var = "value") %>% rename(produk = i,
                                                                           gula_1 = `1`,gula_2 = `2`,
                                                                           gula_3 = `3`,gula_4 = `4`,
                                                                           gula_5 = `5`,gula_6 = `6`),
  
  "Penggunaan pada w5",
  temporary[[3]] %>% reshape2::dcast(i ~ k,value.var = "value") %>% rename(produk = i,
                                                                           gula_1 = `1`,gula_2 = `2`,
                                                                           gula_3 = `3`,gula_4 = `4`,
                                                                           gula_5 = `5`,gula_6 = `6`),
  
  "Penggunaan pada w6",
  temporary[[4]] %>% reshape2::dcast(i ~ k,value.var = "value") %>% rename(produk = i,
                                                                           gula_1 = `1`,gula_2 = `2`,
                                                                           gula_3 = `3`,gula_4 = `4`,
                                                                           gula_5 = `5`,gula_6 = `6`)
)

# masukin semua tabel ke sheet tersebut
xl_write(tabel_all, wb, sh)


# ==============================================================================
# bikin sheet
nama_sheet = paste0("b_ijk")
sh = addWorksheet(wb, nama_sheet)

temporary = 
  solusi_5 %>% 
  select(i,j,k,value) %>% 
  group_split(j)

tabel_all = list(
  "Penggunaan pada w3",
  temporary[[1]] %>% reshape2::dcast(i ~ k,value.var = "value") %>% rename(produk = i,
                                                                           gula_1 = `1`,gula_2 = `2`,
                                                                           gula_3 = `3`,gula_4 = `4`,
                                                                           gula_5 = `5`,gula_6 = `6`),
  
  "Penggunaan pada w4",
  temporary[[2]] %>% reshape2::dcast(i ~ k,value.var = "value") %>% rename(produk = i,
                                                                           gula_1 = `1`,gula_2 = `2`,
                                                                           gula_3 = `3`,gula_4 = `4`,
                                                                           gula_5 = `5`,gula_6 = `6`),
  
  "Penggunaan pada w5",
  temporary[[3]] %>% reshape2::dcast(i ~ k,value.var = "value") %>% rename(produk = i,
                                                                           gula_1 = `1`,gula_2 = `2`,
                                                                           gula_3 = `3`,gula_4 = `4`,
                                                                           gula_5 = `5`,gula_6 = `6`),
  
  "Penggunaan pada w6",
  temporary[[4]] %>% reshape2::dcast(i ~ k,value.var = "value") %>% rename(produk = i,
                                                                           gula_1 = `1`,gula_2 = `2`,
                                                                           gula_3 = `3`,gula_4 = `4`,
                                                                           gula_5 = `5`,gula_6 = `6`)
)

# masukin semua tabel ke sheet tersebut
xl_write(tabel_all, wb, sh)



# ==============================================================================
# bikin sheet
nama_sheet = paste0("Cek pers 4 Dj")
sh = addWorksheet(wb, nama_sheet)

tabel_1 = 
  solusi_3 %>% 
  select(j,k,value) %>% 
  rename(z_jk = value)

tabel_2 = 
  solusi_2 %>% 
  select(j,k,value) %>% 
  rename(x_hat_jk = value)

hasil_z_x_hat = 
  merge(tabel_1,tabel_2) %>% 
  group_by(j) %>% 
  summarise(total_z_j = sum(z_jk),
            total_x_hat_j = sum(x_hat_jk)) %>% 
  ungroup() %>% 
  mutate(`total z_j dan x_hat_j` = total_z_j + total_x_hat_j)

tabel_Dj = data.frame(j = 3:6,
                      Dj = Dj[3:6])

tabel_all = list(
  "Total hasil z_jk dan x_hat_jk",
  hasil_z_x_hat,
  "Total Dj pada j = M_hat",
  tabel_Dj
)

# masukin semua tabel ke sheet tersebut
xl_write(tabel_all, wb, sh)

# export ke Excel
saveWorkbook(wb, "data hasil v5.xlsx", overwrite = TRUE)
