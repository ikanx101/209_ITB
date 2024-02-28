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
solusi_6 = get_solution(result, butuh[j,k]) %>% as.data.frame()
solusi_7 = get_solution(result, x_stb[k]) %>% as.data.frame()
solusi_8 = get_solution(result, x_hat_stb[j,k]) %>% as.data.frame()

save(solusi_1,
     solusi_2,
     solusi_3,
     solusi_6,
     solusi_7,
     solusi_8,
     file = "report.rda")

# stok pada akhir cycle
stok = data.frame(k = 1:6,Z_0k)

# total demand dari masing-masing week
demand = data.frame(j = 1:4,Dj)

# lalu saya buat variabel bernama tabel_all
# berisi list dari semua tabel yang telah kita buat bersama-sama
tabel_all = list("Hasil x_k -- total gula k yang dibeli",
                 solusi_1,
                 "Hasil x_hat_jk -- total gula k yang dikirim pada minggu j.\nUntuk membuktikan constraint III, jumlahkan x_hat_jk utk setiap j sehingga menghasilkan angka yang sama dengan x_k",
                 solusi_2,
                 "Total demand pada week j",
                 demand,
                 "z_0k -- stok gula k pada minggu 0 (merupakan parameter input)",
                 stok,
                 "Hasil z_jk -- stok gula k pada akhir minggu j.",
                 solusi_3,
                 "a_ijk -- binary: jika item i diproduksi dengan gula k pada week j",
                 solusi_4,
                 "b_ijk -- proporsi item i diproduksi dengan gula k pada week j",
                 solusi_5
                )

# bikin sheet
nama_sheet = paste0("Hasil Run Codes")
sh = addWorksheet(wb, nama_sheet)

# masukin semua tabel ke sheet tersebut
xl_write(tabel_all, wb, sh)



# ==============================================================================
# bikin sheet
nama_sheet = paste0("Objective Func")
sh = addWorksheet(wb, nama_sheet)

final = 
  solusi_1 %>% 
  mutate(price = df_1$harga_gula) %>%
  mutate(total_cost = value * price) %>%
  mutate(cost_obj_func = c_k) %>% 
  mutate(total_cost_obj = value * cost_obj_func) %>% 
  rename(pembelian = value) 

cost = sum(final$total_cost) / 1000000000
cost = round(cost,3)

pesan = paste0("Total biaya yang dikeluarkan: Rp",
               cost,
               " Miliar"
)

cost = sum(final$total_cost_obj) / 1000000000
cost = round(cost,3)

pesan_lagi = paste0("Total Cost Obj Function: Rp",
                    cost,
                    " Miliar")

# hapus variabel
final = final %>% select(-total_cost,-total_cost_obj)

tabel_all = list(
  pesan,
  pesan_lagi,
  final
)

# masukin semua tabel ke sheet tersebut
xl_write(tabel_all, wb, sh)

# ==============================================================================
# bikin sheet
nama_sheet = paste0("Hasil z_jk")
sh = addWorksheet(wb, nama_sheet)

df_z = 
  solusi_3 %>% 
  select(-variable) %>%
  reshape2::dcast(k ~ j)

colnames(df_z)[2:5] = paste0("week ",1:4)

tabel_all = list(
  "Stok akhir gula k pada week j",
  df_z
)

# masukin semua tabel ke sheet tersebut
xl_write(tabel_all, wb, sh)



# ==============================================================================
# bikin sheet
nama_sheet = paste0("Hasil a_ijk")
sh = addWorksheet(wb, nama_sheet)

solusi_4 = get_solution(result, a[i,j,k]) %>% as.data.frame() 
solusi_4 =
  solusi_4 %>% 
  select(-variable) %>%
  reshape2::dcast(i + j ~ k,value.var = "value")

colnames(solusi_4)[3:8] = paste0("gula ",1:6)

tabel_all = list(
  "Jika item i diproduksi dengan gula k pada week j",
  solusi_4
)

# masukin semua tabel ke sheet tersebut
xl_write(tabel_all, wb, sh)


# ==============================================================================
# bikin sheet
nama_sheet = paste0("Proporsi b_ijk")
sh = addWorksheet(wb, nama_sheet)

solusi_5 =
  solusi_5 %>% 
  select(-variable) %>%
  reshape2::dcast(i + j ~ k,value.var = "value")

colnames(solusi_5)[3:8] = paste0("gula ",1:6)

tabel_all = list(
  "Berikut adalah proporsi penggunaan gula pada produk i di minggu j",
  solusi_5
)

# masukin semua tabel ke sheet tersebut
xl_write(tabel_all, wb, sh)

# ==============================================================================
# bikin sheet
nama_sheet = paste0("FINAL")
sh = addWorksheet(wb, nama_sheet)

tabel_all = list(solusi_6,solusi_7,solusi_8)

# masukin semua tabel ke sheet tersebut
xl_write(tabel_all, wb, sh)


# ==============================================================================
# bikin sheet
nama_sheet = paste0("Bukti constraint II")
sh = addWorksheet(wb, nama_sheet)

# pembuktian constraint II
tabel_all = vector("list",5*4)

# pada week I
pesan = paste0("Total demand pada minggu 1: ",Dj[1])
# x_hat pada pada minggu 1
temp_1 = solusi_2 %>% filter(j == 1)

tabel_all[[1]] = pesan
tabel_all[[2]] = "Stok pada minggu 0"
tabel_all[[3]] = stok
tabel_all[[4]] = "x_hat[1,k] -- gula k yang dikirim pada minggu 1"
tabel_all[[5]] = temp_1

ikang = 6

# pada week II
for(idx in 2:4){
 pesan = paste0("Total demand pada minggu ",idx," : ",Dj[idx])
 # x_hat pada pada minggu idx
 temp_2 = solusi_2 %>% filter(j == idx)
 temp_3 = solusi_3 %>% filter(j == (idx-1))
 
 tabel_all[[k]] = "================================================"
 k = k + 1
 tabel_all[[k]] = pesan
 k = k + 1
 tabel_all[[k]] = paste0("Stok pada minggu ",idx-1)
 k = k + 1
 tabel_all[[k]] = temp_3
 k = k + 1
 tabel_all[[k]] = paste0("x_hat[",idx,",k] -- gula k yang dikirim pada minggu ",idx)
 k = k + 1
 tabel_all[[k]] = temp_2
 k = k + 1
}

# masukin semua tabel ke sheet tersebut
xl_write(tabel_all, wb, sh)


# ==============================================================================
# bikin sheet
nama_sheet = paste0("Bukti constraint V")
sh = addWorksheet(wb, nama_sheet)

solusi_4 = get_solution(result, a[i,j,k]) %>% as.data.frame() 

# pembuktian constraint V
solusi_4 = solusi_4 %>% filter(value == 1)

# pada week 1
temp_1 = solusi_4 %>% filter(j == 1 & i %in% P_1_2) %>% group_by(i) %>% tally() %>% ungroup() %>% 
         rename("butuh berapa gula" = n)
         
# pada week 2
temp_2 = solusi_4 %>% filter(j == 2 & i %in% P_2_2) %>% group_by(i) %>% tally() %>% ungroup() %>% 
         rename("butuh berapa gula" = n)

# pada week 3
temp_3 = solusi_4 %>% filter(j == 3 & i %in% P_3_2) %>% group_by(i) %>% tally() %>% ungroup() %>% 
         rename("butuh berapa gula" = n)
 
# pada week 4
temp_4 = solusi_4 %>% filter(j == 4 & i %in% P_4_2) %>% group_by(i) %>% tally() %>% ungroup() %>% 
         rename("butuh berapa gula" = n)

tabel_all = list(
  "Berikut adalah hasil summary dari a_ijk.\n======================================\nPada item dengan penggunaan min 2 jenis gula di minggu I:",
  temp_1,
  "======================================\nPada item dengan penggunaan min 2 jenis gula di minggu II:",
  temp_2,
  "======================================\nPada item dengan penggunaan min 2 jenis gula di minggu III:",
  temp_3,
  "======================================\nPada item dengan penggunaan min 2 jenis gula di minggu IV:",
  temp_4
)

# masukin semua tabel ke sheet tersebut
xl_write(tabel_all, wb, sh)

# export ke Excel
saveWorkbook(wb, nama_file_output, overwrite = TRUE)
