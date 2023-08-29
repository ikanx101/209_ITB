# skrip untuk mengambil parameter-parameter

# reminder buat diri sendiri
# indexing
  # i menandakan index untuk produk
  # k menandakan index gula
  # j menandakan index minggu

# read database input
nama = "data input.xlsx"
sheets = excel_sheets(nama)

# ambilin data
df_1 = read_excel(nama,sheet = sheets[1]) # ambil spek
df_2 = read_excel(nama,sheet = sheets[2]) # ambil matriks gula x produk
df_3 = read_excel(nama,sheet = sheets[3]) # ambil matriks produk x minggu

df_4 = merge(df_2,df_3)  # gabung menjadi matriks besar gula x produk x minggu
df_4[is.na(df_4)] = 0

maxcap = read_excel(nama,sheet = sheets[4]) # ambil max capacity gudang 
maxcap = 900000 #maxcap$maxcap

# bikin f_ik
# produk i bisa diproduksi dengan gula k
f_ik = 
  df_4 %>% 
  select(Gula_1,Gula_2,Gula_3,Gula_4,Gula_5,Gula_6) %>% 
  as.matrix()
colnames(f_ik) = NULL

# mengambil himpunan produk yang diproduksi per minggu
df_4 = 
  df_4 %>% 
  mutate(pakai_gula = Gula_1 + Gula_2 + Gula_3 + Gula_4 + Gula_5 + Gula_6)

# mengambil list produk dengan pemakaian gula >= 2
P_j_2 = df_4$code_product[df_4$pakai_gula >= 2]
  # bikin perminggu
  P_1_2 = df_4$code_product[df_4$pakai_gula >= 2 & df_4$w1 > 0]
  P_2_2 = df_4$code_product[df_4$pakai_gula >= 2 & df_4$w2 > 0]
  P_3_2 = df_4$code_product[df_4$pakai_gula >= 2 & df_4$w3 > 0]
  P_4_2 = df_4$code_product[df_4$pakai_gula >= 2 & df_4$w4 > 0]

# mengambil list produk dengan pemakaian gula == 1
P_j_1 = df_4$code_product[df_4$pakai_gula == 1]

# list produk per minggu
P1 = df_4$code_product[df_4$w1 > 0]
P2 = df_4$code_product[df_4$w2 > 0]
P3 = df_4$code_product[df_4$w3 > 0]
P4 = df_4$code_product[df_4$w4 > 0]

# Dj sebagai kebutuhan gula di minggu perencanaan
temp = 
  df_3 %>% 
  select(w1,w2,w3,w4) %>% 
  reshape2::melt() %>% 
  group_by(variable) %>% 
  summarise(Dj = sum(value)) %>% 
  ungroup()
Dj = temp$Dj

# total semua kebutuhan gula di bulan perencanaan
D = sum(Dj)

# harga gula per kg
# ini saya pakai harga gula dulu
c_k = df_1$cost_obj_func

# min order quantity
o_k = df_1$min_order

# kebutuhan bahan baku gula k (dalam ton) dari produk i pada week j
g_ijk = function(i,j,k){
  temp = 
    df_4 %>% 
    filter(code_product == i) %>% 
    select(k,j)
  hasil = as.numeric(temp[1]) * as.numeric(temp[2])
  return(hasil)
}

# contoh
matt_g_ijk = array(NA,dim = c(51,4,6))
for(i in 1:51){
  for(j in 1:4){
    for(k in 1:6){
      matt_g_ijk[i,j,k] = g_ijk(i,
                                paste0("w",j),
                                paste0("Gula_",k)
      )
    }
  }
}  

# stok level bahan baku k di gudang pada akhir week 1
Z_0k = df_1$stok_akhir_bulan

# safety stock
ss = 2500

# inventory cost
ic = 1000

cat("\014")
print("DONE load database input")

# summary
o_k # min order per gula
Z_0k # stok akhir cycle sebelumnya
