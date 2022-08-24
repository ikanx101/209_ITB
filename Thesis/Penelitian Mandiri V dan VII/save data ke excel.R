# ==============================================================================
# 
# supplier selection in R
# optimization modelling
#
# ==============================================================================

setwd("~/209_ITB/Thesis/Penelitian Mandiri V dan VII")

# memanggil libraries
library(readxl)             # untuk mengimport data excel
library(dplyr)              # untuk data carpentry
library(ompr)               # untuk optimisasi
library(ompr.roi)           # untuk optimisasi
library(ROI.plugin.glpk)    # untuk optimisasi

rm(list=ls())

options(scipen = 99)

# ambil data yang dibutuhkan
load("raw.rda")

# kita ganti stok akhir bulan dengan data ini:
df_1$stok_akhir_bulan = c(467470,466910,375400,470148,373600,404600)
df_1$nama_gula = 1:6 

# menghitung kebutuhan gula w3 hingga w6
df_3$kebutuhan_gula_w3_w6 = NULL

# max kapasitas
maxcap = 1427 * 1000
maxcap = data.frame(maxcap = maxcap)

# x_hat_1k untuk k di G
# banyak gula yang dikirim pada minggu 1
x_hat_1k = sample(10:40,6) * 200
# banyak gula yang dikirim pada minggu 2
x_hat_2k = sample(10:40,6) * 200

info = 
  data.frame(gula = 1:6,
             d2k = NA,
             x_hat_1k,
             x_hat_2k)



# ==============================================================================
# save ke sini dulu
library(expss)
library(openxlsx)

# kita bikin workbook-nya
wb = createWorkbook()

# bikin sheet
nama_sheet = paste0("data spek")
sh = addWorksheet(wb, nama_sheet)

# masukin semua tabel ke sheet tersebut
xl_write(df_1, wb, sh)

# bikin sheet
nama_sheet = paste0("matriks gula x produk")
sh = addWorksheet(wb, nama_sheet)

# masukin semua tabel ke sheet tersebut
xl_write(df_2, wb, sh)

# bikin sheet
nama_sheet = paste0("matriks produk x minggu")
sh = addWorksheet(wb, nama_sheet)

# masukin semua tabel ke sheet tersebut
xl_write(df_3, wb, sh)

# bikin sheet
nama_sheet = paste0("max_cap")
sh = addWorksheet(wb, nama_sheet)

# masukin semua tabel ke sheet tersebut
xl_write(maxcap, wb, sh)

# bikin sheet
nama_sheet = paste0("info lain")
sh = addWorksheet(wb, nama_sheet)

# masukin semua tabel ke sheet tersebut
xl_write(info, wb, sh)


# export ke Excel
saveWorkbook(wb, "data input.xlsx", overwrite = TRUE)