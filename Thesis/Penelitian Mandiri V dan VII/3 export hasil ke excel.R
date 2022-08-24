# ==============================================================================
# save ke sini dulu
library(expss)
library(openxlsx)

# kita bikin workbook-nya
wb = createWorkbook()

# lalu saya buat variabel bernama tabel_all
# berisi list dari semua tabel yang telah kita buat bersama-sama
tabel_all = list("Data Spek Bahan Baku",
                 df_1,
                 "Data Kebutuhan Bahan Baku Pada Bulan May",
                 df_4,
                 "kebutuhan gula di bulan perencanaan w3-w6",
                 D,
                 "max kapasitas",
                 maxcap,
                 "total proporsi portofolio bahan baku gula k yang ditetapkan dalam setahun",
                 Prk,
                 "Demand gula pada w1-w2",
                 d_2k,
                 "stok level bahan baku k di gudang pada akhir week 1",
                 Z_1k
)

# bikin sheet
nama_sheet = paste0("Raw Data")
sh = addWorksheet(wb, nama_sheet)

# masukin semua tabel ke sheet tersebut
xl_write(tabel_all, wb, sh)

# berisi list dari semua tabel yang telah kita buat bersama-sama
tabel_all = list("Hasil x_k",
                 solusi_1,
                 "Hasil x_cap",
                 solusi_2,
                 "Hasil z_k",
                 solusi_3)

# bikin sheet
nama_sheet = paste0("Hasil")
sh = addWorksheet(wb, nama_sheet)

# masukin semua tabel ke sheet tersebut
xl_write(tabel_all, wb, sh)


# export ke Excel
saveWorkbook(wb, "data mentah dan hasil.xlsx", overwrite = TRUE)