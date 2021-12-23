# ===========================================================
# rangkuman semua pycodes yang digunakan selama kuliah
#
# ikanx101.com
# ===========================================================

# program repetisi untuk sum semua bilangan integer yang ada
jumlah = 0
n = int(input("masukan angka (integer) = "))
for i in range(1, n + 1, 1):
    jumlah += i
print("\n")
print("Jumlah semua angka sampai", n ,"adalah =", jumlah)
