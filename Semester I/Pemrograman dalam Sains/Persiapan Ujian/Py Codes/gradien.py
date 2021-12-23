# ===========================================================
# rangkuman semua pycodes yang digunakan selama kuliah
#
# ikanx101.com
# ===========================================================

# program untuk hitung gradien dari dua titik
x1 = input("Masukkan titik koordinat x1: ")
y1 = input("Masukkan titik koordinat y1: ")
x2 = input("Masukkan titik koordinat x2: ")
y2 = input("Masukkan titik koordinat y2: ")

x1 = float(x1)
x2 = float(x2)
y1 = float(y1)
y2 = float(y2)

atas = y2 - y1
bawah = x2 - x1

m = atas / bawah
m = round(m,3)

print("Slope garis yang dibuat oleh dua titik tersebut adalah ",m)
