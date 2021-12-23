# ===========================================================
# rangkuman semua pycodes yang digunakan selama kuliah
#
# ikanx101.com
# ===========================================================

# program untuk menghitung jarak antar dua titik
import math

x1 = input("Masukkan titik koordinat x1: ")
y1 = input("Masukkan titik koordinat y1: ")
x2 = input("Masukkan titik koordinat x2: ")
y2 = input("Masukkan titik koordinat y2: ")

x1 = float(x1)
x2 = float(x2)
y1 = float(y1)
y2 = float(y2)

del_x = (x1-x2)**2
del_y = (y1-y2)**2

jarak = math.sqrt(del_x + del_y)

print("Jarak antara kedua titik adalah: ",jarak)
