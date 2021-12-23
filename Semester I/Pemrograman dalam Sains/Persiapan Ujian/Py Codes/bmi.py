# ===========================================================
# rangkuman semua pycodes yang digunakan selama kuliah
#
# ikanx101.com
# ===========================================================

# program untuk hitung body mass index
nama = input("Masukkan nama Anda: ")
berat = input("Masukkan berat badan Anda: ")
tinggi = input("Masukkan tinggi badan Anda: ")

berat = int(berat)
tinggi = int(tinggi)

bmi = berat / (tinggi/100)**2
bmi = round(bmi,2)

print("Yth. Bpk/Ibu ",nama,", BMI Anda adalah sebesar: ",bmi)
