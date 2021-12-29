import time
import random
import math

# bikin fungsi f(x)
def fx (x) :
   return(math.sqrt(1 - x**2) * 4)

# bikin fungsi untuk integral numerik
def int_numeric (a,b,n):
   # set terlebih dahulu agar tidak error pada tipe variabel
   n = int(n)
   a = float(a)
   b = float(b)
   sum = float(0)
   # hitung selang integrasi diksritisasi
   h = (b-a) / n
   # mulai iterasi untuk menghitung penjumlahan
   for i in range(n):
     xi = a + h/2 + i*h
     sum = sum + fx(xi)
   # kalikan dengan h untuk menjadi full integral
   sum = h * sum
   return(sum) 

# bikin fungsi untuk monte carlo
def monte_pi (n):
   n = int(n)
   monte = float(0)
   for i in range(n):
     xi = random.random()
     yi = fx(xi)
     monte = monte + yi
   return(monte/n)

# initial condition
a = 0
b = 1
n = 10**8

# hitung soal dengan midpoint
# hitung waktu mulai
mulai = time.time()
nilai_1 = int_numeric(a,b,n)
print("Nilai integral f(x) dx adalah: ",nilai_1)
end = time.time()
waktu_1 = end - mulai
print("Waktu yang dibutuhkan dengan midpoint: ",waktu_1)

# hitung soal dengan montecarlo
mulai = time.time()
nilai_2 =  monte_pi(n)
print("Nilai integral f(x) dx adalah: ",nilai_2)
end = time.time()
waktu_2 = end - mulai
print("Waktu yang dibutuhkan dengan monte: ",waktu_2)


f = open("rekap_serial.txt","w+")
f.write("MIDPOINT\nNilai integral: ")
f.write(str(nilai_1))
f.write("\nRuntime selama: ")
f.write(str(waktu_1))
f.write("\n\nMONTE\nNilai integral: ")
f.write(str(nilai_2))
f.write("\nRuntime selama: ")
f.write(str(waktu_2))
f.close()

# selesai
print("DONE")
