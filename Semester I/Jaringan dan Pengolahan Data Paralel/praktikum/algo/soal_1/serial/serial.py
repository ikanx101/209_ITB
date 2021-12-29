import time

# hitung waktu mulai
mulai = time.time()

# bikin fungsi f(x)
def fx (x) :
   return(x**2)

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

# initial condition
a = 0
b = 1
n = 10**10

# hitung soal
nilai = int_numeric(a,b,n)
print("Nilai integral f(x) dx adalah: ",nilai)

# hitung waktu selesai
end = time.time()
waktu = end - mulai
print(waktu)

# selesai
print("DONE")
