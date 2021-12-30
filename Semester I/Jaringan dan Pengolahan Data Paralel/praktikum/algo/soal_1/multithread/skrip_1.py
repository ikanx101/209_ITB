import time
import threading

# hitung waktu mulai
mulai = time.time()

# initial condition
a = 0
b = 1
n = 10**3
h = (b-a) / n
nthreads = 8
mt_size = float(n/nthreads)
threads = []

# bikin fungsi f(x)
def fx (x) :
   return(x**2)

# bikin fungsi untuk integral numerik
def int_numeric (a,b,mt_size):
   sum = float(0)
   # mulai iterasi untuk menghitung penjumlahan
   for i in range(n):
     xi = a + h/2 + i*h
     sum = sum + fx(xi)
   # kalikan dengan h untuk menjadi full integral
   sum = h * sum
   return(sum) 

for i in range (nthreads):
  t = threading.Thread(target = int_numeric,args = (mt_size,n,h))
  threads.append(t)
  t.start()

for t in threads:
  t.join()
  
# hitung soal
nilai = int_numeric(a,b,n)
print("Nilai integral f(x) dx adalah: ",nilai)

# hitung waktu selesai
end = time.time()
waktu = end - mulai
print(waktu)

f = open("rekap runtime.txt","w+")
f.write("MIDPOINT\n\nThreading\n\nNilai integral: ")
f.write(str(nilai))
f.write("\n\nRuntime: ")
f.write(str(waktu))
f.close()

# selesai
print("DONE")
