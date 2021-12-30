import time
import multiprocessing as mp
import math

mulai = time.time()

# initial condition
a = 0
b = 1
n = 10**8
h = (b-a)/n

# bikin fungsi f(x)
def fx (x) :
   return(math.sqrt(1 - x**2) * 4)

# bikin fungsi untuk integral numerik
def int_numeric (rank, nprocs,n,h):
   sum = float(0)
   # mulai iterasi untuk menghitung penjumlahan
   for i in range(rank,n,nprocs):
     xi = a + h/2 + i*h
     sum = sum + fx(xi)
   # kalikan dengan h untuk menjadi full integral
   sum = h * sum
   return(sum) 

nprocs = mp.cpu_count()
inputs = [(rank,nprocs,n,h) for rank in range(nprocs)]

pool = mp.Pool(processes = nprocs)
result = pool.starmap(int_numeric,inputs)

nilai = sum(result)

end = time.time()
waktu = end - mulai

print(nilai)
print(waktu)

f = open("rekap runtime.txt","w+")
f.write("MIDPOINT\n\nMultiprocessing\n\nNilai integral: ")
f.write(str(nilai))
f.write("\n\nRuntime: ")
f.write(str(waktu))
f.close()

# selesai
print("DONE")
