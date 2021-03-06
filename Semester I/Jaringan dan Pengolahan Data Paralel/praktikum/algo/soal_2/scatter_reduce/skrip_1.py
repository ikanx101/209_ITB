from mpi4py import MPI
import time
import math

# hitung waktu mulai
mulai = time.time()

comm = MPI.COMM_WORLD
rank = comm.Get_rank()
size = 8

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

# initial condition
a = 0
b = 1
n = 10**8
h = (b-a)/n
local_n = n/size

i = 1
local_a = [0 for i in range(size)]
local_b = [0 for i in range(size)]


if rank == 0:
  for i in range(size):
      local_a[i] = a + rank * local_n * h
      local_b[i] = local_a[i] + local_n * h
  data = [(local_a[i],local_b[i]) for i in range(size)]
else:
  data = None
  
data = comm.scatter(data,root = 0)

local_n = n/size
local_a = a + rank * local_n * h
local_b = local_a + local_n * h

# hitung soal
s = int_numeric(local_a,local_b,local_n)
nilai = s

nilai = comm.reduce(nilai,root = 0)

if rank == 0:
  print("Nilai integral f(x) dx adalah: ",nilai)
  # hitung waktu selesai
  end = time.time()
  waktu = end - mulai
  print(waktu)
  f = open("rekap runtime mid.txt","w+")
  f.write("MIDPOINT\n\nScatter-Reduce\n\nNilai integral: ")
  f.write(str(nilai))
  f.write("\n\nRuntime: ")
  f.write(str(waktu))
  f.close()
  # selesai
  print("DONE")
