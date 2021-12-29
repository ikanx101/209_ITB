from mpi4py import MPI
import time

# hitung waktu mulai
mulai = time.time()

comm = MPI.COMM_WORLD
rank = comm.Get_rank()
size = 8

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
h = (b-a) / n
local_n = n / size

i = 1
local_a = [0 for i in range(size)]
local_b = [0 for i in range(size)]

if rank == 0:
  for i in range(size):
    local_a[i] = a + rank * local_n * h
    local_a[i] = a + rank * local_n * h
  data = [(local_a[i], local_b[i]) for i in range(size)]
else:
  data = None

data = comm.scatter(data,root = 0)

# hitung soal
s = int_numeric(a,b,n)
nilai = s

if rank == 0:
  print("Nilai integral f(x) dx adalah: ",nilai)
  # hitung waktu selesai
  end = time.time()
  waktu = end - mulai
  print(waktu)
  # bikin rekap .txt
  f = open("rekap runtime.txt","w+")
  f.write("MIDPOINT\n\nScatter\n\nNilai integral: ")
  f.write(str(nilai))
  f.write("\n\nRuntime: ")
  f.write(str(waktu))
  f.close()
  # selesai
  print("DONE")
