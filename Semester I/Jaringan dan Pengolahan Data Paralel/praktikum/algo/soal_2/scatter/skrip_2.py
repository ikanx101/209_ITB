from mpi4py import MPI
import time
import math
import random

# hitung waktu mulai
mulai = time.time()

comm = MPI.COMM_WORLD
rank = comm.Get_rank()
size = 8

# bikin fungsi f(x)
def fx (x) :
   return(math.sqrt(1 - x**2) * 4)

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
s = monte_pi(n)
nilai = s

if rank == 0:
  print("Nilai integral f(x) dx adalah: ",nilai)
  # hitung waktu selesai
  end = time.time()
  waktu = end - mulai
  print(waktu)
  # bikin rekap .txt
  f = open("rekap runtime Monte.txt","w+")
  f.write("MONTE\n\nScatter\n\nNilai integral: ")
  f.write(str(nilai))
  f.write("\n\nRuntime: ")
  f.write(str(waktu))
  f.close()
  # selesai
  print("DONE")
