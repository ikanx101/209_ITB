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

if rank == 0:
  n = 10**8
else:
  n = 0
  
n = comm.bcast(n,root = 0)

local_n = n/size

# hitung soal
s = monte_pi(local_n)
nilai = comm.reduce(s,op = MPI.SUM,root = 0)

if rank == 0:
  print("Nilai integral f(x) dx adalah: ",nilai)
  # hitung waktu selesai
  end = time.time()
  waktu = end - mulai
  print(waktu)
  f = open("rekap runtime Monte.txt","w+")
  f.write("MONTE\n\nBroadcast-Reduce\n\nNilai integral: ")
  f.write(str(nilai))
  f.write("\n\nRuntime: ")
  f.write(str(waktu))
  f.close()
  # selesai
  print("DONE")
