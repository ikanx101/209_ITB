from mpi4py import MPI
from mpi4py.MPI import ANY_SOURCE
import timeit
import math
import random

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
n = 10**8
dest = 0
local_n = n/size

mulai = MPI.Wtime()

# hitung soal
s = monte_pi(local_n)

if rank == 0:
  nilai = s
  for _ in range(size - 1):
    total2 = comm.recv(source = MPI.ANY_SOURCE)
    total2 += total2
  print("Nilai integral f(x) dx adalah: ",nilai)
  # hitung waktu selesai
  end = MPI.Wtime()
  waktu = end - mulai
  print(waktu)
  f = open("rekap runtime Monte.txt","w+")
  f.write("MONTE\n\np2p\n\nNilai integral: ")
  f.write(str(nilai))
  f.write("\n\nRuntime: ")
  f.write(str(waktu))
  f.close()
  # selesai
  print("DONE")
else:
  comm.send(s,dest=0)
