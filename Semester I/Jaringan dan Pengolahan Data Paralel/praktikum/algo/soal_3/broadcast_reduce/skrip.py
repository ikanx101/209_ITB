from mpi4py import MPI
import time
import numpy as np

# hitung waktu mulai
mulai = time.time()

comm = MPI.COMM_WORLD
rank = comm.Get_rank()
size = 8

# penjumlahan dan perkalian
def operasi_matrix (n):
  # membuat matriks 10x10
  a = np.random.randint(n, size = (n,n))
  b = np.random.randint(n, size = (n,n))
  c = np.dot(a,b)
  d = np.add(a,b)
  return(c,d)
 
if rank == 0:
  n = 64
else:
  n = 0
  
n = comm.bcast(n,root = 0)

local_n = int(n/size)

# hitung soal
s = operasi_matrix(local_n)
nilai = comm.reduce(s,op = MPI.SUM,root = 0)

if rank == 0:
  print("Nilainya adalah: ",nilai)
  # hitung waktu selesai
  end = time.time()
  waktu = end - mulai
  print(waktu)
  f = open("rekap runtime.txt","w+")
  f.write("Broadcast-Reduce\n\nNilai integral: ")
  f.write(str(nilai))
  f.write("\n\nRuntime: ")
  f.write(str(waktu))
  f.close()
  # selesai
  print("DONE")
