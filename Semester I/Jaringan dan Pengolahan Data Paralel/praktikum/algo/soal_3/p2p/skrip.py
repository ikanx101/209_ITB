from mpi4py import MPI
from mpi4py.MPI import ANY_SOURCE
import timeit
import numpy as np

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

# initial condition
n = 64
dest = 0
local_n = int(n/size)

mulai = MPI.Wtime()

# hitung soal
s = operasi_matrix(local_n)

if rank == 0:
  nilai = s
  for _ in range(size - 1):
    total2 = comm.recv(source = MPI.ANY_SOURCE)
    total2 += total2
  print("Nilainya adalah: ",nilai)
  # hitung waktu selesai
  end = MPI.Wtime()
  waktu = end - mulai
  print(waktu)
  f = open("rekap runtime.txt","w+")
  f.write("P2P\n\nNilai: ")
  f.write(str(nilai))
  f.write("\n\nRuntime: ")
  f.write(str(waktu))
  f.close()
  # selesai
  print("DONE")
else:
  comm.send(s,dest=0)
