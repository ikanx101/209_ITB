from mpi4py import MPI
import time
import numpy as np

comm = MPI.COMM_WORLD
rank = comm.Get_rank()
size = 8

# hitung waktu mulai
mulai = time.time()

# penjumlahan dan perkalian
def operasi_matrix (n):
  # membuat matriks 10x10
  a = np.random.randint(n, size = (n,n))
  b = np.random.randint(n, size = (n,n))
  c = np.dot(a,b)
  d = np.add(a,b)
  return(c,d)
  
if rank == 0 :
  n = 50
else:
  n = 0

n = comm.bcast(n,root = 0)

s = operasi_matrix(n)
total = s


# hitung soal
if rank == 0:
  print("Nilai integral f(x) dx adalah: ",total)
  end = time.time()
  waktu = end - mulai
  print(waktu)
  # membuat file rekap .txt
  f = open("rekap runtime.txt","w+")
  f.write("Broadcast\n\nNilai integral: ")
  f.write(str(total))
  f.write("\n\nRuntime: ")
  f.write(str(waktu))
  f.close()
  # selesai
  print("DONE")
