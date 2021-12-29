from mpi4py import MPI
from mpi4py.MPI import ANY_SOURCE
import timeit

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
n = 10**8
dest = 0
h = (b-a)/n
local_n = n/size
local_a = a + rank * local_n * h
local_b = local_a + local_n * h

mulai = MPI.Wtime()

# hitung soal
s = int_numeric(local_a,local_b,local_n)

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
  f = open("rekap runtime.txt","w+")
  f.write("MIDPOINT\n\np2p\n\nNilai integral: ")
  f.write(str(nilai))
  f.write("\n\nRuntime: ")
  f.write(str(waktu))
  f.close()
  # selesai
  print("DONE")
else:
  comm.send(s,dest=0)
