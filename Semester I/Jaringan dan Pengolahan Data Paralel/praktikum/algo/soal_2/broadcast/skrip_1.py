from mpi4py import MPI
import time
import random
import math

comm = MPI.COMM_WORLD
rank = comm.Get_rank()
size = 8

mulai = time.time()

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

if rank == 0 :
  n = 10**8
else:
  n = 0

n = comm.bcast(n,root = 0)

s = int_numeric(a,b,n)
total = s


# hitung soal
if rank == 0:
  print("Nilai integral f(x) dx adalah: ",total)
  end = time.time()
  waktu = end - mulai
  print(waktu)
  # membuat file rekap .txt
  f = open("rekap runtime mid.txt","w+")
  f.write("MIDPOINT\n\nBroadcast\n\nNilai integral: ")
  f.write(str(total))
  f.write("\n\nRuntime: ")
  f.write(str(waktu))
  f.close()
  # selesai
  print("DONE")
