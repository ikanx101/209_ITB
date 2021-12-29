from mpi4py import MPI
import time

comm = MPI.COMM_WORLD
rank = comm.Get_rank()
size = 8

# hitung waktu mulai
mulai = time.time()

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
  f = open("rekap runtime.txt","w+")
  f.write("MIDPOINT\n\nBroadcast\n\nNilai integral: ")
  f.write(str(total))
  f.write("\n\nRuntime: ")
  f.write(str(waktu))
  f.close()
  # selesai
  print("DONE")
