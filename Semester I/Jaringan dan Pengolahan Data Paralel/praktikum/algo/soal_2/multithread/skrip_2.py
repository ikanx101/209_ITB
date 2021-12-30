import time
import threading
import math
import random

# hitung waktu mulai
mulai = time.time()

# initial condition
a = 0
b = 1
n = 10**8
h = (b-a) / n
nthreads = 8
mt_size = float(n/nthreads)
threads = []

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

for i in range (nthreads):
  t = threading.Thread(target = monte_pi,args = (mt_size,n))
  threads.append(t)
  t.start()

for t in threads:
  t.join()
  
# hitung soal
nilai = monte_pi(n)
print("Nilai integral f(x) dx adalah: ",nilai)

# hitung waktu selesai
end = time.time()
waktu = end - mulai
print(waktu)

f = open("rekap runtime Monte.txt","w+")
f.write("MONTE\n\nThreading\n\nNilai integral: ")
f.write(str(nilai))
f.write("\n\nRuntime: ")
f.write(str(waktu))
f.close()

# selesai
print("DONE")
