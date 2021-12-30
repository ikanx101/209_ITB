import time
import multiprocessing as mp
import random
import math

mulai = time.time()

# initial condition
a = 0
b = 1
n = 10**8
h = (b-a)/n

# bikin fungsi f(x)
def fx (x) :
   return(math.sqrt(1 - x**2) * 4)

# bikin fungsi untuk monte carlo
def monte_pi (rank, nprocs,n):
   n = int(n)
   monte = float(0)
   for i in range(rank,n,nprocs):
     xi = random.random()
     yi = fx(xi)
     monte = monte + yi
   return(monte/n)

nprocs = mp.cpu_count()
inputs = [(rank,nprocs,n) for rank in range(nprocs)]

pool = mp.Pool(processes = nprocs)
result = pool.starmap(monte_pi,inputs)

nilai = sum(result)

end = time.time()
waktu = end - mulai

print(nilai)
print(waktu)

f = open("rekap runtime Monte.txt","w+")
f.write("MONTE\n\nMultiprocessing\n\nNilai integral: ")
f.write(str(nilai))
f.write("\n\nRuntime: ")
f.write(str(waktu))
f.close()

# selesai
print("DONE")
