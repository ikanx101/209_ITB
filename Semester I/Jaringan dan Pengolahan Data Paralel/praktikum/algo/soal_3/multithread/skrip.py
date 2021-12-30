import time
import threading
import numpy as np

# hitung waktu mulai
mulai = time.time()

# initial condition
n = 64
nthreads = 8
mt_size = float(n/nthreads)
threads = []

# penjumlahan dan perkalian
def operasi_matrix (n,mt_size):
  # membuat matriks 10x10
  a = np.random.randint(n, size = (n,n))
  b = np.random.randint(n, size = (n,n))
  c = np.dot(a,b)
  d = np.add(a,b)
  return(c,d)

for i in range (nthreads):
  t = threading.Thread(target = operasi_matrix,args = (mt_size,n))
  threads.append(t)
  t.start()

for t in threads:
  t.join()
  
# hitung soal
nilai = operasi_matrix(n,8)
print("Nilai adalah: ",nilai)

# hitung waktu selesai
end = time.time()
waktu = end - mulai
print(waktu)

f = open("rekap runtime.txt","w+")
f.write("MnThreading\n\nNilai integral: ")
f.write(str(nilai))
f.write("\n\nRuntime: ")
f.write(str(waktu))
f.close()

# selesai
print("DONE")
