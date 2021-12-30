import numpy as np
import time

# hitung waktu mulai
mulai = time.time()

# ukuran matriks
n = 64

# penjumlahan dan perkalian
def operasi_matrix (n):
  # membuat matriks 10x10
  a = np.random.randint(n, size = (n,n))
  b = np.random.randint(n, size = (n,n))
  c = np.dot(a,b)
  d = np.add(a,b)
  return(c,d)

hitung = operasi_matrix(n)

# hitung waktu selesai
end = time.time()
waktu = end - mulai
print(waktu)

f = open("rekap runtime.txt","w+")
f.write("Serial\n\nHasil: ")
f.write(str(hitung))
f.write("\n\nRuntime: ")
f.write(str(waktu))
f.close()

# selesai
print("DONE")
