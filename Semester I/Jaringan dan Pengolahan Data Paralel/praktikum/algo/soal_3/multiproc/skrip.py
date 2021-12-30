import time
import multiprocessing as mp
import numpy as np

mulai = time.time()

# initial condition
n = 64

# penjumlahan dan perkalian
def operasi_matrix (n):
  # membuat matriks 10x10
  a = np.random.randint(n, size = (n,n))
  b = np.random.randint(n, size = (n,n))
  c = np.dot(a,b)
  d = np.add(a,b)
  return(c,d)

if __name__ == '__main__':
    with mp.Pool(8) as p:
        print(p.map(operasi_matrix, [n]))

end = time.time()
waktu = end - mulai

print(waktu)

f = open("rekap runtime.txt","w+")
f.write("\n\nRuntime: ")
f.write(str(waktu))
f.close()

# selesai
print("DONE")
