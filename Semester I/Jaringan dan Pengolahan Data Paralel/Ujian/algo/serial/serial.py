import time

a = 0
b = 2
c = 2
d = 3
nx = 10**4
ny = 10**4

def fxy (x,y):
  f = float(2*float(x) + float(y))
  return(f)

hx = (b-a)/nx
hy = (d-c)/ny

int = float(0)
start = time.time()

for i in range(nx):
  for j in range(ny):
   xi = a + hx/2 + i*hx
   yj = c + hy/2 + j*hy
   int = int + hx*hy*fxy(xi,yj)

end = time.time()
waktu = end - start

print("Nilai integral: ",int)
print("Runtime: ",waktu)
