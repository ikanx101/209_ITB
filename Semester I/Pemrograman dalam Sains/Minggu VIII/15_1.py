# SOAL 15.1

import numpy as np
from numpy.polynomial.polynomial import polyval
import matplotlib.pyplot as plt

# pecah data menjadi 30 selang
# initial condition
M = 30     # number of data points
xi = -5.0  # first value of x
xf = 5.0   # final value

# membuat x dan y = f(x)
x = np.linspace(xi, xf, M)

# koefisien polinomial
c = np.array([7,0,4,0,1])
print ("Coefficient list")
print (c)

# menghitung y = f(x)
y = polyval(x, c)

# print (x,y)
# kita bulatkan menjadi 4 angka di belakang koma
print ("Evaluating a polynomial")

# save ke dalam csv
# memberikan nama file
f = open("15_1.csv","w+")

for j in range(M):
     print (j,". x = ",round(x[j],4),"; y = ",round(y[j],4))
     f.write(str(j)+","+str(round(x[j],4))+","+str(round(y[j],4)))

# save file
f.close()

plt.plot(x,y,'o-')
plt.xlabel('x')
plt.ylabel('y')
plt.savefig('15_1.png')
