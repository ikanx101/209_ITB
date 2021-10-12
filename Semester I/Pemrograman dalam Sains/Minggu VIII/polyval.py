# This program evaluates a polynomial given
#  the coefficients in list c
# at points in list x
# J M Garrido, August 2, 2014
import numpy as np
from numpy.polynomial.polynomial import polyval
import matplotlib.pyplot as plt

M = 50     # number of data points
xi = -5.0  # first value of x
xf = 5.0   # final value

x = np.linspace(xi, xf, M)
c = np.array([7,0,4,0,1])

print ("Coefficient list")
print (c)
y = polyval(x, c)
print ("Evaluating a polynomial")
for j in range(M):
     print (x[j], y[j])
plt.plot(x,y,'o-')
plt.xlabel('x')
plt.ylabel('y')
plt.savefig('15_1.png')
