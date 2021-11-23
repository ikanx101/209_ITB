import numpy as np
import matplotlib.pyplot as plt
plt.style.use('seaborn-poster')

# compute function
omega = 100
epsilon = 0.01

x = np.arange(0, 2*np.pi, 0.01) 
# compute function
y = -np.sin(x) 
y_noise = y + epsilon*omega*np.cos(omega*x)

# Plot solution
plt.figure(figsize = (12, 8))
plt.plot(x, y_noise, 'r-', \
         label = 'Derivative cos(x) + noise')
plt.plot(x, y, 'b-', \
         label = 'Derivative of cos(x)')

plt.xlabel('x')
plt.ylabel('y')

plt.legend()
#plt.show()
plt.savefig('plot4.png',dpi = 450)
