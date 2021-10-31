import numpy as np
from scipy.integrate import odeint
import matplotlib.pyplot as plt

# initial condition
y0 = [4,0]

# definisi selang t
delta_1 = 50
delta_2 = 100

# time points
t_eksak = np.linspace(0,2*np.pi,delta_2)
t_ode = np.linspace(0,2*np.pi,delta_1)

# function that returns dy/dt
def model(y,t):
    return(y[1], - 5*y[0])

# fungsi solusi eksak
def y(t):
    y = 4 * np.cos(np.sqrt(5)*t)
    return y

# solve ODE
y_ode_hit = odeint(model,y0,t_ode)
y_ode = y_ode_hit[:,0]

# y eksak
y_exact = y(t_eksak)

# =============================
# metode euler
a = float(0)
b = float(2*np.pi)
h = float(0.005)
n = int((b-a)/h) 
z = np.zeros(n)
z[0] = 4
w = np.zeros(n)
w[0] = 0
for k in range(0,(n-1)):
  z[k+1] = z[k] + h*w[k]
  w[k+1] = w[k] + h * -5 * z[k]
y_euler = z
t_euler = np.linspace(a,b,n)

# =============================
# plot results
plt.figure(figsize = (16,9))
plt.scatter(t_ode,y_ode,color = "green",label='y ode')
plt.plot(t_euler,y_euler,color = "blue",label='y euler',linewidth=.5,linestyle="-")
plt.plot(t_eksak,y_exact,color = "red",linewidth=2,linestyle="dotted",label='y exact')
plt.xlabel('time')
plt.ylabel('y(t)')
plt.legend()
plt.savefig('soal_2.png',dpi = 450)
