import numpy as np
from scipy.integrate import odeint
import matplotlib.pyplot as plt

# initial condition
y0 = 5
k = 0.3

# definisi selang t
delta = 30

# function that returns dy/dt
def model(y,t):
    dydt = -k * y
    return dydt

# fungsi solusi eksak
def y(t):
    y = y0*np.exp(-k*t)
    return y

# time points
t = np.linspace(0,20,delta)

# solve ODE
y_ode = odeint(model,y0,t)

# y eksak
y_exact = y(t)

# plot results
plt.figure(figsize = (16,9))
plt.plot(t,y_ode,color = "green",linewidth=2,linestyle="-",label='y ode')
plt.plot(t,y_exact,color = "red",linewidth=2,linestyle="dotted",label='y exact')
plt.xlabel('time')
plt.ylabel('y(t)')
plt.legend()
plt.savefig('soal_1b.png',dpi = 450)
