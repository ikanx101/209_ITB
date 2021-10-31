import numpy as np
from scipy.integrate import odeint
import matplotlib.pyplot as plt

# initial condition
y0 = 5
k = 0.3

# definisi selang t
delta_1 = 5
delta_2 = 10
delta_3 = 30

# function that returns dy/dt
def model(y,t):
    dydt = -k * y
    return dydt

# time points
t_1 = np.linspace(0,20,delta_1)
t_2 = np.linspace(0,20,delta_2)
t_3 = np.linspace(0,20,delta_3)

# solve ODE
y_1 = odeint(model,y0,t_1)
y_2 = odeint(model,y0,t_2)
y_3 = odeint(model,y0,t_3)

# plot results
plt.figure(figsize = (16,9))
plt.plot(t_1,y_1,color = "red",linewidth=0.5,linestyle="--",label='delta = 5')
plt.plot(t_2,y_2,color = "blue",linewidth=1,linestyle="--",label='delta = 10')
plt.plot(t_3,y_3,color = "green",linewidth=2,linestyle="-",label='delta = 30')
plt.xlabel('time')
plt.ylabel('y(t)')
plt.legend()
plt.savefig('soal_1a.png',dpi = 450)
