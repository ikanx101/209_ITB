rm(list=ls())

dt = .5
t0 = 0
P0 = 1000

# fungsi dP/dt
f = function(t,P){
  return(t - P)
}

# iterasi pertama
t1 = t0 + 1 * dt

Y1 = P0 + f(t0,P0) * dt

P1 = P0 + 0.5 * (f(t0,P0) + f(t1,Y1)) * dt

# iterasi kedua
t2 = t0 + 2 * dt

Y2 = P1 + f(t1,P1) * dt

P2 = P1 + 0.5 * (f(t1,P1) + f(t2,Y2)) * dt

# iterasi ketiga
t3 = t0 + 3 * dt

Y3 = P2 + f(t2,P2) * dt

P3 = P2 + 0.5 * (f(t2,P2) + f(t3,Y3)) * dt

# iterasi keempat
t4 = t0 + 4 * dt

Y4 = P3 + f(t3,P3) * dt

P4 = P3 + 0.5 * (f(t3,P3) + f(t4,Y4)) * dt

t1
Y1
P1

t2
Y2
P2

t3
Y3
P3

t4
Y4
P4



Pt = function(t){(1001 * exp(-t)) + t - 1}

abs(Pt(0.5) - P1) / abs(Pt(0.5)) * 100 
abs(Pt(1) - P2) / abs(Pt(1)) * 100
abs(Pt(1.5) - P3) / abs(Pt(1.5)) * 100
abs(Pt(2) - P4) / abs(Pt(2)) * 100