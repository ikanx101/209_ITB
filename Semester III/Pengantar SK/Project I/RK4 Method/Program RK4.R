# definisi dan initial condition
r_a = .5   # rate peluruhan A dan pertumbuhan B
r_b = .4   # rate peluruhan B dan pertumbuhan C
r_c = .3   # rate peluruhan C

q_a = 10  # massa awal zat radioaktif A
q_b = 0   # massa awal zat radioaktif B
q_c = 0   # massa awal zat radioaktif C
t   = 0   # waktu awal t = 0

dt = .25           # delta t
iter_length = 5    # panjang iterasi
num_iter = (iter_length / dt) + 1 # berapa banyak iterasi 
h = .01

d_a = function(q_a,t){
  (-r_a * q_a) * dt
  }
d_b = function(q_a,q_b,t){
  ((r_a * q_a) - (r_b * q_b)) * dt
  }
d_c = function(q_b,q_c,t){
  ((r_b * q_b) - (r_c * q_c)) * dt
  }


for(i in 1:num_iter){
  # kita akan hitung dulu zat a
  x = q_a
  y = t
  
  k1 = d_a(x0,y0)
  k2 = d_a(x0 + 0.5*h,y0 + 0.5*k1*h)
  k3 = d_a(x0 + 0.5*h,y0 + 0.5*k2*h)
  k4 = d_a(x0 + h,y0 + k3*h)
  y0 = y0 + (1/6)*(k1 + 2*k2 + 2*k3 + k4) * h
  
  t = t + h
  
  x = c(x, x0)
  y = c(y, y0)
}

rk_4order = function(f, x0, y0, h, n){
  # initial condition
  x = x0
  y = y0
  # proses iterasi
  for(i in 1:n){
    k1 = f(x0,y0)
    k2 = f(x0 + 0.5*h,y0 + 0.5*k1*h)
    k3 = f(x0 + 0.5*h,y0 + 0.5*k2*h)
    k4 = f(x0 + h,y0 + k3*h)
    y0 = y0 + (1/6)*(k1 + 2*k2 + 2*k3 + k4) * h
    x0 = x0 + h
    x = c(x, x0)
    y = c(y, y0)
  }
  # output
  output = data.frame(x = x,
                      y = y)
  return(output)
}