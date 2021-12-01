rm(list=ls())

soa_mrf_ip_3_var = function(
  N,	    # banyak titik
  x1_d,  # batas bawah x1  
  x1_u,  # batas atas x1
  x2_d,  # batas bawah x2
  x2_u,  # batas atas x2
  x3_d,  # batas bawah x3
  x3_u,  # batas atas x3
  rot,	  # berapa banyak rotasi
  k_max, # iterasi maks
  r){	  # berapa rate konstraksi
  
  # N pasang titik random di selang [a,b] di R3
  x1 = runif(N,x1_d,x1_u)
  x2 = runif(N,x2_d,x2_u)
  x3 = runif(N,x3_d,x3_u)
  
  # hitung theta
  theta = 2*pi / rot
  # definisi matriks rotasi
  R12 = matrix(c(cos(theta),-sin(theta),0,
                 sin(theta),cos(theta),0,
                 0,0,1),
               ncol = 3,byrow = T)
  R13 = matrix(c(cos(theta),0,-sin(theta),
                 0,1,0,
                 sin(theta),0,cos(theta)),
               ncol = 3,byrow = T)
  R23 = matrix(c(1,0,0,
                 0,cos(theta),-sin(theta),
                 0,sin(theta),cos(theta)),
               ncol = 3,byrow = T)
  
  
  # bikin data frame
  temp = data.frame(x1,x2,x3) %>% 
    mutate(f = f(round(x1,0),
                 round(x2,0),
                 round(x3,0)
                 )
           )
  # proses iterasi
  for(i in 1:k_max){
    # mencari titik x* dengan max(f)
    f_min = 
      temp %>% 
      # memastikan titik ada di D
      filter(x1 >= x1_d & x1 <= x1_u) %>% 
      filter(x2 >= x2_d & x2 <= x2_u) %>% 
      filter(x3 >= x3_d & x3 <= x3_u) %>% 
      # mencari titik max fungsi
      filter(f == max(f))
    # definisi pusat rotasi
    pusat = c(f_min$x1[1],f_min$x2[1],f_min$x3[1])
    for(j in 1:N){
      # kita akan ambil titiknya satu persatu
      x0 = c(temp$x1[j],temp$x2[j],temp$x3[j])
      # proses rotasi dan konstraksi terhadap pusat x*
      # diputar dengan x_bin sebagai pusat
      xk = (R23 %*% (R13 * R12)) %*% (x0-pusat)
      xk = pusat + (r * xk)
      # proses mengembalikan nilai ke temp
      temp$x1[j] = xk[1]
      temp$x2[j] = xk[2]
      temp$x3[j] = xk[3]
    }
    # hitung kembali nilai f(x1,x2)
    temp = temp %>% mutate(f = f(round(x1,0),round(x2,0),round(x3,0)))
  }
  # proses output hasil
  output = 
    temp[N,] %>% 
    filter(f == max(f)) %>% 
    mutate(x1 = round(x1,0),x2 = round(x2,0),x3 = round(x3,0),
           g = g(x1,x2,x3),f = f(x1,x2,x3))
  return(output)
}


# ========================
# solving
N = 200
a = 0  # x dan y punya batas yang sama
b = 50   # x dan y punya batas yang sama
rot = 70
k_max = 90
r = .85
# membuat fungsi g dan f
g = function(x,y,z){x^2 + y^2 + z^2 - 2445}
f = function(x,y,z){1 / (1 + abs(g(x,y,z)))}
# iterasi berulang kali agar mendapatkan hasil yang tepat
solusi = data.frame()
for(num in 1:50){
  temporary = soa_mrf_ip_3_var(N,a,b,a,b,a,b,rot,k_max,r)
  solusi = rbind(solusi,temporary)
}




rm(list=ls())
g = function(x,y,z){x^2 + y^2 + z^2 - 2445}
solusi = data.frame(x = rep(NA,900),y = rep(NA,900),z = rep(NA,900))
k = 1
for(i in 1:900000){
  number = sample(0:50,3,replace = T)
  if(g(number[1],number[2],number[3]) == 0){
    solusi$x[k] = number[1]
    solusi$y[k] = number[2]
    solusi$z[k] = number[3]
    k = k + 1
  }
}
solusi %>% distinct() %>% arrange(x,y,z) %>% filter(!is.na(x))
