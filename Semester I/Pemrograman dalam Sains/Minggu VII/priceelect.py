# Program:   priceelect.py
# Python program for computing monthly price for electric energy
# J. M. Garrido, August 2014.
#dimodifikasi ke python 3 dalam kuliah SK5003 dan di tambah beberapa baris plot dan penyimpanan data
 
import numpy as np
import matplotlib.pyplot as plt


N = 12;   # 12 months
e = np.array([10.22, 10.36, 10.49, 10.60, 10.68, 10.80, 10.88, 10.94, 11.05, 11.15, 11.26, 11.40])
mm = np.arange(N)  # month array
m = mm + 1
print ("Montly price of electricity\n")

# Array Monthly price for electric energy  
print (e)
#  differences in sequence e 
print ("\nDifferences of the given data\n")
de = np.diff(e)
print (de)
#  average of increments 
deltax = np.mean(de)
print ("\nAverage difference: ", deltax)
# Calculating price of electric energy (model)
cc = mm * deltax + e[0]
print ("\nCalculated prices of electricity: \n")
print (cc)
print ("\nData for Plotting\n")
f= open("dataprice.csv","w+")  #mekanisme python cara membuat file baru
for j in range(N):
     print (m[j],",", e[j],",", cc[j]) #cetak dilayar
     f.write(str(m[j])+","+str(e[j])+","+str(cc[j])+"\n") #menyimpan data ke file
    
f.close()   #tutup/simpan file

# plot results
plt.plot(m,cc,'o-')
plt.plot(m,e)
plt.xlabel('month')
plt.ylabel('price')


plt.show()


