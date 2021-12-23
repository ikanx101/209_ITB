# ===========================================================
# rangkuman semua pycodes yang digunakan selama kuliah
#
# ikanx101.com
# ===========================================================

# program untuk converter celcius ke farenheit dengan function
def convert (c):
	f = c * (9.0/5.0) + 32.0
	f = round(f,4)
	return(f)

celcius = input("Masukkan temperatur dalam Celcius: ")
celcius = float(celcius)
celcius = round(celcius,1)

print("Suhu dalam Celcius: ",celcius,
      " sedangkan dalam Farenheit menjadi: ",convert(celcius))
