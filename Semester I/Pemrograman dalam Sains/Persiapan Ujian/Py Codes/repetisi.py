# ===========================================================
# rangkuman semua pycodes yang digunakan selama kuliah
#
# ikanx101.com
# ===========================================================

# program repetisi dengan while
a = 1
a_mak = 10
while a <= a_mak:
   print(a)
   a += 1

# program repetisi dengan for
a = 1
a_mak = 10
for i in range(a,(a_mak + 1),1):
   print(i)

# fungsi range itu membuat sequence dari a hingga a_mak-1
 # oleh karena itu, kita selalu tambahkan 1 agar nilainya jadi 1_mak

# program repetisi dengan for
 # namun kita modifikasi nilai range-nya
n = 6
for i in range(1,20,3):
    perkalian = n * i
    print( "Perkalian", n,"x", i,"=", perkalian)

