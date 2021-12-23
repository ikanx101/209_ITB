# ===========================================================
# rangkuman semua pycodes yang digunakan selama kuliah
#
# ikanx101.com
# ===========================================================

# program plot menggunakan matplotlib
# sebagai contoh:
  # f(x) = |log(x)|
  # 0 < x <= 10

# STEP 1
import numpy as np
import matplotlib.pyplot as plt

# STEP 2
X = np.linspace(0.002, 10, 100)

# STEP 3
Y = abs(np.log(X)) 

# STEP 4
plt.plot(X, Y)
plt.savefig("dual_plot.png", dpi=120)
