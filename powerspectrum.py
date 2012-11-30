# -*- coding: utf-8 -*-
# <nbformat>3.0</nbformat>

# <codecell>

import numpy as np
import matplotlib
matplotlib.use("PDF")
import matplotlib.pyplot as plt

# <headingcell level=2>

# Load data

# <codecell>

power_spectrum = np.loadtxt("data/bf_cbipap5_all.dat") # l, C_l

print power_spectrum[:3]

# <codecell>

boomerang = np.loadtxt("data/boom_powers.dat", comments="!") # l left, l right, C_l (uK^2), +/- 
print boomerang[:3]

# <codecell>

dasi = np.loadtxt("data/dasi_powers.dat", comments="!") # l eff, l left, l right, C_l (uK^2), +/- 
print dasi[:3]

# <codecell>

maxima = np.loadtxt("data/maxima_powers.dat", comments="#") # l eff, l left, l right, C_l (uK^2), +, - 
print maxima[:3]

# <codecell>

even = np.loadtxt("data/joint_final_iso_0.08_200_even.fdat") # lmin, lmax, l-eff, Cl (uk^2), +err, -err
print even[:3].astype(np.int)

# <codecell>

odd = np.loadtxt("data/joint_final_iso_0.08_200_odd.fdat") # lmin, lmax, l-eff, Cl (uk^2), +err, -err
print odd[:3].astype(np.int)

# <headingcell level=2>

# Configure Matplotlib options

# <codecell>

from matplotlib import rc
rc('font',**{'family':'sans-serif','sans-serif':['Helvetica'], 'size':14})
## for Palatino and other serif fonts use:
#rc('font',**{'family':'serif','serif':['Palatino']})
rc('text', usetex=True)

# <headingcell level=2>

# Create the plot

# <codecell>

fig = plt.figure(figsize=(8, 6))
# this should be changed for making a panel of multiple figures
ax = fig.add_subplot(111)

# maxima
for ell_eff, ell_left, ell_right, C_ell, sigma_left, sigma_right in maxima:
    # Rectangle gets the position of the bottom left corner as first argument,
    # then width and height
    rect = plt.Rectangle((ell_left, C_ell - sigma_left), ell_right-ell_left, sigma_left + sigma_right, facecolor="lightgrey", 
edgecolor="white", alpha=.8)
    ax.add_patch(rect)  
# add label only to 1 rectangle
rect.set_label("Maxima")

# boomerang
for ell_left, ell_right,C_ell,sigma in boomerang: # unpack row by row
    # Rectangle gets the position of the bottom left corner as first argument,
    # then width and height
    rect = plt.Rectangle((ell_left, C_ell - sigma), ell_right-ell_left, 2*sigma, facecolor="grey", 
edgecolor="white", alpha=.7)
    ax.add_patch(rect)
rect.set_label("Boomerang")

    # dasi
for ell_eff, ell_left, ell_right,C_ell,sigma in dasi:
    # Rectangle gets the position of the bottom left corner as first argument,
    # then width and height
    rect = plt.Rectangle((ell_left, C_ell - sigma), ell_right-ell_left, 2*sigma, facecolor="magenta", 
edgecolor="white", alpha=.5)
    ax.add_patch(rect)
rect.set_label("Dasi")
   
# power spectrum
plt.plot(power_spectrum[:,0], power_spectrum[:,1], "k", label="Best fit")

# errorbar
plt.errorbar(even[:,2], even[:,3], even[:,4], (even[:,1]-even[:,0])/2., fmt='gs', label="CBI: even") # green squares
plt.errorbar(odd[:,2], odd[:,3], odd[:,4], (odd[:,1]-odd[:,0])/2., fmt='bo', label="CBI: odd") # blue circles
    
# x axis
plt.hlines(0, 0, 3000)

# legend
plt.legend()

# labels
plt.xlabel("$\ell$"); plt.ylabel("$\ell(\ell+1)C_\ell / 2\pi  [ \mu K^2]$")
plt.ylim([-1000, 8000]); plt.xlim([0, 3000]);
# set vertical y axis ticklables
for ticklabel in ax.yaxis.get_ticklabels():
    ticklabel.set_rotation("vertical")

plt.savefig("testb/powerspectrum.pdf", bbox_inches='tight')

# <codecell>


