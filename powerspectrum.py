# -*- coding: utf-8 -*-
# <nbformat>3.0</nbformat>

# <codecell>

from setup_matplotlib import *
from matplotlib.ticker import MaxNLocator

# <headingcell level=2>

# Load data

# <codecFreeSans>

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


# <headingcell level=2>

# Create the plot

# <codecell>

def cm2inch(cm):
    return cm *0.393701

width = 17.
for width in [17., 12., 8.8]:
    fig = plt.figure(figsize=(cm2inch(width), cm2inch(width*6/8.)))
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
    plt.legend(frameon=False)

    # labels
    plt.xlabel(r"$\ell$"); plt.ylabel(r"$\ell(\ell+1)C_\ell / 2\pi \:  \left[ \mu K^2 \right]$")
    ax.yaxis.labelpad = 10*width/17.; ax.xaxis.labelpad = 10*width/17. # distance of axis label to tick labels
    if width < 10:
        ax.yaxis.set_major_locator(MaxNLocator(nbins=5))
    plt.ylim([-1000, 7500]); plt.xlim([0, 3000]);
    plt.subplots_adjust(left=0.01, right=0.99, top=0.99, bottom=0.01)
    # set vertical y axis ticklables
    for ticklabel in ax.yaxis.get_ticklabels():
        ticklabel.set_rotation("vertical")

    plt.savefig("testb/powerspectrum_%dmm.pdf" % int(width*10), bbox_inches='tight')

    # <codecell>
