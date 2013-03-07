# Configure Matplotlib options
from setup_matplotlib import *
from matplotlib.ticker import MaxNLocator

# Load data
power_spectrum = np.loadtxt("../../data/bf_cbipap5_all.dat") # l, C_l
boomerang = np.loadtxt("../../data/boom_powers.dat", comments="!") # l left, l right, C_l (uK^2), +/- 
dasi = np.loadtxt("../../data/dasi_powers.dat", comments="!") # l eff, l left, l right, C_l (uK^2), +/- 
maxima = np.loadtxt("../../data/maxima_powers.dat", comments="#") # l eff, l left, l right, C_l (uK^2), +, - 
even = np.loadtxt("../../data/joint_final_iso_0.08_200_even.fdat") # lmin, lmax, l-eff, Cl (uk^2), +err, -err
odd = np.loadtxt("../../data/joint_final_iso_0.08_200_odd.fdat") # lmin, lmax, l-eff, Cl (uk^2), +err, -err

# Create the plot

for width in [18., 12., 8.8]:
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
    rect.set_label("DASI")
       
    # power spectrum
    plt.plot(power_spectrum[:,0], power_spectrum[:,1], "k", label="Best fit")

    # errorbar
    plt.errorbar(even[:,2], even[:,3], even[:,4], (even[:,1]-even[:,0])/2., fmt='gs', label="CBI: even") # green squares
    plt.errorbar(odd[:,2], odd[:,3], odd[:,4], (odd[:,1]-odd[:,0])/2., fmt='bo', label="CBI: odd") # blue circles
        
    # x axis
    plt.hlines(0, 0, 3300)

    # legend
    leg = plt.legend(frameon=True)
    # remove box around legend
    leg.get_frame().set_edgecolor("white")
    leg.get_frame().set_alpha(.8)

    # labels
    plt.xlabel(r"$\ell$"); plt.ylabel(r"$\ell(\ell+1)C_\ell / 2\pi \:  \left[ \mu \mathrm{K}^2 \right]$")
    ax.yaxis.labelpad = 10*width/17.; ax.xaxis.labelpad = 10*width/17. # distance of axis label to tick labels

    # reduce ticks for small figures
    if width < 10:
        ax.yaxis.set_major_locator(MaxNLocator(nbins=5))
    
    # grid
    plt.grid(True, which="major", axis="both")

    # axes limits
    plt.ylim([-1000, 7500]); plt.xlim([0, 3100]);

    # reduce white space around figure
    plt.subplots_adjust(left=0, right=1, top=1, bottom=0)

    # set vertical y axis ticklables
    for ticklabel in ax.yaxis.get_ticklabels():
        ticklabel.set_rotation("vertical")

    # save to pdf with right bounding box
    plt.savefig("../figures/PlanckFig_lineplot_python_%dmm.pdf" % int(width*10), bbox_inches='tight', pad_inches=0.02)
