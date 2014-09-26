from setup_matplotlib import cm2inch
from glob import glob
import healpy as hp
import numpy as np
import matplotlib.pyplot as plt
from planckcolors import planck_universal_cmap

"""This script creates pdf mollview plots using the Planck universal colortable,
the output plots can then be aggregated in a single page using ../figures/FreqMapFig.tex

http://github.com/zonca/paperplots
"""

# 10^ formatter
from matplotlib import ticker
def format_func(x, pos):
    if np.abs(x) < 90:
        formatted = "%d" % x
    else:
        formatted = "%d^{%d}" % (10*np.sign(x), int(np.log10(np.abs(x))))
    out = r"$%s$" % formatted 
    return out 
formatter = ticker.FuncFormatter(format_func)

# ratio is always 1/2
xsize = 2000
ysize = xsize/2.

# this is the mollview min and max, should *NOT* be changed
vmin = -1e3; vmax = 1e7

theta = np.linspace(np.pi, 0, ysize)
phi   = np.linspace(-np.pi, np.pi, xsize)
longitude = np.radians(np.linspace(-180, 180, xsize))
latitude = np.radians(np.linspace(-90, 90, ysize))

nside = 1024
# project the map to a rectangular matrix xsize x ysize
PHI, THETA = np.meshgrid(phi, theta)
grid_pix = hp.ang2pix(nside, THETA, PHI)

cmap = planck_universal_cmap
colormaptag = "universal_"
width = 18

map_offsets = {
    100 : -12.29,
    143 : -21.26,
    217 : -68.2547,
    353 : -452.605,
}

component = ["I_STOKES"]
smoothing_degrees = 1

c = 299792458  # speed of light in m/s
K_b = 1.3806488e-23  # Boltzmann constant in J/K

for freq in [30, 44, 70, 100, 143, 217, 353, 545, 857]: 

    # Use glob to find filename of the map
    filename = glob("NEVERCOMMIT/*_%03d_*.fits" % freq)[0]
    m = hp.ud_grade(hp.read_map(filename, field=component), nside) * 1e6
    m += map_offsets.get(freq, 0)
    if smoothing_degrees:
        m = hp.smoothing(m, fwhm=np.radians(smoothing_degrees))

    grid_map = m[grid_pix]

    unit = r"$\mathrm{\mu K}$"

    if freq > 500:
        # input maps are in MJy/sr
        # Jysr_to_muKRJ = c**2 / 2. / K_b / (freq * 1e9) **2 * 1e-26 * 1e6 #  [uK_RJ/(Jy/sr)]
        grid_map /= 1e3 # convert to kJy/sr
        unit = r"$\mathrm{kJy/sr}$"

    fig = plt.figure(figsize=(cm2inch(width), cm2inch(width*3/4.)))
    # matplotlib is doing the mollveide projection
    ax = fig.add_subplot(111,projection='mollweide')

    # rasterized makes the map bitmap while the labels remain vectorial
    # flip longitude to the astro convention
    image = plt.pcolormesh(longitude[::-1], latitude, grid_map, vmin=vmin, vmax=vmax, rasterized=True, cmap=cmap)

    # remove ticks and tick labels
    plt.grid(False)
    ax.xaxis.set_ticklabels([])
    ax.xaxis.set_ticks([])
    ax.yaxis.set_ticklabels([])
    ax.yaxis.set_ticks([])

    # colorbar
    colorbar_ticks = np.array([-1e3, -1e2, -10, -1, 0, 1, 10, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7])
    colorbar_boundaries = np.concatenate([-1 * np.logspace(0, 3, 80)[::-1], np.linspace(-1, 1, 10), np.logspace(0, 7, 150)])
    cb = fig.colorbar(image, orientation='horizontal', format=formatter, shrink=1., pad=0.05, boundaries = colorbar_boundaries, ticks=colorbar_ticks)
    cb.ax.xaxis.set_label_text(unit)
    # workaround for issue with viewers, see colorbar docstring
    cb.solids.set_edgecolor("face")
    # align the colorbar tick labels
    for label in cb.ax.xaxis.get_ticklabels():
        label.set_verticalalignment("baseline")
        label.set_position((label.get_position()[0], -.3))

    plt.savefig("../figures/PlanckFig_map_" + colormaptag + "python_%dmm_%dGHz.pdf" % (int(width*10), freq), bbox_inches='tight', pad_inches=0.02)
