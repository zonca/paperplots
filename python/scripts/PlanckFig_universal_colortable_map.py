from setup_matplotlib import cm2inch
from glob import glob
import healpy as hp
import numpy as np
import matplotlib.pyplot as plt


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

# setup linear colormap
from matplotlib.colors import ListedColormap
planck_freqmap_cmap = ListedColormap(np.loadtxt("../../data/Planck_FreqMap_RGB.txt")/255.)
planck_freqmap_cmap.set_bad("gray") # color of missing pixels
planck_freqmap_cmap.set_under("white") # color of background, necessary if you want to use

# setup nonlinear colormap
from matplotlib.colors import LinearSegmentedColormap
class PlanckUniversalColormap(LinearSegmentedColormap):
    name = "planckuniv"
    def __init__(self, cmap):
        self.cmap = cmap
        self.N = self.cmap.N

    def __call__(self, xi, alpha=1.0, **kw):
        x = xi * (1e7+1e3) - 1e3
        yi = self.modsinh(x)
        # range 0-1
        yi = (yi + 3)/10.
        return self.cmap(yi, alpha)

    def modsinh(self, x):
        return np.log10(0.5*(x + np.sqrt(x**2 + 4)))

planck_universal_cmap = PlanckUniversalColormap(planck_freqmap_cmap)

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
    30  : -64.7,
    44  : -24.1,
    70  : -28.5,
    100 : -30.1,
    143 : -55.7,
    217 : -133,
    353 : -681 + 250,
}

c = 299792458  # speed of light in m/s
K_b = 1.3806488e-23  # Boltzmann constant in J/K

#for freq in [30, 44, 70, 100, 143, 217, 353, 545, 857]: 
for freq in [545, 857]:

    m = hp.ud_grade(hp.read_map(glob("NEVERCOMMIT/*_%d_*.fits" % freq)[0]), nside) * 1e6 + map_offsets.get(freq, 0)
    grid_map = m[grid_pix]

    unit = r"$\mathrm{\mu K}$"

    if freq > 500:
        # Jysr_to_muKRJ = c**2 / 2. / K_b / (freq * 1e9) **2 * 1e-26 * 1e6 #  [uK_RJ/(Jy/sr)]
        grid_map /= 1e3
        unit = r"$\mathrm{kJy/sr}$"

    fig = plt.figure(figsize=(cm2inch(width), cm2inch(width*3/4.)))
    # matplotlib is doing the mollveide projection
    ax = fig.add_subplot(111,projection='mollweide')

    # remove white space around the image
    # plt.subplots_adjust(left=0.01, right=0.99, top=0.95, bottom=0.01)

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
