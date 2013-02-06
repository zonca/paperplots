from setup_matplotlib import *
import healpy as hp

m = hp.ma(hp.read_map("../../data/wmap_band_iqumap_r9_7yr_W_v4.fits", 0)) * 1e3 # muK
nside = hp.npix2nside(len(m))

# setup colormap
from matplotlib.colors import ListedColormap
colombi1_cmap = ListedColormap(np.loadtxt("../../data/parchment1.dat")/255.)
colombi1_cmap.set_bad("gray") # color of missing pixels

use_mask = False

# using directly matplotlib instead of mollview has higher
# quality output, I plan to merge this into healpy

# ratio is always 1/2
xsize = 2000
ysize = xsize/2.

unit = r"$\mathrm{\mu K}$"

# this is the mollview min and max
vmin = -1e3; vmax = 1e3

theta = np.linspace(np.pi, 0, ysize)
phi   = np.linspace(-np.pi, np.pi, xsize)
longitude = np.radians(np.linspace(-180, 180, xsize))
latitude = np.radians(np.linspace(-90, 90, ysize))

# project the map to a rectangular matrix xsize x ysize
PHI, THETA = np.meshgrid(phi, theta)
grid_pix = hp.ang2pix(nside, THETA, PHI)

if use_mask:
    # mask
    m.mask = np.logical_not(hp.read_map("../../data/wmap_ext_temperature_analysis_mask_r9_7yr_v4.fits"))
    grid_mask = m.mask[grid_pix]
    grid_map = np.ma.MaskedArray(m[grid_pix], grid_mask)
else:
    grid_map = m[grid_pix]

from matplotlib.projections.geo import GeoAxes

class ThetaFormatterShiftPi(GeoAxes.ThetaFormatter):
    """Shifts labelling by pi

    Shifts labelling from -180,180 to 0-360"""
    def __call__(self, x, pos=None):
        if x != 0:
            x *= -1
        if x < 0:
            x += 2*np.pi
        return super(ThetaFormatterShiftPi, self).__call__(x, pos)

for width in [18., 12., 8.8]:
    for cmap, colormaptag in [(None, ''), (colombi1_cmap, "colombi1_")]:

        fig = plt.figure(figsize=(cm2inch(width), cm2inch(width/2.)))
        # matplotlib is doing the mollveide projection
        ax = fig.add_subplot(111,projection='mollweide')

        # remove white space around the image
        plt.subplots_adjust(left=0.01, right=0.99, top=0.95, bottom=0.01)

        # rasterized makes the map bitmap while the labels remain vectorial
        # flip longitude to the astro convention
        image = plt.pcolormesh(longitude[::-1], latitude, grid_map, vmin=vmin, vmax=vmax, rasterized=True, cmap=cmap)

        # graticule
        ax.set_longitude_grid(60)
        ax.xaxis.set_major_formatter(ThetaFormatterShiftPi(60))
        if width < 10:
            ax.set_latitude_grid(45)
            ax.set_longitude_grid_ends(90)


        # colorbar
        cb = fig.colorbar(image, orientation='horizontal', shrink=.4, pad=0.05, ticks=[vmin, vmax])
        cb.ax.xaxis.set_label_text(unit)
        cb.ax.xaxis.labelpad = -8
        # workaround for issue with viewers, see colorbar docstring
        cb.solids.set_edgecolor("face")

        ax.tick_params(axis='x', labelsize=10)
        ax.tick_params(axis='y', labelsize=10)

        # remove longitude tick labels
        # ax.xaxis.set_ticklabels([])
        # remove horizontal grid
        # ax.xaxis.set_ticks([])

        plt.grid(True)
        plt.savefig("../figures/PlanckFig_map_" + colormaptag + "python_%dmm.pdf" % int(width*10), bbox_inches='tight', pad_inches=0)
