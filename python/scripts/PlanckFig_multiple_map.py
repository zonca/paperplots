from setup_matplotlib import *
import healpy as hp
from planckcolors import colombi1_cmap

m = hp.ma(hp.read_map("../../data/wmap_band_iqumap_r9_7yr_W_v4.fits", 0)) * 1e3 # muK
nside = hp.npix2nside(len(m))

# using directly matplotlib instead of mollview has higher
# quality output, I plan to merge this into healpy

# ratio is always 1/2
xsize = 1000
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
        return GeoAxes.ThetaFormatter.__call__(self, x, pos)

width = 18
cmap = colombi1_cmap
colormaptag = "colombi1_"

fig = plt.figure(figsize=(cm2inch(width), cm2inch(width)*.8))

figure_rows, figure_columns = 2, 2
for submap in range(4):
    # matplotlib is doing the mollveide projection
    #ax = fig.add_subplot(figure_rows, figure_columns, submap,projection='mollweide')
    ax = plt.subplot(figure_rows, figure_columns, submap,projection='mollweide')

    # rasterized makes the map bitmap while the labels remain vectorial
    # flip longitude to the astro convention
    image = plt.pcolormesh(longitude[::-1], latitude, grid_map, vmin=vmin, vmax=vmax, rasterized=True, cmap=cmap)

    plt.title("Title of submap %d" % submap, fontsize='small')
    # remove tick labels
    ax.xaxis.set_ticklabels([])
    ax.yaxis.set_ticklabels([])
    # remove grid
    ax.xaxis.set_ticks([])
    ax.yaxis.set_ticks([])


# colorbar
cax = fig.add_axes([0.35, 0.08, 0.3, 0.04])
cb = fig.colorbar(image, cax=cax, orientation='horizontal', ticks=[vmin, vmax])
cb.ax.xaxis.set_label_text(unit)
cb.ax.xaxis.labelpad = -8
# workaround for issue with viewers, see colorbar docstring
cb.solids.set_edgecolor("face")

plt.subplots_adjust(left=0, right=1, top=.9, wspace=.1, hspace=.01, bottom=.14)

plt.savefig("../figures/PlanckFig_multiple_map_" + colormaptag + "python_%dmm.pdf" % int(width*10), bbox_inches='tight', pad_inches=0.02)
