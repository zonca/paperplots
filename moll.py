from setup_matplotlib import *
import healpy as hp
import newplot

hp.newplot = newplot

m = hp.read_map("data/wmap_band_iqumap_r9_7yr_W_v4.fits", 0)

# using directly matplotlib instead of mollview has higher
# quality output, I plan to merge this into healpy

#margins = [0.01, 0.99, 0.99, 0.01]
#hp.mollview(m, min=-1, max=1, unit="mK", title="", xsize=4000, margins=margins)
#hp.graticule()
#plt.savefig("moll.pdf", dpi=200, bbox_inches="tight")

nside = hp.npix2nside(len(m))
xsize = 2000
ysize = xsize/2.
unit = r"$\mu K$"
vmin = -1e3; vmax = 1e3
theta = np.linspace(np.pi, 0, ysize)
phi   = np.linspace(-np.pi, np.pi, xsize)
longitude = np.radians(np.linspace(-180, 180, xsize))
latitude = np.radians(np.linspace(-90, 90, ysize))
PHI, THETA = np.meshgrid(phi, theta)
# map on a matrix
grid_map = m[hp.ang2pix(nside, THETA, PHI)] * 1e3

for width in [18., 12., 8.8]:
    fig = plt.figure(figsize=(cm2inch(width), cm2inch(width/2.)))
    ax = fig.add_subplot(111,projection='mollweide')

    plt.subplots_adjust(left=0.01, right=0.99, top=0.95, bottom=0.01)

    # rasterized makes the map bitmap while the labels remain vectorial
    image = plt.pcolormesh(longitude, latitude, grid_map, vmin=vmin, vmax=vmax, rasterized=True)

    # graticule
    ax.set_longitude_grid(60)
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
    plt.savefig("latex/mollview_%dmm.pdf" % int(width*10), bbox_inches='tight')
