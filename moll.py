from setup_matplotlib import *
import healpy as hp
import newplot

hp.newplot = newplot

m = hp.read_map("data/wmap_band_iqumap_r9_7yr_W_v4.fits", 0)

#margins = [0.01, 0.99, 0.99, 0.01]
#hp.mollview(m, min=-1, max=1, unit="mK", title="", xsize=4000, margins=margins)
#hp.graticule()
#plt.savefig("moll.pdf", dpi=200, bbox_inches="tight")

nside = hp.npix2nside(len(m))
xsize = 2000
ysize = xsize/2.
unit = "mK"
vmin = -1; vmax = 1
theta = np.linspace(np.pi, 0, ysize)
phi   = np.linspace(-np.pi, np.pi, xsize)
longitude = np.radians(np.linspace(-180, 180, xsize))
latitude = np.radians(np.linspace(-90, 90, ysize))
PHI, THETA = np.meshgrid(phi, theta)
grid_map = m[hp.ang2pix(nside, THETA, PHI)]

for width in [18., 12., 8.8]:
    fig = plt.figure(figsize=(cm2inch(width), cm2inch(width/2.)))
    ax = fig.add_subplot(111,projection='mollweide')
    plt.subplots_adjust(left=0.01, right=0.99, top=0.95, bottom=0.01)
    # rasterized makes the map bitmap while the labels remain vectorial
    image = plt.pcolormesh(longitude, latitude, grid_map, vmin=vmin, vmax=vmax, rasterized=True)
    #image = plt.imshow(grid_map, extent=np.radians([-180, 180, -90, 90]), vmin=vmin, vmax=vmax, interpolation="nearest", aspect=.5)
    cb = fig.colorbar(image, orientation='horizontal', shrink=.4, pad=0.05, ticks=[vmin, vmax])
    ax.set_longitude_grid(60)
    if width < 10:
        ax.set_latitude_grid(45)
    cb.ax.text(0.5,-1.0,unit, transform=cb.ax.transAxes,ha='center',va='center')
    # workaround for issue with viewers, see colorbar docstring
    cb.solids.set_edgecolor("face")
    # remove longitude tick labels
    # ax.xaxis.set_ticklabels([])
    # remove horizontal grid
    # ax.xaxis.set_ticks([])
    plt.grid(True)
    plt.savefig("testb/mollview_%dmm.pdf" % int(width*10), bbox_inches='tight')
