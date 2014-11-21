from setup_matplotlib import *
import healpy as hp


filename = "../../data/wmap_band_iqumap_r9_7yr_W_v4.fits"
vmin = -1e2; vmax = 1e2
comp = 1 # 0->T 1->Q 2->U
unit = r"$\mathrm{K}$"
m = hp.ma(hp.read_map(filename, [comp])) * 1e3

nside = hp.npix2nside(len(m))

import planckcolors

use_mask = False

def remove_monopole_outside_mask(m, mask):
    m_mask = hp.ma(m)
    m_mask.mask = mask
    return m - hp.fit_monopole(m_mask, gal_cut=75)

monopole_fit_mask_filename = "../../data/wmap_ext_temperature_analysis_mask_r9_7yr_v4.fits"
# in healpy masks are 1 for bad pixels, 0 for good pixels
monopole_fit_mask = np.logical_not(np.floor(hp.ud_grade(hp.read_map(monopole_fit_mask_filename), nside)))
m = remove_monopole_outside_mask(m, monopole_fit_mask)

# using directly matplotlib instead of mollview has higher
# quality output, I plan to merge this into healpy

# ratio is always 1/2
xsize = 2000
ysize = xsize/2.


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
        return GeoAxes.ThetaFormatter.__call__(self, x, pos)

# Official colorscale for CMB
cmap = planckcolors.colombi1_cmap
# Universal non-linear colorscale
#cmap = planckcolors.planck_universal_cmap
# Other colorscales, available in ../../data
#cmap = planckcolors.load_colormap("color_table_11_BlueRed.txt")
# Other colorscale, made nonlinear with Glog, needs to setup vmin and vmax
#cmap = planckcolors.GlogColormap(planckcolors.load_colormap("color_table_0_BlackWhiteLinear.txt"), vmin=vmin, vmax=vmax)

colormaptag = "colombi1_"

for width in [18., 12., 8.8]:

    fig = plt.figure(figsize=(cm2inch(width), cm2inch(width)/(3./2.)))
    # matplotlib is doing the mollveide projection
    ax = fig.add_subplot(111,projection='mollweide')


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
    cb = fig.colorbar(image, orientation='horizontal', shrink=.6, pad=0.05, ticks=[vmin, vmax])
    cb.ax.xaxis.set_label_text(unit)
    cb.ax.xaxis.labelpad = -8
    # workaround for issue with viewers, see colorbar docstring
    cb.solids.set_edgecolor("face")

    ax.tick_params(axis='x', labelsize=10)
    ax.tick_params(axis='y', labelsize=10)

    # remove tick labels
    ax.xaxis.set_ticklabels([])
    ax.yaxis.set_ticklabels([])
    # remove grid
    ax.xaxis.set_ticks([])
    ax.yaxis.set_ticks([])

    # remove white space around figure
    spacing = 0.01
    plt.subplots_adjust(bottom=spacing, top=1-spacing, left=spacing, right=1-spacing)

    plt.grid(True)

    plt.savefig("../figures/PlanckFig_map_" + str(comp) + "_" + colormaptag + "python_%dmm.pdf" % int(width*10))#bbox_inches='tight', pad_inches=0.02)
