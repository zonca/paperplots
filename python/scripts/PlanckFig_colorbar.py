from setup_matplotlib import *
import healpy as hp
import matplotlib as mpl


vmin = -1e3; vmax = 1e7
comp = 1 # 0->T 1->Q 2->U
unit = r"$\mathrm{K}$"

import planckcolors

# Official colorscale for CMB
cmap = planckcolors.colombi1_cmap
# Universal non-linear colorscale
# Other colorscales, available in ../../data
# Other colorscale, made nonlinear with Glog, needs to setup vmin and vmax
#cmap = planckcolors.planck_universal_cmap
#cmap = planckcolors.GlogColormap(planckcolors.load_colormap("color_table_0_BlackWhiteLinear.txt"), vmin=vmin, vmax=vmax)
cmap = planckcolors.GlogColormap(planckcolors.load_colormap("color_table_11_BlueRed.txt"), vmin=vmin, vmax=vmax)
colormaptag = "bluered_"

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

for width in [18.]:

    fig = plt.figure(figsize=(cm2inch(width), cm2inch(3)))
    # matplotlib is doing the mollveide projection
    ax = fig.add_axes([0.1, 0.5, 0.8, 0.4])

    colorbar_ticks = np.array([-1e3, -1e2, -10, -1, 0, 1, 10, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7])
    colorbar_boundaries = np.concatenate([-1 * np.logspace(0, 3, 80)[::-1], np.linspace(-1, 1, 10), np.logspace(0, 7, 150)])
    cb = mpl.colorbar.ColorbarBase(ax, cmap=cmap,
                                        #norm=norm, 
                                        boundaries=colorbar_boundaries, 
                                        ticks=colorbar_ticks,
                                        #shrink=1.,
                                        #pad=0.05,
                                        format=formatter,
                                        orientation='horizontal')

    cb.ax.xaxis.set_label_text(unit)
    # workaround for issue with viewers, see colorbar docstring
    cb.solids.set_edgecolor("face")
    # align the colorbar tick labels
    for label in ax.xaxis.get_ticklabels():
        label.set_verticalalignment("baseline")
        label.set_position((label.get_position()[0], -.3))

    # remove white space around figure
    #spacing = 0.01
    #plt.subplots_adjust(bottom=spacing, top=1-spacing, left=spacing, right=1-spacing)

    output_filename = "../figures/PlanckFig_colormap_" + colormaptag + "python_%dmm.pdf" % int(width*10)
    print(output_filename)
    plt.savefig(output_filename)
