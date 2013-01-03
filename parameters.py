import os
from glob import glob
from setup_matplotlib import *

def confint2d(hist, which=[.95, .68, 0]):
    """
    Return confidence levels in a histogram.
    
    Parameters
    ----------
    
    hist:
        A 2D histogram
    which: 
        A list of confidence levels, e.g. [.68, .95]
    """
    H = np.sort(hist.ravel())[::-1]
    cdf = np.array([sum(H[H>x])/H.sum() for x in H])
    return np.interp(which, cdf, H)

# Load data
parameters = "ns", "r"

#datasets = "wmap7_r", "spt_wmap7_r", "spt_wmap7_h0_bao_r"
#colors = plt.cm.Greys, plt.cm.Reds, plt.cm.Blues
datasets = "spt_wmap7_r", "spt_wmap7_h0_bao_r"
labels = "SPT + WMAP7", "SPT + WMAP7 + $H_0$ + BAO"

colors = plt.cm.Reds, plt.cm.Blues
alphas = 1, .9 

data = {}
for dataset in datasets:
    data_folder = os.path.join("data", "parameters", dataset)
    data[dataset] = {}
    for p in parameters:
        filename = glob(os.path.join(data_folder, p + "*"))[0]
        data[dataset][p] = np.loadtxt(filename)
        if data[dataset][p].ndim > 1:
            # remove index column
            data[dataset][p] = data[dataset][p][:,1]

for width in [18., 12., 8.8]:
    fig = plt.figure(figsize=(cm2inch(width), cm2inch(width*6/8.)))
    # this should be changed for making a panel of multiple figures
    ax = fig.add_subplot(111)
    nbins = 20
    proxy = []

    for colormap, dataset, alpha, label in zip(colors, datasets, alphas, labels):
        # create 2d histogram
        H,xe,ye = np.histogram2d(data[dataset][parameters[0]],data[dataset][parameters[1]],nbins)

        xem, yem = plt.mlab.movavg(xe,2), plt.mlab.movavg(ye,2)

        # .68 and .95 levels
        levels = confint2d(H)

        # filling contour
        CS = ax.contourf(xem, yem, H.T, levels=levels, norm=plt.Normalize(), cmap=colormap, alpha=alpha, rasterized=True)
        # sets border to the same color as the inner filling color
        CS2 = plt.contour(CS, levels=levels[:1], hold='on', colors=CS.tcolors[1], alpha=alpha/2.)

        # create fake rectangles for the legend
        proxy.append(plt.Rectangle((0,0),1,1,fc = CS.tcolors[1][0]))

    legend = plt.legend(proxy, labels, frameon=False)
    # set edge color to same as face color
    for rect in legend.get_patches():
        rect.set_edgecolor(rect.get_facecolor())

    plt.xlabel(r"Primordial Tilt ($n_s$)")
    plt.ylabel(r"Tensor-to-Scalar Ratio ($r$)")
    plt.ylim([ax.get_ylim()[0], 0.35])

    # reduce white space around figure
    plt.subplots_adjust(left=0.01, right=0.99, top=0.99, bottom=0.01)

    # set vertical y axis ticklables
    for ticklabel in ax.yaxis.get_ticklabels():
        ticklabel.set_rotation("vertical")

    plt.savefig("latex/parameters_%dmm.pdf" % int(width*10), bbox_inches='tight')
