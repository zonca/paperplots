import numpy as np

############### CMB colormap
from matplotlib.colors import ListedColormap
import os
import glob
this_dir, this_filename = os.path.split(__file__)
DATA_PATH = os.path.join(this_dir, "..", "..", "data")

def list_available_colormaps():
    return map(os.path.basename, glob.glob(os.path.join(DATA_PATH, "*.txt")))

def load_colormap(filename):
    """Load a colormap defined in a text file

    filename is the .txt file name located in the
    data/ path, not the full path.
    list_available_colormaps() lists the available color tables
    """
    try:
        colormap = ListedColormap(np.loadtxt(os.path.join(DATA_PATH, filename))/255.)
    except IOError:
        print("Cannot load colormap, available colormaps: \n* " + "\n* ".join(list_available_colormaps()))
        raise
    colormap.set_bad("gray") # color of missing pixels
    colormap.set_under("white") # color of background, necessary if you want to use
    # this colormap directly with hp.mollview(m, cmap=colormap)
    return colormap

colombi1_cmap = load_colormap("Planck_Parchment_RGB.txt")

############### Universal colormap
# setup linear colormap
from matplotlib.colors import ListedColormap
planck_freqmap_cmap = load_colormap("Planck_FreqMap_RGB.txt")

# setup nonlinear colormap
from matplotlib.colors import LinearSegmentedColormap
class GlogColormap(LinearSegmentedColormap):
    name = "glog_colormap"
    def __init__(self, cmap, vmin=-1e3, vmax=1e7):
        self.cmap = cmap
        self.N = self.cmap.N
        self.vmin = vmin
        self.vmax = vmax

    def is_gray(self):
        return False

    def __call__(self, xi, alpha=1.0, **kw):
        x = xi * (self.vmax - self.vmin) + self.vmin
        yi = self.modsinh(x)
        # range 0-1
        yi = (yi + 3)/10.
        return self.cmap(yi, alpha)

    def modsinh(self, x):
        return np.log10(0.5*(x + np.sqrt(x**2 + 4)))

planck_universal_cmap = GlogColormap(planck_freqmap_cmap)
