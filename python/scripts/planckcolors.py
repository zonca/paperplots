import numpy as np

############### CMB colormap
from matplotlib.colors import ListedColormap
colombi1_cmap = ListedColormap(np.loadtxt("../../data/Planck_Parchment_RGB.txt")/255.)
colombi1_cmap.set_bad("gray") # color of missing pixels
colombi1_cmap.set_under("white") # color of background, necessary if you want to use
# this colormap directly with hp.mollview(m, cmap=colombi1_cmap)

############### Universal colormap
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

    def is_gray(self):
        return False

    def __call__(self, xi, alpha=1.0, **kw):
        x = xi * (1e7+1e3) - 1e3
        yi = self.modsinh(x)
        # range 0-1
        yi = (yi + 3)/10.
        return self.cmap(yi, alpha)

    def modsinh(self, x):
        return np.log10(0.5*(x + np.sqrt(x**2 + 4)))

planck_universal_cmap = PlanckUniversalColormap(planck_freqmap_cmap)
