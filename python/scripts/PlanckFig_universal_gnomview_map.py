import healpy as hp
import numpy as np
from planckcolors import planck_universal_cmap

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

m = hp.ma(hp.read_map("../../data/wmap_band_iqumap_r9_7yr_W_v4.fits", 0)) * 1e3 # muK
nside = hp.npix2nside(len(m))

hp.gnomview(m, cmap=planck_universal_cmap, min=-1e3, max=1e7, cbar=False)
