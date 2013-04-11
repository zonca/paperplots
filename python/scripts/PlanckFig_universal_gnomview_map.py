import healpy as hp
from planckcolors import planck_universal_cmap

m = hp.ma(hp.read_map("../../data/wmap_band_iqumap_r9_7yr_W_v4.fits", 0)) * 1e3 # muK
nside = hp.npix2nside(len(m))

hp.gnomview(m, cmap=planck_universal_cmap, min=-1e3, max=1e7, cbar=False, title="", notext=True)
