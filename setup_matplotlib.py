import numpy as np
from matplotlib import rcParams, rc
params = {'backend': 'agg',
          'axes.labelsize': 12,
          'text.fontsize': 12,
          'legend.fontsize': 12,
          'xtick.labelsize': 12,
          'ytick.major.pad': 6,
          'xtick.major.pad': 6,
          'ytick.labelsize': 12,
          'text.usetex': True,
          'font.family':'sans-serif',
          'font.sans-serif':'FreeSans'}
rc('text.latex', preamble='\usepackage{sfmath}')
# for Helvetica you might need to install texlive-fonts-extra
rcParams.update(params)
import matplotlib.pyplot as plt

