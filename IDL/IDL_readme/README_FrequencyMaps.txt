Here are a few comments on using the frequency maps example.

1. In order to get the maps to conform to the Planck style guide, I have had to modify some of the Healpix routines.  These modified routines are within the HFI_plot.pro file.
2. HFI_plot will need compiled 2-3 times on first use, after an healpix init scripts are run.  This is beause there are many subroutines also included.
3. This was developed with Healpix 3.00.  It may need some finessing to work with earlier versions of healpix (I have already had to debug a healpix 2.20 problem).  Check what version of healpix you are using if you encounter healpix-related problems, or if HFI_plot does not compile.
4. Within the LS_MOLLVIEW call, there are some extra keywords.  They are as follows:
     MODASINH  Set this to use the modified colour scaling specific for the frequency maps.  It is designed for a hard coded plot range of -10^3 to +10^7, change your units to maintain this if needed.
     CBLBL     The colorbar label (to go below the colur bar -- the UNITS keyword still places text to the right of the colorbar if desired.)
     CBLIN     Set this to 1 to linearize the scaling of the colourbar (i.e. make a linear scale with uneven tick marks rather than even marks on a nonlinear scale).
     CBTICKS   Set this to one to include tick marks on the colourbar.
     CBTICKLAB Set this to include tick value labels above the colourbar (otherwise the edges only are marked on either side).
     CBTICKVAL Set this to an array of colour values to draw a tick line on, i.e. set CBTICKVAL=[-100,0,100] to get marks at colors of -100,0,100.
     CBTICKLBL Set this as an array of strings with the text to use at the preefined CBTICKVAL locations.  This is optional, the CBTICKVAL values are used if this is not set. 
     CBOUT     Set this to one to draw a black outline around the colourbar.
  See the Freq_Map_fig_example.pro file for examples of these KEYWORDS in use.
5. A latex template has been provided to generate multiple map images. Chane the image names within LS_FreqMapFig_Template.tex to your local maps and try it out.  You may need to tweak the trim/clip settigns to remove the colourbars and/or isolate them.
6. The plots use a scalar offset to get the CMB portion of the maps the same across all frequencies.  These offsets are as follows (all offsets in uK_CMB):
  map30 = map30 - 64.7d
  map44 = map44 - 24.1d
  map70 = map70 - 28.5d
  map100 = map100 - 30.1d
  map143 = map143 - 55.7d
  map217 = map217 - 133d  
  map353 = map353 - 681d + 250d = map353 - 431d
7. The 545 and 857 maps are plotted in kJy/sr, not uK_CMB.  Plots of the 545/857 GHz maps in uK_RJ do work, but do not utilize the full dynamic range of the colourtable, so use the kJy/sr scaling instead.
8. WARNING:  along with pt. 6 above, the plot routine is very sensitive to offsets in the map monopole value.  small offsets can cause the maps to appear more blue or more red than desired.  Please check this as you generate your plots.  Also, please state what offsets are removed from any figures within the caption or other text.  
9. The frequency map example also requires the file rgb_37.idl, which is restored within the HFI_CT routine via the /HIGHDR toggle keyword and HDFILE='/path/rgb_37.idl' setting.