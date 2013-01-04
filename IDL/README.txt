There are two IDL .pro files required to generate the sample Planck power spectrum figures.

The HFI_plot.pro file contains several utility routines needed by the main example script.  
This file should thus be compiled first.

The main routine is HFI_plot, to be used instead of the IDL plot procedure for the main plot.  
This rotates the y-axis labels by 90 deg ccw, as requested by the Planck style guide.  

There are other routines to set the colours, round numbers to a desired number of decimal places and convert them to strings,
change the IDL plot symbol, and use latex to change the plot axis labels to use latex code rather than IDL formating (using the psfrag package).

The FDIR directory in the main script (line 56) needs to be changed to something suitable for your local directory structure.  Once this is done,
and the input data text files are located in this directory, then the script should run and produce a series of .eps files with different widths.
If running on a linux system, then the IDL routine spawns a epstopdf command to the terminal and converts the eps files to pdf format.  Otherwise
this may need to be done manually if pdf output is desired.