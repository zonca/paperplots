PRO Map_fig_example
  ;
  ;
  ;
  ;Written by L.D.Spencer, Jan. 2013
  ;
  ;   This program is free software: you can redistribute it and/or modify
  ;   it under the terms of the GNU General Public License as published by
  ;   the Free Software Foundation, either version 3 of the License, or
  ;   (at your option) any later version.
  ;
  ;   This program is distributed in the hope that it will be useful,
  ;   but WITHOUT ANY WARRANTY; without even the implied warranty of
  ;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  ;   GNU General Public License for more details.
  ;
  ;   A copy of the GNU General Public License is available at 
  ;   <http://www.gnu.org/licenses/>.
  ;   
  ;   Copyright Locke D. Spencer, 2013
  ;
  ;   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;
  ;  This script sets a new local colour table, and plots a healpix mollview map for the PIP template examples.
  ;
  ; First restore the WMAP 9 year map.  go to: http://lambda.gsfc.nasa.gov/product/map/dr5/maps_band_r9_iqu_9yr_get.cfm to get a map if you need one.
  ;
  FDIR = '/export/data/spxls/Docs/Planck/PIPfigTemplate/'
  ;FDIR = '/space/lspencer/PIPtmplt/'
  CTDIR = FDIR    ;  The CTDIR needs to be a directory that IDL can read/write to.  
  CTFILE = 'Planck_CT.tbl'
  ; It was introduced as non-admin users may not have write access to the default IDL colourtable. 
  ;
  Mname = 'wmap_band_iqumap_r9_9yr_W_v5' ; .fits
  ;Mname = 'wmap_band_forered_iqumap_r9_7yr_W_v4' ; .fits     ; the 7 year map
  ;Mname = 'wmap_band_forered_imap_r9_9yr_W_v5' ; .fits
  ;
  ; Now read in the map.
  ;
  READ_FITS_MAP, fDIR+Mname+'.fits', map, hdr, ehdr, NSIDE=NS, COORDSYS=COORD, ORDERING=ORDER, SILENT=SILENT
  ;
  map = REFORM(map[*,0])*1d3 ; the first [*,0] entry is the temperature/intensity portion. now in uK instead of mK.
  ;
  Npts = N_ELEMENTS(map)
  ;
  MINVAL = -1d3 ; The minimum value to plot is -1000 uK
  MAXVAL =  1d3 ; The maximum value to plot is  1000 uK
  ;
  ;  Set the new colortable
  HFI_CT, CTDIR=CTDIR, CTFILE=CTFILE, /LOAD ; this tells me the location of the revised colour table, file CTFILE in directory CTDIR, 
  ;       these are also inputs if you have a colourtable file already.  The LOAD keyword then loads the colour table after it has been created.
  ;
  ;
  ; Make an outline for the lattitude and longitude lines. This is needed to draw an ellipse around the map [if desired]. 
  ;
  Ngrat = 181d ; number of points for the additional graticule curves.
  out_ = {COORD:'G',RA:DBLARR(Ngrat),DEC:DBLARR(Ngrat), LINESTYLE:0, PSYM:0, SYMSIZE:0} ; the outline structure accepted by mollview.
  ; 
  Nout = 2  ; the outline is done as two half-curves.
  out = REPLICATE(out_,Nout)
  ;
  RA = DBLARR(Ngrat) ; held constant while DEC changes  ;  The RA and DEC are in healpix/mollview notation.
  DEC = DINDGEN(Ngrat) - 90d       ; bottom to top, -90 to +90 deg.
  ;
  out[0].RA = RA - 180d  ;  The half curve at -180 deg.
  out[0].DEC = DEC
  out[0].LINESTYLE=0
  ;
  out[1].RA = RA + 180d  ;  The half curve at +180 deg.
  out[1].DEC = DEC
  out[1].LINESTYLE=0
  ;
  ;
  GR_88 = [60,45]        ;  The graticule spacing for the 88 mm figure.
  GR_120 = [60,15]       ;  The graticule spacing for the 120 mm figure.
  GR_180 = [60,15]       ;  The graticule spacing for the 180 mm figure.
  ;
  W_88 = 8.8d ; cm
  W_120 = 12d ; cm
  W_180 = 18d ; cm
  ;
  NmPref = 'PlanckFig_map_columbi1_IDL_'
  NmSuf = 'mm'
  Nm_88  = NmPref + '88' + NmSuf
  Nm_120 = NmPref + '120'+ NmSuf
  Nm_180 = NmPref + '180'+ NmSuf
  ;
  FigRes = 300d   ; in dpi.  The paper figures should be at least 300 dpi, according to the A&A author guide.  For Maps, 600 dpi would be better.  
  ;
  PX_88  =  8.8d/2.54d*FigRes
  PX_120 = 12.0d/2.54d*FigRes
  PX_180 = 18.0d/2.54d*FigRes
  ;
  !P.FONT = 0
  ;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 88 mm figure ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;
  SZ    = 88d
  SZstr = '88'
  PX  = PX_88
  GR  = GR_88
  W   =  W_88
  Nm  = Nm_88
  LS_mollview, map, /NESTED, COLT=41, CTDIR=CTDIR, CTFILE=CTFILE, MIN=MINVAL, MAX=MAXVAL, CHARSIZE=1,GRATICULE=GR, GLSIZE=1,HXSIZE=W, FLIP=1, $
    GRMIN=[-179, -89], GRMAX=[179,89], GRLS =1, OUTLINE=out, TITLE=' ', PXSIZE=PX, PS=FDIR+Nm+'_.eps', CBLBL='u';, , UNIT='!Xu'  
  ;  
  ;  Note:
  ;  - The above routine is a modified version of the standard healpix mollview procedure.  It calls modified versions of the proj2out and oplot_graticule procedures
  ;  - The FLIP keyword was set to get the orientation of the IDL example to match that of the python example.  I have not tested if this is needed for the Planck maps, 
  ;  nor have I checked if this is the desired orientation...just a consistent one for now.
  ;  - Colour table 41 is the revised colour table after calling the HFI_CT routine with the /LOAD keyword set.
  ;  - CTDIR is the location of the modified colour table file, with CTFILE being the name of it.  running HFI_CT without specifying these causes it to output 
  ;  the values for the default settings.
  ;  - GRMIN and GRMAX are values outside of which the graticule labeles will not be printed. This was to get around the outline ring being placed above the 180 deg. label.
  ;  - GRLS is the graticule line style, any of the standard IDL linestyles are accepted.  IGRLS is the same for IGRATICULE (if set), IGRMIN and IGRMAX also exist.
  ;  - CBLBL is the added colourbar label/name.  This is done to get the label centred and below the colorbar, as in the python example.
  ;  - The colourbar is also made slightly larger than in the standard healpix version.
  ;  - `u' is a placeholder for the colorbar label which will be done in latex/psfrag to get the mu correct.  This is not needed for units without greek letters.
  ;  - latex/psfrag is also used to get a degree sign on the graticule labels.  This is not needed if graticule or igraticule is not set.
  ;
  outname1 = NmPref+SZstr+NmSuf; 'LS_HFI_PIP_fig_sample_'+SZstr+'mm_v3'
  ; height and width in cm
  IDLstrs = ['u','-120','-60','0','60','120','-45','45']
  TEXstrs = ['$\mu {\rm K}$','240$^\circ$','300$^\circ$','0$^\circ$','60$^\circ$','120$^\circ$','-45$^\circ$','45$^\circ$']
  FNTszs = DBLARR(N_ELEMENTS(IDLstrs)) + 1d
  LS_latexify, NmPref+SZstr+NmSuf+'_.eps', IDLstrs, TEXstrs, FNTszs*8d/11d, outname=outname1+'.eps', height=SZ/10d/1.6d, width=SZ/10d, FDIR=FDIR;, /full
  ;
  IF !D.Name EQ 'X' THEN SPAWN, FDIR+'epstopdf.pl --res='+STRTRIM(STRING(FIX(FigRes)),2)+' '+FDIR+outname1+'.eps &', msg  ; If in linux, convert the postscript to a pdf file straight away.
  ;
  ;
  ;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 120 mm figure ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;
  SZ   =   120d
  SZstr = '120'
  PX  = PX_120
  GR  = GR_120
  W   =  W_120
  Nm  = Nm_120
  LS_mollview, map, /NESTED, COLT=41, CTDIR=CTDIR, CTFILE=CTFILE, MIN=MINVAL, MAX=MAXVAL, CHARSIZE=1,GRATICULE=GR, GLSIZE=1,HXSIZE=W, FLIP=1, $
    GRMIN=[-179, -89], GRMAX=[179,89], GRLS =1, OUTLINE=out, TITLE=' ', PXSIZE=PX, PS=FDIR+Nm+'_.eps', CBLBL='u';, , UNIT='!Xu'  
  ;
  outname1 = NmPref+SZstr+NmSuf; 'LS_HFI_PIP_fig_sample_'+SZstr+'mm_v3'
  ; height and width in cm
  IDLstrs = ['u','-120','-60','0','60','120','-75','-45','-30','-15','15','30','45','75']
  TEXstrs = ['$\mu {\rm K}$','240$^\circ$','300$^\circ$','0$^\circ$','60$^\circ$','120$^\circ$','-75$^\circ$','-45$^\circ$','-30$^\circ$','-15$^\circ$','15$^\circ$','30$^\circ$','45$^\circ$','75$^\circ$']
  FNTszs = DBLARR(N_ELEMENTS(IDLstrs)) + 1d
  LS_latexify, NmPref+SZstr+NmSuf+'_.eps', IDLstrs, TEXstrs, FNTszs*8d/11d, outname=outname1+'.eps', height=SZ/10d/1.6d, width=SZ/10d, FDIR=FDIR;, /full
  ;
  IF !D.Name EQ 'X' THEN SPAWN, FDIR+'epstopdf.pl --res='+STRTRIM(STRING(FIX(FigRes)),2)+' '+FDIR+outname1+'.eps &', msg  ; If in linux, convert the postscript to a pdf file straight away.
  ;
  ;
  ;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 180 mm figure ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;
  SZ   =   180d
  SZstr = '180'
  PX  = PX_180
  GR  = GR_180
  W   =  W_180
  Nm  = Nm_180
  LS_mollview, map, /NESTED, COLT=41, CTDIR=CTDIR, CTFILE=CTFILE, MIN=MINVAL, MAX=MAXVAL, CHARSIZE=1,GRATICULE=GR, GLSIZE=1,HXSIZE=W, FLIP=1, $
    GRMIN=[-179, -89], GRMAX=[179,89], GRLS =1, OUTLINE=out, TITLE=' ', PXSIZE=PX, PS=FDIR+Nm+'_.eps', CBLBL='u';, , UNIT='!Xu'  
  ;
  outname1 = NmPref+SZstr+NmSuf; 'LS_HFI_PIP_fig_sample_'+SZstr+'mm_v3'
  ; height and width in cm
  IDLstrs = ['u','-120','-60','0','60','120','-75','-45','-30','-15','15','30','45','75']
  TEXstrs = ['$\mu {\rm K}$','240$^\circ$','300$^\circ$','0$^\circ$','60$^\circ$','120$^\circ$','-75$^\circ$','-45$^\circ$','-30$^\circ$','-15$^\circ$','15$^\circ$','30$^\circ$','45$^\circ$','75$^\circ$']
  FNTszs = DBLARR(N_ELEMENTS(IDLstrs)) + 1d
  LS_latexify, NmPref+SZstr+NmSuf+'_.eps', IDLstrs, TEXstrs, FNTszs*8d/11d, outname=outname1+'.eps', height=SZ/10d/1.6d, width=SZ/10d, FDIR=FDIR;, /full
  ;
  IF !D.Name EQ 'X' THEN SPAWN, FDIR+'epstopdf.pl --res='+STRTRIM(STRING(FIX(FigRes)),2)+' '+FDIR+outname1+'.eps &', msg  ; If in linux, convert the postscript to a pdf file straight away.
  ;
  ;
  ;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  Done all of the figure sizes now.  
  ;
  ;
  ;
  ;
  stop
 END