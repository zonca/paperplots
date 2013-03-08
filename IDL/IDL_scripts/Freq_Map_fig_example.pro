PRO Freq_Map_fig_example
  ;
  ;
  ;
  ;Written by L.D.Spencer, Mar.  2013
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
  FDIR = '/export/data/spxls/Docs/Planck/PIPfigTemplate/'   ;;;;;;!!!!!!!!  You need to change this to yoru local directory.
  ;FDIR = '/space/lspencer/PIPtmplt/'
  CTDIR = FDIR    ;  The CTDIR needs to be a directory that IDL can read/write to.  
  CTFILE = 'Planck_CT.tbl'  ; The HFI_CT script will create this file for you as needed in the specified CTDIR directory
  ;
  ; It was introduced as non-admin users may not have write access to the default IDL colourtable. 
  ;
  HDRDIR = CTDIR            ; This is where the RGB vectors for the high dynamic range (frequency map colour table) are located, default is the same place as the CMB colour table.
  HDRFILE = 'RGB_Planck_hdr.idl'  ; This is the file containing the high dynamic range RGB vectors.  Put it in the specified HDRDIR folder (change the HDRDIR location as you see fit)
  ;
  Mname = 'wmap_band_iqumap_r9_9yr_W_v5' ; .fits
  ;Mname = 'wmap_band_forered_iqumap_r9_7yr_W_v4' ; .fits     ; the 7 year map
  ;Mname = 'wmap_band_forered_imap_r9_9yr_W_v5' ; .fits
  ;
  ;
  ; ;;;;;;;;;;;;;;;;;;;;;    The lines below set up the names for the HFI maps.  You will likely need to change this to direct to your local copy of these maps.
  ;
  HFImPREF = 'HFI_'
  HFImSuf = '_2048_20121208_nominal'
  HFIfs = ['100','143','217','353','545','857']
  HFIms = HFImPref+HFIfs+HFImSuf
  ;
  ; ;;;;;;;;;;;;;;;;;;;;;   The line below identifies the LFI maps.  You will need to change this to your local version of the maps. 
  ;
  LFIms = ['LFI_30_1024_20120914_nominal','LFI_44_1024_20120914_nominal','LFI_70_1024_20120912_nominal']
  ;
  ; Now read in the maps.
  ;
  ;stop
  ;
  ; ;;;;;;;;;;  You may wish to ingest your maps differently!! THe few lines below are just a suggestion.
  ;
  LFImaps = 0  ; set this to 1 to read the map from fitws file and save it as an idl .sav file (the maps have I, Q, U, ..., whereas I plot just I, so I don't want to restore everything every time).
  HFImaps = 0  ;  set this to one on first run to get just the I from the maps
  ;
  IF KEYWORD_SET(LFImaps) THEN BEGIN
    ;
    READ_FITS_MAP, fDIR+LFIms[0]+'.fits', map30, hdr, ehdr30, NSIDE=NS30, COORDSYS=COORD, ORDERING=ORDER, SILENT=SILENT
    READ_FITS_MAP, fDIR+LFIms[1]+'.fits', map44, hdr, ehdr44, NSIDE=NS44, COORDSYS=COORD, ORDERING=ORDER, SILENT=SILENT
    READ_FITS_MAP, fDIR+LFIms[2]+'.fits', map70, hdr, ehdr70, NSIDE=NS70, COORDSYS=COORD, ORDERING=ORDER, SILENT=SILENT
    ;
    map30 = REFORM(map30[*,0])*1d6  ; in uK now.
    map44 = REFORM(map44[*,0])*1d6  ; in uK now.
    map70 = REFORM(map70[*,0])*1d6  ; in uK now.
    ;
    SAVE, FILENAME=FDIR+'LFI_maps.sav', map30, map44, map70, ehdr30, ehdr44, ehdr70
    ;
    print, 'LFI maps saved to an idl file. Hopefully this will restore quicker now.'
    stop
    ;
  ENDIF ELSE BEGIN
    RESTORE, FILENAME = FDIR+'LFI_maps.sav'
  ENDELSE
  ;
  ;stop
  IF KEYWORD_SET(HFImaps) THEN BEGIN
    READ_FITS_MAP, fDIR+HFIms[0]+'.fits', map100, hdr, ehdr100, NSIDE=NS, COORDSYS=COORD, ORDERING=ORDER, SILENT=SILENT
    READ_FITS_MAP, fDIR+HFIms[1]+'.fits', map143, hdr, ehdr143, NSIDE=NS, COORDSYS=COORD, ORDERING=ORDER, SILENT=SILENT
    READ_FITS_MAP, fDIR+HFIms[2]+'.fits', map217, hdr, ehdr217, NSIDE=NS, COORDSYS=COORD, ORDERING=ORDER, SILENT=SILENT
    READ_FITS_MAP, fDIR+HFIms[3]+'.fits', map353, hdr, ehdr353, NSIDE=NS, COORDSYS=COORD, ORDERING=ORDER, SILENT=SILENT
    READ_FITS_MAP, fDIR+HFIms[4]+'.fits', map545, hdr, ehdr545, NSIDE=NS, COORDSYS=COORD, ORDERING=ORDER, SILENT=SILENT
    READ_FITS_MAP, fDIR+HFIms[5]+'.fits', map857, hdr, ehdr857, NSIDE=NS, COORDSYS=COORD, ORDERING=ORDER, SILENT=SILENT
    ;
    map100 = REFORM(map100[*,0])*1d6 ; in uK now.
    map143 = REFORM(map143[*,0])*1d6 ; in uK now.
    map217 = REFORM(map217[*,0])*1d6 ; in uK now.
    map353 = REFORM(map353[*,0])*1d6 ; in uK now.
    map545 = REFORM(map545[*,0])*1d6 ; in Jy/sr.
    map857 = REFORM(map857[*,0])*1d6 ; in Jy/sr now.
    ;
    print, 'HFI maps saved to an idl file. Hopefully this will restore quicker now.'
    SAVE, FILENAME=FDIR+'HFI_maps.sav', map100, map143, map217, map353, map545, map857, ehdr100, ehdr143, ehdr217, ehdr353, ehdr545,ehdr857
    ;
    stop
    ;
  ENDIF ELSE BEGIN
    ;
    RESTORE, FILENAME = FDIR+'HFI_maps.sav'    
    ;
  ENDELSE
  ;  
  ;  
  ;  Look at using the official HFI offsets (monopole + CIB) [LFI offsets here are the KMG ones included below]
  ;  
  ;  A more complete discussion of map offsets, and official numbers for other data processing are avaialble at http://wiki.planck.fr/index.php/Proc/HFIMonopoleDipole
  ;  The MONOPOLE offsets are subtracted, and the CIB offsets are added.
  ;
  ;  After processing, it looks like the offsets for 143, 217, and 353 are a bit off, so I will comment these out and revert back to the KMG ones.
  ;
  ;map30 = map30 - 64.7d
  ;map44 = map44 - 24.1d
  ;map70 = map70 - 28.5d
  ;map100 = map100 - 19.7d + 12.3d
  ;map143 = map143 - 36.9 + 21.3d
  ;map217 = map217 - 78.6d + 67.6d  
  ;map353 = map353 - 308.8d + 452.6d
  ;map545 = map545 - 106.0d3 + 350d3
  ;map857 = map857 - 145.3d3 + 640d3
  ;  
  ;  Apply the KMG magic offsets to the maps.
  ;  
  ;  Kris used some offsets to make the plots suitable by visual inspection of the CMB portion.  His offsets are included below to the maps in uK_CMB
  ;  
  map30 = map30 - 64.7d
  map44 = map44 - 24.1d
  map70 = map70 - 28.5d
  map100 = map100 - 30.1d
  map143 = map143 - 55.7d
  map217 = map217 - 133d  ;+ 100d
  map353 = map353 - 681d + 250d
  ;  
  ;  The above offsets are based purely on making the maps look the same wrt the CMB.  Whatever offsets used in plotting should be clearly stated within a caption or the text.
  ;  A more complete discussion of map offsets, and official numbers for other data processing are avaialble at http://wiki.planck.fr/index.php/Proc/HFIMonopoleDipole
  ;  
  ;  I toyed with plotting the 545 and 857 GHz data in K_RJ instead of K_CMB (actually uK_RJ or nK_RJ)  The conversion factors are below
  ;
  sol = 299792458.d0  ; speed of light in m/s
  kb = 1.3806488e-23  ; Boltzmann constant in J/K
  ;
  UcTB545 = sol^2d/2d/Kb/545d9^2d*1d-26*1d6  ;  [uK_RJ/(Jy/sr)]
  UcTB857 = sol^2d/2d/Kb/857d9^2d*1d-26*1d6  ;  [uK_RJ/(Jy/sr)]
  ;  
  ;  
  ;stop
  ;
  LFImaps = 1  ; now the maps are restored, I do want to plot them so set this to 1 to do so.
  HFImaps = 1  ;  set these to zero if you do not have all of the planck frequency maps 
  ;            ;  If the above two lines are zero then the WMAP example will be plot using the frequency map[ colour table and scaling.
  ;
  ; ;;;;;;;;;;; Read in the WMAP map now.  
  ;
  READ_FITS_MAP, fDIR+Mname+'.fits', map, hdr, ehdr, NSIDE=NS, COORDSYS=COORD, ORDERING=ORDER, SILENT=SILENT
  ;
  map = REFORM(map[*,0])*1d3 ; the first [*,0] entry is the temperature/intensity portion. now in uK instead of mK.
  ;
  Npts = N_ELEMENTS(map)
  ;
  MINVAL = -1d3 ; The minimum value to plot is -1000 uK  The CMB maps have a -1000,1000 uK_CMB scale
  MAXVAL =  1d3 ; The maximum value to plot is  1000 uK
  MINVAL_ = -1d3 ; The minimum value to plot is -1000 uK  The frequency maps have a -10^3 to +10^7 colour range.  
  MAXVAL_ =  1d7 ; The maximum value to plot is  1000 uK  This range is hard coded (actually the 10^7/10^3 ratio is hard coded, but the decision by the EB is to hard code this plot range).
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  If you do not like this range, then change to some crazy unit which allows this range
  ;                                                       The neg/pos ratio sets the zero level of the figure to be the right colour.
  ;                                                       You could do a plot from -10^6 to +10^14, or -(10^3)/2 to (10^7)/2, etc., ...
  ;                                                       
  ; stop  ;
  ; 
  ;  Set the new colortable, use the /HIGHDR keyword and HDRFILE (high-dynamic-range-file) keyword to direct to your local copy of rgb_37.idl
  ;
  HFI_CT, CTDIR=CTDIR, CTFILE=CTFILE, /LOAD, /HIGHDR, HDRFILE=HDRFILE ; this tells me the location of the revised colour table, file CTFILE in directory CTDIR, 
  ;       these are also inputs if you have a colourtable file already.  The LOAD keyword then loads the colour table after it has been created.
  ;       The HDRDIR is where the high-dynamic range RGB vectors are stored.  The default location is the CTDIR location.  This file must be manually placed in the correct directory.
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
  NmPref = 'PlanckFig_map_columbi1_IDL_HighDR_'
  NmPrefHFI = 'PlanckFig_map_HighDR_IDL_'
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
  ;     Do a frequency map colour scale 88mm wide figure, then do the CMB colour scale figures at 88, 120, and 180 mm widths, then do some 180 mm wide frequency map figures.
  ;
  SZ    = 88d
  SZstr = '88'
  PX  = PX_88
  GR  = GR_88
  W   =  W_88
  Nm  = Nm_88
  LS_mollview, map, /NESTED, COLT=41, CTDIR=CTDIR, CTFILE=CTFILE, MIN=MINVAL_, MAX=MAXVAL_, CHARSIZE=8d/11d, GRATICULE=GR, GLSIZE=1,HXSIZE=W, FLIP=0, $
    GRMIN=[-179, -89], GRMAX=[179,89], GRLS =1, OUTLINE=out, TITLE=' ', PXSIZE=PX, PS=FDIR+Nm+'_.eps', CBLBL='u', /CBLIN, /MODASINH, /CBTICKS, /CBTICKLAB, /CBOUT;, /HIST;, , UNIT='!Xu'  
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
  IF KEYWORD_SET(HFImaps) THEN BEGIN  ; plot some 88mm wide frequency maps.
    LS_mollview, map100, /NESTED, COLT=41, CTDIR=CTDIR, CTFILE=CTFILE, MIN=MINVAL_, MAX=MAXVAL_, CHARSIZE=8d/11d, GRATICULE=GR, GLSIZE=1,HXSIZE=W, FLIP=0, $
      GRMIN=[-179, -89], GRMAX=[179,89], GRLS =1, OUTLINE=out, TITLE=' ', PXSIZE=PX, PS=FDIR+Nm+'_100GHz'+'_.eps', CBLBL='u', /CBLIN, /MODASINH, /CBTICKS, /CBTICKLAB, /CBOUT;, /HIST;, , UNIT='!Xu'  
    outname1 = NmPrefHFI+SZstr+NmSuf+'_100GHz'; 'LS_HFI_PIP_fig_sample_'+SZstr+'mm_v3'
    LS_latexify, NmPref+SZstr+NmSuf+'_100GHz_.eps', IDLstrs, TEXstrs, FNTszs*8d/11d, outname=outname1+'.eps', height=SZ/10d/1.6d, width=SZ/10d, FDIR=FDIR;, /full
    IF !D.Name EQ 'X' THEN SPAWN, FDIR+'epstopdf.pl --res='+STRTRIM(STRING(FIX(FigRes)),2)+' '+FDIR+outname1+'.eps &', msg  ; If in linux, convert the postscript to a pdf file straight away.
    ;
    LS_mollview, map143, /NESTED, COLT=41, CTDIR=CTDIR, CTFILE=CTFILE, MIN=MINVAL_, MAX=MAXVAL_, CHARSIZE=8d/11d, GRATICULE=GR, GLSIZE=1,HXSIZE=W, FLIP=0, $
      GRMIN=[-179, -89], GRMAX=[179,89], GRLS =1, OUTLINE=out, TITLE=' ', PXSIZE=PX, PS=FDIR+Nm+'_143GHz'+'_.eps', CBLBL='u', /CBLIN, /MODASINH, /CBTICKS, /CBTICKLAB, /CBOUT;, /HIST;, , UNIT='!Xu'  
    outname1 = NmPrefHFI+SZstr+NmSuf+'_143GHz'; 'LS_HFI_PIP_fig_sample_'+SZstr+'mm_v3'
    LS_latexify, NmPref+SZstr+NmSuf+'_143GHz_.eps', IDLstrs, TEXstrs, FNTszs*8d/11d, outname=outname1+'.eps', height=SZ/10d/1.6d, width=SZ/10d, FDIR=FDIR;, /full
    IF !D.Name EQ 'X' THEN SPAWN, FDIR+'epstopdf.pl --res='+STRTRIM(STRING(FIX(FigRes)),2)+' '+FDIR+outname1+'.eps &', msg  ; If in linux, convert the postscript to a pdf file straight away.
    ;
    LS_mollview, map217, /NESTED, COLT=41, CTDIR=CTDIR, CTFILE=CTFILE, MIN=MINVAL_, MAX=MAXVAL_, CHARSIZE=8d/11d, GRATICULE=GR, GLSIZE=1,HXSIZE=W, FLIP=0, $
      GRMIN=[-179, -89], GRMAX=[179,89], GRLS =1, OUTLINE=out, TITLE=' ', PXSIZE=PX, PS=FDIR+Nm+'_217GHz'+'_.eps', CBLBL='u', /CBLIN, /MODASINH, /CBTICKS, /CBTICKLAB, /CBOUT;, /HIST;, , UNIT='!Xu'  
    outname1 = NmPrefHFI+SZstr+NmSuf+'_217GHz'; 'LS_HFI_PIP_fig_sample_'+SZstr+'mm_v3'
    LS_latexify, NmPref+SZstr+NmSuf+'_217GHz_.eps', IDLstrs, TEXstrs, FNTszs*8d/11d, outname=outname1+'.eps', height=SZ/10d/1.6d, width=SZ/10d, FDIR=FDIR;, /full
    IF !D.Name EQ 'X' THEN SPAWN, FDIR+'epstopdf.pl --res='+STRTRIM(STRING(FIX(FigRes)),2)+' '+FDIR+outname1+'.eps &', msg  ; If in linux, convert the postscript to a pdf file straight away.
    ;
    LS_mollview, map353, /NESTED, COLT=41, CTDIR=CTDIR, CTFILE=CTFILE, MIN=MINVAL_, MAX=MAXVAL_, CHARSIZE=8d/11d, GRATICULE=GR, GLSIZE=1,HXSIZE=W, FLIP=0, $
      GRMIN=[-179, -89], GRMAX=[179,89], GRLS =1, OUTLINE=out, TITLE=' ', PXSIZE=PX, PS=FDIR+Nm+'_353GHz'+'_.eps', CBLBL='u', /CBLIN, /MODASINH, /CBTICKS, /CBTICKLAB, /CBOUT;, /HIST;, , UNIT='!Xu'  
    outname1 = NmPrefHFI+SZstr+NmSuf+'_353GHz'; 'LS_HFI_PIP_fig_sample_'+SZstr+'mm_v3'
    LS_latexify, NmPref+SZstr+NmSuf+'_353GHz_.eps', IDLstrs, TEXstrs, FNTszs*8d/11d, outname=outname1+'.eps', height=SZ/10d/1.6d, width=SZ/10d, FDIR=FDIR;, /full
    IF !D.Name EQ 'X' THEN SPAWN, FDIR+'epstopdf.pl --res='+STRTRIM(STRING(FIX(FigRes)),2)+' '+FDIR+outname1+'.eps &', msg  ; If in linux, convert the postscript to a pdf file straight away.
    ;
    LS_mollview, map545/1d2, /NESTED, COLT=41, CTDIR=CTDIR, CTFILE=CTFILE, MIN=MINVAL_, MAX=MAXVAL_, CHARSIZE=8d/11d, GRATICULE=GR, GLSIZE=1,HXSIZE=W, FLIP=0, $
      GRMIN=[-179, -89], GRMAX=[179,89], GRLS =1, OUTLINE=out, TITLE=' ', PXSIZE=PX, PS=FDIR+Nm+'_545GHz'+'_.eps', CBLBL='10!U2!N Jy/sr', /CBLIN, /MODASINH, /CBTICKS, /CBTICKLAB, /CBOUT;, /HIST;, , UNIT='!Xu'  
    outname1 = NmPrefHFI+SZstr+NmSuf+'_545GHz'; 'LS_HFI_PIP_fig_sample_'+SZstr+'mm_v3'
    IDLstrs = ['-120','-60','0','60','120','-45','45'] 
    TEXstrs = ['240$^\circ$','300$^\circ$','0$^\circ$','60$^\circ$','120$^\circ$','-45$^\circ$','45$^\circ$']
    FNTszs = DBLARR(N_ELEMENTS(IDLstrs)) + 1d
    LS_latexify, NmPref+SZstr+NmSuf+'_545GHz_.eps', IDLstrs, TEXstrs, FNTszs*8d/11d, outname=outname1+'.eps', height=SZ/10d/1.6d, width=SZ/10d, FDIR=FDIR;, /full
    IF !D.Name EQ 'X' THEN SPAWN, FDIR+'epstopdf.pl --res='+STRTRIM(STRING(FIX(FigRes)),2)+' '+FDIR+outname1+'.eps &', msg  ; If in linux, convert the postscript to a pdf file straight away.
    ;
    LS_mollview, map857/1d2, /NESTED, COLT=41, CTDIR=CTDIR, CTFILE=CTFILE, MIN=MINVAL_, MAX=MAXVAL_, CHARSIZE=8d/11d, GRATICULE=GR, GLSIZE=1,HXSIZE=W, FLIP=0, $
      GRMIN=[-179, -89], GRMAX=[179,89], GRLS =1, OUTLINE=out, TITLE=' ', PXSIZE=PX, PS=FDIR+Nm+'_857GHz'+'_.eps', CBLBL='10!U2!N Jy/sr', /CBLIN, /MODASINH, /CBTICKS, /CBTICKLAB, /CBOUT;, /HIST;, , UNIT='!Xu'  
    outname1 = NmPrefHFI+SZstr+NmSuf+'_857GHz'; 'LS_HFI_PIP_fig_sample_'+SZstr+'mm_v3'
    LS_latexify, NmPref+SZstr+NmSuf+'_857GHz_.eps', IDLstrs, TEXstrs, FNTszs*8d/11d, outname=outname1+'.eps', height=SZ/10d/1.6d, width=SZ/10d, FDIR=FDIR;, /full
    IF !D.Name EQ 'X' THEN SPAWN, FDIR+'epstopdf.pl --res='+STRTRIM(STRING(FIX(FigRes)),2)+' '+FDIR+outname1+'.eps &', msg  ; If in linux, convert the postscript to a pdf file straight away.
    ;
    IDLstrs = ['u','-120','-60','0','60','120','-45','45'] 
    TEXstrs = ['$\mu {\rm K}$','240$^\circ$','300$^\circ$','0$^\circ$','60$^\circ$','120$^\circ$','-45$^\circ$','45$^\circ$']
    FNTszs = DBLARR(N_ELEMENTS(IDLstrs)) + 1d
    ;
  ENDIF
  ;stop
  ;
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
  LS_mollview, map, /NESTED, COLT=41, CTDIR=CTDIR, CTFILE=CTFILE, MIN=MINVAL, MAX=MAXVAL, CHARSIZE=0.6,GRATICULE=GR, GLSIZE=1,HXSIZE=W, FLIP=0, $
    GRMIN=[-179, -89], GRMAX=[179,89], GRLS =1, OUTLINE=out, TITLE=' ', PXSIZE=PX, PS=FDIR+Nm+'_.eps', CBLBL='u', /CBOUT, /CBTICKS, /CBTICKLAB;, /HIST;, , UNIT='!Xu'  
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
  ;stop
  ;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 120 mm figure ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;
  SZ   =   120d
  SZstr = '120'
  PX  = PX_120
  GR  = GR_120
  W   =  W_120
  Nm  = Nm_120
  LS_mollview, map, /NESTED, COLT=41, CTDIR=CTDIR, CTFILE=CTFILE, MIN=MINVAL, MAX=MAXVAL, CHARSIZE=1,GRATICULE=GR, GLSIZE=1,HXSIZE=W, FLIP=0, $
    GRMIN=[-179, -89], GRMAX=[179,89], GRLS =1, OUTLINE=out, TITLE=' ', PXSIZE=PX, PS=FDIR+Nm+'_.eps', CBLBL='u', /CBOUT, /CBTICKS, /CBTICKLAB;, , UNIT='!Xu'  
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
  LS_mollview, map, /NESTED, COLT=41, CTDIR=CTDIR, CTFILE=CTFILE, MIN=MINVAL, MAX=MAXVAL, CHARSIZE=1,GRATICULE=GR, GLSIZE=1,HXSIZE=W, FLIP=0, $
    GRMIN=[-179, -89], GRMAX=[179,89], GRLS =1, OUTLINE=out, TITLE=' ', PXSIZE=PX, PS=FDIR+Nm+'_.eps', CBLBL='u', /CBOUT, /CBTICKS, /CBTICKLAB;, , UNIT='!Xu'  
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
  ;stop
  ;
  ;HFImaps = 1
  ;
  IF KEYWORD_SET(HFImaps) THEN BEGIN
    ;
    HFI_CT, CTDIR=CTDIR, CTFILE=CTFILE, /LOAD, /HIGHDR, HDRFILE=HDRFILE
    FigRes = 600d   ; in dpi.  The paper figures should be at least 300 dpi, according to the A&A author guide.  For Maps, 600 dpi would be better.  
    ;
    PX_88  =  8.8d/2.54d*FigRes
    PX_120 = 12.0d/2.54d*FigRes
    PX_180 = 18.0d/2.54d*FigRes
    ;
    SZ   =   180d
    SZstr = '180'
    PX  = PX_180
    GR  = GR_180
    W   =  W_180
    Nm  = Nm_180
    ;
    ;
    LS_mollview, map100, /NESTED, COLT=41, CTDIR=CTDIR, CTFILE=CTFILE, MIN=MINVAL_, MAX=MAXVAL_, CHARSIZE=8d/11d, GRATICULE=GR, GLSIZE=1,HXSIZE=W, FLIP=0, $
      GRMIN=[-179, -89], GRMAX=[179,89], GRLS =1, OUTLINE=out, TITLE=' ', PXSIZE=PX, PS=FDIR+Nm+'_100GHz'+'_.eps', CBLBL='u', $
      /CBLIN, /MODASINH, /CBTICKS, /CBTICKLAB, /CBOUT, /LATLONGDIFF;, /HIST;, , UNIT='!Xu'  
    IDLstrs = ['u','-120_l','-60_l','0_l','60_l','120_l','-75_b','-60_b','-45_b','-30_b','-15_b','0_b','15_b','30_b','45_b','60_b','75_b']
    TEXstrs = ['$\mu {\rm K}$','240$^\circ$','300$^\circ$','0$^\circ$','60$^\circ$','120$^\circ$',$
               '-75$^\circ$','-60$\circ$','-45$^\circ$','-30$^\circ$','-15$^\circ$','0$^\circ$','15$^\circ$','30$^\circ$','45$^\circ$','60$^\circ$','75$^\circ$']
    FNTszs = DBLARR(N_ELEMENTS(IDLstrs)) + 1d
    outname1 = NmPrefHFI+SZstr+NmSuf+'_100GHz'; 'LS_HFI_PIP_fig_sample_'+SZstr+'mm_v3'
    LS_latexify, NmPref+SZstr+NmSuf+'_100GHz_.eps', IDLstrs, TEXstrs, FNTszs*8d/11d, outname=outname1+'.eps', height=SZ/10d/1.6d, width=SZ/10d, FDIR=FDIR;, /full
    IF !D.Name EQ 'X' THEN SPAWN, FDIR+'epstopdf.pl --res='+STRTRIM(STRING(FIX(FigRes)),2)+' '+FDIR+outname1+'.eps &', msg  ; If in linux, convert the postscript to a pdf file straight away.
    ;
    LS_mollview, map143, /NESTED, COLT=41, CTDIR=CTDIR, CTFILE=CTFILE, MIN=MINVAL_, MAX=MAXVAL_, CHARSIZE=8d/11d, GRATICULE=GR, GLSIZE=1,HXSIZE=W, FLIP=0, $
      GRMIN=[-179, -89], GRMAX=[179,89], GRLS =1, OUTLINE=out, TITLE=' ', PXSIZE=PX, PS=FDIR+Nm+'_143GHz'+'_.eps', CBLBL='u', $
      /CBLIN, /MODASINH, /CBTICKS, /CBTICKLAB, /CBOUT, /LATLONGDIFF;, /HIST;, , UNIT='!Xu'  
    outname1 = NmPrefHFI+SZstr+NmSuf+'_143GHz'; 'LS_HFI_PIP_fig_sample_'+SZstr+'mm_v3'
    LS_latexify, NmPref+SZstr+NmSuf+'_143GHz_.eps', IDLstrs, TEXstrs, FNTszs*8d/11d, outname=outname1+'.eps', height=SZ/10d/1.6d, width=SZ/10d, FDIR=FDIR;, /full
    IF !D.Name EQ 'X' THEN SPAWN, FDIR+'epstopdf.pl --res='+STRTRIM(STRING(FIX(FigRes)),2)+' '+FDIR+outname1+'.eps &', msg  ; If in linux, convert the postscript to a pdf file straight away.
    ;
    LS_mollview, map217, /NESTED, COLT=41, CTDIR=CTDIR, CTFILE=CTFILE, MIN=MINVAL_, MAX=MAXVAL_, CHARSIZE=8d/11d, GRATICULE=GR, GLSIZE=1,HXSIZE=W, FLIP=0, $
      GRMIN=[-179, -89], GRMAX=[179,89], GRLS =1, OUTLINE=out, TITLE=' ', PXSIZE=PX, PS=FDIR+Nm+'_217GHz'+'_.eps', CBLBL='u', $
      /CBLIN, /MODASINH, /CBTICKS, /CBTICKLAB, /CBOUT, /LATLONGDIFF;, /HIST;, , UNIT='!Xu'  
    outname1 = NmPrefHFI+SZstr+NmSuf+'_217GHz'; 'LS_HFI_PIP_fig_sample_'+SZstr+'mm_v3'
    LS_latexify, NmPref+SZstr+NmSuf+'_217GHz_.eps', IDLstrs, TEXstrs, FNTszs*8d/11d, outname=outname1+'.eps', height=SZ/10d/1.6d, width=SZ/10d, FDIR=FDIR;, /full
    IF !D.Name EQ 'X' THEN SPAWN, FDIR+'epstopdf.pl --res='+STRTRIM(STRING(FIX(FigRes)),2)+' '+FDIR+outname1+'.eps &', msg  ; If in linux, convert the postscript to a pdf file straight away.
    ;
    LS_mollview, map353, /NESTED, COLT=41, CTDIR=CTDIR, CTFILE=CTFILE, MIN=MINVAL_, MAX=MAXVAL_, CHARSIZE=8d/11d, GRATICULE=GR, GLSIZE=1,HXSIZE=W, FLIP=0, $
      GRMIN=[-179, -89], GRMAX=[179,89], GRLS =1, OUTLINE=out, TITLE=' ', PXSIZE=PX, PS=FDIR+Nm+'_353GHz'+'_.eps', CBLBL='u', $
      /CBLIN, /MODASINH, /CBTICKS, /CBTICKLAB, /CBOUT, /LATLONGDIFF;, /HIST;, , UNIT='!Xu'  
    outname1 = NmPrefHFI+SZstr+NmSuf+'_353GHz'; 'LS_HFI_PIP_fig_sample_'+SZstr+'mm_v3'
    LS_latexify, NmPref+SZstr+NmSuf+'_353GHz_.eps', IDLstrs, TEXstrs, FNTszs*8d/11d, outname=outname1+'.eps', height=SZ/10d/1.6d, width=SZ/10d, FDIR=FDIR;, /full
    IF !D.Name EQ 'X' THEN SPAWN, FDIR+'epstopdf.pl --res='+STRTRIM(STRING(FIX(FigRes)),2)+' '+FDIR+outname1+'.eps &', msg  ; If in linux, convert the postscript to a pdf file straight away.
    ;
    LS_mollview, map545/1d3, /NESTED, COLT=41, CTDIR=CTDIR, CTFILE=CTFILE, MIN=MINVAL_, MAX=MAXVAL_, CHARSIZE=8d/11d, GRATICULE=GR, GLSIZE=1,HXSIZE=W, FLIP=0, $
      GRMIN=[-179, -89], GRMAX=[179,89], GRLS =1, OUTLINE=out, TITLE=' ', PXSIZE=PX, PS=FDIR+Nm+'_545GHz'+'_.eps', CBLBL='kJy/sr', $
      /CBLIN, /MODASINH, /CBTICKS, /CBTICKLAB, /CBOUT, /LATLONGDIFF;, /HIST;, , UNIT='!Xu'  
    outname1 = NmPrefHFI+SZstr+NmSuf+'_545GHz'; 'LS_HFI_PIP_fig_sample_'+SZstr+'mm_v3'
    IDLstrs = ['-120_l','-60_l','0_l','60_l','120_l','-75_b','-60_b','-45_b','-30_b','-15_b','0_b','15_b','30_b','45_b','60_b','75_b']
    TEXstrs = ['240$^\circ$','300$^\circ$','0$^\circ$','60$^\circ$','120$^\circ$',$
               '-75$^\circ$','-60$\circ$','-45$^\circ$','-30$^\circ$','-15$^\circ$','0$^\circ$','15$^\circ$','30$^\circ$','45$^\circ$','60$^\circ$','75$^\circ$']
    FNTszs = DBLARR(N_ELEMENTS(IDLstrs)) + 1d
    LS_latexify, NmPref+SZstr+NmSuf+'_545GHz_.eps', IDLstrs, TEXstrs, FNTszs*8d/11d, outname=outname1+'.eps', height=SZ/10d/1.6d, width=SZ/10d, FDIR=FDIR;, /full
    IF !D.Name EQ 'X' THEN SPAWN, FDIR+'epstopdf.pl --res='+STRTRIM(STRING(FIX(FigRes)),2)+' '+FDIR+outname1+'.eps &', msg  ; If in linux, convert the postscript to a pdf file straight away.
    ;
    LS_mollview, map857/1d3, /NESTED, COLT=41, CTDIR=CTDIR, CTFILE=CTFILE, MIN=MINVAL_, MAX=MAXVAL_, CHARSIZE=8d/11d, GRATICULE=GR, GLSIZE=1,HXSIZE=W, FLIP=0, $
      GRMIN=[-179, -89], GRMAX=[179,89], GRLS =1, OUTLINE=out, TITLE=' ', PXSIZE=PX, PS=FDIR+Nm+'_857GHz'+'_.eps', CBLBL='kJy/sr', $
      /CBLIN, /MODASINH, /CBTICKS, /CBTICKLAB, /CBOUT, /LATLONGDIFF;, /HIST;, , UNIT='!Xu'  
    outname1 = NmPrefHFI+SZstr+NmSuf+'_857GHz'; 'LS_HFI_PIP_fig_sample_'+SZstr+'mm_v3'
    LS_latexify, NmPref+SZstr+NmSuf+'_857GHz_.eps', IDLstrs, TEXstrs, FNTszs*8d/11d, outname=outname1+'.eps', height=SZ/10d/1.6d, width=SZ/10d, FDIR=FDIR;, /full
    IF !D.Name EQ 'X' THEN SPAWN, FDIR+'epstopdf.pl --res='+STRTRIM(STRING(FIX(FigRes)),2)+' '+FDIR+outname1+'.eps &', msg  ; If in linux, convert the postscript to a pdf file straight away.
    ;
    ;
    LS_mollview, map545*UcTB545, /NESTED, COLT=41, CTDIR=CTDIR, CTFILE=CTFILE, MIN=MINVAL_, MAX=MAXVAL_, CHARSIZE=8d/11d, GRATICULE=GR, GLSIZE=1,HXSIZE=W, FLIP=0, $
      GRMIN=[-179, -89], GRMAX=[179,89], GRLS =1, OUTLINE=out, TITLE=' ', PXSIZE=PX, PS=FDIR+Nm+'_545GHz_uKRJ'+'_.eps', CBLBL='u', $
      /CBLIN, /MODASINH, /CBTICKS, /CBTICKLAB, /CBOUT, /LATLONGDIFF;, /HIST;, , UNIT='!Xu'  
    outname1 = NmPrefHFI+SZstr+NmSuf+'_545GHz_uKRJ'; 'LS_HFI_PIP_fig_sample_'+SZstr+'mm_v3'
    IDLstrs = ['u','-120_l','-60_l','0_l','60_l','120_l','-75_b','-60_b','-45_b','-30_b','-15_b','0_b','15_b','30_b','45_b','60_b','75_b']
    TEXstrs = ['$\mu {\rm K}_{\mbox{\tiny{RJ}}}$','240$^\circ$','300$^\circ$','0$^\circ$','60$^\circ$','120$^\circ$',$
               '-75$^\circ$','-60$\circ$','-45$^\circ$','-30$^\circ$','-15$^\circ$','0$^\circ$','15$^\circ$','30$^\circ$','45$^\circ$','60$^\circ$','75$^\circ$']
    FNTszs = DBLARR(N_ELEMENTS(IDLstrs)) + 1d
    LS_latexify, NmPref+SZstr+NmSuf+'_545GHz_uKRJ_.eps', IDLstrs, TEXstrs, FNTszs*8d/11d, outname=outname1+'.eps', height=SZ/10d/1.6d, width=SZ/10d, FDIR=FDIR;, /full
    IF !D.Name EQ 'X' THEN SPAWN, FDIR+'epstopdf.pl --res='+STRTRIM(STRING(FIX(FigRes)),2)+' '+FDIR+outname1+'.eps &', msg  ; If in linux, convert the postscript to a pdf file straight away.
    ;
    LS_mollview, map857*UcTb857, /NESTED, COLT=41, CTDIR=CTDIR, CTFILE=CTFILE, MIN=MINVAL_, MAX=MAXVAL_, CHARSIZE=8d/11d, GRATICULE=GR, GLSIZE=1,HXSIZE=W, FLIP=0, $
      GRMIN=[-179, -89], GRMAX=[179,89], GRLS =1, OUTLINE=out, TITLE=' ', PXSIZE=PX, PS=FDIR+Nm+'_857GHz_uKRJ'+'_.eps', CBLBL='u', $
      /CBLIN, /MODASINH, /CBTICKS, /CBTICKLAB, /CBOUT, /LATLONGDIFF;, /HIST;, , UNIT='!Xu'  
    outname1 = NmPrefHFI+SZstr+NmSuf+'_857GHz_uKRJ'; 'LS_HFI_PIP_fig_sample_'+SZstr+'mm_v3'
    LS_latexify, NmPref+SZstr+NmSuf+'_857GHz_uKRJ_.eps', IDLstrs, TEXstrs, FNTszs*8d/11d, outname=outname1+'.eps', height=SZ/10d/1.6d, width=SZ/10d, FDIR=FDIR;, /full
    IF !D.Name EQ 'X' THEN SPAWN, FDIR+'epstopdf.pl --res='+STRTRIM(STRING(FIX(FigRes)),2)+' '+FDIR+outname1+'.eps &', msg  ; If in linux, convert the postscript to a pdf file straight away.
    ;
    ;
    ;
    LS_mollview, map545*UcTB545*1d3, /NESTED, COLT=41, CTDIR=CTDIR, CTFILE=CTFILE, MIN=MINVAL_, MAX=MAXVAL_, CHARSIZE=8d/11d, GRATICULE=GR, GLSIZE=1,HXSIZE=W, FLIP=0, $
      GRMIN=[-179, -89], GRMAX=[179,89], GRLS =1, OUTLINE=out, TITLE=' ', PXSIZE=PX, PS=FDIR+Nm+'_545GHz_nKRJ'+'_.eps', CBLBL='u', $
      /CBLIN, /MODASINH, /CBTICKS, /CBTICKLAB, /CBOUT, /LATLONGDIFF;, /HIST;, , UNIT='!Xu'  
    outname1 = NmPrefHFI+SZstr+NmSuf+'_545GHz_nKRJ'; 'LS_HFI_PIP_fig_sample_'+SZstr+'mm_v3'
    IDLstrs = ['u','-120_l','-60_l','0_l','60_l','120_l','-75_b','-60_b','-45_b','-30_b','-15_b','0_b','15_b','30_b','45_b','60_b','75_b']
    TEXstrs = ['${\rm nK}_{\mbox{\tiny{RJ}}}$','240$^\circ$','300$^\circ$','0$^\circ$','60$^\circ$','120$^\circ$',$
               '-75$^\circ$','-60$\circ$','-45$^\circ$','-30$^\circ$','-15$^\circ$','0$^\circ$','15$^\circ$','30$^\circ$','45$^\circ$','60$^\circ$','75$^\circ$']
    FNTszs = DBLARR(N_ELEMENTS(IDLstrs)) + 1d
    LS_latexify, NmPref+SZstr+NmSuf+'_545GHz_nKRJ_.eps', IDLstrs, TEXstrs, FNTszs*8d/11d, outname=outname1+'.eps', height=SZ/10d/1.6d, width=SZ/10d, FDIR=FDIR;, /full
    IF !D.Name EQ 'X' THEN SPAWN, FDIR+'epstopdf.pl --res='+STRTRIM(STRING(FIX(FigRes)),2)+' '+FDIR+outname1+'.eps &', msg  ; If in linux, convert the postscript to a pdf file straight away.
    ;
    LS_mollview, map857*UcTb857*1d3, /NESTED, COLT=41, CTDIR=CTDIR, CTFILE=CTFILE, MIN=MINVAL_, MAX=MAXVAL_, CHARSIZE=8d/11d, GRATICULE=GR, GLSIZE=1,HXSIZE=W, FLIP=0, $
      GRMIN=[-179, -89], GRMAX=[179,89], GRLS =1, OUTLINE=out, TITLE=' ', PXSIZE=PX, PS=FDIR+Nm+'_857GHz_nKRJ'+'_.eps', CBLBL='u', $
      /CBLIN, /MODASINH, /CBTICKS, /CBTICKLAB, /CBOUT, /LATLONGDIFF;, /HIST;, , UNIT='!Xu'  
    outname1 = NmPrefHFI+SZstr+NmSuf+'_857GHz_nKRJ'; 'LS_HFI_PIP_fig_sample_'+SZstr+'mm_v3'
    LS_latexify, NmPref+SZstr+NmSuf+'_857GHz_nKRJ_.eps', IDLstrs, TEXstrs, FNTszs*8d/11d, outname=outname1+'.eps', height=SZ/10d/1.6d, width=SZ/10d, FDIR=FDIR;, /full
    IF !D.Name EQ 'X' THEN SPAWN, FDIR+'epstopdf.pl --res='+STRTRIM(STRING(FIX(FigRes)),2)+' '+FDIR+outname1+'.eps &', msg  ; If in linux, convert the postscript to a pdf file straight away.
    ;
    spawn, FDIR+'eps2png.pl -width 1050 '+FDIR+'PlanckFig_map_HighDR_IDL_180mm_100GHz.eps &'
    spawn, FDIR+'eps2png.pl -width 1050 '+FDIR+'PlanckFig_map_HighDR_IDL_180mm_143GHz.eps &'
    spawn, FDIR+'eps2png.pl -width 1050 '+FDIR+'PlanckFig_map_HighDR_IDL_180mm_217GHz.eps &'
    spawn, FDIR+'eps2png.pl -width 1050 '+FDIR+'PlanckFig_map_HighDR_IDL_180mm_353GHz.eps &'
    spawn, FDIR+'eps2png.pl -width 1050 '+FDIR+'PlanckFig_map_HighDR_IDL_180mm_545GHz.eps &'
    spawn, FDIR+'eps2png.pl -width 1050 '+FDIR+'PlanckFig_map_HighDR_IDL_180mm_857GHz.eps &'
    spawn, FDIR+'eps2png.pl -width 1050 '+FDIR+'PlanckFig_map_HighDR_IDL_180mm_545GHz_uKRJ.eps &'
    spawn, FDIR+'eps2png.pl -width 1050 '+FDIR+'PlanckFig_map_HighDR_IDL_180mm_857GHz_uKRJ.eps &'
    spawn, FDIR+'eps2png.pl -width 1050 '+FDIR+'PlanckFig_map_HighDR_IDL_180mm_545GHz_nKRJ.eps &'
    spawn, FDIR+'eps2png.pl -width 1050 '+FDIR+'PlanckFig_map_HighDR_IDL_180mm_857GHz_nKRJ.eps &'
    ;
  ENDIF
  ;
  IF KEYWORD_SET(LFImaps) THEN BEGIN
    ;
    LS_mollview, map30, /NESTED, COLT=41, CTDIR=CTDIR, CTFILE=CTFILE, MIN=MINVAL_, MAX=MAXVAL_, CHARSIZE=8d/11d, GRATICULE=GR, GLSIZE=1,HXSIZE=W, FLIP=0, $
      GRMIN=[-179, -89], GRMAX=[179,89], GRLS =1, OUTLINE=out, TITLE=' ', PXSIZE=PX, PS=FDIR+Nm+'_30GHz'+'_.eps', CBLBL='u', $
      /CBLIN, /MODASINH, /CBTICKS, /CBTICKLAB, /CBOUT, /LATLONGDIFF;, /HIST;, , UNIT='!Xu'  
    IDLstrs = ['u','-120_l','-60_l','0_l','60_l','120_l','-75_b','-60_b','-45_b','-30_b','-15_b','0_b','15_b','30_b','45_b','60_b','75_b']
    TEXstrs = ['$\mu {\rm K}$','240$^\circ$','300$^\circ$','0$^\circ$','60$^\circ$','120$^\circ$',$
             '-75$^\circ$','-60$\circ$','-45$^\circ$','-30$^\circ$','-15$^\circ$','0$^\circ$','15$^\circ$','30$^\circ$','45$^\circ$','60$^\circ$','75$^\circ$']
    FNTszs = DBLARR(N_ELEMENTS(IDLstrs)) + 1d
    outname1 = NmPrefHFI+SZstr+NmSuf+'_30GHz'; 'LS_HFI_PIP_fig_sample_'+SZstr+'mm_v3'
    LS_latexify, NmPref+SZstr+NmSuf+'_30GHz_.eps', IDLstrs, TEXstrs, FNTszs*8d/11d, outname=outname1+'.eps', height=SZ/10d/1.6d, width=SZ/10d, FDIR=FDIR;, /full
    IF !D.Name EQ 'X' THEN SPAWN, FDIR+'epstopdf.pl --res='+STRTRIM(STRING(FIX(FigRes)),2)+' '+FDIR+outname1+'.eps &', msg  ; If in linux, convert the postscript to a pdf file straight away.
    ;
    ;
    LS_mollview, map44, /NESTED, COLT=41, CTDIR=CTDIR, CTFILE=CTFILE, MIN=MINVAL_, MAX=MAXVAL_, CHARSIZE=8d/11d, GRATICULE=GR, GLSIZE=1,HXSIZE=W, FLIP=0, $
      GRMIN=[-179, -89], GRMAX=[179,89], GRLS =1, OUTLINE=out, TITLE=' ', PXSIZE=PX, PS=FDIR+Nm+'_44GHz'+'_.eps', CBLBL='u', $
      /CBLIN, /MODASINH, /CBTICKS, /CBTICKLAB, /CBOUT, /LATLONGDIFF;, /HIST;, , UNIT='!Xu'  
    IDLstrs = ['u','-120_l','-60_l','0_l','60_l','120_l','-75_b','-60_b','-45_b','-30_b','-15_b','0_b','15_b','30_b','45_b','60_b','75_b']
    TEXstrs = ['$\mu {\rm K}$','240$^\circ$','300$^\circ$','0$^\circ$','60$^\circ$','120$^\circ$',$
             '-75$^\circ$','-60$\circ$','-45$^\circ$','-30$^\circ$','-15$^\circ$','0$^\circ$','15$^\circ$','30$^\circ$','45$^\circ$','60$^\circ$','75$^\circ$']
    FNTszs = DBLARR(N_ELEMENTS(IDLstrs)) + 1d
    outname1 = NmPrefHFI+SZstr+NmSuf+'_44GHz'; 'LS_HFI_PIP_fig_sample_'+SZstr+'mm_v3'
    LS_latexify, NmPref+SZstr+NmSuf+'_44GHz_.eps', IDLstrs, TEXstrs, FNTszs*8d/11d, outname=outname1+'.eps', height=SZ/10d/1.6d, width=SZ/10d, FDIR=FDIR;, /full
    IF !D.Name EQ 'X' THEN SPAWN, FDIR+'epstopdf.pl --res='+STRTRIM(STRING(FIX(FigRes)),2)+' '+FDIR+outname1+'.eps &', msg  ; If in linux, convert the postscript to a pdf file straight away.
    ;
    ;
    LS_mollview, map70, /NESTED, COLT=41, CTDIR=CTDIR, CTFILE=CTFILE, MIN=MINVAL_, MAX=MAXVAL_, CHARSIZE=8d/11d, GRATICULE=GR, GLSIZE=1,HXSIZE=W, FLIP=0, $
      GRMIN=[-179, -89], GRMAX=[179,89], GRLS =1, OUTLINE=out, TITLE=' ', PXSIZE=PX, PS=FDIR+Nm+'_70GHz'+'_.eps', CBLBL='u', $
      /CBLIN, /MODASINH, /CBTICKS, /CBTICKLAB, /CBOUT, /LATLONGDIFF;, /HIST;, , UNIT='!Xu'  
    IDLstrs = ['u','-120_l','-60_l','0_l','60_l','120_l','-75_b','-60_b','-45_b','-30_b','-15_b','0_b','15_b','30_b','45_b','60_b','75_b']
    TEXstrs = ['$\mu {\rm K}$','240$^\circ$','300$^\circ$','0$^\circ$','60$^\circ$','120$^\circ$',$
             '-75$^\circ$','-60$\circ$','-45$^\circ$','-30$^\circ$','-15$^\circ$','0$^\circ$','15$^\circ$','30$^\circ$','45$^\circ$','60$^\circ$','75$^\circ$']
    FNTszs = DBLARR(N_ELEMENTS(IDLstrs)) + 1d
    outname1 = NmPrefHFI+SZstr+NmSuf+'_70GHz'; 'LS_HFI_PIP_fig_sample_'+SZstr+'mm_v3'
    LS_latexify, NmPref+SZstr+NmSuf+'_70GHz_.eps', IDLstrs, TEXstrs, FNTszs*8d/11d, outname=outname1+'.eps', height=SZ/10d/1.6d, width=SZ/10d, FDIR=FDIR;, /full
    IF !D.Name EQ 'X' THEN SPAWN, FDIR+'epstopdf.pl --res='+STRTRIM(STRING(FIX(FigRes)),2)+' '+FDIR+outname1+'.eps &', msg  ; If in linux, convert the postscript to a pdf file straight away.
    ;
    spawn, FDIR+'eps2png.pl -width 1050 '+FDIR+'PlanckFig_map_HighDR_IDL_180mm_30GHz.eps &'
    spawn, FDIR+'eps2png.pl -width 1050 '+FDIR+'PlanckFig_map_HighDR_IDL_180mm_44GHz.eps &'
    spawn, FDIR+'eps2png.pl -width 1050 '+FDIR+'PlanckFig_map_HighDR_IDL_180mm_70GHz.eps &'
    
    ;
  ENDIF
    ;
    ;wait, 10d
    ;spawn, 'cd '+FDIR+' && pdflatex LS_PlanckHighDRmaps_20130305' ; These will nopt work without my latex template in place, and the frequency maps.
    ;
  stop
 END