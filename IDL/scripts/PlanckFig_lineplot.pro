;+
; NAME:
; PowerSpectrum_fig_example
;
; PURPOSE:
; This procedure produces several example Planck paper plots to conform to the Planck paper style guide. 
;
;
; CALLING SEQUENCE:
; PowerSpectrum_fig_example
;
; INPUTS:
; requires several text files containing input data to plot.  
;  f1 = 'bf_cbipap5_all.dat'                   ; 2 column text file, 0 header lines, l, C_l
;  f2 = 'boom_powers.dat'                      ; 4 column text file, 3 header lines, l, c_l (uK^2), +/- 
;  f3 = 'dasi_powers.dat'                      ; 5 column text file, 2 header lines, leff, l-range 1, l-range 2, Cl (uk^2), +/-
;  f4 = 'maxima_powers.dat'                    ; 6 column text file, 2 header lines, leff, l-range 1, l-range 2, Cl (uk^2), +err, -err
;  f5 = 'joint_final_iso_0.08_200_even.fdat'   ; 7 column text file, 0 header lines, lmin, lmax, l-eff, Cl (uk^2), +err, -err
;  f6 = 'joint_final_iso_0.08_200_odd.fdat'    ; same as f5 above.
;
; This is intended as an example for other members of the Planck consortia to follow in generating their own Planck figures using IDL,
; suitable for publication. 
;
; KEYWORD PARAMETERS:
; none
;
; OUTPUTS:
; several .eps and .pdf files are saved to disk
;
; RESTRICTIONS:
;
;
; MODIFICATION HISTORY:
;   Written by: L.D.Spencer 2013/Jan/04
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
;-
PRO PowerSpectrum_fig_example
  ; 
  ; This script creates sample figures appropriate for submission to A&A for HFI PIP papers, using IDL to produce them.
  ;
  ; First restore the data provided by Tim Pearson.
  ;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   Just restore the sample data as provided, ignore this bit as you already have your own data to plot
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;
  ;
  fDIR = '/export/data/spxls/Docs/Planck/PIPfigTemplate/' ; change this to your own directory as needed.
  ;
  ; Note: If using windows, use `\' or `//' as a file separator, `//' works in linux too.
  ;
  f1 = 'bf_cbipap5_all.dat'                   ; 2 column text file, 0 header lines, l, C_l
  f2 = 'boom_powers.dat'                      ; 4 column text file, 3 header lines, l, c_l (uK^2), +/- 
  f3 = 'dasi_powers.dat'                      ; 5 column text file, 2 header lines, leff, l-range 1, l-range 2, Cl (uk^2), +/-
  f4 = 'maxima_powers.dat'                    ; 6 column text file, 2 header lines, leff, l-range 1, l-range 2, Cl (uk^2), +err, -err
  f5 = 'joint_final_iso_0.08_200_even.fdat'   ; 7 column text file, 0 header lines, lmin, lmax, l-eff, Cl (uk^2), +err, -err
  f6 = 'joint_final_iso_0.08_200_odd.fdat'    ; same as above.
  ;
  dat1 = READ_ASCII(fDIR+f1, HEADER=hdr, DATA_START=0)
  l1 = REFORM(dat1.field1[0,*])
  Cl1 = REFORM(dat1.field1[1,*])
  ;
  dat2 = READ_ASCII(fDIR+f2, HEADER=hdr2, DATA_START=3)
  l2_1 = REFORM(dat2.field1[0,*]) ; start of range
  l2_2 = REFORM(dat2.field1[1,*]) ; end of range
  l2   = (l2_1 + l2_2)/2d         ; mean
  Cl2 = REFORM(dat2.field1[2,*])  ; mean value
  Cl2pm = REFORM(dat2.field1[3,*]); 1-sigma errors
  ;
  dat3  = READ_ASCII(fDIR+f3, HEADER=hdr3, DATA_START=2)
  l3    = REFORM(dat3.field1[0,*])   ; effective el of range
  l3_1  = REFORM(dat3.field1[1,*]) ; start of range
  l3_2  = REFORM(dat3.field1[2,*]) ; end of range
  Cl3   = REFORM(dat3.field1[3,*])  ; mean value
  Cl3pm = REFORM(dat3.field1[4,*]); 1-sigma errors
  ;
  dat4  = READ_ASCII(fDIR+f4, HEADER=hdr4, DATA_START=2)
  l4    = REFORM(dat4.field1[0,*])   ; effective el of range
  l4_1  = REFORM(dat4.field1[1,*]) ; start of range
  l4_2  = REFORM(dat4.field1[2,*]) ; end of range
  Cl4   = REFORM(dat4.field1[3,*])  ; mean value
  Cl4p = REFORM(dat4.field1[4,*]); 1-sigma errors
  Cl4m = REFORM(dat4.field1[5,*]); 1-sigma errors
  ;
  dat5  = READ_ASCII(fDIR+f5, HEADER=hdr5, DATA_START=0)
  l5_1  = REFORM(dat5.field1[0,*])   ; effective el of range
  l5_2  = REFORM(dat5.field1[1,*]) ; start of range
  l5    = REFORM(dat5.field1[2,*]) ; end of range
  Cl5   = REFORM(dat5.field1[3,*])  ; mean value
  Cl5pm = REFORM(dat5.field1[4,*]); 1-sigma errors
  Cl5_X = REFORM(dat5.field1[5,*]); I don't know what this is (chi^2 perhaps?)
  Cl5_Y = REFORM(dat5.field1[5,*]); I also don't know what this one is...
  ;
  dat6  = READ_ASCII(fDIR+f6, HEADER=hdr6, DATA_START=0)
  l6_1  = REFORM(dat6.field1[0,*])   ; effective el of range
  l6_2  = REFORM(dat6.field1[1,*]) ; start of range
  l6    = REFORM(dat6.field1[2,*]) ; end of range
  Cl6   = REFORM(dat6.field1[3,*])  ; mean value
  Cl6pm = REFORM(dat6.field1[4,*]); 1-sigma errors
  Cl6_X = REFORM(dat6.field1[5,*]); I don't know what this is (chi^2 perhaps?)
  Cl6_Y = REFORM(dat6.field1[5,*]); I also don't know what this one is...
  ;
  ;
  ;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   Done Reading the data in
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;
  ;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   Now set up the sizes for each figure.  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   I have used an aspect ratio of 1.5 (but this is not a requirement)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   Also set up some common margins, font sizes, etc.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;
  ;
  ;
  ;
  X1 = 88d/25.4d  ; 88 mm by 25.m mm/inch to get the size in inches
  AR = 1.5d  ; an aspect ratio of 1.5
  Y1 = X1/AR
  ;
  X2 = 120d/25.4d ; 120 mm
  Y2 = X2/AR
  ;
  X3 = 180d/25.4d ; 
  Y3 = X3/AR
  ;
  X4 = X1
  Y4 = Y1*2 ; this will be a 2 x 1 plot.
  ;
  X5 = X2
  Y5 = Y2*2 ; this will be a 2 x 1 plot.
  ;
  X6 = X3
  Y6 = Y3*2
  ;
  colours								;	This routine is included in this file (see bottom) and sets up a few default colours.
  Dname = !D.NAME							;	This is different for windows and linux (and likely MAC)
  ;
  XMAR_ = [8.15,2.25]     ; Set this to minimize white space on left and right sides of figure.
  XMAR_ = [6,2.25]
  YMAR_ = [3,0.5]     ; Set this to minimize white space on bottom and top of figure.
  ;
  XMAR1 = XMAR_
  XMAR2 = XMAR_*120d/88d
  XMAR3 = XMAR_*180d/88d
  ;
  YMAR1 = YMAR_
  YMAR2 = YMAR_*120d/88d
  YMAR3 = YMAR_*180d/88d
  ;
  YTTL_DXs = [-50d, -50d*88d/120d, -50d*88d/180d]
  XTTL_DYs = [-100d, -100d*88d/120d, -100d*88d/180d]
  ;
  FNTsz = 8          ; Set this to the desired font size (pt)
  ;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   I have the sizes for 5 figures mapped out:
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  small, medium, and large single figures, and a small and large 1x2 figure pair.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  Do the first three as a for loop, and the last two as custom runs.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;
  ;
  SZstrs = ['88','120','180']
  Xs = [X1,X2,X3]
  Ys = [Y1,Y2,Y3]
  ;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   First figure, 88mm wide, then 120 mm wide, then 180 mm wide, all in a for loop
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;
  ;
  FOR jj = 0, 2 DO BEGIN
    Xsz = Xs[jj]
    Ysz = Ys[jj]
    CASE jj OF
      0: XMAR = XMAR1
      1: XMAR = XMAR2
      2: XMAR = XMAR3
    ENDCASE
    ;
    CASE jj OF
      0: YMAR = YMAR1
      1: YMAR = YMAR2
      2: YMAR = YMAR3
    ENDCASE
    ;
    SZstr = SZstrs[jj]
    plotDIR = fDIR
    fName = 'LS_HFI_PIP_fig_sample_'+SZstr+'mm_v2'  
    set_plot, "ps"
    !P.font = 0
    device, FILENAME=plotDIR+fName+'.eps',xsize=Xsz,ysize=Ysz,/inches,/color, /ENCAPSULATED, /HELVETICA, FONT_size=FNTsz
    ;
    !P.CHARTHICK = 1d
    !P.CHARSIZE=1						;	Set the charactersize to not be scaled from that above.
    !X.CHARSIZE=1						;	Set the X-label the same as the main figure text.
    !Y.CHARSIZE=1						;	Set the Y-label the same as the main figure text.
    !p.thick = 1.0d						;	Set the lines a bit thicker the nthe minimum of 1 pt
    !x.thick = !P.thick 					;	Set x-axis lines the same as others within the plot
    !y.thick = !P.thick 					;	The same for y-axis lines
    ;
    ;  HFI_plot is a wrapper function for plot which rotates the orientation of the y-axis tick labels 90 deg. ccw.
    ;  Use HFI_plot in place of the IDL plot command.  This should only change the orientation of the y-axis number labels, 
    ;  with all else being as if plot were called (but I have not tested it infinitely).  HFI_plot also accepts a few 
    ;  additional keywords to tweak the placement of the x and y axis tick labels.
    ;  DECMODX=DECMODX, DECMODY=DECMODY, Y_DX=Y_DX, Y_DY=Y_DY, X_DX=X_DX, X_DY=X_DY
    ;  
    ;  DECMODX[/Y] adjusts the number of significant digits in the X[/Y] axis tick labels, 1,2,etc. to to the default amount, -1,-2,etc. to reduce from the default amount.
    ;  Y_DX adjusts the location of the y-axis tick labels horizontal position, in data units.
    ;  Y_DY adjusts the location of the y-axis tick labels vertical position, in data units.
    ;  X_DX adjusts the location of the x-axis tick labels horizontal position, in data units.
    ;  X_DY adjusts the location of the x-axis tick labels vertical position, in data units.
    ;  
    ;  Use of the IDL _EXTRA keyword passes additional plot keywords onto the plot call within HFI_plot.
    ;
    HFI_plot, l1, cl1, /xs, xr=[0,3000], /ys, yr=[-1000,8000], BACKGROUND=255, COLOR=0, $
      XTITLE='xttl', YTITLE='yttl', XMARGIN=XMAR, YMARGIN=YMAR, YTTL_DX = YTTL_DXs[jj], XTTL_DY=XTTL_DYs[jj]
    plots, !X.crange, 0d					;	Plot a horizontal line at y=0
    ;
    ;	Fill in the BOOMERANG data
    FOR ii = 0, 11 DO POLYFILL, [l4_1[ii],l4_1[ii],l4_2[ii],l4_2[ii],l4_1[ii]], [-1d*CL4m[ii],CL4p[ii],CL4p[ii],-1d*CL4m[ii],-1d*CL4m[ii]] + CL4[ii], NOCLIP=0, COLOR=8
    FOR ii = 0, 18 DO POLYFILL, [l2_1[ii],l2_1[ii],l2_2[ii],l2_2[ii],l2_1[ii]], [-1d,1d,1d,-1d,-1d]*CL2pm[ii] + CL2[ii], NOCLIP=0, COLOR=10
    ;
    ;    Fill in the DASI data.
    FOR ii = 0, 8 DO POLYFILL, [l3_1[ii],l3_1[ii],l3_2[ii],l3_2[ii],l3_1[ii]], [-1d,1d,1d,-1d,-1d]*CL3pm[ii] + CL3[ii], NOCLIP=0, COLOR=26, /LINE_FILL, ORIENTATION=45, THICK=1.5, SPACING=0.05
    FOR ii = 0, 8 DO POLYFILL, [l3_1[ii],l3_1[ii],l3_2[ii],l3_2[ii],l3_1[ii]], [-1d,1d,1d,-1d,-1d]*CL3pm[ii] + CL3[ii], NOCLIP=0, COLOR=26, /LINE_FILL, ORIENTATION=-45,THICK=1.5, SPACING=0.05
    ;
    oplot, l1, cl1, color=0					;	re-plot the black curve over the polyfill-ed regions.
    plots, !X.crange, 0d, color=0				;	the same for the line across the x-axis
    ;
    ;  Fill in the legend polyfill regions.
    ;  Put the legend in the upper right corner
    ;  IDL has a legend procedure which will do most of this, but it will not do the polyfill without modification, 
    ;  so I am doing it manually for demonstration purposes.  There is an easier way, but perhaps less clear for demonstration purposes. 
    ;
    ;  Boomerang is the longest word, so figure out where it wants to be to avoid going out of the plot region.  
    w_pix = !D.X_SIZE - ((!X.range)[0] + (!X.range)[1])*!D.X_CH_SIZE    ;   height of plot range in device coordinates
    w_dat = !X.crange[1] - !X.crange[0] ; height in data units.   ; height of plot-region in data units
    Rscl = 0.75                 ; Scale this text a bit smaller
    txtWidth = (!D.X_CH_SIZE)*w_dat/w_pix*Rscl        ; The approximate width of one character...with an extra 2 characters on either side
    strWidth = txtWidth*15d ; boomerang is only 9 characters, but I am putting in a few more as a safety factor. 
    ;
    h_pix = !D.Y_SIZE - ((!Y.range)[0] + (!Y.range)[1])*!D.Y_CH_SIZE    ;   height of plot range in device coordinates
    h_dat = !Y.crange[1] - !Y.crange[0] ; height in data units.   ; height of plot-region in data units
    Rscl = 0.75                 ; Scale this text a bit smaller
    txtOffset = (!D.Y_CH_SIZE)/2d*h_dat/h_pix*Rscl        ; Calculate the offset between the centre of the text and bottom
    txtHeight = txtOffset*2d  ; This is the approx height of the text in the legend.  
    ;
    txtStrt = !X.Crange[1] - strWidth
    polyEnd = txtStrt - txtWidth*2d
    polyStart = polyEnd - txtWidth*6d   ;   Make the polygon boxes 6 characters wide.
    legxCen = (polyStart + polyEnd)/2d
    ;
    legytop = !Y.crange[1] - txtHeight*2d
    legybot = legytop - txtHeight*1.5d        ; make the boxes 1.5 times the character height.
    ;
    legyCen = (legytop + legybot)/2d
    dLegY = txtHeight*2d
    ;
    lx1 = polyStart
    lx2 = polyEnd
    ly1 = legybot
    ly2 = legytop
    ;
    plots, [lx1,lx2], legyCen
    POLYFILL, [lx1,lx1,lx2,lx2,lx1], [ly1,ly2,ly2,ly1,ly1] - dLegY*1d, COLOR=8
    POLYFILL, [lx1,lx1,lx2,lx2,lx1], [ly1,ly2,ly2,ly1,ly1] - dLegY*2d, COLOR=10
    POLYFILL, [lx1,lx1,lx2,lx2,lx1], [ly1,ly2,ly2,ly1,ly1] - dLegY*3d, COLOR=26, /LINE_FILL, ORIENTATION=45, THICK=1.5, SPACING=0.05
    POLYFILL, [lx1,lx1,lx2,lx2,lx1], [ly1,ly2,ly2,ly1,ly1] - dLegY*3d, COLOR=26, /LINE_FILL, ORIENTATION=-45,THICK=1.5, SPACING=0.05
    LS_square, /FILL    ;   Switch the plot symbol to a filled square
    oploterror, legxCen - txtWidth*1.5d, legyCen - dLegY*4d, txtWidth*1.1d, txtHeight*0.6d, color=18, ERRcolor=18, PSYM=8, THICK=3, SYMSIZE=0.5
    oploterror, legxCen + txtWidth*1.5d, legyCen - dLegY*4d, txtWidth*1.1d, txtHeight*0.6d, color=18, ERRcolor=18, PSYM=8, THICK=3, SYMSIZE=0.5
    LS_circle, /FILL    ;   Switch the plot symbol to a filled circle
    oploterror, legxCen - txtWidth*1.5d, legyCen - dLegY*5d, txtWidth*1.1d, txtHeight*0.6d, color=13, ERRcolor=13, PSYM=8, THICK=3, SYMSIZE=0.5
    oploterror, legxCen + txtWidth*1.5d, legyCen - dLegY*5d, txtWidth*1.1d, txtHeight*0.6d, color=13, ERRcolor=13, PSYM=8, THICK=3, SYMSIZE=0.5    
    ;
    ; Calculate the plot height in device units to centre the legend text with the polyfill labels
    xyouts, txtStrt,legYCen - dLegY*0d - txtOffset, 'Best fit', COLOR=0, CHARSIZE = Rscl
    xyouts, txtStrt,legYCen - dLegY*1d - txtOffset, 'Maxima', COLOR=0, CHARSIZE=Rscl
    xyouts, txtStrt,legYCen - dLegY*2d - txtOffset, 'Boomerang', COLOR=0, CHARSIZE=Rscl	;	Print the legend labels
    xyouts, txtStrt,legYCen - dLegY*3d - txtOffset, 'DASI', COLOR=0, CHARSIZE=Rscl
    xyouts, txtStrt,legYCen - dLegY*4d - txtOffset, 'CBI: even', COLOR=0, CHARSIZE=Rscl
    xyouts, txtStrt,legYCen - dLegY*5d - txtOffset, 'CBI: odd', COLOR=0, CHARSIZE=Rscl
    ;
    ;  Plot the CBI-even data points, with error bars
    LS_square, /FILL    ;   Switch the plot symbol to a filled square
    oploterror, l5, cl5, (l5 - l5_1), cl5pm, /LOBAR, color=18, ERRCOLOR=18, PSYM=8, THICK=3, SYMSIZE=0.5, NOCLIP=0	
    oploterror, l5, cl5, (l5_2 - l5), cl5pm, /HIBAR, color=18, ERRCOLOR=18, PSYM=8, THICK=3, SYMSIZE=0.5, NOCLIP=0
    ;
    ;  Plot the CBI-odd data points, with error bars
    LS_circle, /FILL    ;   Switch the plot symbol to a filled square
    oploterror, l6, cl6, (l6 - l6_1), cl6pm, /LOBAR, color=13, ERRCOLOR=13, PSYM=8, THICK=3, SYMSIZE=0.5, NOCLIP=0
    oploterror, l6, cl6, (l6_2 - l6), cl6pm, /HIBAR, color=13, ERRCOLOR=13, PSYM=8, THICK=3, SYMSIZE=0.5, NOCLIP=0
    ;
    ; Re-plot the frame of the figure as some of the polygons have overshaded tick-marks, etc.  
    ; This does not re-plot the data curves, just graph border and text labels.
    HFI_plot, l1, cl1, /xs, xr=[0,3000], /ys, yr=[-1000,8000], BACKGROUND=255, COLOR=0, $
      XTITLE='xttl', YTITLE='yttl', XMARGIN=XMAR, YMARGIN=YMAR, /NOERASE, /NODATA, YTTL_DX = YTTL_DXs[jj], XTTL_DY=XTTL_DYs[jj]
    ;stop
    ;
    device, /close								;	Close the postscript device/file. 
    set_plot, Dname								;	Restore plotting to the display screen ('X' in linux, 'win' in windows)
    ;
    ;stop
    ;
    outname1 = 'LS_HFI_PIP_fig_sample_'+SZstr+'mm_v3'
    ; height and width in cm
    LS_latexify, FNAME+'.eps', ['yttl','xttl'], ['$\ell (\ell+1)C_\ell/2\pi$\quad $\left[\mu{\rm K}^2\right]$','$\ell$'], [1d,1d]*8d/11d, outname=outname1+'.eps', height=Ysz*2.54d, width=Xsz*2.54d, FDIR=FDIR;, /full
    ;LS_latexify, outname1+'.eps', 'xttl', '$\ell$', 1d, outname=outname2+'.eps', height=Ysz*2.54d - 0.2d, width=Xsz*2.54d - 0.2d
    ;
    IF Dname EQ 'X' THEN SPAWN, 'epstopdf '+FDIR+outname1+'.eps &'	;	If in linux, convert the postscript to a pdf file straight away.
    ;
    ;stop
    ;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;
  ENDFOR
  ;stop									;	This plot done, now repeat the same for the other sizes.
  ;
  ;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   single figures done, repeat for multi-plots
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;
  ;
  ;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   Do a small figure with two parts, vertically stacked.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;
  ;
  Xs = [X4,X5,X6]
  Ys = [Y4,Y5,Y6]
  ;
  FNTsz = 8
  ;
  FOR jj = 0, 2 DO BEGIN
    Xsz = Xs[jj]
    Ysz = Ys[jj]
    CASE jj OF
      0: XMAR = XMAR1
      1: XMAR = XMAR2
      2: XMAR = XMAR3
    ENDCASE
    ;
    CASE jj OF
      0: YMAR = YMAR1
      1: YMAR = YMAR2
      2: YMAR = YMAR3
    ENDCASE
    ;
    SZstr = SZstrs[jj]
    plotDIR = fDIR
    fName = 'LS_HFI_PIP_fig_sample_2x'+SZstr+'mm_v2'  
    set_plot, "ps"
    !P.font = 0
    device, FILENAME=plotDIR+fName+'.eps',xsize=Xsz,ysize=Ysz,/inches,/color, /ENCAPSULATED, /HELVETICA, FONT_size=FNTsz
    !P.CHARSIZE=1
    !X.CHARSIZE=1
    !Y.CHARSIZE=1
    !p.thick = 1
    !x.thick = !P.thick
    !y.thick = !P.thick
    !P.MULTI=[0,1,2]								;	Set up the figure with one column and two rows
    ;
    ;  copy-paste the single plot from above...
    ;
    HFI_plot, l1, cl1, /xs, xr=[0,3000], /ys, yr=[-1000,8000], BACKGROUND=255, COLOR=0, $
      XTITLE='xttl1', YTITLE='yttl1', XMARGIN=XMAR, YMARGIN=YMAR, YTTL_DX = YTTL_DXs[jj], XTTL_DY=XTTL_DYs[jj]
    plots, !X.crange, 0d          ; Plot a horizontal line at y=0
    ;
    ; Fill in the BOOMERANG data
    FOR ii = 0, 11 DO POLYFILL, [l4_1[ii],l4_1[ii],l4_2[ii],l4_2[ii],l4_1[ii]], [-1d*CL4m[ii],CL4p[ii],CL4p[ii],-1d*CL4m[ii],-1d*CL4m[ii]] + CL4[ii], NOCLIP=0, COLOR=8
    FOR ii = 0, 18 DO POLYFILL, [l2_1[ii],l2_1[ii],l2_2[ii],l2_2[ii],l2_1[ii]], [-1d,1d,1d,-1d,-1d]*CL2pm[ii] + CL2[ii], NOCLIP=0, COLOR=10
    ;
    ;    Fill in the DASI data.
    FOR ii = 0, 8 DO POLYFILL, [l3_1[ii],l3_1[ii],l3_2[ii],l3_2[ii],l3_1[ii]], [-1d,1d,1d,-1d,-1d]*CL3pm[ii] + CL3[ii], NOCLIP=0, COLOR=26, /LINE_FILL, ORIENTATION=45, THICK=1.5, SPACING=0.05
    FOR ii = 0, 8 DO POLYFILL, [l3_1[ii],l3_1[ii],l3_2[ii],l3_2[ii],l3_1[ii]], [-1d,1d,1d,-1d,-1d]*CL3pm[ii] + CL3[ii], NOCLIP=0, COLOR=26, /LINE_FILL, ORIENTATION=-45,THICK=1.5, SPACING=0.05
    ;
    oplot, l1, cl1, color=0         ; re-plot the black curve over the polyfill-ed regions.
    plots, !X.crange, 0d, color=0       ; the same for the line across the x-axis
    ;
    ;  Fill in the legend polyfill regions.
    ;  Put the legend in the upper right corner
    ;  IDL has a legend procedure which will do most of this, but it will not do the polyfill without modification, 
    ;  so I am doing it manually for demonstration purposes.  There is an easier way, but perhaps less clear for demonstration purposes. 
    ;
    ;  Boomerang is the longest word, so figure out where it wants to be to avoid going out of the plot region.  
    w_pix = !D.X_SIZE - ((!X.range)[0] + (!X.range)[1])*!D.X_CH_SIZE    ;   height of plot range in device coordinates
    w_dat = !X.crange[1] - !X.crange[0] ; height in data units.   ; height of plot-region in data units
    Rscl = 0.75                 ; Scale this text a bit smaller
    txtWidth = (!D.X_CH_SIZE)*w_dat/w_pix*Rscl        ; The approximate width of one character...with an extra 2 characters on either side
    strWidth = txtWidth*15d ; boomerang is only 9 characters, but I am putting in a few more as a safety factor. 
    ;
    h_pix = (!D.Y_SIZE - ((!Y.range)[0] + (!Y.range)[1])*!D.Y_CH_SIZE*2d)/2d    ;   height of plot range in device coordinates, there are two rows, hence /2d
    h_dat = (!Y.crange[1] - !Y.crange[0]) ; height in data units.   ; height of plot-region in data units
    Rscl = 0.75                 ; Scale this text a bit smaller
    txtOffset = (!D.Y_CH_SIZE)/2d*h_dat/h_pix*Rscl        ; Calculate the offset between the centre of the text and bottom
    txtHeight = txtOffset*2d  ; This is the approx height of the text in the legend.  
    ;
    ;
    txtStrt = !X.Crange[1] - strWidth
    polyEnd = txtStrt - txtWidth*2d
    polyStart = polyEnd - txtWidth*6d   ;   Make the polygon boxes 6 characters wide.
    legxCen = (polyStart + polyEnd)/2d
    ;
    legytop = !Y.crange[1] - txtHeight*2d
    legybot = legytop - txtHeight*1.5d        ; make the boxes 1.5 times the character height.
    ;
    legyCen = (legytop + legybot)/2d
    dLegY = txtHeight*2d
    ;
    lx1 = polyStart
    lx2 = polyEnd
    ly1 = legybot
    ly2 = legytop
    ;
    plots, [lx1,lx2], legyCen
    POLYFILL, [lx1,lx1,lx2,lx2,lx1], [ly1,ly2,ly2,ly1,ly1] - dLegY*1d, COLOR=8
    POLYFILL, [lx1,lx1,lx2,lx2,lx1], [ly1,ly2,ly2,ly1,ly1] - dLegY*2d, COLOR=10
    POLYFILL, [lx1,lx1,lx2,lx2,lx1], [ly1,ly2,ly2,ly1,ly1] - dLegY*3d, COLOR=26, /LINE_FILL, ORIENTATION=45, THICK=1.5, SPACING=0.05
    POLYFILL, [lx1,lx1,lx2,lx2,lx1], [ly1,ly2,ly2,ly1,ly1] - dLegY*3d, COLOR=26, /LINE_FILL, ORIENTATION=-45,THICK=1.5, SPACING=0.05
    LS_square, /FILL    ;   Switch the plot symbol to a filled square
    oploterror, legxCen - txtWidth*1.5d, legyCen - dLegY*4d, txtWidth*1.1d, txtHeight*0.6d, color=18, ERRcolor=18, PSYM=8, THICK=3, SYMSIZE=0.5
    oploterror, legxCen + txtWidth*1.5d, legyCen - dLegY*4d, txtWidth*1.1d, txtHeight*0.6d, color=18, ERRcolor=18, PSYM=8, THICK=3, SYMSIZE=0.5
    LS_circle, /FILL    ;   Switch the plot symbol to a filled circle
    oploterror, legxCen - txtWidth*1.5d, legyCen - dLegY*5d, txtWidth*1.1d, txtHeight*0.6d, color=13, ERRcolor=13, PSYM=8, THICK=3, SYMSIZE=0.5
    oploterror, legxCen + txtWidth*1.5d, legyCen - dLegY*5d, txtWidth*1.1d, txtHeight*0.6d, color=13, ERRcolor=13, PSYM=8, THICK=3, SYMSIZE=0.5    
    ;
    ; Calculate the plot height in device units to centre the legend text with the polyfill labels
    xyouts, txtStrt,legYCen - dLegY*0d - txtOffset, 'Best fit', COLOR=0, CHARSIZE = Rscl
    xyouts, txtStrt,legYCen - dLegY*1d - txtOffset, 'Maxima', COLOR=0, CHARSIZE=Rscl
    xyouts, txtStrt,legYCen - dLegY*2d - txtOffset, 'Boomerang', COLOR=0, CHARSIZE=Rscl ; Print the legend labels
    xyouts, txtStrt,legYCen - dLegY*3d - txtOffset, 'DASI', COLOR=0, CHARSIZE=Rscl
    xyouts, txtStrt,legYCen - dLegY*4d - txtOffset, 'CBI: even', COLOR=0, CHARSIZE=Rscl
    xyouts, txtStrt,legYCen - dLegY*5d - txtOffset, 'CBI: odd', COLOR=0, CHARSIZE=Rscl
    ;
    ;  Plot the CBI-even data points, with error bars
    LS_square, /FILL    ;   Switch the plot symbol to a filled square
    oploterror, l5, cl5, (l5 - l5_1), cl5pm, /LOBAR, color=18, ERRCOLOR=18, PSYM=8, THICK=3, SYMSIZE=0.5, NOCLIP=0  
    oploterror, l5, cl5, (l5_2 - l5), cl5pm, /HIBAR, color=18, ERRCOLOR=18, PSYM=8, THICK=3, SYMSIZE=0.5, NOCLIP=0
    ;
    ;  Plot the CBI-odd data points, with error bars
    LS_circle, /FILL    ;   Switch the plot symbol to a filled square
    oploterror, l6, cl6, (l6 - l6_1), cl6pm, /LOBAR, color=13, ERRCOLOR=13, PSYM=8, THICK=3, SYMSIZE=0.5, NOCLIP=0
    oploterror, l6, cl6, (l6_2 - l6), cl6pm, /HIBAR, color=13, ERRCOLOR=13, PSYM=8, THICK=3, SYMSIZE=0.5, NOCLIP=0
    ;
    ; Re-plot the frame of the figure as some of the polygons have overshaded tick-marks, etc.  
    ; This does not re-plot the data curves, just graph border and text labels.
    !P.MULTI=[0,1,2]
    HFI_plot, l1, cl1, /xs, xr=[0,3000], /ys, yr=[-1000,8000], BACKGROUND=255, COLOR=0, $
      XTITLE='xttl1', YTITLE='yttl1', XMARGIN=XMAR, YMARGIN=YMAR, /NOERASE, /NODATA, YTTL_DX = YTTL_DXs[jj], XTTL_DY=XTTL_DYs[jj]
    ;
    xyouts, !X.crange[0] + (!X.crange[1] - !X.crange[0])*0.015d,!Y.crange[0] + (!Y.crange[1] - !Y.crange[0])*0.95d - txtOffset, '(a)'
    ;
    ;
    ;
    ;   Done the top plot, now on to the bottom one...
    ;   Cut-paste the same figure from above, but do not need the legend this time...
    ;
    ;
    !P.MULTI=[1,1,2]
    HFI_plot, l1, cl1, /xs, xr=[0,3000], /ys, yr=[-1000,8000], BACKGROUND=255, COLOR=0, $
      XTITLE='xttl2', YTITLE='yttl2', XMARGIN=XMAR, YMARGIN=YMAR, YTTL_DX = YTTL_DXs[jj], XTTL_DY=XTTL_DYs[jj]
    plots, !X.crange, 0d          ; Plot a horizontal line at y=0
    ;
    ; Fill in the BOOMERANG data
    FOR ii = 0, 11 DO POLYFILL, [l4_1[ii],l4_1[ii],l4_2[ii],l4_2[ii],l4_1[ii]], [-1d*CL4m[ii],CL4p[ii],CL4p[ii],-1d*CL4m[ii],-1d*CL4m[ii]] + CL4[ii], NOCLIP=0, COLOR=8
    FOR ii = 0, 18 DO POLYFILL, [l2_1[ii],l2_1[ii],l2_2[ii],l2_2[ii],l2_1[ii]], [-1d,1d,1d,-1d,-1d]*CL2pm[ii] + CL2[ii], NOCLIP=0, COLOR=10
    ;
    ;    Fill in the DASI data.
    FOR ii = 0, 8 DO POLYFILL, [l3_1[ii],l3_1[ii],l3_2[ii],l3_2[ii],l3_1[ii]], [-1d,1d,1d,-1d,-1d]*CL3pm[ii] + CL3[ii], NOCLIP=0, COLOR=26, /LINE_FILL, ORIENTATION=45, THICK=1.5, SPACING=0.05
    FOR ii = 0, 8 DO POLYFILL, [l3_1[ii],l3_1[ii],l3_2[ii],l3_2[ii],l3_1[ii]], [-1d,1d,1d,-1d,-1d]*CL3pm[ii] + CL3[ii], NOCLIP=0, COLOR=26, /LINE_FILL, ORIENTATION=-45,THICK=1.5, SPACING=0.05
    ;
    oplot, l1, cl1, color=0         ; re-plot the black curve over the polyfill-ed regions.
    plots, !X.crange, 0d, color=0       ; the same for the line across the x-axis
    ;
    ;  Plot the CBI-even data points, with error bars
    LS_square, /FILL    ;   Switch the plot symbol to a filled square
    oploterror, l5, cl5, (l5 - l5_1), cl5pm, /LOBAR, color=18, ERRCOLOR=18, PSYM=8, THICK=3, SYMSIZE=0.5, NOCLIP=0  
    oploterror, l5, cl5, (l5_2 - l5), cl5pm, /HIBAR, color=18, ERRCOLOR=18, PSYM=8, THICK=3, SYMSIZE=0.5, NOCLIP=0
    ;
    ;  Plot the CBI-odd data points, with error bars
    LS_circle, /FILL    ;   Switch the plot symbol to a filled square
    oploterror, l6, cl6, (l6 - l6_1), cl6pm, /LOBAR, color=13, ERRCOLOR=13, PSYM=8, THICK=3, SYMSIZE=0.5, NOCLIP=0
    oploterror, l6, cl6, (l6_2 - l6), cl6pm, /HIBAR, color=13, ERRCOLOR=13, PSYM=8, THICK=3, SYMSIZE=0.5, NOCLIP=0
    ;
    ; Re-plot the frame of the figure as some of the polygons have overshaded tick-marks, etc.  
    ; This does not re-plot the data curves, just graph border and text labels.
    !P.MULTI=[1,1,2]
    HFI_plot, l1, cl1, /xs, xr=[0,3000], /ys, yr=[-1000,8000], BACKGROUND=255, COLOR=0, $
      XTITLE='xttl2', YTITLE='yttl2', XMARGIN=XMAR, YMARGIN=YMAR, /NOERASE, /NODATA, YTTL_DX = YTTL_DXs[jj], XTTL_DY=XTTL_DYs[jj]
    ;
    xyouts, !X.crange[0] + (!X.crange[1] - !X.crange[0])*0.015d,!Y.crange[0] + (!Y.crange[1] - !Y.crange[0])*0.95d - txtOffset, '(b)'
    ;
    !P.MULTI=0
    device, /close
    set_plot, Dname
    ;
    outname1 = 'LS_HFI_PIP_fig_sample_2x'+SZstr+'mm_v3'
    ; height and width in cm
    LS_latexify, FNAME+'.eps', ['yttl1','xttl1','yttl2','xttl2'], $
      ['\vspace{0pt}$\ell (\ell+1)C_\ell/2\pi$\quad $\left[\mu{\rm K}^2\right]$','$\ell$',$
      '\vspace{0pt}$\ell (\ell+1)C_\ell/2\pi$\quad $\left[\mu{\rm K}^2\right]$','$\ell$'], [1d,1d,1d,1d]*8d/11d, outname=outname1+'.eps', height=Ysz*2.54d, width=Xsz*2.54d, FDIR=FDIR;, /full
    ;
    IF Dname EQ 'X' THEN SPAWN, 'epstopdf '+FDIR+outname1+'.eps &'  ; If in linux, convert the postscript to a pdf file straight away.
    ;
    ;stop
    ;
  ENDFOR
  ;
  ;stop
  ;
  ;  
  Print, 'Figures saved to directory: ', plotDIR
  ;stop									;	Done generating the figures, go check them out.
  ;
  ;
  ;stop
  ;
END