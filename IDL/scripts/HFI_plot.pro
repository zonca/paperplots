;+
;PURPOSE
; to rotate the orientation of y-axis plot tick-mark-labels for Planck/HFI figures by 90 deg. ccw.
; This should work for output to a file (i.e. .eps) or to the screen (e.g. for tvread to .png)
; returns  the same result as the plot command, with the y-axis labels rotated.
;SYNTAX
; HFI_plot, x, y, ..., [DECMODX=], [DECMODY=], [Y_DX=], [Y_DY=], [X_DX=], [X_DY=]
;INPUTS
; x, y, similar to the plot command
; _extra: extra keywords for plot()
;KEYWORDS
; DECMODX - set this to an integer to increase (or decrease) the number of digits in the x-axis tick mark labels (default=0).
; DECMODY - set this to an integer to increase (or decrease) the number of digits in the y-axis tick mark labels (default=0).
; Y_DX    - set this to manually shift the y-axis tick labels horizontally, in data coordinates.
; Y_DY    - set this to manually shift the y-axis tick labels vertically, in data coordinates.
; X_DX    - set this to manually shift the x-axis tick labels horizontally, in data coordinates.
; X_DY    - set this to manually shift the x-axis tick labels vertically, in data coordinates.
;
;Written by L.D.Spencer, Dec. 2012 / Jan. 2013
;
;
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
;
;-
PRO HFI_plot, x, y, _EXTRA=_EXTRA, DECMODX=DECMODX, DECMODY=DECMODY, $
  Y_DX=Y_DX, Y_DY=Y_DY, X_DX=X_DX, X_DY=X_DY, $
  YTTL_DX=YTTL_DX, YTTL_DY=YTTL_DY, XTTL_DX=XTTL_DX, XTTL_DY=XTTL_DY
;
; This script is to plot to a virtual device to get the y-axis labels, and allow them to be re-plotted with a 90^o anti-clockwise rotation.
; ;
; 
IF N_ELEMENTS(DECMODX) EQ 0 THEN DECMODX = 0
IF N_ELEMENTS(DECMODY) EQ 0 THEN DECMODY = 0
IF N_ELEMENTS(Y_DX) EQ 0 THEN Y_DX = 0
IF N_ELEMENTS(Y_DY) EQ 0 THEN Y_DY = 0
IF N_ELEMENTS(X_DX) EQ 0 THEN X_DX = 0
IF N_ELEMENTS(X_DY) EQ 0 THEN X_DY = 0
IF N_ELEMENTS(YTTL_DX) EQ 0 THEN YTTL_DX = 0
IF N_ELEMENTS(YTTL_DY) EQ 0 THEN YTTL_DY = 0
IF N_ELEMENTS(XTTL_DX) EQ 0 THEN XTTL_DX = 0
IF N_ELEMENTS(XTTL_DY) EQ 0 THEN XTTL_DY = 0
; stop ;   
IF N_ELEMENTS(y) EQ 0 THEN y = DINDGEN(5) + 1d
Ny = N_ELEMENTS(y)
IF N_ELEMENTS(x) EQ 0 THEN x = dindgen(Ny)
;y = (x + 3);/1d-1
;
YLG = TAG_EXIST(_EXTRA, 'YLOG')
XLG = TAG_EXIST(_EXTRA, 'XLOG')
CHSZ_Tag = TAG_EXIST(_EXTRA, 'CHARSIZE')
IF CHSZ_TAG THEN BEGIN
  CHSZ = CHARSIZE
  CHSZ_ = CHARSIZE 
ENDIF ELSE BEGIN
  CHSZ = 0
  CHSZ_ = 1d
ENDELSE
;
YS_ = (TAG_EXIST(_EXTRA, 'YS')); OR (TAG_EXIST(_EXTRA, 'YSTYLE')))
XS_ = (TAG_EXIST(_EXTRA, 'XS'))
;
YR_ = (TAG_EXIST(_EXTRA, 'YR'))
XR_ = (TAG_EXIST(_EXTRA, 'XR'))
;
IF YS_ THEN BEGIN
  YS_bin = STRING(_EXTRA.YS, FORMAT='(B0)')
  YS_4 = STRMID('000000'+YS_bin, 2, 1, /REVERSE_OFFSET)
  IF YS_4 EQ '1' THEN YST = _EXTRA.YS ELSE YST = _EXTRA.YS + 4
ENDIF ELSE BEGIN
  YST = 4
ENDELSE
;
;
IF XS_ THEN BEGIN
  XS_bin = STRING(_EXTRA.XS, FORMAT='(B0)')
  XS_4 = STRMID('000000'+XS_bin, 2, 1, /REVERSE_OFFSET)
  IF XS_4 EQ '1' THEN XST = _EXTRA.XS ELSE XST = _EXTRA.XS + 4
ENDIF ELSE BEGIN
  XST = 4
ENDELSE
;
IF YR_ THEN YRN = _EXTRA.YR ELSE YR=[0d,0d]
IF XR_ THEN XRN = _EXTRA.XR ELSE XR=[0d,0d]
;
;stop
;
plot, x, y, YLOG=YLG, XLOG=XLG, /NODATA, /NOERASE, YS=YST, XS=XST, YTICK_GET = YT, CHARSIZE=CHSZ, XTICK_GET=XT, XR=XRN, YR=YRN
; The above will plot no axes, and no lines.  It just gets the ytick values.
; , YS=4, XS=4;, YTICKFORMAT='(A1)' ; just to get Y tick values...
;
NumY = N_ELEMENTS(YT)
YTICKNAME = STRARR(NumY) + ' '
Ytick_Xval = !X.crange[0] - (!X.crange[1] - !X.crange[0])/100d
Ytick_Xval = DBLARR(NumY) + Ytick_Xval
Ytick_Yval = YT
;
NumX = N_ELEMENTS(XT)
XTICKNAME = STRARR(NumX) + ' '
Xtick_Yval = !Y.crange[0] - (!Y.crange[1] - !Y.crange[0])/100d
Xtick_Yval = DBLARR(NumX) + Xtick_Yval
Xtick_Xval = XT
;
IF YLG THEN BEGIN
  ;  DO some things differently as it is a log plot
  YTICKNAME = STRARR(NumY) + '  '
  YT_exp = FIX(ALOG10(YT))
  YT_str = '10!U'+STRTRIM(STRING(YT_exp),2)+'!N'
ENDIF ELSE BEGIN
  ;
  dY = (!Y.crange[1] - !Y.crange[0])/DOUBLE(NumY - 1d)
  ;
  ; FIXME, include a check for /YLOG being set!!
  NumDec = CEIL(ALOG10(dY)*(-1d)) + DECMODY ;+ 1
  ;If NumDec LT 0 THEN NumDec = 0
  LS_DecRound, YT, DEC=NumDEC, STR=YT_STR, SCI=SCI, NOSCI=NOSCI, RNDSCI=RNDSCI, WASSCI=WASSCI, ALLOWSCI=ALLOWSCI ; FIXME, need to check all of this. 
  ;
ENDELSE
;
IF XLG THEN BEGIN
  ;  DO some things differently as it is a log plot
  XT_exp = FIX(ALOG10(XT))
  XT_str = '10!U'+STRTRIM(STRING(XT_exp),2)+'!N'
ENDIF ELSE BEGIN
  ;
  dX = (!X.crange[1] - !X.crange[0])/DOUBLE(NumX - 1d)
  ;
  ; FIXME, include a check for /YLOG being set!!
  NumDec = CEIL(ALOG10(dX)*(-1d)) + DECMODX ;+ 1
  ;If NumDec LT 0 THEN NumDec = 0
  LS_DecRound, XT, DEC=NumDEC, STR=XT_STR, SCI=SCI, NOSCI=NOSCI, RNDSCI=RNDSCI, WASSCI=WASSCI, ALLOWSCI=ALLOWSCI ; FIXME, need to check all of this. 
  ;
ENDELSE
;
_EXTRA_orig = _EXTRA
IF TAG_EXIST(_EXTRA,'XTITLE') THEN BEGIN
  remove_tags, _EXTRA, 'XTITLE', _EXTRA_
  _EXTRA = _EXTRA_
ENDIF 
;
IF TAG_EXIST(_EXTRA,'YTITLE') THEN BEGIN
  remove_tags, _EXTRA, 'YTITLE', _EXTRA_
  _EXTRA = _EXTRA_
ENDIF 
; 
plot, x, y, _EXTRA=_EXTRA, YTICKNAME=YTICKNAME, XTICKNAME=XTICKNAME, YTITLE=' ', XTITLE=' '
;
_EXTRA = _EXTRA_orig
;
; FIXME: I need to check the width of the character string to be sure that it doesn't overlap with neighboring data points.
;        If so then take every other data point.
CH_YSZ = !D.Y_CH_SIZE ; MAX([!D.X_CH_SIZE,!D.Y_CH_SIZE])*CHSZ_
CH_XSZ = !D.X_CH_SIZE
;
IF TAG_EXIST(_EXTRA,'YCHARSIZE') THEN CH_YSZ = CH_YSZ*YCHARSIZE
IF TAG_EXIST(_EXTRA,'XCHARSIZE') THEN CH_XSZ = CH_XSZ*XCHARSIZE
;
CH_lenY = STRLEN(YT_STR)
CH_lenX = STRLEN(XT_STR) 
;
ITERY = 1
ITERX = 1
IF YLG THEN BEGIN 
  ;
  Dev_to_DataY = (ALOG10(!Y.CRANGE[1]) - ALOG10(!Y.CRANGE[0]))/DOUBLE(!P.CLIP[3] - !P.CLIP[1])
  ;
  CH_strtsY = ALOG10(Ytick_Yval) - CH_lenY*CH_YSZ*Dev_to_DataY/2d
  CH_strtsY = 10d^CH_strtsY
  ;
  CH_endsY  = ALOG10(Ytick_Yval) + CH_lenY*CH_YSZ*Dev_to_DataY/2d  
  CH_endsY = 10d^CH_endsY
  ;
ENDIF ELSE BEGIN
  ;
  Dev_to_DataY = (!Y.CRANGE[1] - !Y.CRANGE[0])/DOUBLE(!P.CLIP[3] - !P.CLIP[1])
  ;
  CH_strtsY = Ytick_Yval - CH_lenY*CH_YSZ*Dev_to_DataY/2d
  ;
  CH_endsY  = Ytick_Yval + CH_lenY*CH_YSZ*Dev_to_DataY/2d  
  ;
ENDELSE
;
IF XLG THEN BEGIN 
  ;
  Dev_to_DataX = (ALOG10(!X.CRANGE[1]) - ALOG10(!X.CRANGE[0]))/DOUBLE(!P.CLIP[2] - !P.CLIP[0])
  ;
  CH_strtsX = ALOG10(Xtick_Xval) - CH_lenX*CH_XSZ*Dev_to_DataX/2d
  CH_strtsX = 10d^CH_strtsX
  ;
  CH_endsX  = ALOG10(Xtick_Xval) + CH_lenX*CH_XSZ*Dev_to_DataX/2d  
  CH_endsX  = 10d^CH_endsX
  ; 
ENDIF ELSE BEGIN
  ;
  Dev_to_DataX = (!X.CRANGE[1] - !X.CRANGE[0])/DOUBLE(!P.CLIP[2] - !P.CLIP[0])
  ;
  CH_strtsX = Xtick_Xval - CH_lenX*CH_XSZ*Dev_to_DataX/2d
  ;
  CH_endsX  = Xtick_Xval + CH_lenX*CH_XSZ*Dev_to_DataX/2d  
  ;
ENDELSE
;
;
OlapY = CH_strtsY[1:NumY - 1] - CH_endsY[0:NumY - 2]
OlapX = CH_strtsX[1:NumX - 1] - CH_endsX[0:NumX - 2]
;
NegOlapY = WHERE(OlapY LE 0d, NnegOlapY)
NegOlapX = WHERE(OlapX LE 0d, NnegOlapX)
;
IF NnegOlapY GT 0 THEN ITERY = 2 ELSE ITERY = 1
IF NnegOlapX GT 0 THEN ITERX = 2 ELSE ITERX = 1
;
IF CH_endsY[NumY - 1] GT !Y.crange[1] THEN YT_str[NumY - 1] = ' '
IF CH_endsX[NumX - 1] GT !X.crange[1] THEN XT_str[NumX - 1] = ' '
;
;stop
;
Xtick_Yval = Xtick_Yval - CH_YSZ*Dev_to_DataY
;
xyouts, (Ytick_Xval + Y_DX)[0:*:ITERY], (Ytick_Yval + Y_DY)[0:*:ITERY], YT_STR[0:*:ITERY], ALIGNMENT=0.5, ORIENTATION=90d, /DATA, _EXTRA=_EXTRA
xyouts, (Xtick_Xval + X_DX)[0:*:ITERX], (Xtick_Yval + X_DY)[0:*:ITERX], XT_STR[0:*:ITERX], ALIGNMENT=0.5, ORIENTATION=0d, /DATA, _EXTRA=_EXTRA
;
; Now position the axis labels, including the Y[/X]TTL_DX[/Y] KEYWORDS.
; First determine where I think they should be anyways.
; 
;w_pix = !D.X_SIZE - ((!X.range)[0] + (!X.range)[1])*!D.X_CH_SIZE    ;   height of plot range in device coordinates
;w_dat = !X.crange[1] - !X.crange[0] ; height in data units.   ; height of plot-region in data units
;txtWidth = (!D.X_CH_SIZE)*w_dat/w_pix        ; The approximate width of one character...with an extra 2 characters on either side
;;
;h_pix = !D.Y_SIZE - ((!Y.range)[0] + (!Y.range)[1])*!D.Y_CH_SIZE    ;   height of plot range in device coordinates
;h_dat = !Y.crange[1] - !Y.crange[0] ; height in data units.   ; height of plot-region in data units
;txtHeight = (!D.Y_CH_SIZE)*h_dat/h_pix        ; Calculate the average character height.
;
;  Now figure out where xttl and yttl should be, I want yttl centred vertically, and offset 2.5 character spaces from the axis.
;  I want xttl centred horizontally, and vertically offset 2.5 character spaces.
;  or perhaps three spaces...
;  
IF YLG THEN yttl_y = 10d^((!Y.crange[1] + !Y.crange[0])/2d) ELSE yttl_y = ((!Y.crange[1] + !Y.crange[0])/2d) 
IF XLG THEN yttl_x = 10d^(!X.CRANGE[0]) - CH_XSZ*Dev_to_DataX*3.5d ELSE yttl_x = (!X.CRANGE[0]) - CH_XSZ*Dev_to_DataX*3d
;
IF XLG THEN xttl_x = 10d^((!X.crange[1] + !X.crange[0])/2d) ELSE xttl_x = ((!X.crange[1] + !X.crange[0])/2d)
IF YLG THEN xttl_y = 10d^(!Y.CRANGE[0]) - CH_YSZ*Dev_to_DataY*3.5d ELSE xttl_y = (!Y.CRANGE[0]) - CH_YSZ*Dev_to_DataY*2.75d
;
;stop
;
IF TAG_EXIST(_EXTRA,'XTITLE') THEN xyouts, xttl_x + xttl_dx, xttl_y + xttl_dy, ALIGNMENT=0.5, /DATA, _EXTRA.XTITLE, _EXTRA=_EXTRA
IF TAG_EXIST(_EXTRA,'YTITLE') THEN xyouts, yttl_x + yttl_dx, yttl_y + yttl_dy, ALIGNMENT=0.5, ORIENTATION=90d, /DATA, _EXTRA.YTITLE, _EXTRA=_EXTRA
;
;stop
;
;CLP = !P.CLIP
;Xsz = !D.X_SIZE
;Ysz = !D.Y_SIZE
;;
;Y1 = CLP[1]
;Y2 = CLP[3]
;Y_ = DOUBLE(Y2 - Y1)/DOUBLE(Ysz)
;;
;X1 = CLP[0]
;X2 = CLP[2]
;X_ = DOUBLE(X2 - X1)/DOUBLE(Xsz)
;;
;OrthScl = DOUBLE(Y2 - Y1)/DOUBLE(X2 - X1)
;;
;T3D, /RESET, ROTATE=[0,0,90]
;T3D, TRANSLATE=[DOUBLE(Y2)/DOUBLE(Ysz),(-1d)*DOUBLE(X1)/DOUBLE(Xsz),0d]
;T3D, SCALE = [1d,Y_/X_,1d]
;T3D, SCALE = [OrthScl*Y_/X_,1d,1d]
;T3D, TRANSLATE=[DOUBLE(X1)/DOUBLE(Xsz),0d,0d]
;T3D, TRANSLATE=[0d,DOUBLE(Y1)/DOUBLE(Ysz),0d]
;;
;axis, XAXIS=1, XRANGE=[!Y.crange], color=2, XS=1, /T3D, XTICKV=YT, CHARSIZE=1d/OrthScl*Y_/X_/1.15d
;;xyouts, Ytick_Xval, Ytick_Yval, STRTRIM(STRING(FIX(YT)),2), ALIGNMENT=0.5, ORIENTATION=90d, /DATA
;;
;stop
;;
END
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;+
; NAME:
; LS_circle
;
; PURPOSE:
; This procedure defines a circle to be used as the user-defined plot symbol (PSYM=8) 
;
; CATEGORY:
; Utilities
;
; CALLING SEQUENCE:
; LS_square
;
; INPUTS:
; SZ - the size of the symbol (default=1)
; TK - the symbol thickness (default=1)
; FILL - whether or not to fill the symbol (default = empty)
;
; KEYWORD PARAMETERS:
; see INPUTS above
;
; OUTPUTS:
; This procedure sets the IDL PSYM[=8] user-defined plot symbol.
;
; RESTRICTIONS:
;
;
; MODIFICATION HISTORY:
;   Written by: L.D.Spencer 2013/Jan/04
;-
PRO LS_circle, SZ=SZ, TK=TK, FILL=FILL
  ;
  ; This routine allows a circle to be plotted using the IDL user-selected PSYM (PSYM=8)
  ; It can be filled or open.
  ;
  IF ~KEYWORD_SET(SZ) THEN SZ=1.  ; default to one
  IF ~KEYWORD_SET(TK) THEN TK=1.  ; default to one
  IF N_ELEMENTS(FILL) EQ 0 THEN FILL = 0 
  ;
  A = FINDGEN(17) * (!PI*2./16.)
  X = COS(A)
  Y = SIN(A)
  SZ = SZ*1.15
  ;
  X = X*SZ
  Y = Y*SZ
  ;
  USERSYM, X, Y, THICK=TK, FILL=FILL
  ;
END
;
;+
; NAME:
; LS_square
;
; PURPOSE:
; This procedure defines a square to be used as the user-defined plot symbol (PSYM=8) 
;
; CATEGORY:
; Utilities
;
; CALLING SEQUENCE:
; LS_square
;
; INPUTS:
; SZ - the size of the symbol (default=1)
; TK - the symbol thickness (default=1)
; FILL - whether or not to fill the square (default = empty)
;
; KEYWORD PARAMETERS:
; see INPUTS above
;
; OUTPUTS:
; This procedure sets the IDL PSYM[=8] user-defined plot symbol.
;
; RESTRICTIONS:
;
;
; MODIFICATION HISTORY:
;   Written by: L.D.Spencer 2013/Jan/04
;-
PRO LS_square, SZ=SZ, TK=TK, FILL=FILL
  ;
  ; This routine allows a circle to be plotted using the IDL user-selected PSYM (PSYM=8)
  ; It can be filled or open.
  ;
  IF ~KEYWORD_SET(SZ) THEN SZ=1.  ; default to one
  IF ~KEYWORD_SET(TK) THEN TK=1.  ; default to one
  IF N_ELEMENTS(FILL) EQ 0 THEN FILL = 0 
  ;
  X = [-.5,-.5,.5,.5,-.5]
  Y = [-.5,.5,.5,-.5,-.5]
  SZ = SZ*2.
  ;
  X = X*SZ
  Y = Y*SZ
  ;
  USERSYM, X, Y, THICK=TK, FILL=FILL
  ;
END
;
;
;+
; NAME:
; COLORINDEX
;
; PURPOSE:
; This function returns the color index or RGB triplet for the given color name
;
; CATEGORY:
; Utilities
;
; CALLING SEQUENCE:
; Result = COLORINDEX(Name)
; (i.e., NO KEYWORDS)
;
; INPUTS:
; Name: The name(s) of the color(s) to return. This name must exist in the array of colors.
;
; KEYWORD PARAMETERS:
; ALL:  Set this keyword to return the full table of values
; RGB:  Set this keyword to return an RGB triple instead of an index
; COUNT: set this keyword to a named variable to return the total number of entries in the table.
;
; OUTPUTS:
; This function returns an index into the color table or an RGB value.
;
; RESTRICTIONS:
;
;
; MODIFICATION HISTORY:
;   Written by: Brad Gom 2000
; July 5 2006 (BGG) - no longer uses external text file. Colors are defined in-line.
; Copyright Brad G. Gom 2006
;-
function colorindex,name,rgb=rgb,count=count,decomposed=decomposed,all=all
  COMPILE_OPT HIDDEN

  names=['black','brown','red','orange','yellow','green','blue','violet','grey',$
    'white','dgrey','cyan','dbrown','lblue','skyblue','navyblue','iceblue',$
    'forrestgreen','lgreen','olive','lyellow','dyellow','brickred','hotpink',$
    'lpink','lviolet','lpurple','dpurple','turquoise','khaki','dorange','neon',$
    'lime','tan','coral','teal','indigo','ivory','gold']

  rgb_table=[[0,0,0],$
    [220,140,127],$
    [255,0,0],$
    [255,127,0],$
    [255,255,0],$
    [0,255,0],$
    [0,0,255],$
    [255,0,255],$
    [198,198,198],$
    [255,255,255],$
    [100,100,100],$
    [0,255,255],$
    [102,51,51],$
    [0,0,200],$
    [0,204,255],$
    [0,51,153],$
    [153,255,255],$
    [0,102,51],$
    [51,204,102],$
    [153,153,51],$
    [255,255,204],$
    [255,204,0],$
    [204,51,0],$
    [255,51,153],$
    [255,204,204],$
    [255,153,255],$
    [204,102,255],$
    [51,0,102],$
    [102,255,204],$
    [153,153,102],$
    [255,102,51],$
    [255,0,102],$
    [50,205,50],$
    [210,180,140],$
    [255,127,80],$
    [0,128,128],$
    [75,0,130],$
    [255,255,240],$
    [255,215,0] ]

  count=n_elements(names)

  n=n_elements(name)

  if n eq 0 then ind=0 else ind=intarr(n)

  for i=0,n-1 do begin
    ind[i]=where(names eq name[i], count)
    if count eq 0 then begin
      message,'Color '+name[i]+' does not exist!',/cont
      ind[i]=0
      endif
    endfor

  if n eq 1 then ret=ind[0] else ret=ind

  if keyword_Set(all) then ind=indgen(count)  ;return all values

  if keyword_set(rgb) then $ ;return a RGB triple
    return,rgb_table[*,ind]
  if keyword_set(decomposed) then begin ;return a 24 bit color value
    rgb_table=long(rgb_table)
    return,rgb_table[0,ind]+ishft(rgb_table[1,ind],8)+ishft(rgb_table[2,ind],16)
    endif
  return,ret ;return a color index
end
;+
; NAME:
; COLOURS
;
; PURPOSE:
; This procedure defines the default color table for IDL. The first 10 colors
; follow the resistor color code:
; 0 = black; 1 = brown; 2 = red; 3 = orange; 4 = yellow
;  5 = green; 6 = blue; 7 = violet; 8 = grey; 9 = white
; The rest of the colors are defined in the color table defined in colorindex.pro.
;
; CATEGORY:
; Utilities
;
; CALLING SEQUENCE:
; COLOURS
;
; INPUTS:
; None.
;
; KEYWORD PARAMETERS:
; None.
;
; OUTPUTS:
; This procedure sets the IDL color table
;
; RESTRICTIONS:
;
;
; MODIFICATION HISTORY:
;   Written by: GRD 28.11.92
; Modified Oct 2005  BGG - now uses the programrootdir() function to find the color table file.
; July 2006 (BGG) - now uses colorindex.pro to generate the color table, instead of a text file.
; 
; Copyright Brad. G. Gom 2006
;-
PRO COLOURS
  COMPILE_OPT HIDDEN
  ;
  ;basic resistor colors
  ; RED =   [0, 220,  255,  255,  255,  0,    0,    255,  160,  255]
  ; GREEN = [0, 140,  0,    127,  255,  255,  0,    0,    160,  255]
  ; BLUE =  [0, 127,  0,    0,    0,    0,    255,  255,  160,  255]

  names=['black','brown','red','orange','yellow','green','blue','violet',$
        'grey','white','dgrey','cyan','dbrown','lblue','skyblue','navyblue',$
        'iceblue','forrestgreen','lgreen','olive','lyellow','dyellow','brickred',$
        'hotpink','lpink','lviolet','lpurple','dpurple','turquoise','khaki',$
        'dorange','neon']

  ;  To get a color
  ; index for a named color, use ind=colorindex('blue') or plot,data,color=colorindex('khaki')
  ; The resistor color codes still occupy the 0-9 indices.

  rgb=colorindex(names,/rgb)

  TVLCT,rgb[0,*], rgb[1,*], rgb[2,*]

  if (!d.name eq 'WIN') || (!d.name eq 'X') || (!d.name eq 'Z') then begin
    device,decomposed=0 ;This makes IDL interpret color values as indices into a color table.
    !P.COLOR=colorindex('white')  ;set default color to white (9)
    endif else begin
    ;the other devices do not support decomposed colors.
    ;!P.COLOR=colorindex('white',/decomposed) ;set default color to white in 24-bit color
    !p.color=0  ;postscript plots color 0 as black.
    endelse

  message,'Loaded color table.',/info
  RETURN
END
;
;
;
;
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;
;
;
;
;+
; NAME:
; LS_DecRound
;
; PURPOSE:
; This procedure rounds a number to a user-defined number of decimal places and outputs this number as a string (and other optional formats).
; It also rounds to a defined amount of significant digits in scientific notation with the /SCI keyword. 
;
; CATEGORY:
; Utilities
;
; CALLING SEQUENCE:
; LS_DecRound, Num, DEC=3, STR=strNum, etc. 
;
; INPUTS:
;   Num - the number (or array of numbers) to be rounded and converted to a string.
;   Dec - the number of decimal places / digits to include, DEC=2 gets  123.45, DEC=-2 gets 100, etc.
;   STR - the string output, appropriately rounded / truncated.
;
; KEYWORD PARAMETERS:
;   SCI - display the number in scientific notation
;   NOSCI - force the exclusion of scientific notation presentation
;   RNDSCI - round in scientific notation (i.e. DEC = sigFigs rather than decimal number), but present as other wise requested (NOSCI keyword)
;   WASSCI - output to indicate if the number would nomrally be presented in scientific notation in a `print, num' statement.
;   ALLOWSCI - allow scientific notation if it is the default, but do not force it.
;
; OUTPUTS:
; This procedure sets the STR keyword with a string (or array of strings) containing the desired number of decimal places / significant figures.
;
; RESTRICTIONS:
;
;
; MODIFICATION HISTORY:
;   Written by: L.D.Spencer 2011/2012
;   
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
;   
;-
PRO LS_DecRound, Num, DEC=DEC, DBL=DBL, FLT=FLT, STR=STR, SCI=SCI, NOSCI=NOSCI, RNDSCI=RNDSCI, WASSCI=WASSCI, ALLOWSCI=ALLOWSCI
;
; This procedure takes a number (float or double) and rounds it to a certain number of decimal places, returning a float, double, or string.
;
; The string will be truncated at the specified number of decimal places to exclude any trailing zeroes.
;
; KEYWORDS:
;           DEC   -   Number of decimal places to round to
;           DBL   -   Output variable for rounded double
;           FLT   -   Output variable for rounded float
;           STR   -   Output variable for rounded string 
;           
;           SCI   -   Set to 1 to force scientific notation output
;           NOSCI -   Force output to be in standard notation (i.e. not scientific)   ;;;;    SCI takes precedence over NOSCI
;           ALLOWSCI- allow scientific notation if the string conversion does this, but do not externally force it.  
;           RNDSCI  - Round the number in scientific notation rather than as a decimal.  
;           WASSCI-   Indicates if it would have been scientific notation anyways
;
IF N_ELEMENTS(NUM) EQ 0 THEN NUM = !C_ ; 1.23456789
IF N_ELEMENTS(DEC) EQ 0 THEN DEC = 1  ; default is 1 decimal place
IF N_ELEMENTS(SCI) EQ 0 THEN SCI = 0
IF N_ELEMENTS(NOSCI) EQ 0 THEN NOSCI = 0
IF N_ELEMENTS(ALLOWSCI) EQ 0 THEN ALLOWSCI = 1  ; default is to allow scientific as it comes up.
;IF N_ELEMENTS(RNDSCI) EQ 0 THEN RNDSCI = 0 ; If the number comes up as sci normally, then choose to round based on this if this keyword is not set.
IF N_ELEMENTS(RNDSCI) EQ 0 THEN RNDIND = 1 ELSE RNDIND = 0
IF N_ELEMENTS(RNDSCI) EQ 0 THEN RNDSCI = 0
;
IF ((SCI EQ 1) AND (NOSCI EQ 1)) THEN NOSCI = 0   ; I should generate an error here also, but I'll leave it for later. 
;
DecNum = 10d^(DOUBLE(DEC))
;
Nels = N_ELEMENTS(Num)
Nds = N_ELEMENTS(DEC)
;
DBL = DBLARR(Nels)
FLT = FLTARR(Nels)
STR = STRARR(Nels)
;
WasSCI = INTARR(Nels)
;
FOR i = 0, Nels - 1 DO BEGIN
  ;
  IF Nds EQ 1 THEN BEGIN
    DEC_ = DEC
    DecNum_ = DecNum 
  ENDIF ELSE BEGIN
    DEC_ = DEC[i]
    DecNum_ = DecNum[i]
  ENDELSE
  ;
  Numi = Num[i]
  stri = STRING(Numi)
  ;
  ;   First round the number as required.   ; RNDIND means that RNDSCI was not set globally, so set it individually...
  IF RNDIND EQ 1 THEN BEGIN ; check if the number comes up to be scientific by itself, then set the RNDSCI keyword for each number in the loop.
    HasExp = STRPOS(stri,'e')
    IF HasExp LT 0 THEN RNDSCI = 0 ELSE RNDSCI = 1
  ENDIF
  IF RNDSCI EQ 1 THEN BEGIN
    ; get the base number and exponents separated
    strsci = STRING(Numi, FORMAT='(e)')
    Lsci = STRLEN(strsci)
    SciExp = STRPOS(strsci,'e')
    eSTR = STRMID(strsci,(SciExp + 1),(Lsci - (SciExp + 1)))
    eVal = DOUBLE(eSTR)
    baseVal = DOUBLE(STRMID(strsci,0,SciExp - 1))
    DBLsci = DOUBLE(ROUND(baseVal*DecNum_))/DecNum_*(10d^eVal)
    RNDi = DBLsci
    ;FLTsci = FLOAT(DBLsci)
    ;stringsci = STRMID(STRTRIM(STRING(DOUBLE(ROUND(baseVal*DecNum))/DecNum),2),0,2 + DEC)+'e'+eSTR
  ENDIF ELSE BEGIN
    ; check to see if the number can be rounded normally (i.e. is it within the limits of a 64 bit integer?)
    NotTooBig = DOUBLE(Numi*DecNum_) LT DOUBLE(9223372036854775807ll)
    NotTooSmall=DOUBLE(Numi*DecNum_) GT DOUBLE(-9223372036854775808ll)
    IF ((NotTooBig) AND (NotTooSmall)) THEN BEGIN
      DBLi = DOUBLE(ROUND(Numi*DecNum_, /L64))/DecNum_
      RNDi = DBLi
    ENDIF ELSE BEGIN  ; will do sci round anyways
      ; get the base number and exponents separated
      strsci = STRING(Numi, FORMAT='(e)')
      Lsci = STRLEN(strsci)
      SciExp = STRPOS(strsci,'e')
      eSTR = STRMID(strsci,(SciExp + 1),(Lsci - (SciExp + 1)))
      eVal = DOUBLE(eSTR)
      baseVal = DOUBLE(STRMID(strsci,0,SciExp - 1))
      DBLsci = DOUBLE(ROUND(baseVal*DecNum_))/DecNum_*(10d^eVal)
      RNDi = DBLsci
    ENDELSE
  ENDELSE
  ; Have now rounded the number as requested by the user (natural vs. scientific rounding)
  ; Now must format the output as requested (mostly a problem for the string)
  ;
  IF RNDi LT 0d THEN NEGstr = '-' ELSE NEGstr = ''
  IF RNDi LT 0d THEN NEGspc = 1 ELSE NEGspc = 0
  ;
  IF DEC_ LT 8 THEN strsci = STRTRIM(STRING(FLOAT(RNDi), FORMAT='(e)'),2) ELSE strsci = STRTRIM(STRING(RNDi, FORMAT='(e)'),2)
  Lsci = STRLEN(strsci)
  SciExp = STRPOS(strsci,'e')
  eSTR = STRMID(strsci,(SciExp + 1),(Lsci - (SciExp + 1)))
  eVal = DOUBLE(eSTR)
  baseVal = STRMID(strsci,0,SciExp - 1)
  IF RNDSCI EQ 1 THEN BEGIN ; round to DEC number of decimals in the scientific notation, easy...    
    stringi = STRMID(baseVal,0,2 + NEGspc + DEC_)+'e'+eSTR
  ENDIF ELSE BEGIN  ; display the sci notation of the number rounded in normal format, i.e. figure out how many zeroes there should be.
    DECsci = FIX(eVal + DEC_)
    IF DECsci LT 8 THEN strsci = STRTRIM(STRING(FLOAT(RNDi), FORMAT='(e)'),2) ELSE strsci = STRTRIM(STRING(RNDi, FORMAT='(e)'),2)
    Lsci = STRLEN(strsci)
    SciExp = STRPOS(strsci,'e')
    eSTR = STRMID(strsci,(SciExp + 1),(Lsci - (SciExp + 1)))
    eVal = DOUBLE(eSTR)
    baseVal = STRMID(strsci,0,SciExp - 1)
    stringi = STRMID(baseVal,0,2 + NEGspc + DECsci)+'e'+eSTR
  ENDELSE
  STRsci = stringi
  ;
  RNDlen = NEGspc + 1 + FIX(eVAL) + 1 + DEC_
  IF eVAL LT 0d THEN RNDlen = NEGspc + 1 + FIX(ABS(eVAL)) - 1 + 1 + DEC_
  IF eVAL GT 0 THEN BEGIN
    IF RNDlen LT 8 THEN BEGIN 
      strnormi=STRTRIM(STRING(FLOAT(RNDi)),2)
    ENDIF ELSE BEGIN
      IF RNDlen LT 10 THEN BEGIN
        strnormi=STRTRIM(STRING((RNDi)),2)
      ENDIF ELSE BEGIN
        strnormi=STRTRIM(STRING(RNDi, FORMAT='(d64)'),2)  ; this may need fixed still, but it should get me out of my current fix...
      ENDELSE
    ENDELSE
  ENDIF ELSE BEGIN
    IF RNDlen LT 8 THEN BEGIN 
      strnormi=STRTRIM(STRING(FLOAT(RNDi), FORMAT='(d)'),2)
    ENDIF ELSE BEGIN
      IF RNDlen LT 10 THEN BEGIN
        strnormi=STRTRIM(STRING((RNDi), FORMAT='(d)'),2)
      ENDIF ELSE BEGIN
        strnormi=STRTRIM(STRING(RNDi, FORMAT='(d64)'),2)  ; this may need fixed still, but it should get me out of my current fix...
      ENDELSE
    ENDELSE
  ENDELSE
  Strdot = STRPOS(strnormi,'.')
  IF RNDSCI EQ 1 THEN BEGIN ; the number was rounded in sci notation, but is to be displayed in normal format, determine how to do this.
      ; get the sci exponent and DEC number to determine the correct display
      NumDEC = FIX(eVal - DEC_) 
      IF NumDEC GE 0 THEN strnormi = STRMID(strnormi,0,Strdot - 1) ELSE strnormi = STRMID(strnormi,0, strdot + 1 + ABS(NumDEC))
  ENDIF ELSE BEGIN  ; the number was rounded in decimal format, and must now be displayed in decimal format, no problem.
    NumDEC = DEC_ ; FIX(-DEC_)
    IF NumDEC LE 0 THEN strnormi = STRMID(strnormi,0,Strdot) ELSE strnormi = STRMID(strnormi,0,Strdot + DEC_ + 1)
    ;strnormi = STRMID(strnormi,0,Strdot + DEC_ + 1)
    ;stop
  ENDELSE
  ; 
  stri = STRING(RNDi)
  HasExp = STRPOS(stri,'e')
  DBLi = RNDi
  FLTi = FLOAT(RNDi)
  IF HasExp GT 0 THEN BEGIN ; The number is in sci format
    WASSCI[i] = 1
    STRi = STRsci
;    Lsci = STRLEN(stri)
;    SciExp = STRPOS(stri,'e')
;    eSTR = STRMID(stri,(SciExp + 1),(Lsci - (SciExp + 1)))
;    eVal = DOUBLE(eSTR)
;    baseVal = STRMID(stri,0,SciExp - 1)
;    IF RNDSCI EQ 1 THEN stringi = STRMID(baseVal,0,2 + DEC_)+'e'+eSTR $
;    ELSE stringi = STRMID(baseVal,0,2 + DEC_)+'e'+eSTR  ; FIXME this should be displayed differently I think as it rounds the string to sci decimals rather than natural ones, i.e. the two roundings may be different.
  ENDIF ELSE BEGIN  ; The number is not in sci format
    WasSCI[i] = 0
    STRi = STRnormi
;    stringi = STRTRIM(stri,2)
;    ; get rid of end zeroes
;    Strdot = STRPOS(stringi,'.')
;    IF RNDSCI EQ 0 THEN stringi = STRMID(stringi,0,Strdot + DEC_ + 1) $
;    ELSE stringi = STRMID(stringi,0,Strdot + DEC_ + 1) ; FIXME If I did scientific rounding then this needs to be fixed.
  ENDELSE
  ;STRi = stringi
  ;
  ;   Now select the string to include
  IF NOSCI EQ 1 THEN STRi = strnormi
  IF SCI EQ 1 THEN STRi = strsci
  ;
  ;stop
  DBL[i] = DBLi
  FLT[i] = FLTi
  STR[i] = STRi
ENDFOR
 
END
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;
;
;
;
;
;+
;PURPOSE
; to use PSfrag to do latex syntax correctly
;SYNTAX
; LS_latexify, filename, tags, tex, scale
;INPUTS
; filename: name of the eps file
; tags: name of placeholder text string in the .eps file
; tex: the tex you want to replace it
; scale: the scale you want the latex to be [default to 1] (i.e. latex 11pt font) 
;NOTES:
; requires the following latex packages properly installed:
;   geometry, graphicx, psfrag
; requires you to use font=0 in generating the IDL .eps figures.
;
; Follows from discussion by Saurav
;   http://www.astrobetter.com/idl-psfrag/
; Writes and deletes files called LS_HFIfig_temp.*
; The LS version is based on code produced by R. da Silva, UCSC, 2011.
; 
;EXAMPLE
;
;   SET_PLOT, 'PS'
;   !P.FONT=0
;   DEVICE, FILENAME='figure.eps'
;   plot,findgen(10),xtitle='xtitle'
;   DEVICE, /CLOSE
;
;   LS_latexify, 'figure.eps', 'xttl', 'position   [$\mu$m]', FDIR='/some/directory/where/your/file/is/'
; 
; latexify.pro written by R. da Silva, UCSC, 1-4-11 
; LS_latexify written by L.D. Spencer (based on latexify.pro mentioned in previous line).
; 
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
; 
; 
;-
pro LS_latexify, filename, tag, tex, scale, outname=outname, $
      height=height, width=width, full=full, FDIR=FDIR, POSN=POSN, PSPOSN=PSPOSN
  ;
  ;IF N_ELEMENTS(POSN) EQ 0 THEN
  ;
  if ~keyword_set(scale) then scale=replicate(1, n_elements(tag))
  scale = strcompress(string(scale), /remove_all)
  ;
  if keyword_set(outname) EQ 0 then begin
    outname='LS_HFIfig2.eps'
    noname=1
  endif
  openw, lun, FDIR+'LS_HFIfig_temp.tex', /get_lun
  printf, lun, '\documentclass{article}'
  printf, lun, '\usepackage{geometry, graphicx, psfrag}'
  printf, lun, '\renewcommand{\familydefault}{\sfdefault}'
  printf, lun,'\usepackage{helvet}'
  printf, lun,'\pagestyle{empty}'
  printf, lun,'\geometry{paperwidth='+strcompress(string(width+0.1), /remove_all)+'cm,'+$
                      'paperheight='+strcompress(string(height+0.1), /remove_all)+'cm,margin=0pt}'
  printf, lun,'\begin{document}'
  for i=0, n_elements(tag)-1 do $
        printf, lun,'\psfrag{'+tag[i]+'}[cc][cc]['+scale[i]+']{'+tex[i]+'}'
  printf, lun,'\includegraphics[width='+$
       strcompress(string(width), /remove_all)+'cm,height='+strcompress(string(height), /remove_all)+'cm]{'+filename+'}'
  printf, lun,'\end{document}'
  close, lun
  free_lun, lun
  ;
  spawn, 'cd '+FDIR+' && latex '+'LS_HFIfig_temp.tex', msg0
  spawn, 'cd '+FDIR+' && dvips -o '+'LS_HFIfig_temp.ps '+'LS_HFIfig_temp.dvi', msg1
  spawn, 'cd '+FDIR+' && ps2epsi '+'LS_HFIfig_temp.ps '+'LS_HFIfig_temp.epsi', msg2
  ;
  ; Check to see if the file *.eps file already exists, if so rename the old one so the perl script will work.
  ; 
  flCheck = file_exist(FDIR+outname)
  IF flCheck THEN spawn, 'cd '+FDIR+' && mv -f '+outname+' old_'+outname, msg3
  ;
  spawn, " && perl -ne 'print unless /^%%BeginPreview/../^%%EndPreview/' <"+FDIR+'LS_HFIfig_temp.epsi > '+FDIR+outname, msg4
  ;
  ; Clean up the temporary files now.
  ;
  if keyword_set(noname) then begin
    spawn, 'mv LS_HFIfig2.eps '+filename
    outname=filename
  endif
  spawn, 'rm -f '+FDIR+'LS_HFIfig_temp*'
  ;
end
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
;
; NAME:
;    REMOVE_TAGS
;       
; PURPOSE:
;    remove the specified tags from input structure
;
; CALLING SEQUENCE:
;    remove_tags, oldstruct, tagnames, newstruct
;
; INPUTS: 
;    oldstruct: the original structure
;    tagnames: the names of tags to be removed (can be an array)
;
; OPTIONAL INPUTS:
;    NONE.
;
; KEYWORD PARAMETERS:
;    NONE.
;       
; OUTPUTS: 
;    newstruct: the new structure without tags.
;
; OPTIONAL OUTPUTS:
;    NONE
;
; CALLED ROUTINES:
;    
; 
; PROCEDURE: 
;    
; 
;
; REVISION HISTORY:
;    ????? Judith Racusin
;    25-OCT-2000 Modified to handle arbitrary tag types. Also error 
;          handling. Erin Scott Sheldon
;       
;                                      
;-                                       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PRO remove_tags, struct, tagnames, newstruct

  IF n_params() EQ 0 THEN BEGIN 
      print,'Syntax - remove_tags, oldstruct, tagnames, newstruct'
      print
      print,'Use doc_library,"remove_tags"  for more help.'  
      return
  END

  ;; Figure out which tags get removed

  tags=tag_names(struct)
  n=n_elements(tags)
  tagnames=strupcase(tagnames)
  nt=n_elements(tagnames)
  IF nt EQ 1 THEN BEGIN
      t=where(tags NE tagnames[0],nw) 
      IF nw EQ n THEN BEGIN
          print,'-----------------------------------------------------'
          message,'Tag did not match, structure unchanged',/inf
          print,'-----------------------------------------------------'
          newstruct = struct
          return
      ENDIF 
  ENDIF ELSE BEGIN 
      match,tags,tagnames,m
      IF m[0] EQ -1 THEN BEGIN
          print,'-------------------------------------------------'
          message,'No tags matched, structure unchanged',/inf
          print,'-------------------------------------------------'
          newstruct=struct
          return
      ENDIF 
      nm=n_elements(m)
      IF nm EQ n THEN BEGIN 
          print,'-------------------------------------------------------------'
          message,'This would remove all tags! structure unchanged',/inf
          print,'-------------------------------------------------------------'
          newstruct=struct
          return
      ENDIF 
      t=lindgen(n)
      remove, m, t
  ENDELSE 
      
  ;; create new structure
  tags=tags[t]
  n=n_elements(tags)

  newstruct=create_struct(tags[0],struct[0].(t[0]))
  
  FOR i=1L, n-1 DO newstruct = create_struct(temporary(newstruct), $
                                             tags[i], struct[0].(t[i]) )

  newstruct=replicate( temporary(newstruct), n_elements(struct) )
  struct_assign,struct,newstruct

  return
END