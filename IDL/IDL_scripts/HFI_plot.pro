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
  YTTL_DX=YTTL_DX, YTTL_DY=YTTL_DY, XTTL_DX=XTTL_DX, XTTL_DY=XTTL_DY, $
  DEBUG=DEBUG, XTICK_GET=XT, YTICK_GET=YT, XLGNAT=XLGNAT, YLGNAT=YLGNAT, FIXMINUS=FIXMINUS
;
; This script is to plot to a virtual device to get the y-axis labels, and allow them to be re-plotted with a 90^o anti-clockwise rotation.
; ;
; 
;  If no keywords are passed then it doesn't like it, so make a structure to use as a placeholder
IF N_ELEMENTS(_EXTRA) EQ 0 THEN _EXTRA = {HFI:0}
;
IF N_ELEMENTS(y) EQ 0 THEN BEGIN  ;  I think that x was forgotten, generate a new one.
  x_ = DINDGEN(N_ELEMENTS(x))
  y = x
  x = x_
ENDIF
IF N_ELEMENTS(DECMODX) EQ 0 THEN DECMODX = 0
IF N_ELEMENTS(DECMODY) EQ 0 THEN DECMODY = 0
IF N_ELEMENTS(Y_DX) EQ 0 THEN Y_DX = 0d
IF N_ELEMENTS(Y_DY) EQ 0 THEN Y_DY = 0d
IF N_ELEMENTS(X_DX) EQ 0 THEN X_DX = 0d
IF N_ELEMENTS(X_DY) EQ 0 THEN X_DY = 0d
IF N_ELEMENTS(YTTL_DX) EQ 0 THEN YTTL_DX = 0d
IF N_ELEMENTS(YTTL_DY) EQ 0 THEN YTTL_DY = 0d
IF N_ELEMENTS(XTTL_DX) EQ 0 THEN XTTL_DX = 0d
IF N_ELEMENTS(XTTL_DY) EQ 0 THEN XTTL_DY = 0d
IF N_ELEMENTS(XLGNAT) EQ 0 THEN XLGNAT = 0
IF N_ELEMENTS(YLGNAT) EQ 0 THEN YLGNAT = 0
; stop ;   
IF N_ELEMENTS(y) EQ 0 THEN y = DINDGEN(5) + 1d
Ny = N_ELEMENTS(y)
IF N_ELEMENTS(x) EQ 0 THEN x = dindgen(Ny)
;y = (x + 3);/1d-1
;
; check for some alternate names to my keywords:  I shortened XRANGE to XR, XSTYLE to XS, etc.
_EXTRA_orig = _EXTRA
;
IF TAG_EXIST(_EXTRA, 'XRANGE') EQ 1 THEN BEGIN
  _E = create_struct(_EXTRA, 'XR', _EXTRA.XRANGE)
  _EXTRA = temporary(_E)
ENDIF
IF TAG_EXIST(_EXTRA, 'XSTYLE') EQ 1 THEN BEGIN
  _E = create_struct(_EXTRA, 'XS', _EXTRA.XSTYLE)
  _EXTRA = temporary(_E)
ENDIF
IF TAG_EXIST(_EXTRA, 'XSTY') EQ 1 THEN BEGIN
  _E = create_struct(_EXTRA, 'XS', _EXTRA.XSTY)
  _EXTRA = temporary(_E)
ENDIF
IF TAG_EXIST(_EXTRA, 'XST') EQ 1 THEN BEGIN
  _E = create_struct(_EXTRA, 'XS', _EXTRA.XST)
  _EXTRA = temporary(_E)
ENDIF
;
;
IF TAG_EXIST(_EXTRA, 'YRANGE') EQ 1 THEN BEGIN
  _E = create_struct(_EXTRA, 'YR', _EXTRA.YRANGE)
  _EXTRA = temporary(_E)
ENDIF
IF TAG_EXIST(_EXTRA, 'YSTYLE') EQ 1 THEN BEGIN
  _E = create_struct(_EXTRA, 'YS', _EXTRA.YSTYLE)
  _EXTRA = temporary(_E)
ENDIF
IF TAG_EXIST(_EXTRA, 'YSTY') EQ 1 THEN BEGIN
  _E = create_struct(_EXTRA, 'YS', _EXTRA.YSTY)
  _EXTRA = temporary(_E)
ENDIF
IF TAG_EXIST(_EXTRA, 'YST') EQ 1 THEN BEGIN
  _E = create_struct(_EXTRA, 'YS', _EXTRA.YST)
  _EXTRA = temporary(_E)
ENDIF
;
;IF TAG_EXIST(_EXTRA, 'YRANGE') EQ 1 THEN _EXTRA = create_struct(temporary(_EXTRA), 'YR', _EXTRA.YRANGE)
;IF TAG_EXIST(_EXTRA, 'YSTYLE') EQ 1 THEN _EXTRA = create_struct(temporary(_EXTRA), 'YS', _EXTRA.YSTYLE)
;IF TAG_EXIST(_EXTRA, 'YSTY')   EQ 1 THEN _EXTRA = create_struct(temporary(_EXTRA), 'YS', _EXTRA.YSTY)
;IF TAG_EXIST(_EXTRA, 'YST')    EQ 1 THEN _EXTRA = create_struct(temporary(_EXTRA), 'YS', _EXTRA.YST)
;
IF TAG_EXIST(_EXTRA, 'YLOG') EQ 1 THEN YLG = _EXTRA.YLOG ELSE YLG = 0
IF TAG_EXIST(_EXTRA, 'XLOG') EQ 1 THEN XLG = _EXTRA.XLOG ELSE XLG = 0
CHSZ_Tag = TAG_EXIST(_EXTRA, 'CHARSIZE')
IF CHSZ_TAG THEN BEGIN
  CHSZ = _EXTRA.CHARSIZE
  CHSZ_ = _EXTRA.CHARSIZE 
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
XTICKINT = (TAG_EXIST(_EXTRA,'XTICKINTERVAL'))
YTICKINT = (TAG_EXIST(_EXTRA,'YTICKINTERVAL'))
;
XTCKS = (TAG_EXIST(_EXTRA,'XTICKS'))
YTCKS = (TAG_EXIST(_EXTRA,'YTICKS'))
;
XTKV  = (TAG_EXIST(_EXTRA, 'XTICKV'))
YTKV  = (TAG_EXIST(_EXTRA, 'YTICKV'))
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
IF XTICKINT THEN XTINT = _EXTRA.XTICKINTERVAL ELSE XTINT = 0d
IF YTICKINT THEN YTINT = _EXTRA.YTICKINTERVAL ELSE YTINT = 0d
;
IF YTCKS THEN YTS = _EXTRA.YTICKS ELSE YTS=0
IF XTCKS THEN XTS = _EXTRA.XTICKS ELSE XTS=0
;
IF YTKV THEN YVS = _EXTRA.YTICKV ELSE YVS=0
IF XTKV THEN XVS = _EXTRA.XTICKV ELSE XVS=0
;
;stop
;
plot, x, y, YLOG=YLG, XLOG=XLG, /NODATA, /NOERASE, YS=YST, XS=XST, YTICK_GET = YT, CHARSIZE=CHSZ, XTICK_GET=XT, XR=XRN, YR=YRN, $
  XTICKINTERVAL=XTINT, YTICKINTERVAL=YTINT, XTICKS=XTS, YTICKS=YTS, XTICKV=XVS, YTICKV=YVS
; The above will plot no axes, and no lines.  It just gets the ytick values.
; , YS=4, XS=4;, YTICKFORMAT='(A1)' ; just to get Y tick values...
;
NumY = N_ELEMENTS(YT)
YTICKNAME = STRARR(NumY) + ' '
Ytick_Xval = !X.crange[0] ;  - (!X.crange[1] - !X.crange[0])/100d
Ytick_Xval = DBLARR(NumY) + Ytick_Xval
Ytick_Yval = YT
;
NumX = N_ELEMENTS(XT)
XTICKNAME = STRARR(NumX) + ' '
Xtick_Yval = !Y.crange[0] ;  - (!Y.crange[1] - !Y.crange[0])/100d
Xtick_Yval = DBLARR(NumX) + Xtick_Yval
Xtick_Xval = XT
;
IF YLG THEN BEGIN
  ;  DO some things differently as it is a log plot
  YTICKNAME = STRARR(NumY) + '  '
  YT_exp = ALOG10(YT)
  YT_exp_DEC = 0 + DECMODY
  LS_DecRound, YT_exp, DEC=YT_exp_DEC, STR=YT_exp_str_
;  YT_str = '10!U'+STRTRIM(STRING(YT_exp),2)+'!N'
  YT_str = '10!U'+YT_exp_str_+'!N'
  IF KEYWORD_SET(FIXMINUS) THEN BEGIN
    FOR Minus_i = 0, N_ELEMENTS(YT_STR)-1 DO BEGIN
      YT_STR[Minus_i] = StrJoin(StrSplit(YT_STR[Minus_i], '-', /Regex, /Extract, /Preserve_Null), '!M-!X!U')
    ENDFOR
  ENDIF
  IF KEYWORD_SET(YLGNAT) THEN BEGIN
    ;  Do not want the axis labels in 10^n notation.
    YT_EXP_DEC = CEIL((-1d)*YT_EXP) + DECMODY
    LS_DecRound, YT, DEC=YT_EXP_DEC, STR=YT_STR, SCI=SCI, NOSCI=NOSCI, RNDSCI=RNDSCI, WASSCI=WASSCI, ALLOWSCI=ALLOWSCI
    ;
    IF KEYWORD_SET(FIXMINUS) THEN BEGIN
      FOR Minus_i = 0, N_ELEMENTS(YT_STR)-1 DO BEGIN
        YT_STR[Minus_i] = StrJoin(StrSplit(YT_STR[Minus_i], '-', /Regex, /Extract, /Preserve_Null), '!M-!X')
      ENDFOR
    ENDIF
    NegYTicks = WHERE(YT LT 0d, Nyneg)
    IF Nyneg GT 0 THEN YT_STR[NegYTicks] = YT_STR[NegYTicks]+' '
  ENDIF
ENDIF ELSE BEGIN
  ; 
  dY = (!Y.crange[1] - !Y.crange[0])/DOUBLE(NumY - 1d)
  ;
  ; FIXME, include a check for /YLOG being set!!
  NumDec = CEIL(ALOG10(dY)*(-1d)) + DECMODY ;+ 1
  ;If NumDec LT 0 THEN NumDec = 0
  LS_DecRound, YT, DEC=NumDEC, STR=YT_STR, SCI=SCI, NOSCI=NOSCI, RNDSCI=RNDSCI, WASSCI=WASSCI, ALLOWSCI=ALLOWSCI ; FIXME, need to check all of this. 
  ;
  IF KEYWORD_SET(FIXMINUS) THEN BEGIN
    FOR Minus_i = 0, N_ELEMENTS(YT_STR)-1 DO BEGIN
      YT_STR[Minus_i] = StrJoin(StrSplit(YT_STR[Minus_i], '-', /Regex, /Extract, /Preserve_Null), '!M-!X')
    ENDFOR
  ENDIF
  NegYTicks = WHERE(YT LT 0d, Nyneg)
  IF Nyneg GT 0 THEN YT_STR[NegYTicks] = YT_STR[NegYTicks]+' '
  ;
ENDELSE
;
IF XLG THEN BEGIN
  ;  DO some things differently as it is a log plot
  ;XT_exp = FIX(ALOG10(XT))
  XT_exp = ALOG10(XT)
  ;XT_str = '10!U'+STRTRIM(STRING(XT_exp),2)+'!N'
  XT_exp_DEC = 0 + DECMODX
  LS_DecRound, XT_exp, DEC=XT_exp_DEC, STR=XT_exp_str_
  XT_str = '10!U'+XT_exp_str_+'!N'
  IF KEYWORD_SET(FIXMINUS) THEN BEGIN
    FOR Minus_i = 0, N_ELEMENTS(XT_STR)-1 DO BEGIN
      XT_STR[Minus_i] = StrJoin(StrSplit(XT_STR[Minus_i], '-', /Regex, /Extract, /Preserve_Null), '!M-!X!U')
    ENDFOR
  ENDIF
  IF KEYWORD_SET(XLGNAT) THEN BEGIN
    ;  Do not want the axis labels in 10^n notation.
    XT_EXP_DEC = CEIL((-1d)*XT_EXP) + DECMODX
    LS_DecRound, XT, DEC=XT_EXP_DEC, STR=XT_STR, SCI=SCI, NOSCI=NOSCI, RNDSCI=RNDSCI, WASSCI=WASSCI, ALLOWSCI=ALLOWSCI
    IF KEYWORD_SET(FIXMINUS) THEN BEGIN
      FOR Minus_i = 0, N_ELEMENTS(XT_STR)-1 DO BEGIN
        XT_STR[Minus_i] = StrJoin(StrSplit(XT_STR[Minus_i], '-', /Regex, /Extract, /Preserve_Null), '!M-!X')
      ENDFOR
    ENDIF
    NegXTicks = WHERE(XT LT 0d, Nxneg)
    IF Nxneg GT 0 THEN XT_STR[NegXTicks] = XT_STR[NegXTicks]+' '
  ENDIF
ENDIF ELSE BEGIN
  ;
  dX = (!X.crange[1] - !X.crange[0])/DOUBLE(NumX - 1d)
  ;
  ; FIXME, include a check for /YLOG being set!!
  NumDec = CEIL(ALOG10(dX)*(-1d)) + DECMODX ;+ 1
  ;If NumDec LT 0 THEN NumDec = 0
  LS_DecRound, XT, DEC=NumDEC, STR=XT_STR, SCI=SCI, NOSCI=NOSCI, RNDSCI=RNDSCI, WASSCI=WASSCI, ALLOWSCI=ALLOWSCI ; FIXME, need to check all of this. 
  ;
  IF KEYWORD_SET(FIXMINUS) THEN BEGIN
    FOR Minus_i = 0, N_ELEMENTS(XT_STR)-1 DO BEGIN
      XT_STR[Minus_i] = StrJoin(StrSplit(XT_STR[Minus_i], '-', /Regex, /Extract, /Preserve_Null), '!M-!X')
    ENDFOR
  ENDIF
  NegXTicks = WHERE(XT LT 0d, Nxneg)
  IF Nxneg GT 0 THEN XT_STR[NegXTicks] = XT_STR[NegXTicks]+' '
  ;
ENDELSE
;stop
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
IF TAG_EXIST(_EXTRA,'YTICKNAME') THEN BEGIN
  remove_tags, _EXTRA, 'YTICKNAME', _EXTRA_
  _EXTRA = _EXTRA_
ENDIF 
;
plot, x, y, _EXTRA=_EXTRA, YTICKNAME=YTICKNAME, XTICKNAME=XTICKNAME, YTITLE=' ', XTITLE=' ' ;  
;
_EXTRA = _EXTRA_orig
;
; FIXME: I need to check the width of the character string to be sure that it doesn't overlap with neighboring data points.
;        If so then take every other data point.
CH_YSZ = !D.Y_CH_SIZE*CHSZ_ ; MAX([!D.X_CH_SIZE,!D.Y_CH_SIZE])*CHSZ_
CH_XSZ = !D.X_CH_SIZE*CHSZ_
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
  ;CH_lenY = (CH_lenY - 4d - 2d)*0.62d + 2d
  IF KEYWORD_SET(YLGNAT) THEN CH_lenY = CH_lenY ELSE CH_lenY = (CH_lenY - 4d - 2d)*0.62d + 2d
  ;
  ;Dev_to_DataY = (ALOG10(!Y.CRANGE[1]) - ALOG10(!Y.CRANGE[0]))/DOUBLE(!P.CLIP[3] - !P.CLIP[1])
  Dev_to_DataY = (!Y.CRANGE[1] - !Y.CRANGE[0])/DOUBLE(!P.CLIP[3] - !P.CLIP[1])
  ;
  CH_strtsY = ALOG10(Ytick_Yval) - (CH_lenY)*CH_YSZ*Dev_to_DataY/2d
  ;CH_strtsY = ALOG10(Ytick_Yval - (CH_lenY)*CH_YSZ*Dev_to_DataY/2d)
  CH_strtsY = 10d^CH_strtsY + Y_DY
  ;
  CH_endsY  = ALOG10(Ytick_Yval) + (CH_lenY)*CH_YSZ*Dev_to_DataY/2d  
  ;CH_endsY  = ALOG10(Ytick_Yval + (CH_lenY)*CH_YSZ*Dev_to_DataY/2d)
  CH_endsY = 10d^CH_endsY + Y_DY
  ;
ENDIF ELSE BEGIN
  ;
  Dev_to_DataY = (!Y.CRANGE[1] - !Y.CRANGE[0])/DOUBLE(!P.CLIP[3] - !P.CLIP[1])
  ;
  CH_strtsY = Ytick_Yval - CH_lenY*CH_YSZ*Dev_to_DataY/2d + Y_DY
  ;
  CH_endsY  = Ytick_Yval + CH_lenY*CH_YSZ*Dev_to_DataY/2d + Y_DY 
  ;
ENDELSE
;
IF XLG THEN BEGIN 
  ;
  ;CH_lenX = (CH_lenX - 4d - 2d)*0.62d + 2d
  IF KEYWORD_SET(XLGNAT) THEN CH_lenX = CH_lenX ELSE CH_lenX = (CH_lenX - 4d - 2d)*0.62d + 2d
  ;
  ;Dev_to_DataX = (ALOG10(!X.CRANGE[1]) - ALOG10(!X.CRANGE[0]))/DOUBLE(!P.CLIP[2] - !P.CLIP[0])
  Dev_to_DataX = (!X.CRANGE[1] - !X.CRANGE[0])/DOUBLE(!P.CLIP[2] - !P.CLIP[0])
  ;
  CH_strtsX = ALOG10(Xtick_Xval) - CH_lenX*CH_XSZ*Dev_to_DataX/2d
  ;CH_strtsX = ALOG10(Xtick_Xval - CH_lenX*CH_XSZ*Dev_to_DataX/2d)
  CH_strtsX = 10d^CH_strtsX + X_DX
  ;
  CH_endsX  = ALOG10(Xtick_Xval) + CH_lenX*CH_XSZ*Dev_to_DataX/2d  
  ;CH_endsX  = ALOG10(Xtick_Xval + CH_lenX*CH_XSZ*Dev_to_DataX/2d)
  CH_endsX  = 10d^CH_endsX + X_DX
  ; 
ENDIF ELSE BEGIN
  ;
  Dev_to_DataX = (!X.CRANGE[1] - !X.CRANGE[0])/DOUBLE(!P.CLIP[2] - !P.CLIP[0])
  ;
  CH_strtsX = Xtick_Xval - CH_lenX*CH_XSZ*Dev_to_DataX/2d + X_DX
  ;
  CH_endsX  = Xtick_Xval + CH_lenX*CH_XSZ*Dev_to_DataX/2d + X_DX 
  ;
ENDELSE
;
;
IF NumY GT 1 THEN OlapY = CH_strtsY[1:NumY - 1] - CH_endsY[0:NumY - 2] ELSE OlapY = 1d
If NumX GT 1 THEN OlapX = CH_strtsX[1:NumX - 1] - CH_endsX[0:NumX - 2] ELSE OlapX = 1d
;
NegOlapY = WHERE(OlapY LE 0d, NnegOlapY)
NegOlapX = WHERE(OlapX LE 0d, NnegOlapX)
;
IF NnegOlapY GT 0 THEN ITERY = 2 ELSE ITERY = 1
IF NnegOlapX GT 0 THEN ITERX = 2 ELSE ITERX = 1
;
;  Check that the last number for either axis does not go outside of the plot range.  
;
IF TAG_EXIST(_EXTRA,'XMARGIN') THEN XMAR = _EXTRA.XMARGIN ELSE XMAR = !X.MARGIN
IF TAG_EXIST(_EXTRA,'YMARGIN') THEN YMAR = _EXTRA.YMARGIN ELSE YMAR = !Y.MARGIN
;
YBOX = !Y.CRANGE + [-1d,1d]*DOUBLE(!D.Y_CH_SIZE)*DOUBLE(YMAR)/DOUBLE(!D.Y_SIZE)/!Y.S[1]
XBOX = !X.CRANGE + [-1d,1d]*DOUBLE(!D.X_CH_SIZE)*DOUBLE(XMAR)/DOUBLE(!D.X_SIZE)/!X.S[1]
;
IF YLG THEN YBOX = 10d^(!Y.CRANGE + [-1d,1d]*DOUBLE(!D.Y_CH_SIZE)*DOUBLE(YMAR)/DOUBLE(!D.Y_SIZE)/!Y.S[1])
IF XLG THEN XBOX = 10d^(!X.CRANGE + [-1d,1d]*DOUBLE(!D.X_CH_SIZE)*DOUBLE(XMAR)/DOUBLE(!D.X_SIZE)/!X.S[1])
;
IF CH_strtsY[0] LT YBOX[0] THEN YT_str[0] = ' '
IF CH_strtsY[0] LT YBOX[0] THEN CH_strtsY_ = CH_strtsY[1:*] ELSE CH_strtsY_ = CH_strtsY
IF CH_strtsY[0] LT YBOX[0] THEN CH_endsY_  = CH_endsY[1:*] ELSE CH_endsY_ = CH_endsY
IF CH_strtsY[0] LT YBOX[0] THEN Yfirst = 1 ELSE Yfirst = 0
;
IF CH_strtsX[0] LT XBOX[0] THEN XT_str[0] = ' '
IF CH_strtsX[0] LT XBOX[0] THEN CH_strtsX_ = CH_strtsX[1:*] ELSE CH_strtsX_ = CH_strtsX
IF CH_strtsX[0] LT XBOX[0] THEN CH_endsX_  = CH_endsX[1:*] ELSE CH_endsX_ = CH_endsX
IF CH_strtsX[0] LT XBOX[0] THEN Xfirst = 1 ELSE Xfirst = 0
;
IF CH_endsY[NumY - 1] GT YBOX[1] THEN YT_str[NumY - 1] = ' '
IF CH_endsY[NumY - 1] GT YBOX[1] THEN CH_endsY_  = CH_endsY_[0:N_ELEMENTS(CH_endsY_) - 2]
IF CH_endsY[NumY - 1] GT YBOX[1] THEN CH_strtsY_ = CH_strtsY_[0:N_ELEMENTS(CH_strtsY_) - 2]
;
IF CH_endsX[NumX - 1] GT XBOX[1] THEN XT_str[NumX - 1] = ' '
IF CH_endsX[NumX - 1] GT XBOX[1] THEN CH_endsX_  = CH_endsX_[0:N_ELEMENTS(CH_endsX_) - 2]
IF CH_endsX[NumX - 1] GT XBOX[1] THEN CH_strtsX_ = CH_strtsX_[0:N_ELEMENTS(CH_strtsX_) - 2]
;
;stop
;
;Xtick_Yval = Xtick_Yval - CH_YSZ*Dev_to_DataY*1.5d   ; Need to subtract one character as xyouts takes the bottom of the letter location.
IF YLG THEN Xtick_Yval = 10d^(Xtick_Yval - CH_YSZ*Dev_to_DataY*1.5d) ELSE Xtick_Yval = Xtick_Yval - CH_YSZ*Dev_to_DataY*1.5d   ; Need to subtract one character as xyouts takes the bottom of the letter location.
;IF YLG THEN Xtick_Yval = 10d^(Xtick_Yval - CH_YSZ*Dev_to_DataY*1.5d) ELSE
;Ytick_Xval = Ytick_Xval - CH_YSZ*Dev_to_DataX*0.5d  ; It is the coords for the bottom of the characters
IF XLG THEN Ytick_Xval = 10d^(Ytick_Xval - CH_YSZ*Dev_to_DataX*0.5d) ELSE Ytick_Xval = Ytick_Xval - CH_YSZ*Dev_to_DataX*0.5d  ; It is the coords for the bottom of the characters
;IF XLG THEN Ytick_Xval = 10d^Ytick_Xval
;
; Check to see if there is overlap between the first x and first y labels
IF (Ytick_Xval + Y_DX)[0] GT (CH_strtsX_)[0] THEN BEGIN  ; the first y axis tick label may be printed over the first x axis tick label
  ;
  IF YLG THEN XticksTop = (10d^( ALOG10(Xtick_Yval) + CH_YSZ*Dev_to_DataY))[0] ELSE XticksTop = (Xtick_Yval + CH_YSZ*Dev_to_DataY)[0]
  IF CH_strtsY_[0] LT XticksTop THEN YT_str[Yfirst] = ' '
  ;
ENDIF
;
IF ~TAG_EXIST(_EXTRA,'YTICKNAME') THEN xyouts, (Ytick_Xval + Y_DX)[0:*:ITERY], (Ytick_Yval + Y_DY)[0:*:ITERY], YT_STR[0:*:ITERY], ALIGNMENT=0.5, ORIENTATION=90d, /DATA, _EXTRA=_EXTRA ELSE xyouts, (Ytick_Xval + Y_DX), (Ytick_Yval + Y_DY), _EXTRA.YTICKNAME, ALIGNMENT=0.5, ORIENTATION=90d, /DATA, _EXTRA=_EXTRA
IF ~TAG_EXIST(_EXTRA,'XTICKNAME') THEN xyouts, (Xtick_Xval + X_DX)[0:*:ITERX], (Xtick_Yval + X_DY)[0:*:ITERX], XT_STR[0:*:ITERX], ALIGNMENT=0.5, ORIENTATION=0d, /DATA, _EXTRA=_EXTRA ELSE xyouts, (Xtick_Xval + X_DX), (Xtick_Yval + X_DY), _EXTRA.XTICKNAME, ALIGNMENT=0.5, ORIENTATION=0d, /DATA, _EXTRA=_EXTRA
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
IF XLG THEN yttl_x = 10d^(!X.CRANGE[0])*10d^((-1d)*CH_XSZ*Dev_to_DataX*3.5d) ELSE yttl_x = (!X.CRANGE[0]) - CH_XSZ*Dev_to_DataX*3d
;
IF XLG THEN xttl_x = 10d^((!X.crange[1] + !X.crange[0])/2d) ELSE xttl_x = ((!X.crange[1] + !X.crange[0])/2d)
IF YLG THEN xttl_y = 10d^(!Y.CRANGE[0])*10d^((-1d)*CH_YSZ*Dev_to_DataY*2.75d) ELSE xttl_y = (!Y.CRANGE[0]) - CH_YSZ*Dev_to_DataY*2.75d
;
;stop
;
IF TAG_EXIST(_EXTRA,'XTITLE') THEN xyouts, xttl_x + xttl_dx, xttl_y + xttl_dy, ALIGNMENT=0.5, /DATA, _EXTRA.XTITLE, _EXTRA=_EXTRA
IF TAG_EXIST(_EXTRA,'YTITLE') THEN xyouts, yttl_x + yttl_dx, yttl_y + yttl_dy, ALIGNMENT=0.5, ORIENTATION=90d, /DATA, _EXTRA.YTITLE, _EXTRA=_EXTRA
;
IF KEYWORD_SET(DEBUG) THEN BEGIN
  print, '    '
  print, 'YTICKS were supposed to be printed at x=', YTICK_XVAL[0],' and y=',YTICK_YVAL
  print, 'The Y ticks were supposed to be :', YT_STR
  IF TAG_EXIST(_EXTRA,'YTITLE') THEN print, 'YTITLE of "',_EXTRA.YTITLE,'" was supposed to be printed at x=',yttl_x + yttl_dx,' and y=',yttl_y + yttl_dy
  print, '    '
  print, 'XTICKS were supposed to be printed at x=', XTICK_XVAL,' and y=',XTICK_YVAL[0]
  print, 'The X ticks were supposed to be :', XT_STR
  IF TAG_EXIST(_EXTRA,'XTITLE') THEN print, 'XTITLE of "',_EXTRA.XTITLE,'" was supposed to be printed at x=',xttl_x + xttl_dx,' and y=',xttl_y + xttl_dy
  print, '     '
ENDIF
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
      height=height, width=width, full=full, FDIR=FDIR, POSN=POSN, PSPOSN=PSPOSN, EXWID=EXWID, EXHGT=EXHGT, ALTEPS=ALTEPS, KEEPPS=KEEPPS
  ;
  ;IF N_ELEMENTS(POSN) EQ 0 THEN
  ;
  IF N_ELEMENTS(EXWID) EQ 0 THEN EXWID = 0.1d  ;  cm
  IF N_ELEMENTS(EXHGT) EQ 0 THEN EXHGT = 0.1d  ;  cm
  ;
  if ~keyword_set(scale) then scale=replicate(1, n_elements(tag))
  scale = strcompress(string(scale), /remove_all)
  ;
  if keyword_set(outname) EQ 0 then begin
    outname='LS_HFIfig2.eps'
    noname=1
  endif
  ;
  IF KEYWORD_SET(KEEPPS) THEN PSname = (STRSPLIT(outname,'.eps',/REGEX, /EXTRACT))[0]+'.ps'
  ;
  openw, lun, FDIR+'LS_HFIfig_temp.tex', /get_lun
  printf, lun, '\documentclass{article}'
  printf, lun, '\usepackage{geometry, graphicx, psfrag}'
  printf, lun, '\renewcommand{\familydefault}{\sfdefault}'
  printf, lun,'\usepackage{helvet}'
  printf, lun,'\pagestyle{empty}'
  printf, lun,'\geometry{paperwidth='+strcompress(string(width+EXWID), /remove_all)+'cm,'+$
                      'paperheight='+strcompress(string(height+EXHGT), /remove_all)+'cm,margin=0pt}'
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
  IF KEYWORD_SET(ALTEPS) THEN BEGIN
    spawn, 'cd '+FDIR+' && ./ps2eps.pl -g -l '+'LS_HFIfig_temp.ps ', msg2
    spawn, 'mv '+FDIR+'LS_HFIfig_temp.eps '+FDIR+outname, msg25 
  ENDIF ELSE BEGIN
    spawn, 'cd '+FDIR+' && ps2epsi '+'LS_HFIfig_temp.ps '+'LS_HFIfig_temp.epsi', msg2
    ;
    ; Check to see if the file *.eps file already exists, if so rename the old one so the perl script will work.
    ; 
    flCheck = file_test(FDIR+outname)
    IF flCheck THEN spawn, 'cd '+FDIR+' && mv -f '+outname+' old_'+outname, msg3
    ;
    ;spawn, " && perl -ne 'print unless /^%%BeginPreview/../^%%EndPreview/' <"+FDIR+'LS_HFIfig_temp.epsi > '+FDIR+outname, msg4
    spawn, " perl -ne 'print unless /^%%BeginPreview/../^%%EndPreview/' <"+FDIR+'LS_HFIfig_temp.epsi > '+FDIR+outname, msg4
    ;
  ENDELSE
  ;
  IF KEYWORD_SET(KEEPPS) THEN BEGIN
    spawn, 'mv '+FDIR+'LS_HFIfig_temp.ps '+FDIR+psname
  ENDIF
  ;
  ; Clean up the temporary files now.
  ;
  if keyword_set(noname) then begin
    spawn, 'mv '+FDIR+'LS_HFIfig2.eps '+FDIR+filename
    outname=filename
  endif
  ;stop
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
      CASE nw OF
        0: BEGIN ; there are no more tags after the one is removed
          ;
          print,'-------------------------------------------------------------'
          message,'This would remove all tags! Dummy tag "foo" added to structure',/inf
          print,'-------------------------------------------------------------'
          newstruct=create_struct('foo',' ')
          return
          ;
        END
        n: BEGIN ;  THe numbwer of tags is the same with and without, therefore no match
          print,'-----------------------------------------------------'
          message,'Tag did not match, structure unchanged',/inf
          print,'-----------------------------------------------------'
          newstruct = struct
          return
        END
        ELSE: ; Maybe something needed here, but think is OK blank for now.
      ENDCASE
;      IF nw EQ n THEN BEGIN
;          print,'-----------------------------------------------------'
;          message,'Tag did not match, structure unchanged',/inf
;          print,'-----------------------------------------------------'
;          newstruct = struct
;          return
;      ENDIF ELSE BEGIN  ;  There is only one tag, and it is desired to be removed.  add a dummy tag
;      ENDELSE
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  Utility files needed to get Planck colourtable loaded, and Healpix mollview plots to work.
;
;
;
;
;
;    Credit to Stephane Columbi for his work on the colour table!!  
;    I just took his code and deleted some of the uneeded bits, and put a wrapper function around it.
;
;
;
;
;
;
;    I have lifted three healpix routines and modified them:  mollview -> LS_mollview, proj2out -> LS_proj2out, and oplot_graticule -> LS_oplot_graticule
;    
;    The version of epstopdf on magique3 does not allow changing the pdf resolution, so I have pointed to a current version (2.18) available from CTAN, 
;    this file must be downloaded and pointed to for this portion of the code to work properly.
;
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;;
;;
;;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;============================================================
pro HFI_CT,ascii=ascii,plotlum=plotlum,verbose=verbose, CTDIR=CTDIR, CTfile=CTfile, LOAD=LOAD, HIGHDR=HIGHDR, HDRFILE=HDRFILE, HDRDIR=HDRDIR
;============================================================
; Create the official Planck colour table.
; ascii=filename : to output the color table in a file.
;          This will create a 3 column ascii file, with
;          R for the 1st one, G for the 2nd one and B for the
;          3rd one.
; /plotlum : to plot the luminance of the color table. 
; /verbose : sets verbose mode
; CTDIR:  is the directory to place the colortable file in (it copies the current colortable file, overwrites table 41, then saves it as CTfile
; CTfile:  is the resultant/modified CT file, default is colors1.tbl
; LOAD: Also loadds the modified colour table.
; 
; For more questions about this routine, ask 
; Locke Spencer (LockeSpencerAtastro.cf.ac.uk), or Stephane Colombi (colombiATiap.fr)
;============================================================
;
OS = !VERSION.OS
IF STRCMP(OS,'Win',3) THEN WIN = 1 ELSE WIN = 0
;IF WIN EQ 1 THEN spawn, 'chdir', CurDIR ELSE spawn, 'pwd', CurDIR ; assume all non windows machines accept the `pwd' command
cd,current=CurDir
ds = path_sep()
CurDir += path_sep()

IF N_ELEMENTS(CTDIR) EQ 0 THEN CTDIR = CurDIR
IF N_ELEMENTS(CTfile) EQ 0 THEN CTfile = 'colors1.tbl'
;IF N_ELEMENTS(HDRfile) EQ 0 THEN HDRfile = 'rgb_37.idl'
IF N_ELEMENTS(HDRfile) EQ 0 THEN HDRfile = 'RGB_Planck_hdr.idl'
IF N_ELEMENTS(HDRDIR) EQ 0 THEN HDRDIR = CTDIR
;
; Check for the file color1.tbl in the current directory.
; 
CT_found = file_test(CTDIR+CTfile)
CTsuf = ds+'resource'+ds+'colors'+ds
CTfile_orig = 'colors1.tbl'
IF WIN EQ 1 THEN cpstr = 'copy ' ELSE cpstr = 'cp ' 
IF CT_found EQ 0 THEN spawn, cpstr+!DIR+CTsuf+CTfile_orig+' '+CTDIR+CTfile , CTmsg
;
   npoints=7
;
Rim=lonarr(npoints)
Gim=lonarr(npoints)
Bim=lonarr(npoints)

 Rim(0)=0
 Gim(0)=0
 Bim(0)=255
 
 Rim(1)=0
 Gim(1)=112
 Bim(1)=255

 Rim(2)=0
 Gim(2)=221
 Bim(2)=255

 Rim(3)=255
 Gim(3)=237
 Bim(3)=217

 Rim(4)=255
 Gim(4)=180
 Bim(4)=0

 Rim(5)=255
 Gim(5)=75
 Bim(5)=0

 Rim(6)=100
 Gim(6)=0
 Bim(6)=0

if (keyword_set(verbose)) then begin
   print, 'luminance=',luminance(Rim,Gim,Bim)
   print, 'R=',Rim
   print, 'G=',Gim
   print, 'B=',Bim
endif

nc=256
R=bytarr(nc)
G=bytarr(nc)
B=bytarr(nc)

ipos=floor(((findgen(npoints))/(npoints-1))*255)
ipos(0)=0
ipos(npoints-1)=255

for i=1,npoints-1 do begin
   ip1=ipos(i-1)
   ip2=ipos(i)
   R1=Rim(i-1)
   R2=Rim(i)
   G1=Gim(i-1)
   G2=Gim(i)
   B1=Bim(i-1)
   B2=Bim(i)
   for j=ip1,ip2 do begin
      R(j)=floor(R1+(R2-R1)*double(j-ip1)/double(ip2-ip1))
      G(j)=floor(G1+(G2-G1)*double(j-ip1)/double(ip2-ip1))
      B(j)=floor(B1+(B2-B1)*double(j-ip1)/double(ip2-ip1))
   endfor
endfor

IF KEYWORD_SET(HIGHDR) THEN BEGIN
  ;
  ;r_ = [REVERSE(r[0:127]),REVERSE(r[128:255])]
  ;g_ = [REVERSE(g[0:127]),REVERSE(g[128:255])]
  ;b_ = [REVERSE(b[0:127]),REVERSE(b[128:255])]
  ;
  r_orig = r & g_orig = g & b_orig = b
  ;
  RESTORE, FILENAME=HDRDIR+HDRFILE ; just restored RGB_Planck_hdr (previously called rgb_37) as a [256 x 3] variable
  IF HDRFILE EQ 'rbg_37.idl' THEN rgb_ = rgb_37 ELSE rgb_ = RGB_Planck_hdr
  r_ = REFORM(rgb_[*,0])
  g_ = REFORM(rgb_[*,1])
  b_ = REFORM(rgb_[*,2])
  ;
  r = r_ & g = g_ & b = b_
  ;
ENDIF

if (keyword_set(ascii)) then begin
   openw, unit, ascii,/get_lun
   for i=0l,255l do begin
      printf,unit, R(i),G(i),B(i)
   endfor
   free_lun,unit
endif
if (keyword_set(verbose)) then begin
   print, 'R final=',R
   print, 'G final=',G
   print, 'B final=',B
endif
;stop
 
modifyct,41,'parchment1',R,G,B, FILE=CTDIR+CTfile

if (keyword_set(plotlum)) then begin
   loadct,0
   window,0
   plot,luminance(R,G,B),xtitle='index',ytitle='luminance',xr=[0,255],/xs
endif

IF KEYWORD_SET(LOAD) THEN loadct, 41, FILE=CTDIR+CTFILE

end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
; -----------------------------------------------------------------------------
;
;  Copyright (C) 1997-2012  Krzysztof M. Gorski, Eric Hivon, Anthony J. Banday
;
;
;
;
;
;  This file is part of HEALPix.
;
;  HEALPix is free software; you can redistribute it and/or modify
;  it under the terms of the GNU General Public License as published by
;  the Free Software Foundation; either version 2 of the License, or
;  (at your option) any later version.
;
;  HEALPix is distributed in the hope that it will be useful,
;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;  GNU General Public License for more details.
;
;  You should have received a copy of the GNU General Public License
;  along with HEALPix; if not, write to the Free Software
;  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
;
;  For more information about HEALPix see http://healpix.jpl.nasa.gov
;
; -----------------------------------------------------------------------------
pro LS_oplot_line_with_label, u, v, linelabel=linelabel, putlabel=putlabel, flush=flush, lines=line_type, _extra = oplot_kw, charsize=charsize, LABROT=LABROT

lensegment = n_elements(u)
minseglen = 20
chars = keyword_set(charsize) ? charsize : 0
do_label =  (keyword_set(putlabel) && size(/tname, linelabel) eq 'STRING' && lensegment gt minseglen && chars gt 0.)
if (do_label) then begin
    middle = lensegment* (keyword_set(flush) ? 0.1 : 0.45)
    ; find out room to leave in line around label
    chsize = chars*!d.y_ch_size/(!d.y_vsize*!y.s[1]) ; size of character in units of data coordinates
    labelsize = chsize*strlen(linelabel)
    blank = long(labelsize/sqrt((u[middle-1]-u[middle+1])^2+(v[middle-1]-v[middle+1])^2))> 0.6
    ; make sure that label is shorther than line to be labelled
    if (2*blank lt lensegment) then begin
                                ; break line under label
        if ((middle-blank) gt 0) then oplot, u[0:middle-blank], v[0:middle-blank], _extra = oplot_kw, lines=line_type
        if ((middle+blank) lt (lensegment-1)) then oplot, u[middle+blank:*], v[middle+blank:*], _extra = oplot_kw, lines=line_type
                                ; put label
        step = 3
        dv = v[middle+step]-v[middle-step]
        du = u[middle+step]-u[middle-step]
        ;angle = atan(dv/du) * !radeg ; angle in degree
        angle = atan(dv,du) * !radeg ; angle in degree
        if (angle lt -89.9) then angle = angle + 180.
        if (angle gt  89.9) then angle = angle - 180.
        ; offset label position to be level with line
        xlab = u[middle] + 0.3*chsize * cos((angle-90.)*!dtor)
        ylab = v[middle] + 0.3*chsize * sin((angle-90.)*!dtor)
        IF KEYWORD_SET(LABROT) THEN BEGIN
          angle = angle + 180d
          xlab = u[middle] + 0.3*chsize * cos((angle-90.)*!dtor)
          ylab = v[middle] + 0.3*chsize * sin((angle-90.)*!dtor)
        ENDIF
        xyouts, xlab, ylab, linelabel, align=0.5,orientation=angle, noclip=0,charsize=chars
    endif else begin
        ; if label too big, drop it and only plot line
        oplot, u, v, _extra = oplot_kw, lines=line_type
    endelse
endif else begin
    ; no label, line only
    oplot, u, v, _extra = oplot_kw, lines=line_type
endelse

return
end

;=============================================

pro LS_oplot_sphere, u, v,  line_type=line_type, _extra = oplot_kw, linelabel=linelabel, flush=flush, charsize=charsize, LABROT=LABROT

if undefined(line_type) then line_type = 0

; find points where line should be interrupted (large step in u)
bad = where(abs(u-shift(u,1)) gt .1, nbad)

iw = index_word(tag_names(oplot_kw),'PSYM',err=errword)
if errword eq 0 then begin
    if (oplot_kw.psym gt 0) then nbad = 0
endif

if (nbad eq 0) then begin
    if (line_type lt 0) then begin
        oplot, u, v, _extra = oplot_kw, col=1 ; white background
        oplot, u, v, _extra = oplot_kw, col=0, lines=abs(line_type)
    endif else begin
        LS_oplot_line_with_label, u, v, linelabel=linelabel, _extra = oplot_kw, lines=line_type, putlabel=1,flush=flush,charsize=charsize, LABROT=LABROT
    endelse
endif else begin
;    bad = [0,bad,n_elements(u)-1]
    bad = [0,bad,n_elements(u)]
    already = 0
    for j=0,nbad do begin
        lensegment = bad[j+1] - bad[j]
        if (lensegment gt 1) then begin
            u1 = u[bad[j]:bad[j+1]-1]
            v1 = v[bad[j]:bad[j+1]-1]
            if (line_type lt 0) then begin
                oplot, u1, v1, _extra = oplot_kw, col=1 ; white background
                oplot, u1, v1, _extra = oplot_kw, col=0, lines=abs(line_type)
            endif else begin
                putlabel = (~already)
                LS_oplot_line_with_label, u1, v1, linelabel=linelabel, _extra = oplot_kw, lines=line_type, putlabel=putlabel, flush=flush, charsize=charsize, LABROT=LABROT
                already = 1
            endelse        
        endif
    endfor
endelse




return
end
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
;
;
;
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; -----------------------------------------------------------------------------
;
;  Copyright (C) 1997-2012  Krzysztof M. Gorski, Eric Hivon, Anthony J. Banday
;
;
;
;
;
;  This file is part of HEALPix.
;
;  HEALPix is free software; you can redistribute it and/or modify
;  it under the terms of the GNU General Public License as published by
;  the Free Software Foundation; either version 2 of the License, or
;  (at your option) any later version.
;
;  HEALPix is distributed in the hope that it will be useful,
;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;  GNU General Public License for more details.
;
;  You should have received a copy of the GNU General Public License
;  along with HEALPix; if not, write to the Free Software
;  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
;
;  For more information about HEALPix see http://healpix.jpl.nasa.gov
;
; -----------------------------------------------------------------------------
pro LS_oplot_graticule, graticule, eul_mat, projection=projection, mollweide=mollweide, gnomic=gnomic, cartesian=cartesian, orthographic=orthographic, $
  flip = flip, _extra = oplot_kw, half_sky=half_sky, coordsys=coordsys, charsize=charsize, reso_rad=reso_rad, GRMIN=GRMIN, GRMAX=GRMAX, LATLONGDIFF=LATLONGDIFF
;+
; NAME:
;       OPLOT_GRATICULE
;
; PURPOSE:
;       overplots graticule (ie, spherical coordinates grid) on top
;       of existing map
;
; CATEGORY:
;
;
; CALLING SEQUENCE:
;       oplot_graticule, graticule, eul_mat, $
;          [projection=,mollweide=,gnomic=,cartesian=, $
;           orthographic=,flip=,half_sky=,coordsys=, reso_rad=, + all oplot keywords]
;
; INPUTS:
;
;
; OPTIONAL INPUTS:
;
;
; KEYWORD PARAMETERS:
;
;
; OUTPUTS:
;
;
; OPTIONAL OUTPUTS:
;
;
; COMMON BLOCKS:
;
;
; SIDE EFFECTS:
;
;
; RESTRICTIONS:
;
;
; PROCEDURE:
;      calls oplot_sphere
;
; EXAMPLE:
;
;
;
; MODIFICATION HISTORY:
;         Feb 2003, corrected bug occuring when combining astrophysical coordinates
;         changes with arbitrary rotation
;         Jan. 2013, L. Spencer: added GRMIN and GRMAX keywords to prevent printing graticule labels on edges of map (i.e. -180, +180, -90, etc.)
;-
IF N_ELEMENTS(GRMIN) EQ 1 THEN GRMIN = DBLARR(2) + GRMIN
IF N_ELEMENTS(GRMAX) EQ 1 THEN GRMAX = DBLARR(2) + GRMAX
;
identify_projection, projtype, projection=projection, mollweide=mollweide, gnomic=gnomic, cartesian=cartesian

if keyword_set(flip) then flipconv=1 else flipconv = -1  ; longitude increase leftward by default (astro convention)

do_ig = (n_elements(coordsys) eq 2) 

; define default graticule spacing, in Degrees
if (projtype eq 2) then begin ; gnom
    dlong = 5. 
    dlat  = 5. 
    gratmin = 0.
endif
if (projtype eq 1) then begin ; moll
    dlong = 45.
    dlat  = 45.
    gratmin = 10.
endif
if (projtype eq 3) then begin ; cart
    dlong = 45. 
    dlat  = 45. 
    gratmin = 2.
endif
if (projtype eq 4) then begin ; ortho
    dlong = 45.
    dlat  = 45.
    gratmin = 10.
endif

; read in user defined grid spacings
if (n_elements(graticule) eq 2 and min(graticule) ge gratmin) then begin
    dlong = float(graticule[0]) & dlat = float(graticule[1])
endif else begin
    if (graticule[0] gt gratmin) then begin 
        dlong = float(graticule[0]) & dlat = dlong
    endif
endelse

fsgrat = (defined(reso_rad)) ? !dtor/(reso_rad+1.d-11) : 1. ; number of pixels / degree
fsgrat = long(fsgrat) > 1 < 5
; define variables
epsilon = 1.d-5
nmerid = fix(181./dlong)
nparal = fix(90./dlat)-1
nv = (projtype eq 2 || projtype eq 3) ? 721*fsgrat : 361 ; more points for partial projections
vector = DINDGEN(nv)/(nv-1.d0) * (1.d0-2.d0*epsilon) + epsilon ; from epsilon to 1-epsilon
bounds = [[-nmerid,nmerid-1],[-nparal,nparal]]

do_rot = (n_elements(eul_mat) eq 9)
form = '(i4)'
if (abs(round(dlong)-dlong) gt 1.e-2 || abs(round(dlat)-dlat) gt 1.e-2) then form = '(f6.1)'

case projtype of
2: begin  ; gnomic : straightforward
    for jg=0,1 do begin
        for i = bounds[0,jg],bounds[1,jg] do begin
;             if (jg eq 0) then ang2vec, vector*!pi,        replicate(i*dlong*!DtoR,nv), vv ; meridians
;             if (jg eq 1) then ang2vec, replicate((90.-i*dlat)*!DtoR,nv), vector*(2.*!pi), vv ; parallels
            if (jg eq 0) then begin
                mylong = i*dlong ; longitude in Deg
                linelabel = strtrim(string(mylong,form=form),2)
                ang2vec, vector*!pi,        replicate(mylong*!DtoR,nv), vv ; meridians
            endif
            if (jg eq 1) then begin
                mylat = i*dlat ; latitude in Deg
                linelabel = strtrim(string(mylat,form=form),2)
                ang2vec, replicate((90.-mylat)*!DtoR,nv), vector*2*!pi, vv ; parallels
            endif
            if (do_ig ) then vv = rotate_coord(vv,in=coordsys[0],out=coordsys[1])
            if (do_rot) then vv = vv # transpose(eul_mat)

            k = where(vv(*,0) gt 0, nk)
            if (nk gt 0) then begin
                u = vv(k,1)/vv(k,0)
                v = vv(k,2)/vv(k,0)
                good = where(abs(u) lt !x.crange[1]*1.01 and abs(v) lt !y.crange[1]*1.01 ,ng)
                ; reorder points to have one continuous segment across the plot
                bad = where(good-shift(good,1) ne 1, nbad)
                if (nbad gt 1) then good = shift(good, bad[1])
;                oplot, flipconv * u, v, _extra = oplot_kw
                IF !D.name EQ 'PS' THEN device, /HELVETICA, FONT_size=8
                ;stop
               if (ng gt 1) then LS_oplot_sphere, flipconv *u[good], v[good], _extra = oplot_kw, linelabel=linelabel,/flush, charsize=charsize
            endif
        endfor
    endfor
end
1: begin  ; mollweide : deal with boundaries
    for jg=0,1 do begin
        for i = bounds[0,jg],bounds[1,jg] do begin
            LABROT = 0
            if (jg eq 0) then begin
                mylong = i*dlong ; longitude in Deg
                IF N_ELEMENTS(GRMIN) GT 0 THEN BEGIN
                   linelabel = strtrim(string(mylong,form=form),2);+'!Xo'
                   IF KEYWORD_SET(LATLONGDIFF) THEN linelabel = linelabel+'_l'
                   IF mylong LT GRMIN[0] THEN linelabel = ''
                   IF N_ELEMENTS(GRMAX) GT 0 THEN BEGIN
                     IF mylong GE GRMAX[0] THEN linelabel = ''
                   ENDIF
                ENDIF ELSE BEGIN
                   linelabel = strtrim(string(mylong,form=form),2);+'!Xo'
                   IF KEYWORD_SET(LATLONGDIFF) THEN linelabel = linelabel+'_l'
                   IF N_ELEMENTS(GRMAX) GT 0 THEN BEGIN
                     IF mylong GE GRMAX[0] THEN linelabel = ''
                   ENDIF ; ELSE linelabel = strtrim(string(mylong,form=form),2)
                ENDELSE
                ;linelabel = strtrim(string(mylong,form=form),2)
                ang2vec, vector*!pi,        replicate(mylong*!DtoR,nv), vv ; meridians
                IF (ABS(mylong) LT 0.1d) THEN LABROT = 1
            endif
            if (jg eq 1) then begin
                mylat = i*dlat ; latitude in Deg
                IF N_ELEMENTS(GRMIN) GT 0 THEN BEGIN
                   linelabel = strtrim(string(mylat,form=form),2);+'!Xo'
                   IF KEYWORD_SET(LATLONGDIFF) THEN linelabel = linelabel+'_b'
                   IF mylat LT GRMIN[1] THEN linelabel = ''
                   IF N_ELEMENTS(GRMAX) GT 0 THEN BEGIN
                     IF mylat GT GRMAX[1] THEN linelabel = ''
                   ENDIF
                ENDIF ELSE BEGIN
                   linelabel = '!X'+strtrim(string(mylat,form=form),2);+'!Xo'
                   IF KEYWORD_SET(LATLONGDIFF) THEN linelabel = linelabel+'_b'
                   IF N_ELEMENTS(GRMAX) GT 0 THEN BEGIN
                     IF mylat GT GRMAX[1] THEN linelabel = ''
                   ENDIF ; ELSE linelabel = strtrim(string(mylat,form=form),2)
                ENDELSE
                ;linelabel = strtrim(string(mylat,form=form),2)
                ang2vec, replicate((90.-mylat)*!DtoR,nv), vector*2*!pi, vv ; parallels
            endif
            if (do_ig ) then vv = rotate_coord(vv,in=coordsys[0],out=coordsys[1])
            if (do_rot) then vv = vv # transpose(eul_mat)

            vec2moll, vv, u, v
            ;oplot_sphere, -flipconv * u, v, _extra = oplot_kw, linelabel=linelabel, charsize=charsize
            IF !D.name EQ 'PS' THEN device, /HELVETICA, FONT_size=8
            LS_oplot_sphere,  u, v, _extra = oplot_kw, linelabel=linelabel, charsize=charsize, LABROT=LABROT;, /FLUSH
;;            oplot_sphere, flipconv * u, v, _extra = oplot_kw
        endfor
    endfor
end
4: begin  ; orthographic : deal with boundaries
    if keyword_set(half_sky) then begin
        nd = 1 ; number of half-sky disc
        c0 = 0
    endif else begin
        nd = 2
        c0 = 1
    endelse
    for jg=0,1 do begin
        for i = bounds[0,jg],bounds[1,jg] do begin
            if (jg eq 0) then begin
                mylong = i*dlong ; longitude in Deg
                linelabel = strtrim(string(mylong,form=form),2)
                ang2vec, vector*!pi,        replicate(mylong*!DtoR,nv), vv ; meridians
            endif
            if (jg eq 1) then begin
                mylat = i*dlat ; latitude in Deg
                linelabel = strtrim(string(mylat,form=form),2)
                ang2vec, replicate((90.-mylat)*!DtoR,nv), vector*2*!pi, vv ; parallels
            endif
            if (do_ig ) then vv = rotate_coord(vv,in=coordsys[0],out=coordsys[1])
            if (do_rot) then vv = vv # transpose(eul_mat)

            for sign = 1,1-nd,-2 do begin ; either (1,-1) or (1)
                k = where(vv[*,0]*sign ge 0, nk)
                if (nk gt 0) then begin
                    u = vv[k,1]
                    v = vv[k,2]
                    ;oplot_sphere, flipconv*(u+c0)*sign, v, _extra = oplot_kw
                    oplot_sphere,  flipconv*(u+c0)*sign, v, _extra = oplot_kw, linelabel=linelabel, charsize=charsize
                endif ; nk>0
            endfor ; loop on sign
        endfor
    endfor
end
3: begin  ; cartesian : straightforward
    for jg=0,1 do begin
        for i = bounds[0,jg],bounds[1,jg] do begin
;             if (jg eq 0) then ang2vec, vector*!pi,        replicate(i*dlong*!DtoR,nv), vv ; meridians
;             if (jg eq 1) then ang2vec, replicate((90.-i*dlat)*!DtoR,nv), vector*2*!pi, vv ; parallels
            if (jg eq 0) then begin
                mylong = i*dlong ; longitude in Deg
                linelabel = strtrim(string(mylong,form=form),2)
                ang2vec, vector*!pi,        replicate(mylong*!DtoR,nv), vv ; meridians
            endif
            if (jg eq 1) then begin
                mylat = i*dlat ; latitude in Deg
                linelabel = strtrim(string(mylat,form=form),2)
                ang2vec, replicate((90.-mylat)*!DtoR,nv), vector*2*!pi, vv ; parallels
            endif
            if (do_ig ) then vv = rotate_coord(vv,in=coordsys[0],out=coordsys[1])
            if (do_rot) then vv = vv # transpose(eul_mat)

            phi = atan(vv[*,1],vv[*,0]) ; in [0,2pi]
      theta = asin(vv[*,2])       ; in [0,pi]
            ; OPLOT,-flipconv*phi,theta, _extra = oplot_kw
;            oplot_sphere, flipconv * phi, theta, _extra = oplot_kw
;            oplot_sphere, flipconv* phi, theta, _extra = oplot_kw,
;            linelabel=linelabel, charsize=charsize
            good = where(abs(phi) lt !x.crange[1]*1.1 and abs(theta) lt !y.crange[1]*1.1 ,ng)
                                ; reorder points to have one continuous segment across the plot
            bad = where(good-shift(good,1) ne 1, nbad)
            if (nbad gt 1) then good = shift(good, bad[1])
            if (ng gt 1) then oplot_sphere, flipconv *phi[good], theta[good], _extra = oplot_kw, linelabel=linelabel,charsize=charsize
        endfor
    endfor
end
endcase


;-----------------------
;     FOR i=-nmerid,nmerid-1 DO begin  ; meridians
;         ang2vec, vector*!pi, replicate(i*dlong*!DtoR,n_elements(vector)), vv
;         if (do_rot) then vv = vv # transpose(eul_mat)

;         k = where(vv(*,0) gt 0, nk)
;         if (nk gt 0) then begin
;             u = vv(k,1)/vv(k,0)
;             v = vv(k,2)/vv(k,0)
;             OPLOT, flipconv * u, v, COLOR = !P.COLOR
;         endif
;     endfor
;     FOR i=-nparal,nparal DO begin  ; parallels
;         ang2vec, replicate((90.-i*dlat)*!DtoR,n_elements(vector)), vector*2*!pi, vv
;         if (do_rot) then vv = vv # transpose(eul_mat)

;         k = where(vv(*,0) gt 0, nk)
;         if (nk gt 0) then begin
;             u = vv(k,1)/vv(k,0)
;             v = vv(k,2)/vv(k,0)
;             OPLOT, flipconv * u, v, COLOR = !P.COLOR
;         endif
;     endfor
; endif

; if (do_moll) then begin
;     ; mollweide : deal with boundaries
;     FOR i=-nmerid,nmerid-1 DO begin  ; meridians
;         ang2vec, vector*!pi, replicate(i*dlong*!DtoR,nv), vv
;         if (do_rot) then vv = vv # transpose(eul_mat)

;         vec2moll, vv, u, v
;         bad = where(abs(u-shift(u,1)) gt .1, nbad)
;         if (nbad eq 0) then begin
;             OPLOT, flipconv * u, v, COLOR = !P.COLOR
;         endif else begin
;             bad = [0,bad,n_elements(u)-1]
;             for j=0,nbad do begin
;                 if (bad[j+1] gt bad[j]) then $
;                   oplot, flipconv * u[bad[j]:bad[j+1]-1], v[bad[j]:bad[j+1]-1], color=!p.color
;             endfor
;         endelse
;     endfor
;     FOR i=-nparal,nparal DO begin  ; parallels
;         ang2vec, replicate((90.-i*dlat)*!DtoR,nv), vector*2*!pi, vv
;         if (do_rot) then vv = vv # transpose(eul_mat)

;         vec2moll, vv, u, v
;         bad = where(abs(u-shift(u,1)) gt .1, nbad)
;         if (nbad eq 0) then begin
;             OPLOT, flipconv * u, v, COLOR = !P.COLOR
;         endif else begin
;             bad = [0,bad,n_elements(u)-1]
;             for j=0,nbad do begin
;                 if (bad[j+1] gt bad[j]) then $
;                   oplot, flipconv * u[bad[j]:bad[j+1]-1], v[bad[j]:bad[j+1]-1], color=!p.color
;             endfor
;         endelse
;     endfor
; endif

return
end
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
; -----------------------------------------------------------------------------
;
;  Copyright (C) 1997-2012  Krzysztof M. Gorski, Eric Hivon, Anthony J. Banday
;
;
;
;
;
;  This file is part of HEALPix.
;
;  HEALPix is free software; you can redistribute it and/or modify
;  it under the terms of the GNU General Public License as published by
;  the Free Software Foundation; either version 2 of the License, or
;  (at your option) any later version.
;
;  HEALPix is distributed in the hope that it will be useful,
;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;  GNU General Public License for more details.
;
;  You should have received a copy of the GNU General Public License
;  along with HEALPix; if not, write to the Free Software
;  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
;
;  For more information about HEALPix see http://healpix.jpl.nasa.gov
;
; -----------------------------------------------------------------------------
function LS_color_map, data, mindata, maxdata, Obs, $
                    color_bar = color_bar, mode = mode, minset = min_set, maxset = max_set, silent = silent
;+
; color_map
;
; color = color_map( data, mindata, maxdata, Obs=obs, color_bar =
; color_bar, mode = mode)
;
;
; INPUT
;    data : input data
;    mindata : min (can be changed by the routine)
;    maxdata : maxdata (can be changed by the routine)
;
;  Obs : list of valid pixels
;  if Obs is not defined, all pixels are assumed valid
;
; mode    0: nothing
;         1: histogram equalized
;         2: log
;         4: asinh
; asin can not be used together with log nor hist_eq
;
;
;
; Sep 2007: added /silent
; July 2008: added Asinh mode
; Oct 2008: make sure that MIN and MAX are properly taken into account in Asinh
; mode
; 2009-04-30: make sure that MINSET and MAXSET are taken into account in Log mode
; 2013-02-27: added the ability to provide the MODASINH colour scheme, and changed name to LS_color_map accordingly.
;-

if undefined (mode) then mode = 0
do_hist  = (mode and 1)
do_log   = (mode and 2)/2
do_asinh = (mode and 4)/4
do_modasinh = (mode and 8)/8
if (do_asinh && (do_log || do_hist)) then begin
    message,'Asinh mode can NOT be used together with Log or Hist mode'
endif
if (do_modasinh && (do_log || do_hist)) then begin
    message,'ModAsinh mode can NOT be used together with Log or Hist mode'
endif

N_Color = !D.n_colors < 256

sz = size(data)
npix = n_elements(data)
Color = MAKE_ARRAY(/BYTE, npix, Value = 2B)
color_bar = [2B]
if (sz(0) eq 2) then Color = reform(color,/over,sz(1),sz(2))
if (sz(0) eq 3) then Color = reform(color,/over,sz(1),sz(2),sz(3))
if defined(Obs) then begin
    if Obs[0] eq -1 then return, color
    N_obs = n_elements(Obs)
    N_no_Obs = npix - N_obs
endif else begin
    ;Obs = lindgen(npix)
    N_obs = npix
    N_no_obs = 0
endelse

; -------------------------------------------------------------
; sets MIN and MAX
; -------------------------------------------------------------
if (~keyword_set(silent)) then print,'plotted area original MIN and MAX: ',mindata, maxdata
IF DEFINED(min_set) THEN BEGIN
    if (min_set gt mindata) then begin
        IF (N_No_Obs eq 0) THEN data = data > min_set ELSE data(Obs)=data(Obs) > min_set
    endif
    mindata = min_set
    if (~keyword_set(silent)) then print,'new MIN : ',mindata
ENDIF

IF DEFINED(max_set) THEN BEGIN
    if (max_set lt maxdata) then begin
        IF (N_No_Obs eq 0) THEN data = data < max_set ELSE data(Obs)=data(Obs) < max_set
    endif
    maxdata = max_set
    if (~keyword_set(silent)) then print,'new MAX : ',maxdata
ENDIF

IF (do_log) THEN BEGIN
;   log
    if (N_No_Obs eq 0) then begin
;         data      = ALOG10(data + (0.0001 -mindata))
        data = Alog10(data > 1.e-6*abs(maxdata))
        mindata = MIN(data,MAX=maxdata)
    endif else begin 
;         data[Obs] = ALOG10(data[Obs] + (0.0001 -mindata))
        data[Obs] = ALOG10(data[Obs] > 1.e-6*abs(maxdata))
        mindata = MIN(data[Obs],MAX=maxdata)
    endelse
    ; use user-defined range if provided
    if defined(min_set) then begin
        if (min_set gt 0) then mindata = alog10(min_set)
    endif
    if defined(max_set) then begin
        if (max_set gt 0) then maxdata = alog10(max_set)
    endif
ENDIF

; IF (do_asinh) THEN BEGIN
; ;   Asinh
;     if (N_No_Obs eq 0) then begin
;         data = asinh(data)
;         mindata = MIN(data,MAX=maxdata)
;     endif else begin 
;         data[Obs] = asinh(data[Obs])
;         mindata = MIN(data[Obs],MAX=maxdata)
;     endelse
; ENDIF


; turn data into colors (in the range [3,N_color-1])
col_scl = FINDGEN(N_Color-3)/(N_Color-4)

if (do_hist) then begin
;   histogram equalised scaling
    Tmax = maxdata & Tmin = mindata
    Bs = (Tmax-Tmin)/5000. ;MIN( [(Tmax-Tmin)/5000.,2.] )
    if (N_No_Obs eq 0) then begin
        Phist = HISTOGRAM( data, Min=Tmin, Max=Tmax, Bin = Bs )
        Phist = total(Phist,/cumul)
        Phist[0] = 0.
        Junk = INTERPOLATE( FLOAT(Phist), (data-Tmin)/Bs )
        Color = 3B + BYTSCL( Junk, Top=N_Color-4 )
    endif else begin 
        Phist = HISTOGRAM( data[Obs], Min=Tmin, Max=Tmax, Bin = Bs )
        Phist = total(Phist,/cumul)
        Phist[0] = 0.
        Junk = INTERPOLATE( FLOAT(Phist), (data[Obs]-Tmin)/Bs )
        Color[Obs] = 3B + BYTSCL( Junk, Top=N_Color-4 )
    endelse

    junk2= INTERPOLATE( FLOAT(Phist), col_scl*(Tmax-Tmin)/bs )
    color_bar = (3B + BYTSCL( junk2, TOP = N_Color-4 ))

endif else begin
;   linear scaling or Asinh mapping
    if (do_asinh) then begin
        Tmax = maxdata*1.0 & Tmin = mindata*1.0
        if (N_No_Obs eq 0) then begin
;             Color = 3B + BYTSCL( asinh(data), Top=N_Color-4 )
            Color = 3B + BYTSCL( asinh(data), MIN=asinh(Tmin), MAX=asinh(Tmax), Top=N_Color-4 )
        endif else begin 
;             Color[Obs] = 3B + BYTSCL( asinh(data[Obs]), Top=N_Color-4 )
            Color[Obs] = 3B + BYTSCL( asinh(data[Obs]), MIN=asinh(Tmin), MAX=asinh(Tmax), Top=N_Color-4 )
        endelse
        
        bs = 5 * N_color
        junk2= asinh(  dindgen(bs)/(bs-1)*(Tmax-Tmin) + Tmin  )
        color_bar = (3B + BYTSCL( junk2, TOP = N_Color-4 ))

    endif else begin
        if (do_modasinh) then begin
          Tmax = maxdata*1.0 & Tmin = mindata*1.0
          if (N_No_Obs eq 0) then begin
;               Color = 3B + BYTSCL( asinh(data), Top=N_Color-4 )
              Color = 3B + BYTSCL( modasinh(data), MIN=modasinh(Tmin), MAX=modasinh(Tmax), Top=N_Color-4 )
          endif else begin 
;               Color[Obs] = 3B + BYTSCL( asinh(data[Obs]), Top=N_Color-4 )
              Color[Obs] = 3B + BYTSCL( modasinh(data[Obs]), MIN=modasinh(Tmin), MAX=modasinh(Tmax), Top=N_Color-4 )
          endelse
          
          bs = 5 * N_color
          junk2= modasinh(  dindgen(bs)/(bs-1)*(Tmax-Tmin) + Tmin  )
          color_bar = (3B + BYTSCL( junk2, TOP = N_Color-4 ))
        endif else begin
          
          if (maxdata ne mindata && $
              ABS((maxdata+mindata)/FLOAT(maxdata-mindata)) lt 5.e-2) then begin
;         if Min and Max are symmetric
;         put data=0 at the center of the color scale
            Tmax = MAX(ABS([mindata,maxdata]))
            Tmin = -Tmax
          endif else begin 
            Tmax = maxdata & Tmin=mindata
          endelse
          if (N_No_Obs eq 0) then begin 
              color = 3B + BYTSCL(data, MIN=Tmin, MAX=Tmax, Top=N_Color-4 )
          endif else begin
              color[Obs] = 3B + BYTSCL(data[Obs], MIN=Tmin, MAX=Tmax, Top=N_Color-4 )
          endelse
          color_bar = (3B + BYTSCL( col_scl, TOP = N_Color-4 ))
        endelse
    endelse
endelse 

mindata = Tmin
maxdata = Tmax

return, color
end
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; -----------------------------------------------------------------------------
;
;  Copyright (C) 1997-2012  Krzysztof M. Gorski, Eric Hivon, Anthony J. Banday
;
;
;
;
;
;  This file is part of HEALPix.
;
;  HEALPix is free software; you can redistribute it and/or modify
;  it under the terms of the GNU General Public License as published by
;  the Free Software Foundation; either version 2 of the License, or
;  (at your option) any later version.
;
;  HEALPix is distributed in the hope that it will be useful,
;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;  GNU General Public License for more details.
;
;  You should have received a copy of the GNU General Public License
;  along with HEALPix; if not, write to the Free Software
;  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
;
;  For more information about HEALPix see http://healpix.jpl.nasa.gov
;
; -----------------------------------------------------------------------------
pro LS_proj2out, planmap, Tmax, Tmin, color_bar, dx, title_display, sunits, $
              coord_out, do_rot, eul_mat, planvec, vector_scale, $
              CHARSIZE=charsize, COLT=colt, CROP=crop, GIF = gif, GRATICULE = graticule, $
              HXSIZE = hxsize, NOBAR = nobar, NOLABELS = nolabels, NOPOSITION = noposition, $
              PREVIEW = preview, $
              PS = ps, PXSIZE=pxsize, PYSIZE=pysize, ROT=rot_ang, SUBTITLE = subtitle, $
              TITLEPLOT = titleplot, XPOS = xpos, YPOS = ypos, $
              POLARIZATION=polarization, $
              PNG = png, OUTLINE = outline, $
              PROJECTION=projection, MOLLWEIDE=mollweide, GNOMIC=gnomic, CARTESIAN=cartesian, $
              ORTHOGRAPHIC=orthographic, FLIP=flip, HALF_SKY=half_sky,COORD_IN=coord_in, $
              IGRATICULE = igraticule, HBOUND = hbound, DIAMONDS = diamonds, WINDOW = window_user, $
              TRANSPARENT = transparent, EXECUTE=execute, SILENT=silent, GLSIZE=glsize, IGLSIZE=iglsize, $
              SHADEMAP=SHADEMAP, RETAIN=retain, TRUECOLORS=truecolors, CHARTHICK=charthick, $
              STAGGER=stagger, AZEQ=azeq, JPEG=jpeg, $
              CTDIR=CTDIR, CTFILE=CTFILE, GRMIN=GRMIN, GRMAX=GRMAX, GRLS=GRLS, IGRMIN=IGRMIN, IGRMAX=IGRMAX, IGRLS=IGRLS, $
              CBLBL=CBLBL, CBLIN=CBLIN, CBTICKS=CBTICKS, CBTICKVAL=CBTICKVAL, CBTICKLBL=CBTICKLBL, CBTICKLAB=CBTICKLAB, CBOUT=CBOUT, $
              MODASINH=MODASINH, HIST_EQUAL=HIST_EQUAL, ASINH=ASINH, LOG=LOG, LATLONGDIFF=LATLONGDIFF, CBLABOFF=CBLABOFF, $
              GNMCORDOFF=GNMCORDOFF, GNMOFF=GNMOFF, FNTsz=FNTsz, CBOFF=CBOFF, MOLOFF=MOLOFF, CRTCORDOFF=CRTCORDOFF, CRTOFF=CRTOFF

;===============================================================================
;+
;  PROJ2OUT
;  ouputs on X-screen or PS, GIF, PNG or JPEG file a gnomonic,
;  mollweide, cartesian, orthographic or azimuth equidistant map
;
;  IN:
;    planmap, Tmax, Tmin, color_bar, dx, title_display, sunits, 
;    coord_out, do_rot, eul_mat, planvec, vector_scale
;
;  KEYWORDS:
;     CHARSIZE=charsize, COLT=colt, CROP=crop, GIF = gif, GRATICULE = graticule, HXSIZE = hxsize, $
;              NOBAR = nobar, NOLABELS = nolabels, NOPOSITION = noposition, $
;              PREVIEW = preview, PS = ps, $
;              PXSIZE=pxsize, PYSIZE=pysize, ROT = rot_ang, SUBTITLE = subtitle, $
;              TITLEPLOT = titleplot, XPOS = xpos, YPOS = ypos, $
;              POLARIZATION=polarization,$
;              PNG=png, OUTLINE = outline,$
;              PROJECTION=projection, MOLLWEIDE=mollweide, $
;              GNOMIC=gnomic, CARTESIAN=cartesian,
;              ORTHOGRAPHIC=orthographic, $
;              FLIP=flip, HALF_SKY=half_sky,COORD_IN=coord_in, IGRATICULE=,
;              HBOUND=, DIAMONDS =, WINDOW =, TRANSPARENT=, EXECUTE=, SILENT=
;              GLSIZE=, IGLSIZE=, SHADEMAP=, STAGGER=, AZEQ=, JPEG=
;
;   for more information, see Gnomview.pro Mollview.pro
;
;   March 1999, EH
;   Nov 2000, EH, plot polarisation
;   May 2002, EH, merge gnom2out and moll2out
;   Jun 2002, EH, added the cartesian projection facility (hacked from
;       G. Giardino's pol2out)
;   Aug 2002, EH, added the orthographic projection facility
;   Jul 2002, EH, changed vector field loop index to LONG
;   Jan 2007, EH, added window keyword
;   Sep 2007, EH, the /CROP-ped image now include graticules, ...,
;   added /TRANSPARENT, EXECUTE=, /SILENT
;   May 2009, EH, added SHADEMAP (shade for orthographic PNG output)
;                 a single call to tvrd()
;                 uses Z buffer when window<0
;                 introduce RETAIN
;   Sep 2009, EH, TRANSPARENT can now be in {0,1,2,3}
;   Nov 2009, EH, retrofitting for GDL. 
;                 Everything works as in IDL except PS outputs and
;                 transparent pixels in PNG
;   Mar 2010, EH, corrected bug with use_z_buffer
;   Apr 2010, EH, accepts array of OUTLINE;
;                  supports CHARTHICK
;   Jan 2012, EH, turns off GRAT, IGRAT, HBOUND, OUTLINE when STAGGER is set
;                 added support of AZEQ and JPEG
;
;
; 2 problems with write_png,...,/transparent in GDL:
;  - it is currently (v0.9.2) not supported
;  - it expects a pixel mask (ie an array of the same size as the image) with
;    values in [0,255] setting the opacity of each pixel
;   while the IDL version expects a color mask (ie, a vector of size 255) with
;   values in [0,255] setting the opacity of each color
;   
;   Jan 2013, L. Spencer: Added CTDIR and CTFILE to allow a custom colortable file to be used (i.e. the Planck `parchment1' colourtable).
;                         Added GRMIN and GRMAX keywords [and IGRMIN and IGRMAX] to limit labelling the graticules on the edges of a mollview map.
;                         Added GRLS and IGRLS keyowrds to over-ride the linestyle of the graticules and igraticules.  
;-
;===============================================================================
;
;WIN = STRCOMP(!VERSION.OS, 'Win',3)
;IF WIN EQ 1 THEN ds = '\' ELSE ds = '/'
IF N_ELEMENTS(CTFILE) EQ 0 THEN CTFILE = 'colors1.tbl'
IF N_ELEMENTS(CTDIR) GT 0 THEN ColTabFl = CTDIR+CTFILE ELSE ColTabFl = !DIR+'//resource//colors//'+CTFILE
;
IF N_ELEMENTS(GRLS) EQ 0 THEN GRLS = 0
IF N_ELEMENTS(IGRLS) EQ 0 THEN BEGIN
  IF KEYWORD_SET(GRATICULE) THEN IGRLS = 1 ELSE IGRLS = 0
ENDIF
IF N_ELEMENTS(CBLABOFF) EQ 0 THEN CBLABOFF = 0d
IF N_ELEMENTS(GNMCORDOFF) EQ 0 THEN GNMCORDOFF = 0d
IF N_ELEMENTS(GNMOFF) EQ 0 THEN GNMOFF = 0d
IF N_ELEMENTS(MOLOFF) EQ 0 THEN MOLOFF = 0d
IF N_ELEMENTS(CRTCORDOFF) EQ 0 THEN CRTCORDOFF = 0d
IF N_ELEMENTS(CRTOFF) EQ 0 THEN CRTOFF = 0d
IF N_ELEMENTS(FNTsz) EQ 0 THEN FNTsz = 8d
IF N_ELEMENTS(CBOFF) EQ 0 THEN CBOFF = 0d
;
identify_projection, projtype, projection=projection, mollweide=mollweide, gnomic=gnomic, cartesian=cartesian, orthographic=orthographic,  diamonds = diamonds, azeq=azeq
do_gnom = 0
do_moll = 0
do_cart = 0
do_orth = 0
do_azeq = 0
do_fullsky = 0 ; dummy, only matters for orthview
do_gif = keyword_set(gif)
do_png = keyword_set(png)
do_ps  = keyword_set(ps)
do_jpeg  = keyword_set(jpeg)
do_image = (do_gif || do_png || do_jpeg)
do_true = keyword_set(truecolors)
do_crop = keyword_set(crop)

if undefined(polarization) then polarization=0
do_polamplitude = (polarization[0] eq 1)
do_poldirection = (polarization[0] eq 2)
do_polvector    = (polarization[0] eq 3)
;-------------------------------------------------
in_gdl = is_gdl()
in_idl = ~in_gdl
;if (do_ps) then 
test_preview
@idl_default_previewer ; defines the paper size
if (do_ps and undefined(papersize)) then papersize = 'a4'

xsize = (size(planmap))(1)
ysize = (size(planmap))(2)

if (projtype eq 2) then begin
;  ---- Gnomonic specific definitions for the plot ----
    routine    = 'GNOMVIEW'
    proj_small = 'gnomic'
    proj_big   = 'Gnomic'
    do_gnom    = 1

;     du_dv = 1.                  ; aspect ratio
    du_dv = xsize/float(ysize)                  ; aspect ratio
    fudge = 1.00                ; 
    xc = (xsize-1)/2. & delta_x = (xsize-1 - xc)
    yc = (ysize-1)/2. & delta_y = (ysize-1 - yc)
; u and v range of the map
    umin = - dx * xc * fudge & umax = dx * xc * fudge
    vmin = - dx * yc * fudge & vmax = dx * yc * fudge
; position of the rectangle in the final window
    ;w_xll = 0.00 & w_xur = 1.00 & w_dx = w_xur - w_xll
    ;w_yll = 0.10 & w_yur = 0.90 & w_dy = w_yur - w_yll
    w_xll = 0.0 & w_xur = 1.0 & w_dx = w_xur - w_xll
    w_yll = 0.2 + GNMOFF & w_yur = 1.0 + GNMOFF & w_dy = w_yur - w_yll
    w_dx_dy = w_dx / w_dy       ; 1.4
; color bar, position, dimension
    ;cbar_dx = 1./3.
    ;cbar_dy = 1./70.
    cbar_dx = 4./5.
    cbar_dy = 1./24.
    cbar_xll = (1. - cbar_dx)/2.
    cbar_xur = (1. + cbar_dx)/2.
    ;cbar_yur = w_yll - cbar_dy
    cbar_yur = w_yll - cbar_dy*1.5d + CBOFF
    cbar_yll = cbar_yur - cbar_dy
; polarisation color ring, position, dimension
    cring_dx = 1./15.
    cring_dy = 1./15.
    cring_xll = .025
; cring_xur = .025 + cring_dx
; cring_yur = w_yll
; cring_yll = cring_yur - cring_dy
    cring_yll = .025
; location of astro. coordinate
    ;x_aspos = 0.5
    ;y_aspos = 0.04
    x_aspos = 0.5
    y_aspos = 0.01 + GNMCORDOFF
; location of pol vector scale
    vscal_x = 0.05
    vscal_y = 0.01
; location of title and subtitle
    x_title = 0.5 & y_title = 0.95
    x_subtl = 0.5 & y_subtl = 0.915
    if (do_ps) then begin
; default X dimension of hardcopy (cm)
        hxsize_def = 15.
; offset along the long axis of the page
        yoffset = (papersize eq 'a4') ? 2 : 1
;yoffset = 2  ; Europe (A4)
;yoffset = 1                 ; US (letter)
    endif
endif

if (projtype eq 1) then begin
;  ---- Mollweide specific definitions for the plot ----
    routine    = 'MOLLVIEW'
    proj_big   = 'Mollweide'
    proj_small = 'mollweide'
    do_moll    = 1

    du_dv = 2.                  ; aspect ratio
    fudge = 1.02                ; spare some space around the Mollweide egg
    xc = 0.5*(xsize-1) & delta_x = (xsize-1 - xc)
    yc = 0.5*(ysize-1) & delta_y = (ysize-1 - yc)
; x and y range of egg
    umin = - du_dv * fudge & umax = du_dv * fudge
    vmin = - fudge         & vmax =         fudge
; position of the egg in the final window
    ;w_xll = 0.0 & w_xur = 1.0 & w_dx = w_xur - w_xll
    ;w_yll = 0.1 & w_yur = 0.9 & w_dy = w_yur - w_yll
    w_xll = 0.0 & w_xur = 1.0 & w_dx = w_xur - w_xll
    w_yll = 0.15 + MOLOFF & w_yur = 0.95 + MOLOFF & w_dy = w_yur - w_yll
    w_dx_dy = w_dx / w_dy       ; 1./.8
; color bar, position, dimension
    ;cbar_dx = 1./3.
    ;cbar_dy = 1./70.
    cbar_dx = 4./5. ; 2./3.
    cbar_dy = 1./24. ; 32.
    cbar_xll = (1. - cbar_dx)/2.
    cbar_xur = (1. + cbar_dx)/2.
    cbar_yur = w_yll - cbar_dy*1.5d + CBOFF
    cbar_yll = cbar_yur - cbar_dy
; polarisation color ring, position, dimension
    cring_dx = 1./10.
    cring_dy = 1./10.
    cring_xll = .025
; cring_xur = .025 + cring_dx
; cring_yur = w_yll
; cring_yll = cring_yur - cring_dy
    cring_yll = .025
; location of pol vector scale
    vscal_x = 0.05
    vscal_y = 0.02
; location of title and subtitle
    x_title = 0.5 & y_title = 0.95
    x_subtl = 0.5 & y_subtl = 0.905
    if (do_ps) then begin
; default X dimension of hardcopy (cm)
        hxsize_def = 26.
; offset along the long axis of the page
        yoffset = (papersize eq 'a4') ? 2 : 1
    ;yoffset = 2  ; Europe (A4)
    ;yoffset = 1                 ; US (letter)
    endif
endif

if (projtype eq 5) then begin
;  ---- Diamonds specific definitions for the plot ----
    routine    = 'DIAMONDS'
    proj_big   = 'Diamonds'
    proj_small = 'diamonds'
    do_moll    = 1

    du_dv = 2.                  ; aspect ratio
    fudge = 1.00                ; spare some space around the 12-Diamonds
    xc = 0.5*(xsize-1) & delta_x = (xsize-1 - xc)
    yc = 0.5*(ysize-1) & delta_y = (ysize-1 - yc)
; x and y range of egg
    umin = - du_dv * fudge & umax = du_dv * fudge
    vmin = - fudge         & vmax =         fudge
; position of the egg in the final window
    w_xll = 0.0 & w_xur = 1.0 & w_dx = w_xur - w_xll
    w_yll = 0.1 & w_yur = 0.9 & w_dy = w_yur - w_yll
    w_dx_dy = w_dx / w_dy       ; 1./.8
; color bar, position, dimension
    cbar_dx = 1./3.
    cbar_dy = 1./70.
    cbar_xll = (1. - cbar_dx)/2.
    cbar_xur = (1. + cbar_dx)/2.
    cbar_yur = w_yll - cbar_dy + CBOFF
    cbar_yll = cbar_yur - cbar_dy
; polarisation color ring, position, dimension
    cring_dx = 1./10.
    cring_dy = 1./10.
    cring_xll = .025
; cring_xur = .025 + cring_dx
; cring_yur = w_yll
; cring_yll = cring_yur - cring_dy
    cring_yll = .025
; location of pol vector scale
    vscal_x = 0.05
    vscal_y = 0.02
; location of title and subtitle
    x_title = 0.5 & y_title = 0.95
    x_subtl = 0.5 & y_subtl = 0.905
    if (do_ps) then begin
; default X dimension of hardcopy (cm)
        hxsize_def = 26.
; offset along the long axis of the page
        yoffset = (papersize eq 'a4') ? 2 : 1
    ;yoffset = 2  ; Europe (A4)
    ;yoffset = 1                 ; US (letter)
    endif
endif

if (projtype eq 4) then begin
;  ---- Orthographic specific definitions for the plot ----
    routine    = 'ORTHVIEW'
    proj_big   = 'Orthographic'
    proj_small = 'orthographic'
    do_orth    = 1
    
    if keyword_set(half_sky) then do_fullsky = 0 else do_fullsky = 1
    if (do_fullsky) then du_dv = 2. else du_dv = 1. ; aspect ratio

    fudge = 1.02                ; spare some space around the Orthographic disc
    xc = 0.5*(xsize-1) & delta_x = (xsize-1 - xc)
    yc = 0.5*(ysize-1) & delta_y = (ysize-1 - yc)
; x and y range of disc
    umin = - du_dv * fudge & umax = du_dv * fudge
    vmin = - fudge         & vmax =         fudge
; position of the disc in the final window
    w_xll = 0.0 & w_xur = 1.0 & w_dx = w_xur - w_xll
    w_yll = 0.1 & w_yur = 0.9 & w_dy = w_yur - w_yll
    w_dx_dy = w_dx / w_dy       ; 1./.8
; color bar, position, dimension
    cbar_dx = 1./3.
    cbar_dy = 1./70.
    cbar_xll = (1. - cbar_dx)/2.
    cbar_xur = (1. + cbar_dx)/2.
    cbar_yur = w_yll - cbar_dy + CBOFF
    cbar_yll = cbar_yur - cbar_dy
; polarisation color ring, position, dimension
    cring_dx = 1./10.
    cring_dy = 1./10.
    cring_xll = .025
; cring_xur = .025 + cring_dx
; cring_yur = w_yll
; cring_yll = cring_yur - cring_dy
    cring_yll = .025
; location of pol vector scale
    vscal_x = 0.05
    vscal_y = 0.02
; location of title and subtitle
    x_title = 0.5 & y_title = 0.95
    x_subtl = 0.5 & y_subtl = 0.905
    if (do_ps) then begin
; default X dimension of hardcopy (cm)
        hxsize_def = 26.
; offset along the long axis of the page
        yoffset = (papersize eq 'a4') ? 2 : 1
    ;yoffset = 2  ; Europe (A4)
    ;yoffset = 1                 ; US (letter)
    endif
endif

if (projtype eq 3) then begin
    ;------------ cartesion projection ----------------
    routine    = 'CARTVIEW'
    proj_small = 'cartesian'
    proj_big   = 'Cartesian'
    do_cart    = 1
    
;     du_dv = 1.                  ; aspect ratio
    du_dv = xsize/float(ysize)                  ; aspect ratio
    fudge = 1.00                ; 
    xc = (xsize-1)/2. & delta_x = (xsize-1 - xc)
    yc = (ysize-1)/2. & delta_y = (ysize-1 - yc)
; u and v range of the map
    umin = - dx * xc * fudge & umax = dx * xc * fudge
    vmin = - dx * yc * fudge & vmax = dx * yc * fudge
; position of the rectangle in the final window
    w_xll = 0.00 & w_xur = 1.00 & w_dx = w_xur - w_xll
    w_yll = 0.20 + CRTOFF & w_yur = 1.0 + CRTOFF & w_dy = w_yur - w_yll
    w_dx_dy = w_dx / w_dy       ; 1.4
; color bar, position, dimension
    ;cbar_dx = 1./3.
    ;cbar_dy = 1./70.
    cbar_dx = 4./5.
    cbar_dy = 1./24.
    cbar_xll = (1. - cbar_dx)/2.
    cbar_xur = (1. + cbar_dx)/2.
    ;cbar_yur = w_yll - cbar_dy
    cbar_yur = w_yll - cbar_dy*1.5d + CBOFF
    cbar_yll = cbar_yur - cbar_dy
; polarisation color ring, position, dimension
    cring_dx = 1./15.
    cring_dy = 1./15.
    cring_xll = .025
    cring_yll = .025
; location of astro. coordinate
    x_aspos = 0.5
    y_aspos = 0.04 + CRTCORDOFF
; pol vector scale
    vscal_x = 0.05
    vscal_y = 0.01
; location of title and subtitle
    x_title = 0.5 & y_title = 0.95
    x_subtl = 0.5 & y_subtl = 0.915
    if (do_ps) then begin
; default X dimension of hardcopy (cm)
        hxsize_def = 15.
; offset along the long axis of the page
        yoffset = (papersize eq 'a4') ? 2 : 1
    ;yoffset = 2  ; Europe (A4)
    ;yoffset = 1                 ; US (letter)
    endif
endif

if (projtype eq 6) then begin
    ;------------ azimuthal equidistant projection ----------------
    routine    = 'AZEQVIEW'
    proj_small = 'azimequid'
    proj_big   = 'AzimEquidistant'
    do_cart    = 1
    
;     du_dv = 1.                  ; aspect ratio
    du_dv = xsize/float(ysize)                  ; aspect ratio
    fudge = 1.00                ; 
    xc = (xsize-1)/2. & delta_x = (xsize-1 - xc)
    yc = (ysize-1)/2. & delta_y = (ysize-1 - yc)
; u and v range of the map
    umin = - dx * xc * fudge & umax = dx * xc * fudge
    vmin = - dx * yc * fudge & vmax = dx * yc * fudge
; position of the rectangle in the final window
    w_xll = 0.00 & w_xur = 1.00 & w_dx = w_xur - w_xll
    w_yll = 0.10 & w_yur = 0.90 & w_dy = w_yur - w_yll
    w_dx_dy = w_dx / w_dy       ; 1.4
; color bar, position, dimension
    cbar_dx = 1./3.
    cbar_dy = 1./70.
    cbar_xll = (1. - cbar_dx)/2.
    cbar_xur = (1. + cbar_dx)/2.
    cbar_yur = w_yll - cbar_dy + CBOFF
    cbar_yll = cbar_yur - cbar_dy
; polarisation color ring, position, dimension
    cring_dx = 1./15.
    cring_dy = 1./15.
    cring_xll = .025
    cring_yll = .025
; location of astro. coordinate
    x_aspos = 0.5
    y_aspos = 0.04
; pol vector scale
    vscal_x = 0.05
    vscal_y = 0.01
; location of title and subtitle
    x_title = 0.5 & y_title = 0.95
    x_subtl = 0.5 & y_subtl = 0.915
    if (do_ps) then begin
; default X dimension of hardcopy (cm)
        hxsize_def = 15.
; offset along the long axis of the page
        yoffset = (papersize eq 'a4') ? 2 : 1
    ;yoffset = 2  ; Europe (A4)
    ;yoffset = 1                 ; US (letter)
    endif
endif
;====================================================

do_shade = (do_orth && defined(shademap))
; set color table and character size
ct          = defined(colt)     ? colt     : 33
charsfactor = defined(charsize) ? charsize : 1.0
mycharthick = defined(charthick)? charthick : 1.0
be_verbose  = ~keyword_set(silent)

; alter the color table
; -----------------------
if (be_verbose) then print,'... computing the color table ...'
if (do_true) then begin
    loadct, 0, /silent, FILE=ColTabFl
    tvlct,red,green,blue,/get
endif else begin
    if (do_poldirection) then begin
        LOADCT, 0, /SILENT, FILE=ColTabFl
        ncol = 256
        one = replicate(1.,ncol)
        tvlct,[0,0,0,findgen(ncol-3)]/(ncol-3)*720,one,one,/hsv ; hue is in degrees
    endif else begin
        loadct, get_names = color_names, FILE=ColTabFl
        nmax_col = n_elements(color_names)-1
        if (abs(ct) le nmax_col) then begin
            LOADCT, abs(ct), /SILENT, FILE=ColTabFl
        endif else begin
            if (be_verbose) then print,'... color table '+strtrim(abs(ct),2)+' unknown, using current color table instead ...'
        endelse
    endelse
    tvlct,red,green,blue,/get
    if (ct lt 0) then begin
        red = reverse(red) & green = reverse(green) & blue = reverse(blue)
    endif
endelse
; set up some specific definitions
; reserve first colors for Black, White and Neutral grey
idx_black = 0B & idx_white = 1B   & idx_grey = 2B   & idx_bwg = [idx_black, idx_white, idx_grey]
col_black = 0B & col_white = 255B & col_grey = 175B & col_bwg = [col_black, col_white, col_grey]
red  [idx_bwg] = col_bwg
green[idx_bwg] = col_bwg
blue [idx_bwg] = col_bwg
TVLCT,red,green,blue

; ---------------------
; open the device
; ---------------------
old_device=!d.name
my_background = !p.background
my_color = !p.color
if (be_verbose) then print,'... here it is.'
titlewindow = proj_big+' projection : ' + title_display
back      = REPLICATE(BYTE(!P.BACKGROUND),xsize,(ysize*cbar_dy*w_dx_dy)>1)
use_z_buffer = 0 ; set it to 0 (for postscript) 2010-03-18
if (do_ps) then begin
    ; 2009-11-04: 'ps' in GDL does not support: COLOR, BITS, XSIZE, ...
    if DEFINED(hxsize) then hxsize = (hxsize > 3) < 200 else hxsize = hxsize_def
    if ((size(ps))(1) ne 7) then file_ps = 'plot_'+proj_small+'.ps' else file_ps = ps
    SET_plot,'ps'
    do_portrait = 0
    do_landscape = 0
    DEVICE, FILE=file_ps, /COLOR, BITS = 8 ; opens the file that will receive the PostScript plot
    if (do_gnom || (do_orth && ~do_fullsky)) then begin
        do_portrait = 1
        ;DEVICE, /PORTRAIT,  XSIZE=hxsize, YSIZE=hxsize/du_dv*w_dx_dy, XOFFSET=4, YOFFSET=2
        DEVICE, /PORTRAIT,  XSIZE=hxsize, YSIZE=hxsize/du_dv*w_dx_dy, XOFFSET=4, YOFFSET=2, /ENCAPSULATED, /HELVETICA, FONT_size=FNTsz
    endif
    if (do_moll || (do_orth && do_fullsky)) then begin
        ;do_landscape = 1
        ;DEVICE, /LANDSCAPE, XSIZE=hxsize, YSIZE=hxsize/du_dv*w_dx_dy, XOFFSET=4, YOFFSET=hxsize+yoffset
        do_portrait = 1
        DEVICE, /PORTRAIT, XSIZE=hxsize, YSIZE=hxsize/du_dv*w_dx_dy, XOFFSET=4, YOFFSET=hxsize+yoffset, /ENCAPSULATED, /HELVETICA, FONT_size=FNTsz
    endif
    if (do_cart || do_azeq) then begin
        do_landscape = 1
;         DEVICE, /LANDSCAPE, XSIZE=hxsize, YSIZE=hxsize/du_dv*w_dx_dy, XOFFSET=4, YOFFSET=hxsize+yoffset
        ;DEVICE, /LANDSCAPE, XSIZE=hxsize, YSIZE=hxsize/du_dv*w_dx_dy, XOFFSET=0, YOFFSET=hxsize+yoffset
        ;DEVICE, /LANDSCAPE, XSIZE=hxsize, YSIZE=hxsize/du_dv*w_dx_dy, XOFFSET=0, YOFFSET=hxsize+yoffset, /ENCAPSULATED, /HELVETICA, FONT_size=FNTsz
        DEVICE, /PORTRAIT,  XSIZE=hxsize, YSIZE=hxsize/du_dv*w_dx_dy, XOFFSET=4, YOFFSET=2, /ENCAPSULATED, /HELVETICA, FONT_size=FNTsz
    endif
    TVLCT,red,green,blue
    thick_dev = 2. ; device dependent thickness factor
endif else begin ; X, png, gif or jpeg output
    idl_window = defined(window_user) ? window_user : 32 ; idl_window = 32 or window_user
    free_window    =  (idl_window gt 31) ; random  window if idl_window > 31
    virtual_window =  (idl_window lt 0)  ; virtual window if idl_window < 0
    reuse_window   =  (~free_window && ~virtual_window && !d.window eq idl_window && !d.x_size eq long(xsize) && !d.y_size eq long(ysize*w_dx_dy))
    use_z_buffer   = (virtual_window && do_image)
    window_retain  = defined(retain) ? retain : 2
    if (use_z_buffer) then begin
        character_size = [!d.x_ch_size,!d.y_ch_size]
        set_plot,'z'
        pixel_depth = (do_true) ? 24 : 8
        if (in_gdl) then begin
; unresolved GDL0.9.2 bug: set_character_size ignored
            device,set_resolution= [xsize, ysize*w_dx_dy], set_character_size=character_size,z_buffering=0
        endif else begin
            device,set_resolution= [xsize, ysize*w_dx_dy], set_character_size=character_size,z_buffering=0, set_pixel_depth=pixel_depth
        endelse
    endif
    ;;if (!D.NAME eq 'X') then  DEVICE, PSEUDO = 8 ; for Windows compatibility ;
    ;;commented out 2009-10-28
    ;to_patch = ((!d.n_colors GT 256) && do_image  && ~do_crop)
    ;to_patch = ((!d.n_colors GT 256) && do_image && in_idl)
    n_colors = !d.n_colors
    if (in_gdl && (!d.name eq 'X' || !d.name eq 'WIN')) then begin ; work-around for GDL0.9.2 bug (!d.n_colors gets correct only after first call to WINDOW)
        device,get_visual_depth=gvd
        n_colors = 2L^gvd
    endif
    to_patch = (n_colors GT 256 && do_image)
    if (in_gdl) then begin
        if (use_z_buffer) then device else device,decomposed=0 ; in GDL0.9.2, decomposed is only available (but ignored) when device='X', or unvalid when device='Z'
        if (to_patch) then loadct,0,/silent, FILE=ColTabFl ; emulate decomposed
    endif else begin
        device, decomposed = use_z_buffer || to_patch
    endelse
    if (reuse_window) then begin
        wset, idl_window
    endif else begin
        if (~use_z_buffer) then begin
            WINDOW, idl_window>0, FREE=free_window, PIXMAP=virtual_window, $
                    XSIZE = xsize, YSIZE = ysize*w_dx_dy, TITLE = titlewindow, $
                    XPOS=(in_gdl && undefined(xpos)) ? 0 : xpos, $
                    YPOS=(in_gdl && undefined(ypos)) ? 0 : ypos, $ ; bug correction 2009-12-05
                    RETAIN=window_retain
        endif
        if (~virtual_window && (!d.x_size lt long(xsize) || !d.y_size lt long(ysize*w_dx_dy))) then begin
            message_patch,level=-1,/info,'==========================================================='
            message_patch,level=-1,/info,'WARNING: Because of screen and window manager limitations,'
            message_patch,level=-1,/info,'         the actual window is not as large as expected !'
            message_patch,level=-1,/info,strtrim(!d.x_size,2)+'*'+  strtrim(!d.y_size,2)+'    <    '+  strtrim(long(xsize),2)+'*'+strtrim(long(ysize*w_dx_dy),2)
            message_patch,level=-1,/info,'         The result is unpredictable.'            
            message_patch,level=-1,/info,' If you are only interested in GIF/PNG/JPEG output, you can use a virtual window (WINDOW<0) instead'            
            message_patch,level=-1,/info,'==========================================================='
        endif
    endelse
    if (in_idl) then TVLCT,red,green,blue
    thick_dev = 1. ; device dependent thickness factor
endelse
!p.background = my_background
!p.color = my_color
; -------------------------------------------------------------
; make the plot
; -------------------------------------------------------------
myplot={urange:[umin,umax],vrange:[vmin,vmax],position:[w_xll,w_yll,w_xur,w_yur],xstyle:5,ystyle:5}
plot, /nodata, myplot.urange, myplot.vrange, pos=myplot.position, XSTYLE=myplot.xstyle, YSTYLE=myplot.ystyle
; Set the background when doing a plot in Z buffer
if (use_z_buffer) then begin
    l64ysize = long64(ysize*w_dx_dy)
    if ~do_true then begin 
        tv, replicate(!p.background, xsize, l64ysize)
    endif else begin
        back = [red[!p.background],green[!p.background],blue[!p.background]] ## replicate(1b, xsize*l64ysize)
        tv, reform(back, xsize, l64ysize, 3, /overwrite),true=3
        back=0
    endelse
endif
; ---------- projection independent ------------------
; map itself
if (do_shade && ~do_image) then begin
    ; shaded for X or PS
    image = planmap
    image3d  =   make_array(/uint, xsize, ysize, 3)
    image3d[*,*,0] = uint( (256. * red  [image] * shademap) < 65535.)
    image3d[*,*,1] = uint( (256. * green[image] * shademap) < 65535.)
    image3d[*,*,2] = uint( (256. * blue [image] * shademap) < 65535.)
    if (do_ps) then loadct,0,/silent, FILE=ColTabFl ; must be in grey-scale for TrueColor PS output
    TV, bytscl(image3d),w_xll,w_yll,/normal,xsize=1.,true=3
    if (do_ps) then tvlct,red,green,blue ; revert to custom color table
    image3d = 0
; endif else if (do_true  && ~do_image) then begin
endif else if (do_true) then begin
                                ; truecolors for X or PS, red, green, blue are
                                ; only useful for the {0,1,2}={Black, white, grey}
    image3d = make_array(/byte, xsize, ysize, 3)
    image3d[*,*,0] = red  [planmap[*,*,0]]
    image3d[*,*,1] = green[planmap[*,*,1]]
    image3d[*,*,2] = blue [planmap[*,*,2]]
;;;    if (do_ps) then loadct,0,/silent; must be in grey-scale for TrueColor PS output
    tv, image3d, w_xll,w_yll,/normal,xsize=1.,true=3
;;;    if (do_ps) then tvlct,red,green,blue ; revert to custom color table
    image3d = 0
endif else begin
    TV, planmap,w_xll,w_yll,/normal,xsize=1.
endelse
hpxv11 = 0

; the polarisation color ring
if (do_poldirection) then begin
    npring = xsize*cring_dx
    one = replicate(1.,npring)
    yy  = one # (findgen(npring) - npring/2)
    xx  = transpose(yy)
    tt  = (xx^2 + yy^2) / float(npring)^2
    mask = byte(tt lt .25 and tt gt 0.05)
    if (hpxv11) then begin
        ; angles are measured from horizontal
        angle = atan(yy,xx)  * !radeg + 180. ; angle in deg in [0,360]
    endif else begin
        ; angles are measured from vertical
        angle = atan(xx,-yy)  * !radeg + 180. ; angle in deg in [0,360]
    endelse
    color_ring = (bytscl(angle,TOP=252) + 3B) * mask + byte((1-mask)*!P.BACKGROUND); in [3,255] in ring and !p.background outside ring
    TV,color_ring,cring_xll,cring_yll,/normal,xsize = npring/float(xsize)
endif

; polarisation vector field
pgparam=[1.,1.]; [scale, grid_step] of grid of headless vector
if (do_polvector) then begin
    pgparam = ([polarization[*], 1., 1.])[1:2] ; 2nd and 3rd elements of polarization (default:[1,1])
    pgparam = pgparam*(pgparam gt 0) + (pgparam le 0) ; replace non-positive values by 1.
    dyg = 10.
    pol_rescale = float(dyg)/ysize * pgparam[0]
    dyg *= pgparam[1] & dxg = dyg
    xg = (lindgen(xsize/dxg)+.5) #  replicate(dxg, ysize/dyg)
    yg = (lindgen(ysize/dyg)+.5) ## replicate(dyg, xsize/dxg)
    u = umin + xg * (umax-umin) / xsize
    v = vmin + yg * (vmax-vmin) / ysize
    for i=0L, n_elements(xg)-1 do begin
        norm = planvec[xg[i],yg[i],0] * pol_rescale * (vmax-vmin)
        angle = planvec[xg[i],yg[i],1]
        if (hpxv11) then begin
            ; angles are measured from horizontal
            if (norm gt 0) then plots, u[i]+norm*cos(angle)*[-.5,.5], v[i]+norm*sin(angle)*[-.5,.5]
        endif else begin
            ; angles are measured from vertical
            if (norm gt 0) then plots, u[i]-norm*sin(angle)*[-.5,.5], v[i]+norm*cos(angle)*[-.5,.5]
        endelse
    endfor
    xg = 0 & yg = 0 & u = 0 & v = 0
endif

;  the color bar
cb_orig = color_bar
Ncb = N_ELEMENTS(color_bar)
cbran = MINMAX(color_bar)
lincb = DINDGEN(Ncb)/DOUBLE(Ncb - 1d)*(cbran[1] - cbran[0]) + cbran[0]
;      
IF KEYWORD_SET(CBLIN) THEN BEGIN
  if (~(keyword_set(nobar) || do_poldirection || do_true)) then begin
      color_bar = lincb
      ;
      color_bar_out = BYTE(CONGRID(color_bar,xsize*cbar_dx)) # REPLICATE(1.,(ysize*cbar_dy*w_dx_dy)>1)
      back(xsize*cbar_xll,0) = color_bar_out
      TV, back,0,cbar_yll,/normal,xsize = 1.
  endif
ENDIF ELSE BEGIN  ; keep the healpix default colourbar output
  if (~(keyword_set(nobar) || do_poldirection || do_true)) then begin
      color_bar_out = BYTE(CONGRID(color_bar,xsize*cbar_dx)) # REPLICATE(1.,(ysize*cbar_dy*w_dx_dy)>1)
      back(xsize*cbar_xll,0) = color_bar_out
      TV, back,0,cbar_yll,/normal,xsize = 1.
  endif
ENDELSE
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;  This code gets a translation from the temperature/data coordinates to the normalized colourbar/x coordinates.
;
cb_Ts_ = DINDGEN(Ncb)/DOUBLE(Ncb - 1d)*(TMAX - TMIN) + TMIN
cb_Y_ = DOUBLE(cb_orig - MIN(cb_orig))/MAX(cb_orig - MIN(cb_orig))*2d - 1d ; from -1 to 1 is the default
cb_Ts = DINDGEN(Ncb*1d3)/DOUBLE(Ncb*1d3 - 1d)*(TMAX - TMIN) + TMIN
cb_Y = INTERPOL(cb_Y_, cb_Ts_, cb_Ts, /SPLINE) ; interpolate onto a higher resolution grid 
IF KEYWORD_SET(MODASINH) THEN cb_Y  = MODASINH(cb_Ts)
IF KEYWORD_SET(ASINH) THEN cb_Y  = ASINH(cb_Ts)
IF KEYWORD_SET(HIST_EQUAL) THEN BEGIN
  cb_Y = (cb_Y + 1d)/2d
  ;Bs = (Tmax-Tmin)/5000. ;MIN( [(Tmax-Tmin)/5000.,2.] )
  ;      Phist = HISTOGRAM( data, Min=Tmin, Max=Tmax, Bin = Bs )
  ;      Phist = total(Phist,/cumul)
  ;      Phist[0] = 0.
  ;      Junk = INTERPOLATE( FLOAT(Phist), (data-Tmin)/Bs )
  ;
ENDIF
; rescale the cb_Y to the color bar width, I can then interpolate from data values to relative colorbar location.
cbYmm = MINMAX(cb_Y)
cbYneg = WHERE(cb_Y LT 0d, Nneg)
cbYpos = WHERE(cb_Y GE 0d, Npos)
;
cbar_Xmid = (cbar_xur + cbar_xll)/2d
cb_Y = (cb_Y - cbYmm[0])/(cbYmm[1] - cbYmm[0])*(cbar_xur - cbar_xll) + (cbar_xll) ; should now translate data units to colorbar x position
;IF Nneg GT 0 THEN cb_Y[cbYneg] = cb_Y[cbYneg]*(cbar_xll - cbar_Xmid)/cbYmm[0]
;IF Npos GT 0 THEN cb_Y[cbYpos] = cb_Y[cbYpos]*(cbar_xur - cbar_Xmid)/cbYmm[1]
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;stop
;
;  Outline the colourbar if desired
;
IF ~KEYWORD_SET(NOBAR) THEN BEGIN
  IF KEYWORD_SET(CBOUT) THEN plots, THICK=2, COLOR=0, [cbar_xll,cbar_xll,cbar_xur,cbar_xur,cbar_xll],[cbar_yll,cbar_yur, cbar_yur, cbar_yll,cbar_yll], /NORMAL
ENDIF
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;  the color bar labels
IF KEYWORD_SET(CBTICKS) THEN BEGIN  ; a set number of colourbar tick intervals was requested
  ; plot a linear colourbar even though the map scale is not linear.
  ;  the color bar labels
  if (~(keyword_set(nobar) || keyword_set(nolabels) || do_true || do_poldirection)) then begin
      format = '(g10.2)'
      if ((Tmax - Tmin) ge 50 and MAX(ABS([Tmax,Tmin])) le 1.e5) then format='(i8)'
      if ((Tmax - Tmin) ge 5  and MAX(ABS([Tmax,Tmin])) le 1.e2) then format='(f6.1)'
      strmin = STRING(Tmin,format=format)
      strmax = STRING(Tmax,format=format)
      ;
      ;  Decide how many orders of magnitude are spanned in the colourbar
      ;  
      Tran = [Tmin,Tmax]
      Texp = FLOOR(ALOG10(ABS(Tran)))
      ;
      cb_Lys = [cbar_yll,cbar_yur] ; the y range for colorbar ticks
      ;
      ;  A few cases for the colorbar lines, if CBTICKVAL is set (dead easy), if linear, logarithmic, ASINH/MODASINH, or histogram (not sure what to do for this one).
      ;
      IF KEYWORD_SET(CBTICKVAL) THEN BEGIN
        ;
        ;  I have the tick values, do I have labels for them?
        IF KEYWORD_SET(CBTICKLBL) THEN BEGIN
          ;
          ; Have both, this is straightforward
          cb_Tmajors = CBTICKVAL
          cb_Tstr    = CBTICKLBL
          ;
        ENDIF ELSE BEGIN
          ;
          ;  I have the values, but not the labels for them
          cb_Tmajors = CBTICKVAL
          cb_Tstr = STRING(CB_Tmajors, format=format)
          cb_Tstr = STRTRIM(cB_Tstr,2)
          ;
        ENDELSE
        ;
        CBTICKLBL = cb_Tstr
        ;
      ENDIF ;ELSE BEGIN
        ;
        ;  A few cases here to consider, one is just a simple linear scale, perhaps leave this for the CBTICKS=0 setting, imply linear values...
        ;
        IF ((KEYWORD_SET(MODASINH)) OR (KEYWORD_SET(ASINH))) THEN BEGIN
          ;  Do the positive colourbar lines
          ;
          IF Npos GT 0 THEN BEGIN
            cb_PosLineTminors = 0d  
            cb_PosLineTmajors = 0d
            cb_PosTstr = ' 0 '
            ;
            FOR ii = 0, Texp[1] - 0 DO cb_PosLineTminors = [cb_PosLineTminors,10d^DOUBLE(ii)*(DINDGEN(9) + 1d)]  
            FOR ii = 0, Texp[1]     DO cb_PosLineTmajors = [cb_PosLineTmajors,10d^DOUBLE(ii)]
            FOR ii = 0, Texp[1]     DO cb_PosTstr = [cb_PosTstr,' 10!U'+STRTRIM(STRING(ii),2)+'!N']
            ;
            Npos_ = N_ELEMENTS(cb_PosTstr)
            IF Npos_ GT 1 THEN cb_PosTstr[1] = '  1'
            IF Npos_ GT 2 THEN cb_PosTstr[2] = '10'
            ;
            IF Tmin GT 0d THEN BEGIN
              ;
              Npos_min = N_ELEMENTS(cb_PosLineTminors)
              Npos_maj = N_ELEMENTS(cb_PosLineTmajors)
              IF Npos_min GT 1 THEN cb_PosLineTminors = cb_PosLineTminors[1:*]  ; get rid of zero entry as it is not within the plot range
              IF Npos_maj GT 1 THEN cb_PosLineTmajors = cb_PosLineTmajors[1:*]
              IF Npos_maj GT 1 THEN cb_PosTstr = cb_PosTstr[1:*]
              ;
            ENDIF 
            ;
            cb_Tminors = cb_PosLineTminors
            cb_Tmajors = cb_PosLineTmajors
            cb_Tstr = cb_PosTstr
            ;
          ENDIF
          ;
          ;  Do the negative color_bar lines, if any
          ;  
          IF Nneg GT 0 THEN BEGIN
            cb_NegLineTminors = 0d  
            cb_NegLineTmajors = 0d
            cb_NegTstr = '-1  '
            ;
            FOR ii = 0, Texp[0] - 0 DO cb_NegLineTminors = [cb_NegLineTminors,10d^DOUBLE(ii)*(DINDGEN(9) + 1d)*(-1d)]  
            FOR ii = 0, Texp[0]     DO cb_NegLineTmajors = [cb_NegLineTmajors,10d^DOUBLE(ii)*(-1d)]
            FOR ii = 0, Texp[0]     DO cb_NegTstr = [cb_NegTstr,'-10!U'+STRTRIM(STRING(ii),2)+'!N ']
            ;
            Nneg_min = N_ELEMENTS(cb_NegLineTminors)
            Nneg_maj = N_ELEMENTS(cb_NegLineTmajors)
            IF Nneg_min GT 1 THEN cb_NegLineTminors = cb_NegLineTminors[1:*]  ; get rid of zero entry as it will be included in positive side (if at all)
            IF Nneg_maj GT 1 THEN cb_NegLineTmajors = cb_NegLineTmajors[1:*]
            IF Nneg_maj GT 1 THEN cb_NegTstr = cb_NegTstr[1:*]
            ;
            Nneg_ = N_ELEMENTS(cb_NegLineTmajors)
            IF Nneg_ GT 0 THEN cb_NegTstr[0] = '-1   '
            IF Nneg_ GT 1 THEN cb_NegTstr[1] = '-10 '
            ;
            cb_NegLineTminors = REVERSE(cb_NegLineTminors)
            cb_NegLineTmajors = REVERSE(cb_NegLineTmajors)
            cb_NegTstr = REVERSE(cb_NegTstr)
            ;
            cb_Tminors = [cb_NegLineTminors, cb_PosLineTminors]
            cb_Tmajors = [cb_NegLineTmajors, cb_PosLineTmajors]
            cb_Tstr = [cb_NegTstr, cb_PosTstr]
            ;
            ;
          ENDIF
          ;
        ENDIF ELSE BEGIN ; the CB ticks for the modasinh and asinh color scalings are done above, do the linear case below
          ;
          IF KEYWORD_SET(HIST_EQUAL) THEN BEGIN
            ;
            ;  The code below is wrong, I need to fix this.
            ;Nticks = 3
            ;cb_ymarks = MINMAX(CB_Y)
            ;cb_Ymarks = [cb_ymarks[0], 0.5d, cb_ymarks[1]]
            ;cb_Tmajors = INTERPOL(cb_Ts, cb_Y, cb_Ymarks)
            ;cb_Tmajors[0] = Tmin
            ;cb_Tmajors[2] = Tmax
            ;;cb_Tmajors = DINDGEN(Nticks)/DOUBLE(Nticks - 1d)*(Tmax - Tmin) + Tmin
            ;cb_Tstr = STRING(CB_Tmajors, format=format)
            ;cb_Tstr = STRTRIM(cb_Tstr,2)
            ;negT = WHERE(cb_Tmajors LT 0d, NnegT)
            ;IF NnegT GT 0 THEN cb_Tstr[negT] = cb_Tstr[negT]+' '
            ;
            cb_Tmajors = [Tmin, Tmax]
            cb_Tstr = STRING(CB_Tmajors, format=format)
            cb_Tstr = STRTRIM(cb_Tstr,2)
            negT = WHERE(cb_Tmajors LT 0d, NnegT)
            IF NnegT GT 0 THEN cb_Tstr[negT] = cb_Tstr[negT]+' '
            ;
          ENDIF ELSE BEGIN  ; linear or log scaling now.  
            ;  
            ;  Assume 5 ticks, linearly spaced.
            IF CBTICKS GT 1 THEN Nticks = CBTICKS ELSE Nticks = 5
            cb_Tmajors = DINDGEN(Nticks)/DOUBLE(Nticks - 1d)*(Tmax - Tmin) + Tmin
            cb_Tstr = STRING(CB_Tmajors, format=format)
            cb_Tstr = STRTRIM(cb_Tstr,2)
            negT = WHERE(cb_Tmajors LT 0d, NnegT)
            IF NnegT GT 0 THEN cb_Tstr[negT] = cb_Tstr[negT]+' '
            ;
            ZeroFind = WHERE(cb_Tmajors EQ 0d, Nzero)
            IF Nzero GT 0 THEN cb_Tstr[ZeroFind] = ' '+cb_Tstr[ZeroFind]+' '
            ;
          ENDELSE
          ;
        ENDELSE 
      ;ENDELSE  ; I now have cb_Tmajors, cb_Tstr, and possibly cb_Tminors
      ;
      IF ~KEYWORD_SET(CBTICKVAL) THEN CBTICKVAL = cb_Tmajors
      IF ~KEYWORD_SET(CBTICKLBL) THEN CBTICKLBL = cb_Tstr
      ;
      cb_Tmajors = CBTICKVAL
      cb_Tstr = CBTICKLBL
      ;
      ;  Check the range of the majors against the Tmin and Tmax values
      ;
      N_minor = N_ELEMENTS(cb_Tminors)
      ;
      IF N_minor GT 0 THEN BEGIN
        GoodcbTs = WHERE(((cb_Tminors GE Tmin) AND (cb_Tminors LE Tmax)),NgoodcbTs)
        IF NgoodcbTs GT 0 THEN BEGIN
          cb_Tminors = cb_Tminors[GoodcbTs]
        ENDIF
        cb_TYminors = INTERPOL(cb_Y, cb_Ts, cb_Tminors)
      ENDIF
      ;  
      GoodcbTs = WHERE(((cb_Tmajors GE Tmin) AND (cb_Tmajors LE Tmax)),NgoodcbTs)
      IF NgoodcbTs GT 0 THEN BEGIN
        cb_Tmajors = cb_Tmajors[GoodcbTs]
        cb_Tstr    = cb_Tstr[GoodcbTs]
      ENDIF
      ;
      IF MIN(cb_Tmajors) GT Tmin THEN BEGIN  ; I may not want to do this due to formatting issues.
        cb_Tmajors = [Tmin,cb_Tmajors]
        cb_Tstr = [strmin,cb_Tstr]
      ENDIF
      ;
      IF MAX(cb_Tmajors) LT Tmax THEN BEGIN  ; I may not want to do this due to formatting issues.
        cb_Tmajors = [cb_Tmajors,Tmax]
        cb_Tstr = [cb_Tstr,strmax]
      ENDIF
      ;
      cb_TYmajors = INTERPOL(cb_Y, cb_Ts, cb_Tmajors)
      ;
      ;  Now print them on the plot
      N_minor = N_ELEMENTS(cb_Tminors)
      N_major = N_ELEMENTS(cb_Tmajors)
      ;
      IF N_minor GT 0 THEN FOR ii = 0, N_minor - 1 DO plots, THICK=1, COLOR=0, (cb_TYminors)[[ii,ii]], cb_Lys, /NORMAL  ;
      IF N_major GT 0 THEN FOR ii = 0, N_major - 1 DO plots, THICK=2, COLOR=0, (cb_TYmajors)[[ii,ii]], cb_Lys, /NORMAL  ;
        ;
        ;XYOUTS, cbar_xll, cbar_yll,'!6'+STRTRIM(strmin,2)+' ',$
        ;        ALIGN=1.,/normal, chars=1.3*charsfactor, charthick=mycharthick
        ;XYOUTS, cbar_xur, cbar_yll,'!6 '+STRTRIM(strmax,2)+' '+sunits,$
        ;        ALIGN=0.,/normal, chars=1.3*charsfactor, charthick=mycharthick
        ;  LSedit to get colorbar label to work with latex/ psfrag
      IF KEYWORD_SET(CBTICKLAB) THEN BEGIN  ; also add the tick labels to the colourbar on the plot
        ;
        IF N_major GT 0 THEN BEGIN
          FOR ii = 0, N_major - 1 DO BEGIN
            IF !D.name EQ 'PS' THEN device, /HELVETICA, FONT_size=FNTsz
            XYOUTS, CHARTHICK=mycharthick, ALIGN=0.5, /NORMAL, chars=1.3*charsfactor, COLOR=0, $
              cb_TYmajors[ii], cbar_yur + (cbar_dy/2d)*0.6d, cb_Tstr[ii] ; - DOUBLE(!D.Y_CH_SIZE)/DOUBLE(!D.Y_SIZE)
          ENDFOR
        ENDIF
        IF KEYWORD_SET(CBLBL) THEN BEGIN  ; there is a unit label
          IF !D.name EQ 'PS' THEN device, /HELVETICA, FONT_size=FNTsz
          XYOUTS, Cbar_xll + cbar_dx/2d, cbar_yll - cbar_dy/2d - DOUBLE(!D.Y_CH_SIZE)/DOUBLE(!D.Y_SIZE) + CBLABOFF, CBLBL, $
                ALIGN=0.5,/NORMAL, chars=1.3*charsfactor, charthick=mycharthick 
        ENDIF
        ;
      ENDIF ELSE BEGIN ; only label the end points.
        ;
        IF KEYWORD_SET(CBLBL) THEN BEGIN  ; there is a unit label
          IF !D.name EQ 'PS' THEN device, /HELVETICA, FONT_size=FNTsz
          XYOUTS, Cbar_xll + cbar_dx/2d, cbar_yll - cbar_dy/2d - DOUBLE(!D.Y_CH_SIZE)/DOUBLE(!D.Y_SIZE) + CBLABOFF, CBLBL, $
                ALIGN=0.5,/NORMAL, chars=1.3*charsfactor, charthick=mycharthick 
          XYOUTS, cbar_xll, cbar_yll,'!X'+STRTRIM(strmin,2)+' ',$
                ALIGN=1.,/normal, chars=1.3*charsfactor, charthick=mycharthick
          XYOUTS, cbar_xur, cbar_yll,' '+STRTRIM(strmax,2)+' ',$
                ALIGN=0.,/normal, chars=1.3*charsfactor, charthick=mycharthick
        ENDIF ELSE BEGIN                   ;  there is not a unit label
          IF !D.name EQ 'PS' THEN device, /HELVETICA, FONT_size=FNTsz
          XYOUTS, cbar_xll, cbar_yll,'!X'+STRTRIM(strmin,2)+' ',$
                ALIGN=1.,/normal, chars=1.3*charsfactor, charthick=mycharthick
          XYOUTS, cbar_xur, cbar_yll,' '+STRTRIM(strmax,2)+' '+sunits,$
                ALIGN=0.,/normal, chars=1.3*charsfactor, charthick=mycharthick
        ENDELSE
      ENDELSE
  endif
ENDIF ELSE BEGIN  ; just plot the edge values, do not place ticks on the colorbar
  if (~(keyword_set(nobar) || keyword_set(nolabels) || do_true || do_poldirection)) then begin
      format = '(g10.2)'
      if ((Tmax - Tmin) ge 50 and MAX(ABS([Tmax,Tmin])) le 1.e5) then format='(i8)'
      if ((Tmax - Tmin) ge 5  and MAX(ABS([Tmax,Tmin])) le 1.e2) then format='(f6.1)'
      strmin = STRING(Tmin,format=format)
      strmax = STRING(Tmax,format=format)
      ;XYOUTS, cbar_xll, cbar_yll,'!6'+STRTRIM(strmin,2)+' ',$
      ;        ALIGN=1.,/normal, chars=1.3*charsfactor, charthick=mycharthick
      ;XYOUTS, cbar_xur, cbar_yll,'!6 '+STRTRIM(strmax,2)+' '+sunits,$
      ;        ALIGN=0.,/normal, chars=1.3*charsfactor, charthick=mycharthick
      ;  LSedit to get colorbar label to work with latex/ psfrag
      IF KEYWORD_SET(CBLBL) THEN BEGIN
        XYOUTS, cbar_xll, cbar_yll,'!X'+STRTRIM(strmin,2)+' ',$
              ALIGN=1.,/normal, chars=1.3*charsfactor, charthick=mycharthick
        XYOUTS, cbar_xur, cbar_yll,' '+STRTRIM(strmax,2)+' ',$
              ALIGN=0.,/normal, chars=1.3*charsfactor, charthick=mycharthick
        XYOUTS, Cbar_xll + cbar_dx/2d, cbar_yll - cbar_dy/2d - DOUBLE(!D.Y_CH_SIZE)/DOUBLE(!D.Y_SIZE), '!X'+CBLBL, $
              ALIGN=0.5,/NORMAL, chars=1.3*charsfactor, charthick=mycharthick 
      ENDIF ELSE BEGIN
        XYOUTS, cbar_xll, cbar_yll,'!X'+STRTRIM(strmin,2)+' ',$
              ALIGN=1.,/normal, chars=1.3*charsfactor, charthick=mycharthick
        XYOUTS, cbar_xur, cbar_yll,'!X '+STRTRIM(strmax,2)+' '+sunits,$
              ALIGN=0.,/normal, chars=1.3*charsfactor, charthick=mycharthick
        ;XYOUTS, Cbar_xll + cbar_dx/2d, cbar_yll - cbar_dy*2d, sunits, $
        ;      ALIGN=0.5,/NORMAL, chars=1.3*charsfactor, charthick=mycharthick 
      ENDELSE    
  endif
ENDELSE

;ENDELSE

; the polarisation vector scale
if (~keyword_set(nobar)  && do_polvector) then begin
    vp_plot = 5*pol_rescale[0] /pgparam[0]; length of ruler on plot
    vp_phys = 5*vector_scale[0]/pgparam[0] ; 'physical' length of ruler
    plots, vscal_x*[1,1], vscal_y+[0, vp_plot]*w_dy, /normal
    format = '(g10.2)'
    if (vp_phys lt 1.e3 && vp_phys ge 10)    then format = '(f5.1)'
    if (vp_phys lt 10   && vp_phys gt 1.e-1) then format = '(f4.2)'
    xyouts, vscal_x, vscal_y + .5*(vp_plot)*w_dy, '!6  '+strtrim(string(vp_phys,form=format),2)+' '+sunits+'!X',ALIGN=0.,/normal, chars=1.1*charsfactor, charthick=mycharthick
endif

;  the title
if (~ keyword_set(titleplot)) then title= '!6'+title_display+'!X' else title='!6'+titleplot+'!X'
XYOUTS, x_title, y_title ,title, align=0.5, /normal, chars=1.6*charsfactor, charthick=mycharthick

;  the subtitle
if (keyword_set(subtitle)) then begin
    XYOUTS, x_subtl, y_subtl ,'!6 '+subtitle+'!X', align=0.5, /normal, chars=1.6*.75*charsfactor, charthick=mycharthick
endif

; ---------- projection dependent ------------------

if (do_gnom) then begin
;  astronomical position of central point
    if (not keyword_set(noposition)) then begin
        if (undefined(rot_ang)) then rot_ang = [0.,0.,0.] else rot_ang = ([rot_ang,0,0])(0:2)
        rot_0 = STRTRIM(STRING(rot_ang(0),form='(f6.1)'),2)
        rot_1 = STRTRIM(STRING(rot_ang(1),form='(f6.1)'),2)
        XYOUTS,x_aspos,y_aspos,'!X('+rot_0+', '+rot_1+') '+decode_coord(coord_out),/normal,align=0.5
    endif

; ; cross in the middle of plot
; plots,0,0,ps=1,syms=5,thick=4
; phi = findgen(31)/30.*2*!pi
; x_circle = cos(phi)
; y_circle = sin(phi)
; radius = tan(1.*!dtor/2.) ; radius = fwhm/2
; xyouts,0.7*umax,-0.8*vmax,'100 GHz'
; oplot,0.92*umax+radius*x_circle,-0.8*vmax+radius*y_circle,thick=3
; radius = tan(1./1.5*!dtor/2.)
; xyouts,0.7*umax,-0.9*vmax,'150 GHz'
; oplot,0.92*umax+radius*x_circle,-0.9*vmax+radius*y_circle,thick=3

endif

; do not plot graticules, outlines or pixel boundaries in stagger mode (orthview)
skip_oplots = do_orth && keyword_set(stagger) && $
  ( keyword_set(graticule) || keyword_set(igraticule) || keyword_set(hbound) || keyword_set(outline))

if (skip_oplots) then begin
    message_patch,/info,level=-1,'*Warning*: GRAT, IGRAT, HBOUND and OUTLINE keywords are ignored in STAGGER mode'
endif else begin
    grattwice=0
;  the graticule in output astrophysical coordinates
    if (KEYWORD_SET(graticule)) then begin
        grattwice =1
        glabelsize = charsfactor * (keyword_set(glsize) ? glsize : 0 )
        ;stop
        LS_oplot_graticule, graticule, eul_mat, projection=proj_small, flip = flip, thick = 1.*thick_dev, color = !p.color, $
          half_sky=half_sky, linestyle=GRLS, charsize=glabelsize, reso_rad=dx, GRMIN=GRMIN, GRMAX=GRMAX, LATLONGDIFF=LATLONGDIFF
    endif 

;  the graticule in input coordinates
    if (KEYWORD_SET(igraticule)) then begin
        lines_ig = 2*grattwice  ; either 0 or 2
        iglabelsize = charsfactor * (keyword_set(iglsize) ? iglsize : 0 )
        LS_oplot_graticule, igraticule, eul_mat, projection=proj_small, flip = flip, thick = 1.*thick_dev, color = !p.color, $
          half_sky=half_sky, linestyle=IGRLS, coordsys=[coord_in,coord_out], charsize=iglabelsize, reso_rad=dx, GRMIN=IGRMIN, GRMAX=IGRMAX, LATLONGDIFF=LATLONGDIFF
    endif 

; outlines on the map
    if (keyword_set(outline)) then begin
        for iol=0, n_elements(outline)-1 do begin
            outline_coord2uv, outline[iol], coord_out, eul_mat, projection=proj_small, flip = flip, /show, thick = 1.5*thick_dev, half_sky=half_sky
        endfor
    endif

; overplot pixel boundaries
    if keyword_set(hbound) then begin
        nhbd = n_elements(hbound)
        if (nhbd gt 3) then message_patch,/info,level=-1,'Hbound must have 3 elements at most'
        lnst = [0,2,1]          ; solid (smallest Nside), dashes (median Nside), dots (largest Nside)
        for i=0, (nhbd<3)-1 do begin
            if (hbound[i] gt 0) then oplot_healpix_bounds, hbound[i], eul_mat, projection=proj_small, flip = flip, thick = 1.*thick_dev, color = !p.color, half_sky=half_sky, linestyle=lnst[i], coordsys=[coord_in,coord_out]
        endfor
    endif
endelse

; overplot user defined commands
if keyword_set(execute) then begin
    junk=execute(execute)
    ; reset the plotting area for cursor to work properly
    plot, /nodata, myplot.urange, myplot.vrange, pos=myplot.position, XSTYLE=myplot.xstyle, YSTYLE=myplot.ystyle,/noerase
endif

; -----------------------------------------------
;       output the PS/GIF/PNG/JPG
; -----------------------------------------------

;  gif/png/jpg output
if do_image then begin
    jquality = 100 ; JPEG quality in [0,100]
    valid_transparent = 0
    if (keyword_set(transparent)) then begin
        itr = nint(transparent)
        if (itr lt 0 or itr gt 3) then begin
            message,/info,'keyword TRANSPARENT must be in {0,1,2,3}'
            message,/info,'current value '+string(transparent)+' will be ignored.'
        endif else valid_transparent = 1
    endif

    if (do_gif)  then file_image = (datatype(gif)  ne 'STR') ? 'plot_'+proj_small+'.gif'  : gif
    if (do_png)  then file_image = (datatype(png)  ne 'STR') ? 'plot_'+proj_small+'.png'  : png
    if (do_jpeg) then file_image = (datatype(jpeg) ne 'STR') ? 'plot_'+proj_small+'.jpeg' : jpeg
        
    image = (do_true) ? tvrd(true=3) : tvrd() ; a single call to tvrd()
    if (do_shade) then begin
        image3d  =   make_array(/uint, 3,!d.x_size,!d.y_size)
        allshade =   make_array(/float,  !d.x_size,!d.y_size,value=1.0)
        allshade[w_xll*!d.x_size,w_yll*!d.y_size] = shademap
        shademap = 0
        image3d[0,*,*] = uint( (256. * red  [image] * allshade) < 65535.)
        image3d[1,*,*] = uint( (256. * green[image] * allshade) < 65535.)
        image3d[2,*,*] = uint( (256. * blue [image] * allshade) < 65535.)
;         if (in_gdl) then image3d = bytscl(image3d) ; GDL's write_png won't deal correctly with 16bit integers
        image3d = bytscl(image3d) ; use 8 bit integers only
        allshade = 0
    endif
    if (do_true) then begin
        dim3d = valid_transparent ? 4 : 3
        image3d  =   make_array(/byte, dim3d, !d.x_size, !d.y_size)
        for i=0,2 do image3d[i,*,*] = image[*,*,i]
        if (valid_transparent) then begin
            image3d[3,*,*] = 255B
            if (itr   and 1) then begin ; turn grey  into transparent
                pix_tr = where( total(abs(image3d[0:2,*,*]-col_grey ),1) eq 0, n_tr)
                if (n_tr gt 0) then image3d[3 +4*pix_tr] = 0B
            endif
            if (itr/2 and 1) then begin ; turn white into transparent
                pix_tr = where( total(abs(image3d[0:2,*,*]-col_white),1) eq 0, n_tr)
                if (n_tr gt 0) then image3d[3 +4*pix_tr] = 0B
            endif
        endif
    endif
    ; deal with transparent colors for not TRUECOLORS, not SHADED images
    if ~(do_true || do_shade) then begin
        transp_colors = replicate(255B, 256) ; all colors are opaque
        if (valid_transparent) then begin
                                ; transparent = {1,3} -> grey pixels  are transparent
                                ; transparent = {2,3} -> white pixels are transparent
            if (itr   and 1) then transp_colors[idx_grey ] = 0B ; turn grey  into transparent
            if (itr/2 and 1) then transp_colors[idx_white] = 0B ; turn white into transparent
        endif
    endif
    if do_crop then begin
        y_crop_low = round(w_yll * n_elements(image[0,*])) & y_crop_hi  = y_crop_low + ysize - 1
        cropped = image[*,y_crop_low:y_crop_hi]
        if do_gif then write_gif,file_image,cropped,red,green,blue
        if do_png then begin
            if (do_shade || do_true) then begin
                write_png, file_image, image3d[*,*,y_crop_low:y_crop_hi]
            endif else begin
                if (keyword_set(transparent)) then begin
                    mytransp = (in_idl) ? transp_colors  :  0 ; transp_colors[cropped]
                    write_png,file_image,cropped,red,green,blue, transparent=mytransp
                endif else begin
                    write_png,file_image,cropped,red,green,blue
                endelse
            endelse
        endif
        if do_jpeg then begin
            if (do_shade || do_true) then begin
                write_jpg_custom, file_image, image3d[*,*,y_crop_low:y_crop_hi], true=1, quality=jquality
            endif else begin
                write_jpg_custom, file_image, cropped, red, green, blue,                 quality=jquality
            endelse
        endif
    endif else begin ; uncropped
        if do_gif then write_gif,file_image, image,red,green,blue
        if do_png then begin
            if (do_shade || do_true) then begin
                write_png, file_image, image3d
            endif else begin
                if (keyword_set(transparent)) then begin
                    mytransp = (in_idl) ? transp_colors  : 0 ; transp_colors[image]
                    write_png,file_image, image,red,green,blue, transparent=mytransp
                endif else begin
                    write_png,file_image, image,red,green,blue
                endelse
            endelse
        endif
        if do_jpeg then begin
            if (do_shade || do_true) then begin
                write_jpg_custom, file_image, image3d,             true=1, quality=jquality
            endif else begin
                write_jpg_custom, file_image, image,red,green,blue,        quality=jquality
            endelse
        endif
    endelse
    if (to_patch && ~use_z_buffer) then begin 
        if (in_gdl) then begin
; unresolved GDL0.9.2 bug: if a window is already open for a given color table
; (selected with loadct) subsequent tvlct are ignored for that window. Only a
; new loadct will do the job.
            device, decomposed=0
            tvlct,red,green,blue ; revert to custom color table
        endif else begin
            device,decomposed=0     ; put back colors on X window and redo color image
        endelse
        if (do_shade || do_true) then begin
            tv, bytscl(image3d),0,0,/normal,xsize=1.,true=1
        endif else begin
            tv, image
        endelse
    endif
    image = 0
    if (be_verbose) then print,'IMAGE file is in '+file_image
    if (keyword_set(preview)) then begin
        test_preview, found_preview ;, /crash
        if (found_preview gt 0) then begin
            resolve_routine,'preview_file',/either ; ,compile_full_file=in_idl
            if do_gif then preview_file, file_image, /gif
            if do_png then preview_file, file_image, /png
            if do_jpeg then preview_file, file_image, /jpeg
        endif
    endif
endif


if (do_ps) then begin
    device,/close
    set_plot,old_device
    if (be_verbose) then print,'PS file is in '+file_ps
    if (keyword_set(preview)) then begin
        test_preview, found_preview ;, /crash
        if (found_preview gt 0) then begin
            resolve_routine,'preview_file',/compile_full_file,/either
            preview_file, file_ps, /ps, landscape=do_landscape
        endif
    endif
endif else if (use_z_buffer) then begin
    device,/close ;,decomp=~to_patch
    set_plot,old_device
endif


return
end
; -----------------------------------------------------------------------------
;
;  Copyright (C) 1997-2012  Krzysztof M. Gorski, Eric Hivon, Anthony J. Banday
;
;
;
;
;
;  This file is part of HEALPix.
;
;  HEALPix is free software; you can redistribute it and/or modify
;  it under the terms of the GNU General Public License as published by
;  the Free Software Foundation; either version 2 of the License, or
;  (at your option) any later version.
;
;  HEALPix is distributed in the hope that it will be useful,
;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;  GNU General Public License for more details.
;
;  You should have received a copy of the GNU General Public License
;  along with HEALPix; if not, write to the Free Software
;  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
;
;  For more information about HEALPix see http://healpix.jpl.nasa.gov
;
; -----------------------------------------------------------------------------
pro LS_data2moll, data, pol_data, pix_type, pix_param, do_conv, do_rot, coord_in, coord_out, eul_mat, $
               planmap, Tmax, Tmin, color_bar, planvec, vector_scale, $
               PXSIZE=pxsize, LOG=log, HIST_EQUAL=hist_equal, MAX=max_set, MIN=min_set, FLIP=flip,$
               NO_DIPOLE=no_dipole, NO_MONOPOLE=no_monopole, UNITS = units, DATA_PLOT = data_plot, $
               GAL_CUT=gal_cut, POLARIZATION=polarization, SILENT=silent, PIXEL_LIST=pixel_list, ASINH=asinh, $
               TRUECOLORS=truecolors, DATA_TC=data_tc, MAP_OUT = map_out, ROT=rot_ang, FITS=fits, MODASINH=MODASINH
;+
;==============================================================================================
;     DATA2MOLL
;
;     turns a Healpix or Quad-cube map into in Mollweide egg
;
;     DATA2MOLL,  data, pol_data, pix_type, pix_param, do_conv, do_rot, coord_in,
;                     coord_out, eul_mat
;          planmap, Tmax, Tmin, color_bar, planvec, vector_scale,
;          pxsize=, log=, hist_equal=, max=, min=, flip=, no_dipole=,
;          no_monopole=, units=, data_plot=, gal_cut=, polarization=, silent=,
;          pixel_list=, asinh=, truecolors=, data_tc=, map_out=, rot= , fits=
;
; IN :
;      data, pol_data, pix_type, pix_param, do_conv, do_rot, coord_in,
;      coord_out, eul_mat, rot
; OUT :
;      planmap, Tmax, Tmin, color_bar, planvec, vector_scale, map_out
; KEYWORDS
;      pxsize, log, hist_equal, max, min, flip, no_dipole, no_monopole, units,
;      polarization
;
;  called by mollview
;
;  HISTORY
; Sep 2007: added /silent
; April 2008: added pixel_list
; July 2008: added asinh
; May 2009: can deal with maps without any valid pixel
; April 2010: added Map_Out
; Feb. 2013 changed to LS_data2moll to add the modified ASINH colour option (via modasinh keyword).
;==============================================================================================
;-

do_true = keyword_set(truecolors)
truetype = do_true ? truecolors : 0
du_dv = 2.    ; aspect ratio
fudge = 1.02  ; spare some space around the Mollweide egg
if keyword_set(flip) then flipconv=1 else flipconv = -1  ; longitude increase leftward by default (astro convention)
if undefined(polarization) then polarization=0
do_polamplitude = (polarization[0] eq 1)
do_poldirection = (polarization[0] eq 2)
do_polvector    = (polarization[0] eq 3)
do_map_out      = arg_present(map_out)
do_fits         = keyword_set(fits)


!P.BACKGROUND = 1               ; white background
!P.COLOR = 0                    ; black foreground

mode_col = keyword_set(hist_equal)
mode_col = mode_col + 2*keyword_set(log) + 4*keyword_set(asinh) + 8*keyword_set(modasinh)

sz = size(data)
obs_npix = sz[1]
bad_data= !healpix.bad_value

if (do_poldirection or do_polvector) then begin
    ; compute new position of pixelisation North Pole in the plot coordinates
    north_pole = [0.,0.,1.]
    if (do_conv) then north_pole = SKYCONV(north_pole, inco= coord_in, outco=coord_out)
    if (do_rot) then north_pole = north_pole # transpose(eul_mat)
endif
;-----------------------------------
; mask out some data
;-----------------------------------
;--------------------------------------
; remove monopole and/or dipole (for temperature, not polarisation)
;----------------------------------------
if not (do_poldirection or do_polamplitude) then begin
    if undefined(gal_cut) then bcut = 0. else bcut = abs(gal_cut)
    if keyword_set(no_dipole) then remove_dipole, data, $
      nside=pix_param, ordering=pix_type, units=units, coord_in = coord_in, coord_out=coord_out, $
      bad_data=bad_data, gal_cut=bcut, pixel=pixel_list
    if keyword_set(no_monopole) then remove_dipole, data, $
      nside=pix_param, ordering=pix_type, units=units, coord_in = coord_in, coord_out=coord_out, $
      bad_data=bad_data, gal_cut=bcut,/only, pixel=pixel_list
endif
; -------------------------------------------------------------
; create the rectangular window
; -------------------------------------------------------------
if DEFINED(pxsize) then xsize= LONG(pxsize>200) else xsize = 800L
ysize = xsize/2L
zsize = (do_true) ? 3 : 1
n_uv = xsize*ysize
indlist = (n_elements(pixel_list) eq obs_npix)
small_file = (n_uv GT obs_npix)  && ~do_map_out &&~do_fits
;small_file = ((n_uv GT npix)  and not do_poldirection)

if (small_file) then begin
    ; file smaller than final map, make costly operation on the file
    ; initial data is destroyed and replaced by color
    if (do_poldirection or do_polvector) then begin
        phi = 0.
        if (do_rot or do_conv) then begin
            ; position of each map pixel after rotation and coordinate changes
            id_pix = lindgen(obs_npix)
            case pix_type of
                'R' : PIX2VEC_RING, pix_param, id_pix, vector ; Healpix ring
                'N' : PIX2VEC_NEST, pix_param, id_pix, vector; Healpix nest
                'Q' : vector = PIX2UV(pix_param, id_pix) ; QuadCube (COBE cgis software)
                else : print,'error on pix_type'
            endcase
            id_pix = 0
            if (do_conv) then vector = SKYCONV(vector, inco= coord_in, outco=coord_out)
            if (do_rot) then vector = vector # transpose(eul_mat)
            ; compute rotation of local coordinates around each vector
            tmp_sin = north_pole[1] * vector[*,0] - north_pole[0] * vector[*,1]
            tmp_cos = north_pole[2] - vector[*,2] * (north_pole[0:2] ## vector)
            if (flipconv lt 0) then tmp_cos = flipconv * tmp_cos
            phi = ATAN(tmp_sin, tmp_cos) ; angle in radians
            tmp_sin = 0. & tmp_cos = 0 & vector = 0.
        endif
        data_plot = data
        if (do_poldirection) then begin
            data = (data - phi + 4*!PI) MOD (2*!PI) ; angle
            min_set = 0. & max_set = 2*!pi
        endif
        if (do_polvector) then begin
            pol_data[*,1] = (pol_data[*,1] - phi + 4*!PI) MOD (2*!PI) ; angle is rotated
        endif
    endif else begin ; temperature only or polarisation amplitude only
        data_plot = data
    endelse
    ; color observed pixels
    if (do_true) then begin
        if (truetype eq 2) then begin
            for i=0,2 do begin
                find_min_max_valid, data_tc[*,i], mindata, maxdata, valid=Obs, bad_data = 0.9 * bad_data
                data_tc[0,i] = LS_COLOR_MAP(data_tc[*,i], mindata, maxdata, Obs, $
                                    color_bar = color_bar, mode=mode_col, silent=silent )
            endfor
        endif else begin
            find_min_max_valid, data_tc, mindata, maxdata, valid=Obs, bad_data = 0.9 * bad_data
            data_tc = LS_COLOR_MAP(data_tc, mindata, maxdata, Obs, $
                                color_bar = color_bar, mode=mode_col, $
                                minset = min_set, maxset = max_set, silent=silent )
        endelse
    endif else begin
        find_min_max_valid, data[*,0], mindata, maxdata, valid=Obs, bad_data= 0.9 * bad_data
        data = LS_COLOR_MAP(data, mindata, maxdata, Obs, $
                         color_bar = color_bar, mode=mode_col, $
                         minset = min_set, maxset = max_set, silent=silent )
    endelse
    if (do_polvector) then begin ; rescale polarisation vector in each valid pixel
        pol_data[0,0] = vector_map(pol_data[*,0], Obs, vector_scale = vector_scale)
    endif
    if defined(Obs) then Obs = 0
    Tmin = mindata & Tmax = maxdata
    planmap = MAKE_ARRAY(/BYTE, xsize, ysize, zsize, Value = !P.BACKGROUND) ; white
endif else begin ; large file
    planmap = MAKE_ARRAY(/FLOAT, xsize, ysize, zsize, Value = bad_data) 
    plan_off = 0L
endelse
if do_polvector then planvec = MAKE_ARRAY(/FLOAT,xsize,ysize, 2, Value = bad_data) 

; -------------------------------------------------
; make the projection
;  we split the projection to avoid dealing with to big an array
; -------------------------------------------------
if (~keyword_set(silent)) then print,'... making the projection ...'
; -------------------------------------------------
; generate the (u,v) position on the mollweide map
; -------------------------------------------------
xll= 0 & xur =  xsize-1
yll= 0 & yur =  ysize-1
xc = 0.5*(xll+xur) & dx = (xur - xc)
yc = 0.5*(yll+yur) & dy = (yur - yc)


yband = LONG(5.e5 / FLOAT(xsize))
for ystart = 0, ysize - 1, yband do begin 
    yend   = (ystart + yband - 1) < (ysize - 1)
    nband = yend - ystart + 1
    u = FINDGEN(xsize)     # REPLICATE(1,nband)
    v = REPLICATE(1,xsize) # (FINDGEN(nband) + ystart)
    u =  du_dv*(u - xc)/(dx/fudge)   ; in [-2,2]*fudge
    v =        (v - yc)/(dy/fudge)   ; in [-1,1] * fudge

    ; -------------------------------------------------------------
    ; for each point on the mollweide map 
    ; looks for the corresponding position vector on the sphere
    ; -------------------------------------------------------------
    ellipse  = WHERE( (u^2/4. + v^2) LE 1. , nellipse)
    if (~small_file) then begin
        off_ellipse = WHERE( (u^2/4. + v^2) GT 1. , noff_ell)
        if (noff_ell NE 0) then plan_off = [plan_off, ystart*xsize+off_ellipse]
    endif
    if (nellipse gt 0) then begin
        u1 =  u(ellipse)
        v1 =  v(ellipse)
        u = 0 & v = 0
        s1 =  SQRT( (1-v1)*(1+v1) )
        a1 =  ASIN(v1)

        z = 2./!PI * ( a1 + v1*s1)
        phi = (flipconv *!Pi/2.) * u1/s1 ; lon in [-pi,pi], the minus sign is here to fit astro convention
        sz = SQRT( (1. - z)*(1. + z) )
        vector = [[sz * COS(phi)], [sz * SIN(phi)], [z]]
        u1 = 0 & v1 = 0 & s1 = 0 & a1 = 0 & z = 0 & phi = 0 & sz = 0
        ; --------------------------------
        ; deal with polarisation direction
        ; --------------------------------
        if ((do_poldirection || do_polvector) && ~small_file) then begin
            phi = 0.
            if (do_rot or do_conv) then begin
                ; compute rotation of local coordinates around each vector
                tmp_sin = north_pole[1] * vector[*,0] - north_pole[0] * vector[*,1]
                tmp_cos = north_pole[2] - vector[*,2] * (north_pole[0:2] ## vector)
                if (flipconv lt 0) then tmp_cos = flipconv * tmp_cos
                phi = ATAN(tmp_sin, tmp_cos) ; angle in radians
                tmp_sin = 0. & tmp_cos = 0
            endif
        endif
        ; ---------
        ; rotation
        ; ---------
        if (do_rot) then vector = vector # eul_mat
        if (do_conv) then vector = SKYCONV(vector, inco = coord_out, outco =  coord_in)
                                ; we go from the final Mollweide map (system coord_out) to
                                ; the original one (system coord_in)
        ; -------------------------------------------------------------
        ; converts the position on the sphere into pixel number
        ; and project the corresponding data value on the map
        ; -------------------------------------------------------------
        case pix_type of
            'R' : VEC2PIX_RING, pix_param, vector, id_pix ; Healpix ring
            'N' : VEC2PIX_NEST, pix_param, vector, id_pix ; Healpix nest
            'Q' : id_pix = UV2PIX(vector, pix_param)    ; QuadCube (COBE cgis software)
            else : print,'error on pix_type'
        endcase
        if (small_file) then begin ; (data and data_pol are already rescaled and color coded)
            if (do_true) then begin
                for i=0,zsize-1 do planmap[(ystart*xsize+i*n_uv)+ellipse] = data_tc[id_pix,i]
            endif else begin
                if (~ (do_polvector || do_polamplitude || do_poldirection) ) then begin
                    planmap[ystart*xsize+ellipse] = sample_sparse_array(data,id_pix,in_pix=pixel_list,default=2B) ; temperature
                endif else begin
                    planmap[ystart*xsize+ellipse] = data[id_pix]
                endelse
                if (do_polvector) then begin
                    planvec[ystart*xsize+ellipse]         = pol_data[id_pix,0] ; amplitude
                    planvec[(ystart*xsize+n_uv)+ellipse]  = pol_data[id_pix,1] ; direction
                endif
            endelse
        endif else begin ; (large file : do the projection first)
            if (do_true) then begin
                for i=0,zsize-1 do planmap[(ystart*xsize+i*n_uv)+ellipse] = data_tc[id_pix,i]
            endif else begin
                if (do_poldirection) then begin
                    planmap[ystart*xsize+ellipse] = (data[id_pix] - phi + 4*!PI) MOD (2*!PI) ; in 0,2pi
                endif else if (do_polvector) then begin
                    planmap[ystart*xsize+ellipse]         = data[id_pix] ; temperature
                    planvec[ystart*xsize+ellipse]         = pol_data[id_pix,0] ; amplitude
                    planvec[(ystart*xsize+n_uv)+ellipse]  = (pol_data[id_pix,1] - phi + 4*!PI) MOD (2*!PI) ; angle
                endif else begin ; temperature only or amplitude only
                                ;planmap[ystart*xsize+ellipse]         = data[id_pix] ; temperature
                    planmap[ystart*xsize+ellipse]         = sample_sparse_array(data,id_pix,in_pix=pixel_list,default=!healpix.bad_value) ; temperature
                endelse
            endelse
        endelse
    endif
    ellipse = 0 & id_pix = 0
endfor


;-----------------------------------
; export in FITS and as an array the original mollweide map before alteration
;----------------------------------------------

; planmap -> IDL array
if (do_map_out) then map_out = proj2map_out(planmap, offmap=plan_off, bad_data=bad_data)

; planmap -> FITS file
if keyword_set(fits) then begin 
    reso_arcmin = 60.d0 * 360.d0/(xsize-1) * fudge
    reso_arcmin *=  sqrt(8.d0) / !dpi ; WCS convention, ellipse surface is 4Pi
    proj2fits, planmap, fits, $
               projection = 'MOLL', flip=flip, $
               rot = rot_ang, coord=coord_out, reso = reso_arcmin, unit = sunits, min=mindata, max = maxdata
endif



if (small_file) then begin
    data = 0 & pol_data = 0
endif else begin
; file larger than final map, make
; costly coloring operation on the Mollweide map
    data_plot = temporary(data)
    pol_data = 0
    if (do_poldirection) then begin
        min_set = 0.
        max_set = 2*!pi
    endif
    find_min_max_valid, planmap, mindata, maxdata, valid= Obs, bad_data = 0.9 * bad_data
    case truetype of
        2: begin
                                ; truecolors=2 map each field to its color independently
            color = bytarr(xsize, ysize, zsize)
            for i=0,zsize-1 do begin
                find_min_max_valid, planmap[*,*,i], mindata, maxdata, valid=Obs, bad_data = 0.9 * bad_data
                color[0,0,i] = LS_COLOR_MAP(planmap[*,*,i], mindata, maxdata, Obs, $
                                         color_bar = color_bar, mode=mode_col, silent=silent)
            endfor
            planmap = color
        end
        3: begin
            intensity = total(planmap,3)/3.
            find_min_max_valid, intensity, mindata, maxdata, valid= Obs, bad_data = 0.1 * bad_data
            bint = LS_COLOR_MAP(intensity, mindata, maxdata, Obs, $
                                color_bar = color_bar, mode=mode_col, $
                                minset = min_set, maxset = max_set, silent=silent)
;             for i=0,2 do begin
;                 ioff = i*xsize*ysize
;                 color[Obs+ioff] = 3B + bytscl((planmap[Obs+ioff]>0)/intensity[Obs] * bint[Obs], min=0,top=252)
;             endfor
            ioff = xsize * ysize
            mat = planmap[[[Obs], [Obs+ioff], [Obs+2*ioff]]] > 0
            mat2 = (bint[Obs]/intensity[Obs]) # [1,1,1]
            color = 3B + bytscl(mat*mat2, min=0, top=252)
            planmap = MAKE_ARRAY(/BYTE, xsize, ysize, 3, Value = 2B)
            for i=0,2 do planmap[Obs + i*ioff] = color[*,i]
            
        end
        else: begin
                                ; same for truecolors=1 and false colors:
            planmap = LS_COLOR_MAP(planmap, mindata, maxdata, Obs, $
                                color_bar = color_bar, mode=mode_col, $
                                minset = min_set, maxset = max_set, silent=silent)
        end
    endcase
    for i=0,zsize-1 do planmap[plan_off+i*n_uv] = !p.background ; white
    if (do_polvector) then begin ; rescale polarisation vector in each valid pixel
        planvec[*,*,0] = vector_map(planvec[*,*,0], Obs, vector_scale = vector_scale)
        planvec[plan_off] = -1
    endif
    Obs = 0 & plan_off = 0
    Tmin = mindata & Tmax = maxdata
endelse


return
end
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
; -----------------------------------------------------------------------------
;
;  Copyright (C) 1997-2012  Krzysztof M. Gorski, Eric Hivon, Anthony J. Banday
;
;
;
;
;
;  This file is part of HEALPix.
;
;  HEALPix is free software; you can redistribute it and/or modify
;  it under the terms of the GNU General Public License as published by
;  the Free Software Foundation; either version 2 of the License, or
;  (at your option) any later version.
;
;  HEALPix is distributed in the hope that it will be useful,
;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;  GNU General Public License for more details.
;
;  You should have received a copy of the GNU General Public License
;  along with HEALPix; if not, write to the Free Software
;  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
;
;  For more information about HEALPix see http://healpix.jpl.nasa.gov
;
; -----------------------------------------------------------------------------
pro LS_data2cart, data, pol_data, pix_type, pix_param, do_conv, do_rot, coord_in, coord_out, eul_mat, $
               color, Tmax, Tmin, color_bar, dx, planvec, vector_scale, $
               PXSIZE=pxsize, PYSIZE=pysize, ROT=rot_ang, LOG=log, HIST_EQUAL=hist_equal, $
               MAX=max_set, MIN=min_set, $
               RESO_ARCMIN=reso_arcmin, FITS = fits, $
               FLIP=flip, DATA_plot = data_plot, $
               POLARIZATION=polarization, SILENT=silent, PIXEL_LIST=pixel_list, ASINH=asinh, $
               TRUECOLORS=truecolors, DATA_TC=data_tc, MAP_OUT=map_out, MODASINH=MODASINH
; Mar. 2013 changed to LS_data2cart to add the modified ASINH colour option (via modasinh keyword).

;+
;==============================================================================================
;     DATA2CART
;
;       turns a Healpix or Quad-cube tessellation of the sphere 
; into a rectangular map in cartesian coordinates
;
;     DATA2CART, data, pix_type, pix_param, do_conv, do_rot, coord_in, coord_out, eul_mat,
;          color, Tmax, Tmin, color_bar, dx, planvec, vector_scale,
;          pxsize=, pysize=, rot=, log=, hist_equal=, max=, min=,
;          reso_arcmin=, fits=, flip=, data_plot=, POLARIZATION=, SILENT=,
;          PIXEL_LIST=, ASINH=, TRUECOLORS=, DATA_TC=, MAP_OUT=
;
; IN :
;      data, pix_type, pix_param, do_conv, do_rot, coord_in, coord_out, eul_mat
; OUT :
;      color, Tmax, Tmin, color_bar, dx, planvec, vector_scale
; KEYWORDS
;      Pxsize, Pysize, Rot, Log, Hist_equal, Max, Min, Reso_arcmin,
;      Fits, flip, data_plot, polarization, silent, pixel_list, asinh
;
;  called by cartview
;
;  HISTORY
;  2002-06:
;    Hacked by E.H from G. Giardino's data2pol
; Sep 2007: added /silent
; April 2008: added pixel_list
; July 2008: added asinh
; April 2010: added Map_Out
;==============================================================================================
;-

do_true = keyword_set(truecolors)
truetype = do_true ? truecolors : 0
;help,data
proj_small = 'cartesian'
du_dv = 1.    ; aspect ratio
fudge = 1.00  ; 
if keyword_set(flip) then flipconv=1 else flipconv = -1  ; longitude increase leftward by default (astro convention)
if undefined(polarization) then polarization=0
do_polamplitude = (polarization[0] eq 1)
do_poldirection = (polarization[0] eq 2)
do_polvector    = (polarization[0] eq 3)

!P.BACKGROUND = 1               ; white background
!P.COLOR = 0                    ; black foreground

mode_col = keyword_set(hist_equal)
mode_col = mode_col + 2*keyword_set(log) + 4*keyword_set(asinh) + 8*keyword_set(modasinh)

obs_npix = N_ELEMENTS(data)
npix_full = (pix_type eq 'Q') ? 6*(4L)^(pix_param-1) : nside2npix(pix_param)

bad_data= !healpix.bad_value

if (do_poldirection or do_polvector) then begin
    ; compute new position of pixelisation North Pole in the plot coordinates
    north_pole = [0.,0.,1.]
    if (do_conv) then north_pole = SKYCONV(north_pole, inco= coord_in, outco=coord_out)
    if (do_rot) then north_pole = north_pole # transpose(eul_mat)
endif
; -------------------------------------------------------------
; create the rectangular window
; -------------------------------------------------------------
if defined(pxsize) then xsize = pxsize*1L else xsize = 500L
if defined(pysize) then ysize = pysize*1L else ysize = xsize
if defined(reso_arcmin) then resgrid = reso_arcmin/60. else resgrid = 1.5/60.
dx      = resgrid * !DtoR
zsize = (do_true) ? 3 : 1
N_uv = xsize*ysize
indlist = (n_elements(pixel_list) eq n_elements(data[*,0]))

if (~keyword_set(silent)) then begin
    print,'Input map  :  ',3600.*6./sqrt(!dpi*npix_full),' arcmin / pixel ',form='(a,f8.3,a)'
    print,'Cartesian map :',resgrid*60.,' arcmin / pixel ',xsize,'*',ysize,form='(a,f8.3,a,i4,a,i4)'
endif

grid = FLTARR(xsize, ysize, zsize)
;; grid = MAKE_ARRAY(/FLOAT,xsize,ysize, Value = bad_data) 
if do_polvector then planvec = MAKE_ARRAY(/FLOAT,xsize,ysize, 2, Value = bad_data) 
; -------------------------------------------------------------
; makes the projection around the chosen contact point
; -------------------------------------------------------------
; position on the planar grid  (1,u,v)
x0 = +1.
xll= 0 & xur =  xsize-1
yll= 0 & yur =  ysize-1
xc = 0.5*(xll+xur) 
yc = 0.5*(yll+yur) 

yband = LONG(5.e5 / FLOAT(xsize))
for ystart = 0, ysize - 1, yband do begin 
    yend   = (ystart + yband - 1) < (ysize - 1)
    nband = yend - ystart + 1
    npb = xsize * nband
    u = flipconv*(FINDGEN(xsize) - xc)# REPLICATE(dx,nband)   ; minus sign = astro convention
    v =           REPLICATE(dx,xsize) # (FINDGEN(nband) + ystart - yc)
    off_mask = WHERE( abs(u) gt !pi or abs(v) gt !pi/2., noff_mask)
    if (noff_mask gt 0) then begin
        if (undefined(plan_off)) then begin
            plan_off = ystart*xsize+off_mask
        endif else begin
            plan_off = [plan_off, ystart*xsize+off_mask]
        endelse
    endif
    x =  cos(reform(v,npb)) * cos(reform(u,npb))
    y =  cos(reform(v,npb)) * sin(reform(u,npb))
    z =  sin(reform(v, npb))
    vector = [[x],[y],[z]] ; normalised vector
    ; --------------------------------
    ; deal with polarisation direction
    ; --------------------------------
    if (do_poldirection or do_polvector) then begin
        phi = 0.
        if (do_rot or do_conv) then begin
            vector = vector / (sqrt(total(vector^2, 2))#replicate(1,3)) ; normalize vector
            ; compute rotation of local coordinates around each vector
            tmp_sin = north_pole[1] * vector[*,0] - north_pole[0] * vector[*,1]
            tmp_cos = north_pole[2] - vector[*,2] * (north_pole[0:2] ## vector)
            if (flipconv lt 0) then tmp_cos = flipconv * tmp_cos
            phi = ATAN(tmp_sin, tmp_cos) ; angle in radians
            tmp_sin = 0. & tmp_cos = 0
        endif
    endif
    ; ---------
    ; rotation
    ; ---------
    if (do_rot) then vector = vector # eul_mat
    if (do_conv) then vector = SKYCONV(vector, inco = coord_out, outco =  coord_in)
          ; we go from the final cartesian map (system coord_out) to
          ; the original one (system coord_in)
    ; ----------------x---------------------------------------------
    ; converts the position on the sphere into pixel number
    ; and project the corresponding data value on the map
    ; -------------------------------------------------------------
    case pix_type of
        'R' : VEC2PIX_RING, pix_param, vector, id_pix ; Healpix ring
        'N' : VEC2PIX_NEST, pix_param, vector, id_pix ; Healpix nest
        'Q' : id_pix = UV2PIX(vector, pix_param) ; QuadCube (COBE cgis software)
        else : print,'error on pix_type'
    endcase
    if (do_true) then begin
        for i=0,zsize-1 do grid[ystart*xsize+i*n_uv] = data_tc[id_pix,i]
    endif else begin
        if (do_poldirection) then begin
            grid[ystart*xsize] = (data[id_pix] - phi + 4*!PI) MOD (2*!PI) ; in 0,2pi
        endif else if (do_polvector) then begin
            grid[ystart*xsize]         = data[id_pix]
            planvec[ystart*xsize]      = pol_data[id_pix,0]
            planvec[ystart*xsize+n_uv] = (pol_data[id_pix,1] - phi + 4*!PI) MOD (2*!PI) ; angle
        endif else begin
                                ;grid[ystart*xsize] = data[id_pix]
            grid[ystart*xsize] = sample_sparse_array(data,id_pix,in_pix=pixel_list,default=!healpix.bad_value)
        endelse
    endelse
endfor
u = 0 & v = 0 & x = 0 & vector = 0

; -------------------------------------------------------------
; Test for unobserved pixels
; -------------------------------------------------------------
data_plot = temporary(data)
pol_data = 0
find_min_max_valid, grid, mindata, maxdata, valid=Obs, bad_data=0.9*bad_data

;-----------------------------------
; export in FITS and as an array the original cartesian map before alteration
;----------------------------------------------

; grid -> IDL array
if arg_present(map_out) then map_out = proj2map_out(grid, offmap=plan_off, bad_data=bad_data)

; grid -> FITS file
if keyword_set(fits) then begin 
    proj2fits, grid, fits, $
               projection = 'CART', flip=flip, $
               rot = rot_ang, coord=coord_out, reso_arcmin = resgrid*60., unit = sunits, min=mindata, max = maxdata
endif

; -------------------------------------------------------------
; set min and max and computes the color scaling
; -------------------------------------------------------------
if (do_poldirection) then begin
    min_set = 0.
    max_set = 2*!pi
endif

if (truetype eq 2) then begin
                                ; truecolors=2 map each field to its color independently
    color = bytarr(xsize,ysize,zsize)
    for i=0,zsize-1 do begin
        find_min_max_valid, grid[*,*,i], mindata, maxdata, valid=Obs, bad_data = 0.9 * bad_data
        color[0,0,i] = LS_COLOR_MAP(grid[*,*,i], mindata, maxdata, Obs, $
                                 color_bar = color_bar, mode=mode_col, silent=silent)
    endfor
endif else begin
                                ; same for truecolors=1 and false colors:
    color = LS_COLOR_MAP(grid, mindata, maxdata, Obs, $
                      color_bar = color_bar, mode=mode_col, $
                      minset = min_set, maxset = max_set, silent=silent)
endelse

if (defined(plan_off)) then begin
    for i=0,zsize-1 do color[plan_off+i*n_uv]  = !P.BACKGROUND ; white
endif
if (do_polvector) then begin    ; rescale polarisation vector in each valid pixel
    planvec[*,*,0] = vector_map(planvec[*,*,0], Obs, vector_scale = vector_scale)
endif
Obs = 0
grid = 0
Tmin = mindata & Tmax = maxdata

return
end
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
FUNCTION MODASINH, X
  return, ALOG10(0.5d*(X + SQRT(X^2d + 4d)))
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
;
; -----------------------------------------------------------------------------
;
;  Copyright (C) 1997-2012  Krzysztof M. Gorski, Eric Hivon, Anthony J. Banday
;
;
;
;
;
;  This file is part of HEALPix.
;
;  HEALPix is free software; you can redistribute it and/or modify
;  it under the terms of the GNU General Public License as published by
;  the Free Software Foundation; either version 2 of the License, or
;  (at your option) any later version.
;
;  HEALPix is distributed in the hope that it will be useful,
;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;  GNU General Public License for more details.
;
;  You should have received a copy of the GNU General Public License
;  along with HEALPix; if not, write to the Free Software
;  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
;
;  For more information about HEALPix see http://healpix.jpl.nasa.gov
;
; -----------------------------------------------------------------------------
pro LS_mollview, file_in, select_in, $
              ASINH = asinh, $
              CHARSIZE = charsize, $
              CHARTHICK = charthick, $
              COLT = colt, $
              COORD = coord, $
              CROP = crop, $
              EXECUTE = execute, $
              FACTOR = factor, $
              FITS = fits, $
              FLIP = flip, $
              GAL_CUT = gal_cut, $
              GIF = gif, $
              GLSIZE = glsize, $
              GRATICULE = graticule, $
              HELP = help, $
              HBOUND = hbound, $
              HIST_EQUAL = hist_equal, $
              HXSIZE = hxsize, $
              IGLSIZE = iglsize, $
              IGRATICULE=igraticule, $
              JPEG=jpeg, $
              LOG = log, $
              MAP_OUT = map_out, $
              MAX = max_set, $
              MIN = min_set, $
              NESTED = nested_online, $
              NOBAR = nobar, $
              NOLABELS = nolabels, $
              NO_DIPOLE = no_dipole, $
              NO_MONOPOLE = no_monopole, $
              OFFSET = offset, $
              ONLINE = online, $
              OUTLINE = outline, $
              PNG = png, $
              POLARIZATION = polarization, $
              PREVIEW = preview, $
              PS = ps, $
              PXSIZE = pxsize, $
              QUADCUBE = quadcube, $
              RETAIN = retain, $
              ROT = rot, $
              SAVE = save, $
              SILENT = silent, $
              SUBTITLE = subtitle, $
              TITLEPLOT = titleplot, $
              TRANSPARENT = transparent, $
              TRUECOLORS = truecolors, $
              UNITS = units, $
              WINDOW = window, $
              XPOS = xpos, $
              YPOS = ypos, $
              CTDIR=CTDIR, $
              CTFILE=CTFILE, GRMIN=GRMIN, GRMAX=GRMAX, GRLS=GRLS, IGRMIN=IGRMIN, IGRMAX=IGRMAX, IGRLS=IGRLS, MODASINH=MODASINH, $
              CBLBL=CBLBL, CBLIN=CBLIN, CBTICKS=CBTICKS, CBTICKVAL=CBTICKVAL, CBTICKLBL=CBTICKLBL, CBTICKLAB=CBTICKLAB, CBOUT=CBOUT, $
              LATLONGDIFF=LATLONGDIFF, FNTsz=FNTsz, CBOFF=CBOFF, MAPOFF=MAPOFF

;+
; NAME:
; MOLLVIEW, GNOMVIEW, CARTVIEW, ORTHVIEW
;
; PURPOSE:
;   tools to view a Mollweide/gnomic/cartesian/orthographic projection of maps binned
; in Healpix or COBE Quad-Cube pixelisation
;
; CALLING SEQUENCE:
;   xxxxVIEW, File, [Select, ] $
;                       [ASINH=, CHARSIZE=, COLT=, COORD=, CROP=, $
;                       EXECUTE=execute, $
;                       FACTOR=, FITS=, FLIP=, $
;                       GAL_CUT=, GIF=, GLSIZE=, GRATICULE=, $
;                       HALF_SKY =, HBOUND =, HELP =, HIST_EQUAL=, HXSIZE=, $
;                       IGLSIZE=, IGRATICULE=, $
;                       JPEG=, $
;                       LOG=, $
;                       MAP_OUT=, MAX=, MIN=, $ 
;                       NESTED=, NOBAR=, NOLABELS=, NOPOSITION =, $
;                       OFFSET =, ONLINE=, OUTLINE=, $
;                       PNG=, POLARIZATION=, PREVIEW=,$
;                       PS=, PXSIZE=, PYSIZE=, $
;                       QUADCUBE= , $
;                       NO_DIPOLE=, NO_MONOPOLE=, $
;                       RESO_ARCMIN= , ROT=, $
;                       SAVE=, SHADED=, SILENT=, STAGGER=, SUBTITLE=, $
;                       TITLEPLOT=, TRANSPARENT=, TRUECOLORS= $
;                       UNITS=, WINDOW=, XPOS=, YPOS=]
;                        
;  all the arguments and parameters are identical for all the
;  routines, excepted stated otherwise.
;
;
; INPUTS:
;   File = 
;          by default,           name of a FITS file containing 
;               the healpix map in an extension or in the image field
;          if Online is set :    name of a variable containing
;               the healpix map
;          if Save is set   :    name of an IDL saveset file containing
;               the healpix map stored under the variable  data
;
; OPTIONAL INPUTS:
;       Select =  if the file read is a multi column BIN table, Select indicates
;                 which column is to be plotted (the default is to plot the
;                 first column containing some signal, as opposed to pixel index)
;               can be either a name : value given in TTYPEi of the FITS file
;                        NOT case sensitive and
;                        can be truncated, 
;                        (only letters, digits and underscore are valid)
;               or an integer        : number i of the column
;                            containing the data, starting with 1
;               (see the Examples below)
;
; OPTIONAL INPUT KEYWORDS:
;
;       ASINH: if set, the color table is altered in to emulate the effect of replacing
;            the data by sinh^{-1}(data) in order to enhance the low contrast regions.
;            Can be used in conjonction with FACTOR and OFFSET, but can not be
;            used with /LOG nor /HIST_EQUAL
;
;       CHARSIZE : overall multiplicative factor applied to the size of all
;               characters appearing on the plot
;                default = 1.0
;
;       CHARTHICK : character thickness (in TITLE, SUBTITLE and color bar labeling).  
;               Other characters thickness (such as graticule labels), can be 
;               controlled with !P.CHARTHICK.
;                default = 1
;
;   COLT : color table index:
;              -Indexes [0,40] are reserved for standard IDL color tables, while
;               [41,255] are used for user defined color tables read from disc (created and
;               written to disc with MODIFYCT), if any.
;              -If the index does not match any existing table, or if it is
;              above 255, the current
;               table (modifiable with TVLCT, XLOADCT, XPALETTE, ... 
;               or eg, J.Davenport's cubehelix.pro implementation of D. Green cubehelix
;               color scheme) is used instead.
;              -If not set, the color table will be 33 (Blue-Red).
;              -If colt<0, the IDL color table ABS(colt) is used, but the scale is
;              reversed (ie a red to blue scale becomes a blue to red scale).
;              (note: -0.1 can be used as negative 0)
;
;       COORD : vector with 1 or 2 elements describing the coordinate system of the map 
;                either 'C' or 'Q' : Celestial2000 = eQuatorial,
;                       'E'        : Ecliptic,
;                       'G'        : Galactic 
;               if coord = ['x','y'] the map is rotated from system 'x' to system 'y'
;               if coord = ['y'] the map is rotated to coordinate system 'y' (with the
;               original system assumed to be Galactic unless indicated otherwise in the file)
;                  see also : Rot
;
;       CROP : if set the image file (gif, png) only contains the mollweide map and
;               not the title, color bar, ...
;               (see also : GIF, PNG)
;
;       EXECUTE: character string containing an IDL command to be executed in
;                the plotting window
;
;       FACTOR : multiplicative factor to be applied to the data (default = 1.0)
;               the data plotted is of the form FACTOR*(data + OFFSET)
;               see also : OFFSET, LOG
;
;       FITS : string containing the name of an output fits file with
;       the projected map in the primary image
;       if set to 0 or not set : no .FITS done
;       if set to 1            : output the plot in plot_XXX.fits
;                with XXX = azimequid, cartesian, gnomic, mollweide or orthographic
;       if set to a file name  : output the plot in that file 
;    * For compatibility with standard FITS viewers (including STIFF), 
;        unobserved pixels, and pixels outside the sphere, take the value {\tt
;        NaN} (ie {\tt !values.f\_nan} in IDL).
;          * The resulting FITS file can be read in IDL with eg. map=readfits(filename). 
;          * In the case of orthographic projection, HALF_SKY must be set.
;
;       FLIP : if set, the longitude increases to the right, whereas by
;               default (astro convention) it increases towards the left
;
;       GAL_CUT: (positive float) specifies the symmetric galactic cut in degree
;             outside of which the the monopole and/or dipole fitting is done
;             (see also: NO_DIPOLE, NO_MONOPOLE)
;             ** mollview and orthview only **
;
; GIF : string containing the name of a .GIF output
;       if set to 0 or not set : no .GIF done
;       if set to 1            : output the plot in plot_XXX.gif
;                with XXX = azimequid, cartesian, gnomic, mollweide or orthographic
;       if set to a file name  : output the plot in that file 
;             (see also : CROP, JPEG, PNG, PS and PREVIEW)
;
;       GLSIZE : character size of the graticule labels in units of CHARSIZE
;             default: 0 (ie, no labeling of graticules)
;             (see also: CHARSIZE, GRATICULE)
;
;   GRATICULE : if set, puts a graticule with delta_long = delta_lat = default
;         if graticule is set to a scalar > gmin delta_long = delta_lat = graticule
;         if set to [x,y] with x,y > gmin then delta_long = x and delta_let = y
;         ** cartview : default =  5, gmin =  0 **
;         ** gnomview : default =  5, gmin =  0 **
;         ** mollview : default = 45, gmin = 10 **
;         ** orthview : default = 45, gmin = 10 **
;
;       HALF_SKY: if set, only shows only one half of the sky 
;          (centered on (0,0) or on the location parametrized by Rot) instead of the full sky
;             ** orthview only **
;        
;       HBOUND: scalar or vector of up to 3 elements.
;          For Hbound[i]>0, overplot the boundaries of Healpix pixels
;           for the resolution parameter Nside=hbound[i].
;           The first Nside will be plotted with solid lines, 
;           the second one (if any) with dashes, 
;           the third one (if any) with dots. Obviously, better results are
;           obtained for Hbounds elements in growing order.
;           Since 0-valued boundaries are not plotted, but used for linestyle
;           assignment, providing Hbound=[0,4] (or [0,0,4]) will
;           plot Nside=4 boundaries with dashes (resp. dots), while Hbound=4 would plot the same
;           boundaries with solid lines.
;
;       HELP : if set, the routine header is printed (by doc_library)
;             and nothing else is done
;
; HIST_EQUAL : if not set, uses linear color mapping and 
;                         puts the level 0 in the middle
;                         of the color scale (ie, green for Blue-Red)
;       unless MIN and MAX are not symmetric
;               if set,     uses a histogram equalized color mapping
;     (useful for non gaussian data field)
;                     (see also : LOG)
;
;   HXSIZE: horizontal dimension (in cm) of the Hardcopy plot : Only for postscript printout
;       ** mollview : default = 26 cm ~ 10 in **
;               ** mollview : default = 15 cm         **
;       (useful for large color printer)
;               (see also : PXSIZE)
;
;       IGLSIZE : character size of the input coordinates graticule labels in units of CHARSIZE
;             default: 0 (ie, no labeling of graticules)
;             (see also: CHARSIZE, IGRATICULE)
;
;       IGRATICULE: if set, puts a graticule in the input coordinates
;          if both graticule and igraticule are set, these ones will
;          be represented by dashes
;
; JPEG : string containing the name of a (lossless) .JPEG output
;       if set to 0 or not set : no .JPEG done
;       if set to 1            : output the plot in plot_XXX.jpeg
;                with XXX = azimequid, cartesian, gnomic, mollweide or orthographic
;       if set to a file name  : output the plot in that file 
;             (see also : CROP, GIF, PNG, PS and PREVIEW)
;
;   LOG: display the log of map (see also : HIST)
;         only applies to pixel with signal > 0.
;         (see OFFSET to offset signal)
;
;       MAP_OUT : name of the IDL variable that will contain
;         an un-altered projected map.
;         Unobserved pixels, and pixels outside the sphere take 
;       value !healpix.bad_value=-1.6375e30
;
;   MAX : max value plotted, 
;   every data point larger than MAX takes the same color as MAX
;
;   MIN : min value plotted, 
;   every data point smaller than MIN takes the same color as MIN
;
; NESTED: specify that the online file is ordered in the nested scheme
;
;   NOBAR : if set, no color bar is present
;
; NOLABELS : if set, no color bar label (min and max) is present
;
; NOPOSITION : if set, the astronomical location of the map
;         central point is not indicated
;               ** gnomview only **
;
;       NO_DIPOLE: if set to 1 (and GAL_CUT is not set) 
;                the best fit monopole *and* dipole over all valid pixels are removed
;                * if GAL_CUT is set to b>0, the best monopole and dipole fit is done on all valid
;                pixels with |galactic latitude|>b (in deg) and is removed from all
;                pixels
;             can not be used together with NO_MONOPOLE 
;             (see: GAL_CUT, NO_MONOPOLE)
;               ** mollview and orthview only **
;
;       NO_MONOPOLE: if set to 1 (and GAL_CUT is not set) 
;                the best fit monopole over all valid pixels is removed
;                * if GAL_CUT is set to b>0, the best monopole fit is done on all valid
;                pixels with |galactic latitude|>b (in deg) and is removed from all
;                pixels
;             can not be used together with NO_DIPOLE 
;             (see: GAL_CUT, NO_DIPOLE)
;               ** mollview and orthview only **
;
;       OFFSET: additive offset to apply to data (default = 0)
;               the data plotted is of the form FACTOR*(data + OFFSET)
;               can be used together with LOG
;               see also : FACTOR, LOG
;               Note : does NOT apply to polarization direction or amplitude
;               when POLARIZATION=3. Will apply to polarization amplitude when POLARIZATION=1.
;
;   ONLINE: if set, you can put directly A HEALPIX VECTOR on File (and
;       without header): useful when the vector is already
;       available on line, and avoid to have to write it on disk
;       just to be read by mollview
;   N.B. : the content of file_in is NOT altered in the
;   process
;               **  can not be used with /SAVE  **    *** OBSOLETE ***
;
;       OUTLINE : single structure, or set of structures, 
;                 each containing the coordinates of one outline to be overplotted.
;           Each structure should contain the following fields : 
;           - 'COORD' coordinate system (either, 'C', 'G', or 'E') of the contour
;           - 'RA'  or longitude coordinates (array)
;           - 'DEC' or lattitude coordinates (array of same size)
;           - 'LINE[STYLE]' : +2 : black dashes
;                           +1 : black dots
;                            0 : black solid [default]
;                           -1 : black dots on white background
;                           -2 : black dashes on white background
;           - 'PSY[M]' symbol used to represent vertices of outline
;                    (same meaning as standard PSYM in IDL,
;                     if 9<=abs(psym)<=46, D. Fanning's SYMCAT symbols 
;                     definition will be used, for example psym=9 is an open circle)
;                    if <=0, the vertices are represented with the chosen symbols, and
;                        connected, by arcs of geodesics.
;                    if >0, only the vertices are shown
;                    (default = 0)
;           - 'SYM[SIZE]' symbol size (same meaning as SYMSIZE in IDL)
;          Outline can be either a single structure, or an array of structures,
;          or a structure of structures
;
; PNG : string containing the name of a .PNG output
;       if set to 0 or not set : no .PNG done
;       if set to 1            : output the plot in plot_XXX.png
;                with XXX = azimequid, cartesian, gnomic, mollweide or orthographic
;       if set to a file name  : output the plot in that file 
;             (see also : CROP, GIF, JPEG, PNG, PS and PREVIEW)
;
;       POLARIZATION: 
;         if set to 0, no polarization information is plotted.
;
;         otherwise, and if the input data contains polarisation information
;             (ie, Stokes parameter Q and U for each pixel)
;
;         if set to 1 
;             the AMPLITUDE P = sqrt( U^2 + Q^2 ) of the polarisation is plotted
;
;         if set to 2 
;             the ANGLE phi = 0.5*ATAN(U/Q) of the polarisation is plotted
;             Note: the angles are color coded with a fixed color table (independant of Colt)
;
;         if set to 3 or [3, scale_factor, step_factor]
;             -the temperature is color coded (with a color table defined by Colt)
;             -and the polarisation is overplot as a headless vector
;             Polarization can be a 3-element vector (the first element being 3).
;             The second element controls the average length of vectors
;             (default=1), while the third one controls the distance between
;             vectors (default=1). Non positive values are replaced by 1.
;
; PREVIEW : if set, there is a 'ghostview' preview of the postscript file (see : PS)
;                    or a 'xv' preview of the gif or png file (see: CROP, GIF,
;                    JPEG, PNG and PS)
;
; PS :  if set to 0 or not set : output to screen
;       if set to 1            : output the plot in plot_XXX.ps
;                with XXX = azimequid, cartesian, gnomic, mollweide or orthographic
;       if set to a file name  : output the plot in that file 
;               (see: CROP, GIF, JPEG, PNG and PREVIEW)
;
;   PXSIZE: number of horizontal screen_pixels / postscript_color_dots of the plot
;       ** mollview : default = 800, gnomview and cartview : default = 500 **
;       (useful for high definition color printer)
;
;   PYSIZE: number of vertical screen_pixels or postscript_color_dots of the plot
;       default = PXSIZE
;       (useful for high definition color printer)
;                ** gnomview only **
;
;       RETAIN: backing store for graphics windows in {0,1,2}. Default=2
;
;       RESO_ARCMIN: resolution of the gnomic map in arcmin
;       (default=1.5)
;                ** gnomview only **
;
;   ROT :   vector with 1, 2 or 3 elements specifing the rotation angles in DEGREE
;               to apply to the map in the 'output' coordinate system (see coord)
;             = ( lon0, [lat0, rat0]) 
;               lon0 : longitude of the point to be put at the center of the plot
;          the longitude increases Eastward, ie to the left of the plot 
;                      (unless flip is set)
;           =0 by default
;               lat0 : latitude of the point to be put at the center of the plot
;           =0 by default
;               rot0 : anti clockwise rotation to apply to the sky around the
;                     center (lon0, lat0) before projecting
;                     =0 by default
;
;   SAVE: if set, tells that the file is in IDL saveset format, 
;       the variable saved should be DATA 
;                 ** can not be used with /ONLINE **
;
;       SHADED: if set, the orthographic sphere is shaded, using a Phong model, to emulate 3D viewing.
;              The sphere is illuminated by isotropic ambiant light plus a single light source.
;                 ** Can NOT be used with GIF. **
;                   ** orthview only **
;
;       SILENT: if set, the code runs silently
;
;       STAGGER: scalar or 2 element vector.
;            - if stagger[0] is in ]0,2], 
;             3 copies of the same sphere centered at [-stagger[0], 0, stagger[0]]
;             (expressed in radius units) along the plot horizontal axis are
;             shown in ORTHOGRAPHIC projection
;             - stagger[1] (if defined), defines the angle of rotation (in degrees) applied
;               to the left and right partial spheres:
;             the lhs sphere is rotated downward by the angle provided, while the rhs one
;             is rotated upward. Rotations are swapped if FLIP is set.
;               ** orthview only **
;
;   SUBTITLE : String containing the subtitle to the plot (see TITLEPLOT)
;
;   TITLEPLOT : String containing the title of the plot, 
;         if not set the title will be File (see SUBTITLE)
;
;       TRANSPARENT: some pixels are transparent in the produced PNG file
;            if set to 1: bad pixels (usually grey) are transparent
;            if set to 2: white background pixels are transparent
;            if set to 3: all of the above
;            only valid with PNG
;
;       TRUECOLORS: if the input data is of the form [Npix,3] then the 3 fields
;            are respectively understood as {Red, Green, Blue} True Colors
;
;
; UNITS : character string containing the units, to be put on the right
;   side of the color bar (see : NOBAR)
;
;       WINDOW: IDL window index (integer)
;                 if WINDOW < 0: virtual window: no visible window opened. Can be
;               used with PNG or GIF. The Z buffer will be used instead of the 
;               X server, allowing much faster production of the image over a slow network
;                 if WINDOW in [0,31]: the specified IDL window with index WINDOW is used
;               (or reused)
;                 if WINDOW > 31: a free (=unused) window with a random index > 31 will be
;               created and used : default

; XPOS : The X position on the screen of the lower left corner
;         of the window, in device coordinate
;
; YPOS : The Y position on the screen of the lower left corner 
;               of the window, in device coordinate
;
; NOTES
;   this routine doesn't use the IDL map_set because it is precisely bugged for 
;   the mollweide projection (at least in IDL 4.0)
;
; SIDE EFFECTS
;   this routine uses ghostview when PS and PREVIEW are selected 
; or xv when GIF or PNG and PREVIEW are selected
;
; EXAMPLES
;       ;to plot the signal of the COBE-DMR 4 year map at 53 GHz
;       read_fits_sb, 'dmr_skymap_53a_4yr.fits', dmr53a, /merge  ; read it only one time
;       mollview, dmr53a, /online, 'Sig', /quad
;
;       ;to plot it in Galactic coordinate instead of Ecliptic
;       mollview, drm53a, /online, 'Sig', /quad, coord='g'
;
; COMMONS USED : view_data
;
; PROCEDURES USED: 
;       in the Healpix package :
;   index_word, read_fits_sb, vec2pix_ring, vec2pix_nest, euler_matrix
;         see  http://www.tac.dk/~healpix
;       it also requires the IDL astro library
;         http://idlastro.gsfc.nasa.gov/homepage.html
;       and the COBE analysis software
;         http://www.gsfc.nasa.gov/astro/cobe/cgis.html
;
; MODIFICATION HISTORY:
;   October 1997, Eric Hivon, TAC
;   Nov, 5, 1997,  correction of some bugs for postscript output
;   13-Nov-97, H. Dole, IAS: save and log keywords
;   4-Dec-97, H. Dole, IAS: online keyword
;   16-Dec-97, E.H, TAC: added pxsize, hxsize, subtitle, nobar
; 17-Dec-97, split the loop for projection, added nolabels
; March-98, E.H. added UNITS keyword
; April-May-98 E.H. added NESTED_ONLINE, XPOS, YPOS, NOPREVIEW
;       March-99     E.H. Caltech, improved the GIF output
;              modified to deal with structures
;              added Select, COORD, ROT, QUADCUBE  suppressed LON0
;       April-99     E.H. Caltech, improved graticule
;       Nov-99         added flip
;       Feb-00   added rmmonopole and dipole, changed common
;       March-00   changed to no_monopole and no_dipole, changed common
;       Sept-00    added polarisation plotting (Polarization)
;       June-02  : EH, Caltech. Hacked G. Giardino's polview into cartview
;       June-02    partial consolidation of gnomview/mollview/cartview
;       Jan-07    added WINDOW keyword
;       Jun-07:  edited doc header about default data to plot from cut sky file
;       Sep-07:  added /SILENT
;       Mar-08:  added GLSIZE and IGLSIZE
;       Apr-08:  can deal with cut sky data set without creating full sky map
;       Nov-08:  restore original color table and plot settings when exiting
;       May-09:  added /SHADED to orthview, implemented EXECUTE in orthview, fix
;              Min-Max for LOG, use Z buffer when window<0, added RETAIN keyword
;       Oct-09:  added /TRUECOLORS to all routines and MAP_OUT= to Gnomview
;       Apr-10:  accept array of structures in Outline; added MAP_OUT= to
;       Cartview and Mollview
;       Jan-12: added STAGGER to orthview; created azeqview; added JPEG to all
;       Jan-2013, L. Spencer: Added CTDIR, CTFILE keywords to point to separate color table.
;                             Added GRMIN, GRMAX keywords to limit the graticule labels on map edges.
;                             Added GRLS to dictate graticule linestyle.
;                             Also added IGRMIN, IGRMAX, and IGRLS to do the same for input graticule.
;-

defsysv, '!healpix', exists = exists
if (exists ne 1) then init_healpix

@viewcom ; define common
data_plot = 0 ; empty common array
; record original color table and PLOTS settings
record_original_settings, original_settings

loadsky                         ; cgis package routine, define rotation matrices
projection = 'MOLLWEIDE'
routine = 'mollview'

uroutine = strupcase(routine)
if keyword_set(help) then begin
    doc_library,'mollview'
    return
endif

if keyword_set(gif) then begin
    message_gif, code=routine, error=error_gif
    if (error_gif) then return
endif

if (n_params() lt 1 or n_params() gt 2) then begin
    PRINT, 'Wrong number of arguments in '+uroutine
    print,'Syntax : '
    print, uroutine+', File, [Select, ]'
    print,'              [ASINH=, CHARSIZE=, COLT=, COORD=, CROP=, '
    print,'              EXECUTE=, FACTOR=, FLIP=, GAL_CUT=, GIF=, GLSIZE=, GRATICULE=, '
    print,'              HBOUND=, HELP=, '
    print,'              HIST_EQUAL=, HXSIZE=,'
    print,'              IGLSIZE=, IGRATICULE=,'
    print,'              JPEG=,'
    print,'              LOG=, '
    print,'              MAP_OUT=, MAX=, MIN=, NESTED=, NOBAR=, NOLABELS=, '
    print,'              NO_DIPOLE, NO_MONOPLE, '
    print,'              OFFSET=, ONLINE=, OUTLINE=,'
    print,'              PNG=,'
    print,'              POLARIZATION=, PREVIEW=, '
    print,'              PS=, PXSIZE=, PYSIZE=, QUADCUBE= ,'
    print,'              RETAIN=, ROT=, SAVE=, SILENT=, '
    print,'              SUBTITLE=, TITLEPLOT=, TRANSPARENT=, TRUECOLORS=, '
    print,'              UNITS=, WINDOW=, XPOS=, YPOS=]'
    print
    print,' Type '+uroutine+', /help '
    print,'   for an extended help'
    return
endif

IF (undefined(file_in)) then begin
    print,routine+': Undefined variable as 1st argument'
    return
endif
do_flip = keyword_set(flip)

if (!D.n_colors lt 4) then begin
    print,' : Sorry ... not enough colors ('+strtrim(string(!d.n_colors),2)+') available'
    return
endif

if (keyword_set(no_monopole) and keyword_set(no_dipole)) then begin
    print,routine+': choose either NO_MONOPOLE or NO_DIPOLE'
    print,'    (removal of best fit monopole only or best fit monopole+dipole)'
    return
endif

polar_type = 0
if keyword_set(polarization) then polar_type = polarization

loaddata_healpix, $
  file_in, select_in,$
  data, pol_data, pix_type, pix_param, do_conv, do_rot, coord_in, coord_out, eul_mat, title_display, sunits, $
  SAVE=save, ONLINE=online, NESTED=nested_online, UNITS=units, COORD=coord, FLIP=flip, $
  ROT=rot, QUADCUBE=quadcube, LOG=log, ERROR=error, $
  POLARIZATION=polarization, FACTOR=factor, OFFSET=offset, SILENT=silent, COMPRESS=1, PIXEL_LIST=pixel_list, $
  TRUECOLORS=truecolors, DATA_TC=data_tc
if error NE 0 then return

LS_data2moll, $
  data, pol_data, pix_type, pix_param, do_conv, do_rot, coord_in, coord_out, eul_mat, $
  planmap, Tmax, Tmin, color_bar, planvec, vector_scale, $
  PXSIZE=pxsize, LOG=log, HIST_EQUAL=hist_equal, MAX=max_set, MIN=min_set, FLIP=flip,  $
  NO_DIPOLE=no_dipole, NO_MONOPOLE=no_monopole, UNITS=sunits, DATA_plot = data_plot, GAL_CUT=gal_cut, $
  POLARIZATION=polarization, SILENT=silent, PIXEL_LIST=pixel_list, ASINH=asinh, $
  TRUECOLORS=truecolors, DATA_TC=data_tc, MAP_OUT = map_out, ROT=rot, FITS=fits, MODASINH=MODASINH

LS_proj2out, $
  planmap, Tmax, Tmin, color_bar, 0., title_display, $
  sunits, coord_out, do_rot, eul_mat, planvec, vector_scale, $
  CHARSIZE=charsize, COLT=colt, CROP=crop, GIF = gif, GRATICULE = graticule, $
  HXSIZE=hxsize, NOBAR = nobar, NOLABELS = nolabels, PNG = png, PREVIEW = preview, PS=ps, PXSIZE=pxsize, $
  SUBTITLE = subtitle, TITLEPLOT = titleplot, XPOS = xpos, YPOS = ypos, $
  POLARIZATION=polarization, OUTLINE=outline, /MOLL, FLIP=flip, COORD_IN=coord_in, IGRATICULE=igraticule, $
  HBOUND = hbound, WINDOW = window, EXECUTE=execute, SILENT=silent, GLSIZE=glsize, $
  IGLSIZE=iglsize, RETAIN=retain, TRUECOLORS=truecolors, TRANSPARENT=transparent, CHARTHICK=charthick, $
  JPEG=jpeg, CTDIR=CTDIR, CTFILE=CTFILE, GRMIN=GRMIN, GRMAX=GRMAX, GRLS=GRLS, IGRMIN=IGRMIN, IGRMAX=IGRMAX, IGRLS=IGRLS, $
  CBLBL=CBLBL, CBLIN=CBLIN, CBTICKS=CBTICKS, CBTICKVAL=CBTICKVAL, CBTICKLBL=CBTICKLBL, CBTICKLAB=CBTICKLAB, CBOUT=CBOUT, $
  MODASINH=MODASINH, HIST_EQUAL=HIST_EQUAL, ASINH=ASINH, LOG=LOG, LATLONGDIFF=LATLONGDIFF, FNTsz=FNTsz, CBOFF=CBOFF, MOLOFF=MAPOFF

w_num = !d.window
; restore original color table and PLOTS settings
record_original_settings, original_settings, /restore

return
end
;
;
;
;
;
; -----------------------------------------------------------------------------
;
;  Copyright (C) 1997-2012  Krzysztof M. Gorski, Eric Hivon, Anthony J. Banday
;
;
;
;
;
;  This file is part of HEALPix.
;
;  HEALPix is free software; you can redistribute it and/or modify
;  it under the terms of the GNU General Public License as published by
;  the Free Software Foundation; either version 2 of the License, or
;  (at your option) any later version.
;
;  HEALPix is distributed in the hope that it will be useful,
;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;  GNU General Public License for more details.
;
;  You should have received a copy of the GNU General Public License
;  along with HEALPix; if not, write to the Free Software
;  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
;
;  For more information about HEALPix see http://healpix.jpl.nasa.gov
;
; -----------------------------------------------------------------------------
pro LS_data2gnom, data, pol_data, pix_type, pix_param, do_conv, do_rot, coord_in, coord_out, eul_mat, $
               color, Tmax, Tmin, color_bar, dx, planvec, vector_scale, $
               PXSIZE=pxsize, PYSIZE=pysize, ROT=rot_ang, LOG=log, HIST_EQUAL=hist_equal, $
               MAX=max_set, MIN=min_set, $
               RESO_ARCMIN=reso_arcmin, FITS = fits, $
               FLIP=flip, DATA_plot = data_plot, $
               POLARIZATION=polarization, SILENT=silent, PIXEL_LIST=pixel_list, ASINH=asinh, $
               TRUECOLORS=truecolors, DATA_TC=data_tc, MAP_OUT=map_out, MODASINH=MODASINH

;+
;==============================================================================================
;     DATA2GNOM
;
;     turns a Healpix or Quad-cube map into in Gnomonic rectangular map
;
;     DATA2GNOM,  data, pix_type, pix_param, do_conv, do_rot, coord_in, coord_out, eul_mat,
;          color, Tmax, Tmin, color_bar, dx, planvec, vector_scale,
;          pxsize=, pysize=, rot=, log=, hist_equal=, max=, min=,
;          reso_arcmin=, fits=, flip=, data_plot=, polarization=, silent=,
;          pixel_list=, TRUECOLORS=, DATA_TC=, MAP_OUT=
;
; IN :
;      data, pix_type, pix_param, do_conv, do_rot, coord_in, coord_out, eul_mat
; OUT :
;      color, Tmax, Tmin, color_bar, dx, planvec, vector_scale
; KEYWORDS
;      Pxsize, Pysize, Rot, Log, Hist_equal, Max, Min, Reso_arcmin,
;      Fits, flip, data_plot, polarization, pixel_list, asinh, map_out
;
;  called by gnomview
;
;  HISTORY; Feb 2005: added small_file to avoid pol direction variation within pixels
; Sep 2007: added /silent
; April 2008: added pixel_list=
; July 2008: added asinh
; May 2009: can deal with maps without any valid pixel
;==============================================================================================
;-

do_true = keyword_set(truecolors) 
truetype = do_true ? truecolors : 0
proj_small = 'gnomic'
du_dv = 1.    ; aspect ratio
fudge = 1.00  ; 
if keyword_set(flip) then flipconv=1 else flipconv = -1  ; longitude increase leftward by default (astro convention)
if undefined(polarization) then polarization=0
do_polamplitude = (polarization[0] eq 1)
do_poldirection = (polarization[0] eq 2)
do_polvector    = (polarization[0] eq 3)

!P.BACKGROUND = 1               ; white background
!P.COLOR = 0                    ; black foreground

mode_col = keyword_set(hist_equal)
;mode_col = mode_col + 2*keyword_set(log) + 4*keyword_set(asinh)
mode_col = mode_col + 2*keyword_set(log) + 4*keyword_set(asinh) + 8*keyword_set(modasinh)

obs_npix = n_elements(data)
npix_full = (pix_type eq 'Q') ? 6*(4L)^(pix_param-1) : nside2npix(pix_param)
bad_data= !healpix.bad_value

if (do_poldirection or do_polvector) then begin
    ; compute new position of pixelisation North Pole in the plot coordinates
    north_pole = [0.,0.,1.]
    if (do_conv) then north_pole = SKYCONV(north_pole, inco= coord_in, outco=coord_out)
    if (do_rot) then north_pole = north_pole # transpose(eul_mat)
endif
; -------------------------------------------------------------
; create the rectangular window
; -------------------------------------------------------------
if defined(pxsize) then xsize = pxsize*1L else xsize = 500L
if defined(pysize) then ysize = pysize*1L else ysize = xsize
if defined(reso_arcmin) then resgrid = reso_arcmin/60. else resgrid = 1.5/60.
dx      = resgrid * !DtoR
zsize = (do_true) ? 3 : 1
N_uv = xsize*ysize
indlist = (n_elements(pixel_list) eq n_elements(data[*,0]))
small_file = ((!pi*4./dx^2 GT npix_full && do_poldirection))


if (~keyword_set(silent)) then begin
    print,'Input map  :  ',3600.*6.d0/sqrt(!dpi*npix_full),' arcmin / pixel ',form='(a,f8.3,a)'
    print,'gnomonic map :',resgrid*60.,' arcmin / pixel ',xsize,'*',ysize,form='(a,f8.3,a,i4,a,i4)'
endif

if (small_file) then begin
    ; file smaller than final map, make costly operation on the file
    ; initial data is destroyed and replaced by color
    if (do_poldirection or do_polvector) then begin
        phi = 0.
        if (do_rot or do_conv) then begin
            ; position of each map pixel after rotation and coordinate changes
            if (indlist) then begin
                id_pix = pixel_list
            endif else begin
                id_pix = lindgen(npix_full)
            endelse
            case pix_type of
                'R' : PIX2VEC_RING, pix_param, id_pix, vector ; Healpix ring
                'N' : PIX2VEC_NEST, pix_param, id_pix, vector; Healpix nest
                'Q' : vector = PIX2UV(pix_param, id_pix) ; QuadCube (COBE cgis software)
                else : print,'error on pix_type'
            endcase
            id_pix = 0
            if (do_conv) then vector = SKYCONV(vector, inco= coord_in, outco=coord_out)
            if (do_rot) then vector = vector # transpose(eul_mat)
            ; compute rotation of local coordinates around each vector
            tmp_sin = north_pole[1] * vector[*,0] - north_pole[0] * vector[*,1]
            tmp_cos = north_pole[2] - vector[*,2] * (north_pole[0:2] ## vector)
            if (flipconv lt 0) then tmp_cos = flipconv * tmp_cos
            phi = ATAN(tmp_sin, tmp_cos) ; angle in radians
            tmp_sin = 0. & tmp_cos = 0 & vector = 0.
        endif
        data_plot = data
        if (do_poldirection) then begin
            data = (data - phi + 4*!PI) MOD (2*!PI) ; angle
            min_set = 0. & max_set = 2*!pi
        endif
        if (do_polvector) then begin
            pol_data[*,1] = (pol_data[*,1] - phi + 4*!PI) MOD (2*!PI) ; angle is rotated
        endif
    endif else begin ; temperature only or polarisation amplitude only
        data_plot = data
    endelse
    ; color observed pixels
    if (do_true) then begin
        if (truetype eq 2) then begin
            for i=0,2 do begin
                find_min_max_valid, data_tc[*,i], mindata, maxdata, valid=Obs, bad_data = 0.9 * bad_data
                data_tc[0,i] = LS_COLOR_MAP(data_tc[*,i], mindata, maxdata, Obs, $
                                    color_bar = color_bar, mode=mode_col, silent=silent )
            endfor
        endif else begin
            find_min_max_valid, data_tc, mindata, maxdata, valid=Obs, bad_data = 0.9 * bad_data
            data_tc = LS_COLOR_MAP(data_tc, mindata, maxdata, Obs, $
                                color_bar = color_bar, mode=mode_col, $
                                minset = min_set, maxset = max_set, silent=silent )
        endelse
    endif else begin
        find_min_max_valid, data, mindata, maxdata, valid=Obs, bad_data = 0.9 * bad_data
        data    = LS_COLOR_MAP(data, mindata, maxdata, Obs, $
                         color_bar = color_bar, mode=mode_col, $
                         minset = min_set, maxset = max_set, silent=silent )
    endelse
    if (do_polvector) then begin ; rescale polarisation vector in each valid pixel
        pol_data[0,0] = vector_map(pol_data[*,0], Obs, vector_scale = vector_scale)
    endif
    if defined(Obs) then Obs = 0
    Tmin = mindata & Tmax = maxdata
    color = MAKE_ARRAY(/BYTE, xsize, ysize, zsize, Value = !P.BACKGROUND) ; white
    grid = FLTARR(xsize, ysize)
endif else begin ; large
    grid = FLTARR(xsize, ysize, zsize)
endelse
if do_polvector then planvec = MAKE_ARRAY(/FLOAT,xsize,ysize, 2, Value = bad_data) 
; -------------------------------------------------------------
; makes the projection around the chosen contact point
; -------------------------------------------------------------
; position on the planar grid  (1,u,v)
x0 = +1.
xll= 0 & xur =  xsize-1
yll= 0 & yur =  ysize-1
xc = 0.5*(xll+xur)  ; & deltax = (xur - xc)
yc = 0.5*(yll+yur)  ; & deltay = (yur - yc)

yband = LONG(5.e5 / FLOAT(xsize))
for ystart = 0, ysize - 1, yband do begin 
    yend   = (ystart + yband - 1) < (ysize - 1)
    nband = yend - ystart + 1
    npb = xsize * nband
    u = flipconv*(FINDGEN(xsize) - xc)# REPLICATE(dx,nband)   ; minus sign = astro convention
    v =           REPLICATE(dx,xsize) # (FINDGEN(nband) + ystart - yc)
    x = replicate(x0, npb)
    vector = [[x],[reform(u,npb,/over)],[reform(v,npb,/over)]] ; non normalised vector
    ; --------------------------------
    ; deal with polarisation direction
    ; --------------------------------
    if (do_poldirection or do_polvector) then begin
        phi = 0.
        if (do_rot or do_conv) then begin
            vector = vector / (sqrt(total(vector^2, 2))#replicate(1,3)) ; normalize vector
            ; compute rotation of local coordinates around each vector
            tmp_sin = north_pole[1] * vector[*,0] - north_pole[0] * vector[*,1]
            tmp_cos = north_pole[2] - vector[*,2] * (north_pole[0:2] ## vector)
            if (flipconv lt 0) then tmp_cos = flipconv * tmp_cos
            phi = ATAN(tmp_sin, tmp_cos) ; angle in radians
            tmp_sin = 0. & tmp_cos = 0
        endif
    endif
    ; ---------
    ; rotation
    ; ---------
    if (do_rot) then vector = vector # eul_mat
    if (do_conv) then vector = SKYCONV(vector, inco = coord_out, outco =  coord_in)
          ; we go from the final Gnomonic map (system coord_out) to
          ; the original one (system coord_in)
    ; -------------------------------------------------------------
    ; converts the position on the sphere into pixel number
    ; and project the corresponding data value on the map
    ; -------------------------------------------------------------
    case pix_type of
        'R' : VEC2PIX_RING, pix_param, vector, id_pix ; Healpix ring
        'N' : VEC2PIX_NEST, pix_param, vector, id_pix ; Healpix nest
        'Q' : id_pix = UV2PIX(vector, pix_param) ; QuadCube (COBE cgis software)
        else : print,'error on pix_type'
    endcase
    if (small_file) then begin ; (data and data_pol are already rescaled and color coded)
        if (do_true) then begin
            for i=0,zsize-1 do color[ystart*xsize+i*n_uv] = data[id_pix,i]
            grid[ystart*xsize]  = data_plot[id_pix] ; unaltered data
        endif else begin
            color[ystart*xsize] = data[id_pix]
            grid[ystart*xsize]  = data_plot[id_pix] ; unaltered data
            if (do_polvector) then begin
                planvec[ystart*xsize]       = pol_data[id_pix,0] ; amplitude
                planvec[ystart*xsize+n_uv]  = pol_data[id_pix,1] ; direction
            endif
        endelse
    endif else begin            ; (large file : do the projection first)
        if (do_true) then begin
            for i=0,zsize-1 do grid[ystart*xsize+i*n_uv] = data_tc[id_pix,i]
        endif else begin
            if (do_poldirection) then begin
                grid[ystart*xsize] = (data[id_pix] - phi + 4*!PI) MOD (2*!PI) ; in 0,2pi
            endif else if (do_polvector) then begin
                grid[ystart*xsize]         = data[id_pix]
                planvec[ystart*xsize]      = pol_data[id_pix,0]
                planvec[ystart*xsize+n_uv] = (pol_data[id_pix,1] - phi + 4*!PI) MOD (2*!PI) ; angle
            endif else begin
;;;            grid[ystart*xsize] = data[id_pix]
                grid[ystart*xsize] = sample_sparse_array(data, id_pix, in_pix=pixel_list, default= !healpix.bad_value)
            endelse
        endelse
    endelse
endfor
u = 0 & v = 0 & x = 0 & vector = 0

; -------------------------------------------------------------
; Test for unobserved pixels
; -------------------------------------------------------------
if (small_file) then begin
    data = 0 & pol_data = 0
endif else begin
    data_plot = temporary(data)
    pol_data = 0
    find_min_max_valid, grid, mindata, maxdata, valid=Obs, bad_data = 0.9 * bad_data
endelse

;-----------------------------------
; export in FITS and as an array the original gnomic map before alteration
;-----------------------------------

; grid -> IDL array
if arg_present(map_out) then map_out = proj2map_out(grid, bad_data=bad_data)

; grid -> FITS file
if keyword_set(fits) then begin 
    proj2fits, grid, fits, $
               projection = 'GNOM', flip=flip, $
               rot = rot_ang, coord=coord_out, reso = resgrid*60., unit = sunits, min=mindata, max = maxdata
endif

; -------------------------------------------------------------
; set min and max and computes the color scaling
; -------------------------------------------------------------
if (small_file) then begin

endif else begin
    if (do_poldirection) then begin
        min_set = 0.
        max_set = 2*!pi
    endif
    if (truetype eq 2) then begin
        ; truecolors=2 map each field to its color independently
        color = bytarr(xsize,ysize,zsize)
        for i=0,zsize-1 do begin
            find_min_max_valid, grid[*,*,i], mindata, maxdata, valid=Obs, bad_data = 0.9 * bad_data
            color[0,0,i] = COLOR_MAP(grid[*,*,i], mindata, maxdata, Obs, $
                          color_bar = color_bar, mode=mode_col, silent=silent)
        endfor
    endif else begin
        ; same for truecolors=1 and false colors:
        color = COLOR_MAP(grid, mindata, maxdata, Obs, $
                          color_bar = color_bar, mode=mode_col, $
                          minset = min_set, maxset = max_set, silent=silent)
    endelse
        
    if (do_polvector) then begin ; rescale polarisation vector in each valid pixel
        planvec[*,*,0] = vector_map(planvec[*,*,0], Obs, vector_scale = vector_scale)
    endif
    Obs = 0
    grid = 0
    Tmin = mindata & Tmax = maxdata
endelse

return
end
;
;
;
;
;
;
;
;
; -----------------------------------------------------------------------------
;
;  Copyright (C) 1997-2012  Krzysztof M. Gorski, Eric Hivon, Anthony J. Banday
;
;
;
;
;
;  This file is part of HEALPix.
;
;  HEALPix is free software; you can redistribute it and/or modify
;  it under the terms of the GNU General Public License as published by
;  the Free Software Foundation; either version 2 of the License, or
;  (at your option) any later version.
;
;  HEALPix is distributed in the hope that it will be useful,
;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;  GNU General Public License for more details.
;
;  You should have received a copy of the GNU General Public License
;  along with HEALPix; if not, write to the Free Software
;  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
;
;  For more information about HEALPix see http://healpix.jpl.nasa.gov
;
; -----------------------------------------------------------------------------
PRO LS_gnomview, file_in, select_in, $
              ASINH = asinh, $
              CHARSIZE = charsize, $
              CHARTHICK = charthick, $
              COLT = colt, $
              COORD = coord, $
              CROP = crop, $
              EXECUTE=execute, $
              FACTOR = factor, $
              FITS = fits, $
              FLIP = flip, $
              GIF = gif, $
              GLSIZE = glsize, $
              GRATICULE = graticule, $
              HBOUND = hbound, $
              HELP = help, $
              HIST_EQUAL = hist_equal, $
              HXSIZE = hxsize, $
              IGLSIZE = iglsize, $
              IGRATICULE = igraticule, $
              JPEG=jpeg, $
              LOG = log, $
              MAP_OUT = map_out, $
              MAX = max_set, $
              MIN = min_set, $
              NESTED = nested_online, $
              NOBAR = nobar, $
              NOLABELS = nolabels, $
              NOPOSITION = noposition, $
              OFFSET = offset, $
              ONLINE = online, $
              OUTLINE = outline, $
              PNG = png, $
              POLARIZATION = polarization, $
              PREVIEW = preview, $
              PS = ps, $
              PXSIZE = pxsize, $
              PYSIZE = pysize, $
              QUADCUBE = quadcube, $
              RESO_ARCMIN = reso_arcmin, $
              RETAIN = retain, $
              ROT = rot, $
              SAVE = save, $
              SILENT = silent, $
              SUBTITLE = subtitle, $
              TITLEPLOT = titleplot, $
              TRANSPARENT = transparent, $
              TRUECOLORS = truecolors, $
              UNITS = units, $
              WINDOW = window, $
              XPOS = xpos, $
              YPOS = ypos, $
              vector_scale = vector_scale, $
              CTDIR=CTDIR, CTFILE=CTFILE, GRMIN=GRMIN, GRMAX=GRMAX, GRLS=GRLS, IGRMIN=IGRMIN, IGRMAX=IGRMAX, IGRLS=IGRLS, $
              CBLBL=CBLBL, CBLIN=CBLIN, CBTICKS=CBTICKS, CBTICKVAL=CBTICKVAL, CBTICKLBL=CBTICKLBL, CBTICKLAB=CBTICKLAB, CBOUT=CBOUT, CBLABOFF=CBLABOFF, $
              MODASINH=MODASINH, LATLONGDIFF=LATLONGDIFF, CORDOFF=CORDOFF, MAPOFF=MAPOFF, FNTsz=FNTsz, CBOFF=CBOFF

;+
; for extended description see mollview or the paper documentation
;-
IF N_ELEMENTS(CBLABOFF) EQ 0 THEN CBLABOFF = 0.01d ; 0.0125d ; 0.025d
IF N_ELEMENTS(CORDOFF) EQ 0 THEN BEGIN
  IF N_ELEMENTS(CBLBL) GT 0 THEN CORDOFF = 0d ELSE CORDOFF = 0.025d
ENDIF
defsysv, '!healpix', exists = exists
if (exists ne 1) then init_healpix
@viewcom ; define common

data_plot = 0 ; empty common array
; record original color table and PLOTS settings
record_original_settings, original_settings

loadsky                         ; cgis package routine, define rotation matrices
projection = 'GNOMIC'
routine = 'gnomview'

uroutine = strupcase(routine)
if keyword_set(help) then begin
    doc_library,'mollview'
    return
endif

if keyword_set(gif) then begin
    message_gif, code=routine, error=error_gif
    if (error_gif) then return
endif

if (n_params() lt 1 or n_params() gt 2) then begin
    PRINT, 'Wrong number of arguments in '+uroutine
    print,'Syntax : '
    print, uroutine+', File, [Select, ]'
    print,'              [ASINH=, CHARSIZE=, COLT=, COORD=, CROP=, '
    print,'              EXECUTE=, FACTOR=, FITS=, FLIP=, GIF=, GLSIZE=, GRATICULE=, '
    print,'              HBOUND=, HELP=, '
    print,'              HIST_EQUAL=, HXSIZE=, '
    print,'              IGLSIZE=, IGRATICULE=,'
    print,'              JPEG=, '
    print,'              LOG=, '
    print,'              MAP_OUT=, MAX=, MIN=, '
    print,'              NESTED=, NOBAR=, NOLABELS=, NOPOSITION = '
    print,'              OFFSET=, ONLINE=, OUTLINE=,'
    print,'              PNG=,'
    print,'              POLARIZATION=, PREVIEW=, '
    print,'              PS=, PXSIZE=, PYSIZE=, QUADCUBE= ,'
    print,'              RESO_ARCMIN=, RETAIN =, ROT=, '
    print,'              SAVE=, SILENT=, SUBTITLE=, '
    print,'              TITLEPLOT=, TRANSPARENT=, TRUECOLORS= '
    print,'              UNITS=, WINDOW=, XPOS=, YPOS=]'
    print
    print,' Type '+uroutine+', /help '
    print,'   for an extended help'
    return
endif

IF (undefined(file_in)) then begin
    print,routine+': Undefined variable as 1st argument'
    return
endif
; file_in1   = file_in
; if defined(select_in) then select_in1 = select_in else select_in1=1
; if defined(save)      then save1 = save           else save1=0
; if defined(online)    then online1 = online       else online1=0
do_flip = keyword_set(flip)

if (!D.n_colors lt 4) then begin
    print,' : Sorry ... not enough colors ('+strtrim(string(!d.n_colors),2)+') available'
    return
endif

polar_type = 0
if keyword_set(polarization) then polar_type = polarization

loaddata_healpix, $
  file_in, select_in,$
  data, pol_data, pix_type, pix_param, do_conv, do_rot, coord_in, coord_out, eul_mat, title_display, sunits, $
  SAVE=save,ONLINE=online,NESTED=nested_online,UNITS=units,COORD=coord,FLIP=flip, $
  ROT=rot,QUADCUBE=quadcube,LOG=log,ERROR=error, $
  POLARIZATION=polarization, FACTOR=factor, OFFSET=offset, SILENT=silent, COMPRESS=1, PIXEL_LIST=pixel_list, $
  TRUECOLORS=truecolors, DATA_TC=data_tc
if error NE 0 then return

LS_data2gnom, $
  data, pol_data, pix_type, pix_param, do_conv, do_rot, coord_in, coord_out, eul_mat, $
  planmap, Tmax, Tmin, color_bar, dx, planvec, vector_scale, $
  PXSIZE=pxsize, PYSIZE=pysize, ROT=rot, LOG=log, HIST_EQUAL=hist_equal, $
  MAX=max_set, MIN=min_set, $
  RESO_ARCMIN = reso_arcmin, FITS = fits, FLIP=flip, DATA_plot = data_plot, $
  POLARIZATION=polarization, SILENT=silent, PIXEL_LIST=pixel_list, ASINH=asinh, $
  TRUECOLORS=truecolors, DATA_TC=data_tc, MAP_OUT=map_out, MODASINH=MODASINH

LS_proj2out, $
  planmap, Tmax, Tmin, color_bar, dx, title_display, $
  sunits, coord_out, do_rot, eul_mat, planvec, vector_scale, $
  CHARSIZE=charsize, COLT=colt, CROP=crop, GIF = gif, GRATICULE = graticule, HXSIZE = hxsize, $
  NOBAR = nobar, NOLABELS = nolabels, NOPOSITION = noposition, PNG = png, PREVIEW = preview, PS = ps, $
  PXSIZE=pxsize, PYSIZE=pysize, ROT = rot, SUBTITLE = subtitle, $
  TITLEPLOT = titleplot, XPOS = xpos, YPOS = ypos, $
  POLARIZATION=polarization, OUTLINE=outline, /GNOM, FLIP=flip, COORD_IN=coord_in, IGRATICULE=igraticule, $
  HBOUND = hbound, WINDOW = window, EXECUTE=execute, SILENT=silent, GLSIZE=glsize, $
  IGLSIZE=iglsize, RETAIN=retain, TRUECOLORS=truecolors, TRANSPARENT=transparent, $
  CHARTHICK=charthick, JPEG=jpeg, CTDIR=CTDIR, CTFILE=CTFILE, GRMIN=GRMIN, GRMAX=GRMAX, GRLS=GRLS, IGRMIN=IGRMIN, IGRMAX=IGRMAX, IGRLS=IGRLS, $
  CBLBL=CBLBL, CBLIN=CBLIN, CBTICKS=CBTICKS, CBTICKVAL=CBTICKVAL, CBTICKLBL=CBTICKLBL, CBTICKLAB=CBTICKLAB, CBOUT=CBOUT, CBLABOFF=CBLABOFF, $
  MODASINH=MODASINH, HIST_EQUAL=HIST_EQUAL, ASINH=ASINH, LOG=LOG, LATLONGDIFF=LATLONGDIFF, GNMCORDOFF=CORDOFF, GNMOFF=MAPOFF, FNTsz=FNTsz, CBOFF=CBOFF

w_num = !d.window
; restore original color table and PLOTS settings
record_original_settings, original_settings, /restore

RETURN
END
;
;
;
;
; -----------------------------------------------------------------------------
;
;  Copyright (C) 1997-2012  Krzysztof M. Gorski, Eric Hivon, Anthony J. Banday
;
;
;
;
;
;  This file is part of HEALPix.
;
;  HEALPix is free software; you can redistribute it and/or modify
;  it under the terms of the GNU General Public License as published by
;  the Free Software Foundation; either version 2 of the License, or
;  (at your option) any later version.
;
;  HEALPix is distributed in the hope that it will be useful,
;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;  GNU General Public License for more details.
;
;  You should have received a copy of the GNU General Public License
;  along with HEALPix; if not, write to the Free Software
;  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
;
;  For more information about HEALPix see http://healpix.jpl.nasa.gov
;
; -----------------------------------------------------------------------------
PRO LS_cartview, file_in, select_in, $
              ASINH = asinh, $
              CHARSIZE = charsize, $
              CHARTHICK = charthick, $
              COLT = colt, $
              COORD = coord, $
              CROP = crop, $
              EXECUTE = execute, $
              FACTOR = factor, $
              FITS = fits, $
              FLIP = flip, $
              GIF = gif, $
              GLSIZE = glsize, $
              GRATICULE = graticule, $
              HBOUND = hbound, $
              HELP = help, $
              HIST_EQUAL = hist_equal, $
              HXSIZE = hxsize, $
              IGLSIZE = iglsize, $
              IGRATICULE = igraticule, $
              JPEG=jpeg, $
              LOG = log, $
              MAP_OUT=map_out, $
              MAX = max_set, $
              MIN = min_set, $
              NESTED = nested_online, $
              NOBAR = nobar, $
              NOLABELS = nolabels, $
              NOPOSITION = noposition, $
              OFFSET=offset, $
              ONLINE = online, $
              OUTLINE = outline, $
              PNG = png, $
              POLARIZATION = polarization, $
              PREVIEW = preview, $
              PS = ps, $
              PXSIZE = pxsize, $
              PYSIZE = pysize, $
              QUADCUBE = quadcube, $
              RESO_ARCMIN = reso_arcmin, $
              RETAIN = retain, $
              ROT = rot, $
              SAVE = save, $
              SILENT = silent, $
              SUBTITLE = subtitle, $
              TITLEPLOT = titleplot, $
              TRANSPARENT = transparent, $
              TRUECOLORS = truecolors, $
              UNITS = units, $
              WINDOW = window, $
              XPOS = xpos, $
              YPOS = ypos, $
              CTDIR=CTDIR, $
              CTFILE=CTFILE, GRMIN=GRMIN, GRMAX=GRMAX, GRLS=GRLS, IGRMIN=IGRMIN, IGRMAX=IGRMAX, IGRLS=IGRLS, MODASINH=MODASINH, $
              CBLBL=CBLBL, CBLIN=CBLIN, CBTICKS=CBTICKS, CBTICKVAL=CBTICKVAL, CBTICKLBL=CBTICKLBL, CBTICKLAB=CBTICKLAB, CBOUT=CBOUT, CBLABOFF=CBLABOFF, $
              LATLONGDIFF=LATLONGDIFF, FNTsz=FNTsz, CBOFF=CBOFF, CORDOFF=CORDOFF, MAPOFF=MAPOFF, $
              vector_scale = vector_scale
;+
; for extended description see mollview or the paper documentation
;-
IF N_ELEMENTS(CBLABOFF) EQ 0 THEN CBLABOFF = 0.01d ; 0.0125d ; 0.025d
defsysv, '!healpix', exists = exists
if (exists ne 1) then init_healpix

@viewcom ; define common
data_plot = 0 ; empty common array
; record original color table and PLOTS settings
record_original_settings, original_settings

loadsky                         ; cgis package routine, define rotation matrices
projection = 'CARTESIAN'
routine = 'cartview'

uroutine = strupcase(routine)
if keyword_set(help) then begin
    doc_library,'mollview'
    return
endif

if keyword_set(gif) then begin
    message_gif, code=routine, error=error_gif
    if (error_gif) then return
endif

if (n_params() lt 1 or n_params() gt 2) then begin
    PRINT, 'Wrong number of arguments in '+uroutine
    print,'Syntax : '
    print, uroutine+', File, [Select, ]'
    print,'              [ASINH=, CHARSIZE=, COLT=, COORD=, CROP=, '
    print,'              EXECUTE=, FACTOR=, FITS=, FLIP=, GIF=, GLSIZE=, GRATICULE=, '
    print,'              HBOUND, HELP=, '
    print,'              HIST_EQUAL=, HXSIZE=,'
    print,'              IGLSIZE=, IGRATICULE=,'
    print,'              JPEG=,'
    print,'              LOG=, '
    print,'              MAX=, MIN=, NESTED=, NOBAR=, NOLABELS=, NOPOSITION = '
;    print,'              NO_DIPOLE, NO_MONOPLE, '
    print,'              OFFSET=, ONLINE=, OUTLINE=,'
    print,'              PNG=,'
    print,'              POLARIZATION=, PREVIEW=, '
    print,'              PS=, PXSIZE=, PYSIZE=, QUADCUBE= ,'
    print,'              RESO_ARCMIN=, RETAIN=, ROT=, '
    print,'              SAVE=, SILENT=, SUBTITLE=, '
    print,'              TITLEPLOT=, TRANSPARENT=, TRUECOLORS= '
    print,'              UNITS=, WINDOW=, XPOS=, YPOS=]'
    print
    print,' Type '+uroutine+', /help '
    print,'   for an extended help'
    return
endif

IF (undefined(file_in)) then begin
    print,routine+': Undefined variable as 1st argument'
    return
endif
; file_in1   = file_in
; if defined(select_in) then select_in1 = select_in else select_in1=1
; if defined(save)      then save1 = save           else save1=0
; if defined(online)    then online1 = online       else online1=0
do_flip = keyword_set(flip)

if (!D.n_colors lt 4) then begin
    print,' : Sorry ... not enough colors ('+strtrim(string(!d.n_colors),2)+') available'
    return
endif

polar_type = 0
if keyword_set(polarization) then polar_type = polarization


loaddata_healpix, $
  file_in, select_in,$
  data, pol_data, pix_type, pix_param, do_conv, do_rot, coord_in, coord_out, eul_mat, title_display, sunits, $
  SAVE=save,ONLINE=online,NESTED=nested_online,UNITS=units,COORD=coord,FLIP=flip, $
  ROT=rot,QUADCUBE=quadcube,LOG=log,ERROR=error, $
  POLARIZATION=polarization, FACTOR=factor, OFFSET=offset, SILENT=silent, COMPRESS=1, $
  PIXEL_LIST=pixel_list, TRUECOLORS=truecolors, DATA_TC=data_tc
if error NE 0 then return


LS_data2cart, $
  data, pol_data, pix_type, pix_param, do_conv, do_rot, coord_in, coord_out, eul_mat, $
  planmap, Tmax, Tmin, color_bar, dx, planvec, vector_scale, $
  PXSIZE=pxsize, PYSIZE=pysize, ROT=rot, LOG=log, HIST_EQUAL=hist_equal, $
  MAX=max_set, MIN=min_set, $
  RESO_ARCMIN = reso_arcmin, FITS = fits, FLIP=flip, DATA_plot = data_plot, $
  POLARIZATION=polarization, SILENT=silent, PIXEL_LIST=pixel_list, ASINH=asinh, $
  TRUECOLORS=truecolors, DATA_TC=data_tc, MAP_OUT=map_out, MODASINH=MODASINH

LS_proj2out, $
  planmap, Tmax, Tmin, color_bar, dx, title_display, $
  sunits, coord_out, do_rot, eul_mat, planvec, vector_scale, $
  CHARSIZE=charsize, COLT=colt, CROP=crop, GIF = gif, GRATICULE = graticule, HXSIZE = hxsize, $
  NOBAR = nobar, NOLABELS = nolabels, NOPOSITION = noposition, PNG = png, PREVIEW = preview, PS = ps, $
  PXSIZE=pxsize, PYSIZE=pysize, ROT = rot, SUBTITLE = subtitle, $
  TITLEPLOT = titleplot, XPOS = xpos, YPOS = ypos, $
  POLARIZATION=polarization, OUTLINE=outline, /CART, FLIP=flip, COORD_IN=coord_in, IGRATICULE=igraticule, $
  HBOUND = hbound, WINDOW = window, TRANSPARENT = transparent, EXECUTE=execute, $
  SILENT=silent, GLSIZE = glsize, IGLSIZE = iglsize, RETAIN=retain, TRUECOLORS=truecolors, CHARTHICK=charthick, $
  JPEG=jpeg, CTDIR=CTDIR, CTFILE=CTFILE, GRMIN=GRMIN, GRMAX=GRMAX, GRLS=GRLS, IGRMIN=IGRMIN, IGRMAX=IGRMAX, IGRLS=IGRLS, $
  CBLBL=CBLBL, CBLIN=CBLIN, CBTICKS=CBTICKS, CBTICKVAL=CBTICKVAL, CBTICKLBL=CBTICKLBL, CBTICKLAB=CBTICKLAB, CBOUT=CBOUT, CBLABOFF=CBLABOFF, $
  MODASINH=MODASINH, HIST_EQUAL=HIST_EQUAL, ASINH=ASINH, LOG=LOG, LATLONGDIFF=LATLONGDIFF, FNTsz=FNTsz, CBOFF=CBOFF, CRTCORDOFF=CORDOFF, CRTOFF=MAPOFF

w_num = !d.window
; restore original color table and PLOTS settings
record_original_settings, original_settings, /restore

RETURN
END
;
;
