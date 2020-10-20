C
C   Calcomp compatible GKS plotting library 
C   modified for GKS by Dan Severance, Purdue, 
C
C   In reality this is just our old calcomp compatible libraries 
C   with the graphics calls converted to GKS calls... Not a lot of work :-)
C
C   You will need to modify the include file declarations if you are
C   not on a vax, you need to define the appropriate binding defs.
C   On a VAX with GKS, just uncomment the include lines and you're set.
C
      SUBROUTINE PLOTS
C
C     INCLUDE 'SYS$LIBRARY:GKSDEFS.BND'
C
      COMMON /PSCALE/ XMIN,XMAX,YMIN,YMAX
      INTEGER WSID
      INTEGER IDUM,GCONID,GWSDEF
      LOGICAL INIPLT
      DATA INIPLT / .FALSE. /
      DATA XMIN,XMAX,YMIN,YMAX / 0.0,12.4,0.0,12.4 /
      DATA WSID / 1 /
      DATA GCONID /0/, GWSDEF /4/
      COMMON /INIT/ INIPLT
      IF (.NOT.INIPLT) THEN
         CALL GOPKS (6, IDUM)
         CALL GOPWK (WSID,GCONID,GWSDEF)
         CALL GACWK (WSID)
         CALL GSWN (1,XMIN,XMAX,YMIN,YMAX)
         Call GSClip(0)
         CALL GSVP (1,0.0,1.0,0.0,1.0)
         CALL GSELNT (1)
         CALL GSLWSC (1.5)
C
C     Colors
C
         !Call gscr(1,0,1.0,1.0,1.0) ! background color white
         Call Gscr(1,0,0.0,0.0,0.0) ! background color black
         !Call gscr(1,1,0.0,0.0,0.0) ! bond color black
         Call Gscr(1,1,1.0,1.0,1.0) ! bond color white
         Call Gscr(1,2,0.0,0.0,1.0) ! dashed line color
         Call Gscr(1,3,1.0,0.0,0.0) ! normal line color
         Call Gscr(1,4,0.5,0.5,0.5) ! text color
C     Set text color
         Call Gstxci(4)
      ELSE
         CALL GCLRWK (WSID,0)
      ENDIF
      INIPLT = .TRUE.
      RETURN
      END
C
C
      SUBROUTINE NEWPEN (I)
      COMMON /INIT/ INIPLT
      LOGICAL INIPLT
      ENTRY LINCLR (I)
      IF (.NOT.INIPLT) CALL PLOTS
      CALL GSPLCI (I)
      RETURN
      END
C
C----------------------------------------------------------------------
C
      SUBROUTINE WHERE (X,Y,FACTR)
C
C----------------------------------------------------------------------
C
C   RETURN THE CURRENT PEN POSITION AND SCALE FACTOR
C
      COMMON /CALCOM/ SFACTR,IXCUR,IYCUR,IORGNX,IORGNY
      COMMON /INIT/ INIPLT
      LOGICAL INIPLT
      IF (.NOT.INIPLT) CALL PLOTS
      SFACTR = 1.0
      X = IXCUR/(100.0*SFACTR)
      Y = IYCUR/(100.0*SFACTR)
      FACTR = SFACTR
      RETURN
      END
C
C----------------------------------------------------------------------
C
      SUBROUTINE SYMBOL (X1,Y1,HEIGHT,STR,THETA,NCHAR)
C
C     INCLUDE 'SYS$LIBRARY:GKSDEFS.BND'
C----------------------------------------------------------------------
C
C     OUTPUT A CHARACTER STRING
C
      COMMON /WHRE/ XDNLST,YDNLST,XARRAY(2),YARRAY(2)
      CHARACTER*(*) STR
      COMMON /ORIGIN/ ORIGX,ORIGY
      COMMON /INIT/ INIPLT
      LOGICAL INIPLT
      IF (.NOT.INIPLT) CALL PLOTS
      H = HEIGHT
      NC = NCHAR
      TH = THETA*0.017453
      SI = SIN(TH)
      CO = COS(TH)
      IF (ABS(SI).GT.ABS(CO)) THEN
         SINEW = SI/ABS(SI)
         CONEW = CO/ABS(SI)
         IF (ABS(CONEW).LT.0.01) CONEW = 0.0
      ELSE
         SINEW = SI/ABS(CO)
         IF (ABS(SINEW).LT.0.01) SINEW = 0.0
         CONEW = CO/ABS(CO)
      ENDIF
      CALL GSCHUP (SINEW,CONEW)
      CALL GSCHH (HEIGHT)
      !CALL GSTXP (GRIGHT)
      !CALL GSTXFP (-12,GSTRKP)
C
C     TITLE text case. set ORIGX,Y = 0,0
C
      If (NC.EQ.120) Then
         TMPX = ORIGX
         TMPY = ORIGY
         ORIGX = 0.0
         ORIGY = 0.0
      End If
      X = X1+ORIGX
      Y = Y1+ORIGY
      IF (NC.EQ.-2) THEN
         XARRAY(2) = X
         YARRAY(2) = Y
         YDNLST = Y
         XDNLST = X
         CALL GPL (2,XARRAY,YARRAY)
         XARRAY(1) = XARRAY(2)
         YARRAY(1) = YARRAY(2)
      ENDIF
      IF (NC.EQ.-1.OR.NC.EQ.-2) THEN
         CALL GTX (X,Y,STR(1:1))
      ELSEIF (NC.GT.0) THEN
         CALL GTX (X,Y,STR(1:NC))
      ENDIF
      If (NC.GT.120) Then
         ORIGX = TMPX
         ORIGY = TMPY
      End If
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE AXIS (X,Y,STRING,NS,SIZE,ANGLE,ZMIN,ZDEL,LROT)
C
C----------------------------------------------------------------------
C
      DIMENSION WW(2),XX(2,4),YY(2,4),ZZ(2,4)
      CHARACTER*6 STRING(1)
      CHARACTER*4 FORMAT(6)
      CHARACTER*5 ARRAY
      DATA ARRAY / 'X10  '/
      DATA FORMAT / 'F6.4','F6.3','F6.2','F6.1','F5.0','F6.0'/
      DATA WW / -0.1,+0.1 /
      DATA XX / -0.25,-0.25,+0.05,+0.05,+0.25,+0.25,-0.05,-0.05 /
      DATA YY / -0.25,+0.15,-0.65,+0.15,-0.15,+0.25,-0.15,+0.65 /
      DATA ZZ / -0.43,+0.29,-0.83,+0.69,-0.43,+0.29,-0.83,+0.69 /
      DATA IZERO / 0 /
C
C     INITIALIZATION
C
      COMMON /INIT/ INIPLT
      LOGICAL INIPLT
      IF (.NOT.INIPLT) CALL PLOTS
      C = COS(.01745329*ANGLE)
      S = SIN(.01745329*ANGLE)
      T = ANGLE+90.*LROT
C
      K = IABS(NS)
      L = MAX0(MIN0(NS,1),0)+1
      M = MOD(MOD(LROT,4)+4,4)+1
      N = SIZE+.99
C
C     PLOT NUMBER ANNOTATION ALONG AXIS
C
      AXP = ALOG10(100.001*AMAX1(ABS(ZMIN),ABS(ZMIN+N*ZDEL)))
      IXP = IFIX(AXP)
      JXP = IXP
      IF (AXP.LT.0.0) THEN
         JXP = IXP-1
         IXP = JXP
      ENDIF
      IF ((JXP.LE.0).OR.(JXP.GT.6)) JXP = 1
      IF ((IXP.NE.JXP).AND.(AXP.LT.IXP+0.0000086)) JXP = 2
      D = 10.0**(IXP-JXP)
      XA = X+XX(L,M)*C-YY(L,M)*S
      YA = Y+XX(L,M)*S+YY(L,M)*C
      ZA = ZMIN/D
      DO 10 I = IZERO, N
         CALL NUMBER (XA,YA,0.1,ZA,T,FORMAT(JXP))
         XA = XA+C
         YA = YA+S
         ZA = ZA+ZDEL/D
   10 CONTINUE
C
C     PLOT AXIS AND TIC MARKS
C
      XB = X+N*C
      YB = Y+N*S
      XA = XB-WW(L)*S
      YA = YB+WW(L)*C
      CALL PLOT (XA,YA,3)
      DO 20 I = 1, N
         CALL PLOT (XB,YB,2)
         XB = XB-C
         YB = YB-S
         CALL PLOT (XB,YB,2)
         XA = XA-C
         YA = YA-S
         CALL PLOT (XA,YA,2)
   20 CONTINUE
C
C     PLOT IDENTIFICATION LABEL ALONG AXIS
C
      T = 0.5*N-0.06*K-0.42*AMIN0(IABS(IXP-JXP),1)
      XA = X+T*C-ZZ(L,M)*S
      YA = Y+T*S+ZZ(L,M)*C
      CALL SYMBOL (XA,YA,.14,STRING,ANGLE,K)
      IF (IXP.EQ.JXP) RETURN
      XA = XA+0.12*(K+1)*C
      YA = YA+0.12*(K+1)*S
      CALL SYMBOL (XA,YA,.14,ARRAY,ANGLE,7)
      XA = XA+0.48*C-0.07*S
      YA = YA+0.48*S+0.07*C
      BXP = FLOAT(IXP-JXP)
      CALL NUMBER (XA,YA,.1,BXP,ANGLE,'I3')
      RETURN
      END
C
C
      SUBROUTINE LINE (X,Y,N,K,J,OCSYM)
      CHARACTER STROCS
      INTEGER OCSYM
      DIMENSION X(1),Y(1)
C
C   PLOT PAIRS X,Y SCALED TO MINIMUM OF
C   X(N*K+1),Y(N*K+1), AND INCREMENT PER INCH
C   OF X(N*(K+1)),Y(N*(K+1)).
C
C   X = HORIZONTAL ARRAY OF POINTS
C   Y = VERTICAL ARRAY OF POINTS
C   N = # OF PAIRS OF POINTS TO PLOIT IN X AND Y
C   K = PLOT N POINTS FROM THE 1ST, K+1ST, 2,K+1ST, ETC..
C       POSITIONS OF ARRAYS X AND Y.
C   J = >0: PLOT SYMBOL OCSYM (HOLLERITH) EACH JTH POINT
C            WITH CONNECTED LINE
C   J =  0: PLOT ONLY THE LINE.
C   J = <0: PLOT ONLY THE SYMBOLS EACH JTH POINT.
C
      N1 = N*K
      IM = N1+1
      ID = IM+K
      IF (X(ID).EQ.0.OR.Y(ID).EQ.0) RETURN
      IPEN = 2
      M = J
      IF (J.LT.0) THEN
         M = -M
         IPEN = 3
      ENDIF
      X1 = (X(1)-X(IM))/X(ID)
      Y1 = (Y(1)-Y(IM))/Y(ID)
      CALL PLOT (X1,Y1,3)
      JCNT = 0
      DO 10 I = 1, N1, K
         JCNT = JCNT+1
         XP = (X(I)-X(IM))/X(ID)
         YP = (Y(I)-Y(IM))/Y(ID)
         IF (J.NE.0) THEN
            IF (M.NE.JCNT) GO TO 10
            JCNT = 0
         ENDIF
         CALL PLOT (XP,YP,IPEN)
         IF (J.NE.0) THEN
            STROCS = CHAR(OCSYM)
            CALL SYMBOL (XP,YP,.14,STROCS,0.,-1)
            CALL PLOT (XP,YP,3)
         ENDIF
   10 CONTINUE
      RETURN
      END
C
      SUBROUTINE DSHLIN (X,Y,N,DSH,GAP,NSEC)
      INTEGER PEN
      DIMENSION X(1),Y(1),DSH(1),GAP(1)
C
      IF (NSEC.NE.0) THEN
C
C     INITIALIZE
C
         K = 1
         PEN = 2
         S = 0.0
         T = DSH(1)
         XMIN = X(N+1)
         XINC = X(N+2)
         YMIN = Y(N+1)
         YINC = Y(N+2)
C
C     MOVE TO FIRST POINT
C
         X2 = (X(1)-XMIN)/XINC
         Y2 = (Y(1)-YMIN)/YINC
         CALL PLOT (X2,Y2,3)
C
C     PLOT DASHED LINE CURVE
C
         DO 20 I = 2, N
   10       X1 = X2
            Y1 = Y2
            X2 = (X(I)-XMIN)/XINC
            Y2 = (Y(I)-YMIN)/YINC
            D = SQRT((X2-X1)**2+(Y2-Y1)**2)
            S = S+D
            IF (S.GE.T) THEN
               X2 = X2+(X1-X2)*(S-T)/D
               Y2 = Y2+(Y1-Y2)*(S-T)/D
               CALL PLOT (X2,Y2,PEN)
               PEN = 5-PEN
               S = 0.0
               T = GAP(K)
               IF (PEN.EQ.3) GO TO 10
               K = MOD(K,NSEC)+1
               T = DSH(K)
               GO TO 10
            ENDIF
            CALL PLOT (X2,Y2,PEN)
   20    CONTINUE
         RETURN
      ENDIF
      CALL LINE (X,Y,N,1,0,0)
      RETURN
      END
C
C----------------------------------------------------------------------
C
      SUBROUTINE FACTOR (A)
C
C----------------------------------------------------------------------
C
      COMMON /TSCALE/ SFACTT
      SFACTT = A
      RETURN
      END
C
C--------------------------------------------------------------------
C
      SUBROUTINE NUMBER (X,Y,HEIGHT,RNUM,ANGLE,FORMAT)
C
C--------------------------------------------------------------------
C
      CHARACTER*4 FORMAT(1)
      CHARACTER*6 STRING
      CHARACTER*3 STRNG1
C
      IF (FORMAT(1).EQ.'I3') THEN
         WRITE (STRNG1,10) RNUM
   10    FORMAT (I3)
         N = 3
         CALL SYMBOL (X,Y,.14,STRNG1,ANGLE,N)
         RETURN
      ENDIF
      N = 6
      IF (FORMAT(1).EQ.'F6.4') THEN
         WRITE (STRING,20) RNUM
   20    FORMAT (F6.4)
         GO TO 80
      ENDIF
      IF (FORMAT(1).EQ.'F6.3') THEN
         WRITE (STRING,30) RNUM
   30    FORMAT (F6.3)
         GO TO 80
      ENDIF
      IF (FORMAT(1).EQ.'F6.2') THEN
         WRITE (STRING,40) RNUM
   40    FORMAT (F6.2)
         GO TO 80
      ENDIF
      IF (FORMAT(1).EQ.'F6.1') THEN
         WRITE (STRING,50) RNUM
   50    FORMAT (F6.1)
         GO TO 80
      ENDIF
      IF (FORMAT(1).EQ.'F5.0') THEN
         WRITE (STRING,60) RNUM
   60    FORMAT (F5.0)
         GO TO 80
      ENDIF
      IF (FORMAT(1).EQ.'F6.0') WRITE (STRING,70) RNUM
   70 FORMAT (F6.0)
   80 CALL SYMBOL (X,Y,.14,STRING,ANGLE,N)
      RETURN
      END
C
C----------------------------------------------------------------------
C
      SUBROUTINE SCALE (X,S,N,K)
C
C----------------------------------------------------------------------
C
      DIMENSION X(1),T(3)
      DATA T / 1.0,2.0,5.0 /
      M = N*K
C
C     ENTRY TSCALE
C
      XMAX = X(1)
      XMIN = X(1)
      DO 10 I = 1, M, K
         XMAX = AMAX1(X(I),XMAX)
         XMIN = AMIN1(X(I),XMIN)
   10 CONTINUE
      X0 = 0.0
      DX = 1.7E38
      W = 0.99999*(XMAX-XMIN)/S
      IF (W.EQ.0.0) W = 0.99999/S
      DO 20 I = 1, 3
         B = 1.0+ALOG10(W/T(I))
         IF (B.LT.0.0) B = B-1.0
         C = T(I)*10.0**IFIX(B)
         IF (C.LE.DX) THEN
            D = C*AINT(1.00001*XMIN/C)
            IF (XMIN.LT.D) D = D-C
            IF (1.00001*S*C+D.GE.XMAX) THEN
               X0 = D
               DX = C
            ENDIF
         ENDIF
   20 CONTINUE
      X(M+1) = X0-DX*AINT((S-(XMAX-XMIN)/DX)/2.0)
      X(M+K+1) = DX
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE PLOT (XX,YY,IMOD)
C
C-----------------------------------------------------------------------
C
C     THIS ROUTINE WILL PLOT A LINE ON THE CURRENT WORKSTATION TYPE
C
C     INCLUDE 'SYS$LIBRARY:GKSDEFS.BND'
C
      REAL XARRAY(2),YARRAY(2),XX,YY,ORIGX,ORIGY,XDNLST,YDNLST
      COMMON /WHRE/ XDNLST,YDNLST,XARRAY,YARRAY
      LOGICAL RESET,PENUP
C
      COMMON /ORIGIN/ ORIGX,ORIGY
      COMMON /RPLOT/ NN,CO(3),CM,THE,GAM,PHI,X(1024),Y(1024),Z(1024),
     *   IDASH,SCALE,PERZ
      INTEGER WSID
      Real Pxa(30),Pya(30)
      DATA ORIGX,ORIGY / 0.0,0.0 /,PENUP / .TRUE. /,RESET / .FALSE. /
      DATA XARRAY / 0.0,0.0 /,YARRAY / 0.0,0.0 /,WSID / 1 /
      COMMON /INIT/ INIPLT
      LOGICAL INIPLT
      IF (.NOT.INIPLT) CALL PLOTS
C
C     IF IMD<0 (RESET ORIGIN) SET RESET FLAG
C
      IF (IMOD.LT.0) THEN
         IMD = -1*IMOD
         RESET = .TRUE.
      ELSE
         IMD = IMOD
      ENDIF
      IF (IMD.EQ.1) THEN
         IF (PENUP) THEN
            IMD = 3
         ELSE
            IMD = 2
         ENDIF
      ENDIF
C
C       WRITE(*,*)' X,Y,IMOD,IMD = ',X,Y,IMOD,IMD
C
C     SHIFT FOR THE ORIGIN, GENERATE CODE
C
      IF (IMD.NE.999) THEN
C
C       SCALE FACTOR HERE???
C
         XARRAY(2) = XX+ORIGX
         YARRAY(2) = YY+ORIGY
      ENDIF
C
C     WRITE THE SEQUENCE CORRESPONDING TO THE MODE
C
      IF (IMD.EQ.999) THEN
C
C        Check mouse feedback
C
         Call Gsskm(WSID,1,0,1) ! stroke
  200    Call Grqsk(WSID,1,30,istat,itnr,np,pxa,pya) ! stroke
         If (istat.NE.1) goto 200

         If (np.gt.30) np = 30
         dx = pxa(np)-pxa(1)
         dy = pya(np)-pya(1)
         If (dx.eq.0.0.and.dy.eq.0.0) goto 200
         If (abs(dx).gt.abs(dy)) then
           THE = THE + dx * 20
         Else
           GAM = GAM + dy * 20
         End If
         IMOD = 99
C
C        Clear
C
         CALL GCLRWK (WSID,0)
         Return
         goto 200
C
C            CALL GTX(0.0,0.0,'TYPE ENTER TO CONTINUE')
C            READ(*,*)
C
         CALL GDAWK (WSID)
         CALL GCLWK (WSID)
         CALL GCLKS ()
         INIPLT = .FALSE.
      ELSEIF (IMD.EQ.3) THEN
         PENUP = .TRUE.
         XARRAY(1) = XARRAY(2)
         YARRAY(1) = YARRAY(2)
      ELSEIF (IMD.EQ.2) THEN
         PENUP = .FALSE.
         CALL GPL (2,XARRAY,YARRAY)
         XDNLST = XARRAY(2)
         YDNLST = YARRAY(2)
         XARRAY(1) = XARRAY(2)
         YARRAY(1) = YARRAY(2)
C
C       NO CHECK FOR IMD=1 AS IT WAS ALREADY SET TO 2 OR 3 DEPENDING
C       ON THE CURRENT PEN STATE....
C
      ELSE
         RETURN
      ENDIF
C
C     IF RESET FLAG THEN USE X,Y FOR THE NEW ORIGIN
C
      IF (RESET) THEN
         ORIGX = XX
         ORIGY = YY
         RESET = .FALSE.
      ENDIF
      RETURN
      END
