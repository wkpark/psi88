C
C
      SUBROUTINE AXIS (X,Y,STRING,NS,SIZE,ANGLE,ZMIN,ZDEL,LROT,IFORM)
      DIMENSION WW(2),XX(2,4),YY(2,4),ZZ(2,4)
      CHARACTER*8 ARRAY
      CHARACTER*1 STRING(96)
      DATA ARRAY / '(X10   )'/
      DATA WW / -0.1,+0.1 /
      DATA XX / -0.25,-0.25,+0.05,+0.05,+0.25,+0.25,-0.05,-0.05 /
      DATA YY / -0.25,+0.15,-0.65,+0.15,-0.15,+0.25,-0.15,+0.65 /
      DATA ZZ / -0.43,+0.29,-0.83,+0.69,-0.43,+0.29,-0.83,+0.69 /
      DATA IZERO / 0 /
C
C     INITIALIZATION
C
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
      XA = X+XX(L,M)*C-YY(L,M)*S-0.125*S
      YA = Y+XX(L,M)*S+YY(L,M)*C+SIGN(0.1*C,FLOAT(NS))
      ZA = ZMIN/D
      DO 10 I = IZERO, N
         CALL NUMBER (XA,YA,0.15,ZA,T,IFORM)
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
      XA = X+T*C-ZZ(L,M)*S-0.20*S
      YA = Y+T*S+ZZ(L,M)*C+SIGN(0.20*C,FLOAT(NS))
      CALL SYMBOL (XA,YA,.15,STRING,ANGLE,K)
      IF (IXP.EQ.JXP) RETURN
      XA = XA+0.12*(K+1)*C
      YA = YA+0.12*(K+1)*S
      CALL SYMBOL (XA,YA,.15,ARRAY,ANGLE,8)
      XA = XA+0.48*C-0.07*S
      YA = YA+0.48*S+0.07*C
      XYZ = FLOAT(IXP-JXP)
      CALL NUMBER (XA,YA,.15,XYZ,ANGLE,8)
      RETURN
      END
C
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
C
      SUBROUTINE FACTOR (A)
      COMMON /TSCALE/ SFACTT
      SFACTT = A
      RETURN
      END
C
C
      SUBROUTINE FIGURE
C
      CALL PLOTS (1)
      RETURN
      END
C
C
      INTEGER FUNCTION LENSTR (STRING)
      CHARACTER*(*) STRING
C
C
C DETERMINE THE LENGTH OF A STRING. THE LENGTH IS DEFINED AS
C RIGHT JUSTIFIED AND ALLOWANCES ARE MADE FOR BLANKS AT THE
C BEGINING OF THE STRING. TRAILING BLANKS ARE REMOVED.
C
      LENGTH = LEN(STRING)
      DO 10 I = LENGTH, 1, -1
         IF (STRING(I:I).NE.' ') GO TO 20
   10 CONTINUE
   20 LENSTR = I
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
      IF (N.LT.2) RETURN
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
      DO 10 I = K+1, N1, K
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
C
      SUBROUTINE NEWPEN (I)
C
C SET THE LINE, TEXT , AND PERIMETER COLOR.
C
      ENTRY LINCLR (I)
      WRITE (10,10) I
   10 FORMAT (' 4'/I2)
      RETURN
      END
C
C
      SUBROUTINE NUMBER (X,Y,HEIGHT,XNUM,ANGLE,IJK)
C
C     USE THE FORMAT TO PLOT AN INTEGER NUMBER
C     MAX. NUMBER OF CHARACTERS IS 80
C
      CHARACTER*80 IWK
C
      CHARACTER*6 IFORM(18)
      DATA IFORM / '(F6.4)','(F6.3)','(F6.2)','(F6.1)','(F6.0)','(F5.3)'
     *   ,'(F5.2)','(F5.1)','(F5.0)','(F4.2)','(F4.1)','(F3.1)','(I6)  '
     *   ,'(I5)  ','(I4)  ','(I3)  ','(I2)  ','(I1)  '/
C
      DO 10 K = 1, 80
         IWK(K:K) = ' '
   10 CONTINUE
C
C ENCODE AND INTEGER OR REAL ?
C
      IF (IJK.LE.12) THEN
C
C     ENCODE A REAL NUMBER
C
         WRITE (IWK,IFORM(IJK)) XNUM
      ELSE
C
C     ENCODE AN INTEGER NUMBER
C
         NUM = INT(XNUM)
         WRITE (IWK,IFORM(IJK)) NUM
      ENDIF
      LEN = LENSTR(IWK)
      CALL SYMBOL (X,Y,HEIGHT,IWK(1:LEN),ANGLE,LEN)
      RETURN
      END
C
C
      SUBROUTINE PDATAX (X,Y,N,XM,DX,YM,DY)
C
C     CALCOMP/DIPL COMPATABLE DATA PLOTING ROUTINE
C
      DIMENSION X(N),Y(N)
      DATA CX,CY / 2*0.0E+00 /
      PX(I) = (X(I)-XM)/DX
      PY(I) = (Y(I)-YM)/DY
C
C
      I1 = 1
      I2 = 1
      TEST1 = AMAX1(ABS(CX-PX(1)),ABS(CY-PY(1)))
      TEST2 = AMAX1(ABS(CX-PX(N)),ABS(CY-PY(N)))
      IF (TEST1.GE.TEST2) THEN
C
C     IF (AMAX1(ABS(CX-PX(1)),ABS(CY-PY(1))) .LT. AMAX1(ABS(CX-PX(N)),
C    X     ABS(CY-PY(N)))) GO TO 10
C
         I1 = N
         I2 = -I2
      ENDIF
      CALL PLOT (PX(I1),PY(I1),3)
      DO 10 I3 = 2, N
         I1 = I1+I2
         CALL PLOT (PX(I1),PY(I1),2)
   10 CONTINUE
      CX = PX(I1)
      CY = PY(I1)
      RETURN
      END
C
C
      SUBROUTINE PLOT (X,Y,IMOD)
C
C PERFORM THE SPECIFIED PEN MOTION.
C
      INTEGER IX(2),IY(2)
      REAL X,Y,ORIGX,ORIGY,IXDNLAST,IYDNLAST
      LOGICAL RESET,PENUP
C
      COMMON /WHERE2/ IXDNLAST,IYDNLAST,IX,IY
      COMMON /ORIGIN/ ORIGX,ORIGY
      COMMON /TSCALE/ SFACTT
      COMMON /PWHERE/ XLAST,YLAST
C
      DATA PENUP / .TRUE. /,RESET / .FALSE. /
      DATA IX / 0,0 /,IY / 0,0 /
C
      XLAST = X
      YLAST = Y
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
C     SHIFT FOR THE ORIGIN FOR A 10" X 10" PIECE OF PAPER.
C
      IF (IMD.NE.999) THEN
         X2 = ORIGX+X*SFACTT
         Y2 = ORIGY+Y*SFACTT
      ENDIF
C
C     WRITE THE SEQUENCE CORRESPONDING TO THE MODE
C
   10 FORMAT (' 2'/2(F9.4,1X),I4)
      IF (IMD.EQ.999) THEN
         WRITE (10,10) X2,Y2,IMD
      ELSEIF (IMD.EQ.3) THEN
         PENUP = .TRUE.
         IX(1) = IX(2)
         IY(1) = IY(2)
         WRITE (10,10) X2,Y2,IMD
      ELSEIF (IMD.EQ.2) THEN
         PENUP = .FALSE.
         WRITE (10,10) X2,Y2,IMD
         IXDNLAST = IX(2)
         IYDNLAST = IY(2)
         IX(1) = IX(2)
         IY(1) = IY(2)
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
         ORIGX = IX(2)
         ORIGY = IY(2)
         RESET = .FALSE.
      ENDIF
      RETURN
      END
C
C
      SUBROUTINE PLOTS (IUSE)
C
C IUSE=0 IMPLIES FULL INITIALIZATION OF ALL PLOTTING VARIABLES
C IUSE=1 IMPLIES SCREEN ERASURE ONLY (I.E. ORIGINS AND SCALING
C        FACTORS ARE NOT CHANGED)
C
      COMMON /TSCALE/ SFACTT
      COMMON /ORIGIN/ ORIGX,ORIGY
C
      WRITE (10,10)
   10 FORMAT (2X/2X/2X/2X)
      IF (IUSE.NE.1) THEN
C
C     SET UP ORIGIN AND STARTING SCALE FACTOR.
C
         ORIGX = 0.0
         ORIGY = 0.0
         SFACTT = 1.0
C
C PICK THE FIRST PEN (BLACK) AND ROMAN FONT.
C
         CALL NEWPEN (1)
      ELSE
         CALL PLOT (0.0,0.0,-3)
      ENDIF
      RETURN
      END
C
C
      SUBROUTINE SCALE (X,S,N,K)
      DIMENSION X(1),T(3)
      DATA T / 1.0,2.0,5.0 /
      M = N*K
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
            D = C*INT(1.00001*XMIN/C)
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
C
      SUBROUTINE SYMBOL (X,Y,H,STRING,THETA,N)
C
C DRAW THE SPECIFIED SYMBOL USING HARDWARE GENERATED CHARACTERS.
C

      IMPLICIT REAL (A-H,O-Z)
      CHARACTER*(*) STRING
      COMMON /ORIGIN/ ORIGX,ORIGY
      COMMON /TSCALE/ SFACTT
      DATA TORAD / 0.017453293 /
C
C CHECK FOR CALCOMP STYLE ON-CENTER SYMBOLS.
C
      IF (ICHAR(STRING(1:1)).GE.0.AND.ICHAR(STRING(1:1)).LE.25) THEN
         RETURN
      ENDIF
C
C SET THE HEIGHT AND ORIENTATION.
C
      X2 = ORIGX+X*SFACTT
      Y2 = ORIGY+Y*SFACTT
      H2 = H*SFACTT
C
C OUTPUT THE STRING.
C
      IF (LSTR.GT.2) RETURN
      WRITE (10,10) X2,Y2,H2,THETA,N,STRING
   10 FORMAT (' 3'/4(F9.4,1X),I3/A)
      RETURN
      END
C
C
      SUBROUTINE WHERE (X,Y,FACTR)
C
C   RETURN THE CURRENT PEN POSITION AND SCALE FACTOR
C
      COMMON /TSCALE/ SFACTT
      COMMON /PWHERE/ XLAST,YLAST
      X = XLAST
      Y = YLAST
      FACTR = SFACTT
      RETURN
      END
C
C
      SUBROUTINE GETPOINT
      RETURN
      END
