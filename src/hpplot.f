C
C   Calcomp compatible HPGL plotting library
C
C      (TO USE X-ON / X-OFF )
C     HP Plotter library used in 
C     William Jorgensen's research group
C     Laboratory of Computational Chemistry, Yale University.
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
      K = ABS(NS)
      L = MAX0(MIN0(NS,1),0)+1
      M = MOD(MOD(LROT,4)+4,4)+1
      N = SIZE+.99
C
C     PLOT NUMBER ANNOTATION ALONG AXIS
C
      AXP = LOG10(100.001*AMAX1(ABS(ZMIN),ABS(ZMIN+N*ZDEL)))
      IXP = INT(AXP)
      JXP = IXP
      IF (AXP.LT.0.0) THEN
         JXP = IXP-1
         IXP = JXP
      ENDIF
      IF ((JXP.LE.0).OR.(JXP.GT.6)) JXP = 1
      IF ((IXP.NE.JXP).AND.(AXP.LT.IXP+0.0000086)) JXP = 2
      D = 10.0**(IXP-JXP)
      XA = X+XX(L,M)*C-YY(L,M)*S-0.20*S
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
      T = 0.5*N-0.06*K-0.42*MIN(ABS(IXP-JXP),1)
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
      COMMON /HSCALE/ SFACTH
      SFACTH = A
      RETURN
      END
C
C
      SUBROUTINE NUMBER (X,Y,HEIGHT,XNUM,ANGLE,IJK)
C
C     USE THE FORMAT TO PLOT AN INTEGER NUMBER
C     MAX. NUMBER OF CHARACTERS IS 96
C
      CHARACTER*1 IWK(96)
      CHARACTER*6 IFORM(17)
      DATA IFORM / '(F6.4)','(F6.3)','(F6.2)','(F6.1)','(F6.0)','(F5.0)'
     *   ,'(F5.1)','(I4)  ','(I3)  ','(I2)  ','(I6)  ','(I1)  ','(I5)  '
     *   ,'(F3.1)','(F4.1)','(F4.2)','(F5.2)'/
      DATA IWK / 96*' '/
C
C     CHECK TO SEE IF "I" OR "O" FORMAT IS REQUESTED
C
      IF ((IJK.LE.7).OR.(IJK.GE.14)) THEN
C
C     ENCODE A REAL NUMBER
C
         WRITE(IWK,IFORM(IJK))XNUM
C
C        ENCODE (96,IFORM(IJK) ,IWK)XNUM
C
      ELSE
C
C  ENCODE AN INTEGER NUMBER - INTERNAL WRITE IS STANDARD, ENCODE IS NOT
C
         NUM = INT(XNUM)
         WRITE(IWK,IFORM(IJK)) NUM
C
C        ENCODE (99,IFORM(IJK) ,IWK)NUM
C
      ENDIF
      DO 10 I = 1, 96
         N = 97-I
         IF (IWK(N).NE.' ') GO TO 20
   10 CONTINUE
      RETURN
   20 CALL SYMBOL (X,Y,HEIGHT,IWK,ANGLE,N)
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
         B = 1.0+LOG10(W/T(I))
         IF (B.LT.0.0) B = B-1.0
         C = T(I)*10.0**INT(B)
         IF (C.LE.DX) THEN
            D = C*INT(1.00001*XMIN/C)
            IF (XMIN.LT.D) D = D-C
            IF (1.00001*S*C+D.GE.XMAX) THEN
               X0 = D
               DX = C
            ENDIF
         ENDIF
   20 CONTINUE
      X(M+1) = X0-DX*INT((S-(XMAX-XMIN)/DX)/2.0)
      X(M+K+1) = DX
      RETURN
      END
C
C
      SUBROUTINE SYMBOL (X1,Y1,HEIGHT,STR,THETA,NCHAR)
      COMMON /VARH/ IHMODE,IHORGX,IHORGY,IHVECT,IHXCUR,IHYCUR
      COMMON /HSCALE/ SFACTH
      COMMON /PASS/ ITYP
C
C     OUTPUT A CHARACTER STRING
C
      CHARACTER*1 STR(*),TERMIN
C
      TERMIN = CHAR(3)
      H = HEIGHT
      W = HEIGHT*2.0/3.0
      NC = NCHAR
      IF (NC.EQ.0) RETURN
      TH = THETA*0.017453
      SI = SIN(TH)
      CO = COS(TH)
      SI = MOD(SI,128.0)
      CO = MOD(CO,128.0)
      WRITE (66,'(1X,A,F9.4,A,F9.4,A)') 'DI ',CO,',',SI,TERMIN
      WRITE (66,'(1X,A,F9.4,A,F9.4,A)') 'SI ',W,',',H,TERMIN
      X2 = X1*1000.*SFACTH
      Y2 = Y1*1000.*SFACTH
      IX = X2
      IY = Y2
      IDX = IX-IHXCUR
      IDY = IY-IHYCUR
      IHXCUR = IX
      IHYCUR = IY
      IX = IHXCUR+IHORGX
      IY = IHYCUR+IHORGY
C
      WRITE (66,'(1X,A,I8,A,I8,A)') 'PU ',IDX,',',IDY,';'
      WRITE (66,'(1X,168A)') 'LB',(STR(I),I=1,NC),TERMIN
C      WRITE (66,'(1X,A)')' PU 0,0;'
      WRITE (66,'(1X,A,I8,A,I8,A)') 'PA ',IX,',',IY,';'
      WRITE (66,'(1X,A)')' PR 0,0;'
C
      RETURN
      END
C
C
      SUBROUTINE WHERE (X,Y,FACTR)
C
C   RETURN THE CURRENT PEN POSITION AND SCALE FACTOR
C
      COMMON /VARH/ IHMODE,IHORGX,IHORGY,IHVECT,IHXCUR,IHYCUR
      COMMON /HSCALE/ SFACTH
      X = IHXCUR/(1000.0*SFACTH)
      Y = IHYCUR/(1000.0*SFACTH)
      FACTR = SFACTH
      RETURN
      END
C
C **********************************************************
C
      SUBROUTINE PLOT (X,Y,I)
      COMMON /VARH/ IHMODE,IHORGX,IHORGY,IHVECT,IHXCUR,IHYCUR
      COMMON /HSCALE/ SFACTH
      COMMON /PASS/ ITYP
      IF (ABS(I).EQ.999) THEN
         IF (ITYP.EQ.0) THEN
            WRITE (66,20)
         ELSE
            WRITE (66,10)
         ENDIF
   10    FORMAT (' SP0 ; IN ; PG;')
   20    FORMAT (' SP0 ; PU 10603,7721 ; IN ; PG;')
         RETURN
      ENDIF
      IX = X*1000.*SFACTH
      IY = Y*1000.*SFACTH
      IDX = IX-IHXCUR
      IDY = IY-IHYCUR
      IHXCUR = IX
      IHYCUR = IY
      II = I
      IF (I.LT.0) THEN
         IHORGX = IHORGX + IX
         IHORGY = IHORGY + IY
         IHXCUR = 0.
         IHYCUR = 0.
         II = -I
      ENDIF
      IF (II.EQ.2) WRITE (66,30) IDX,IDY
      IF (II.EQ.3) WRITE (66,40) IDX,IDY
      IF (II.EQ.1) WRITE (66,50) IDX,IDY
      RETURN
   30 FORMAT (' PD ',I8,1X,I8,' ; ')
   40 FORMAT (' PU ',I8,1X,I8,' ; ')
   50 FORMAT (1X,I8,1X,I8,' ; ')
      END
C
C ***********************************************************
C
      SUBROUTINE PLOTS (ITYPE)
      COMMON /VARH/ IHMODE,IHORGX,IHORGY,IHVECT,IHXCUR,IHYCUR
      COMMON /HSCALE/ SFACTH
      COMMON /PASS/ ITYP
      CHARACTER ESC*1
C
C  USE CALL PLOTS(0) FOR  STANDARD 8.5 * 11 INCH PAPER
C  USE CALL PLOTS(N) FOR ANY OTHER TYPE
C  SET-UP X-ON / X-OFF PROTOCOL
C
      ESC = CHAR(27)
      WRITE (66,10)
   10 FORMAT (' IN ; ')
      WRITE (66,20) ESC
   20 FORMAT (1X,A,'.I128;;17: ')
      WRITE (66,30) ESC
   30 FORMAT (1X,A,'.N;19: ')
      ITYP = ITYPE
      IHMODE = 1000
      IF (ITYPE.NE.0) THEN
         WRITE (66,40)
   40    FORMAT ('      PS1 ; SP1 ; PA 0,0 ; PR ;')
      ELSE
         WRITE (66,50)
      ENDIF
   50 FORMAT ('      PS4 ; SP1 ; PA 0,0 ; PR ;')
C
C     SET UP ORIGIN AND STARTING VECTOR TYPE
C
      IHORGX = 0
      IHORGY = 0
      IHXCUR = 0
      IHYCUR = 0
      IHVECT = 3
      SFACTH = 1.0
      IF (ITYPE.NE.0) RETURN
      CALL PLOT (0.0,0.0,-3)
      RETURN
      END
C
C
      SUBROUTINE NEWPEN (ICLR)
C
      ENTRY LINCLR(ICLR)
C
C             SUBROUTINE TO SPECIFY THE PEN TO USE
C
      IC = ABS(ICLR)
      IC = MOD(IC,7)
      WRITE (66,10) IC
   10 FORMAT (' PU;SP',I1,';')
      RETURN
      END
C
C
C *************** PAUSE FOR PAPER CHANGE ********************
C
      SUBROUTINE FIGURE (IFIG)
      COMMON /PASS/ ITYP
      DATA ICHK / 999 /
      WRITE (66,40)
      WRITE (*,*)
      WRITE (*,10)
      WRITE (*,20)
      WRITE (*,30)
      WRITE (*,*)
      READ (*,50) IGET
      IF (IGET.NE.ICHK) THEN
         CALL PLOTS (ITYP)
      ELSE
         WRITE (*,60)
   10    FORMAT (1X,'   >>>>>  CHANGE PAPER IF DESIRED  <<<<<')
   20    FORMAT (1X,'   >>>>>  ENTER:RETURN TO CONTINUE <<<<<')
   30    FORMAT (1X,'   >>>>>           999 TO STOP     <<<<<')
   40    FORMAT (1X,' SP0 ; ')
   50    FORMAT (I3)
   60    FORMAT (1X,'  ---------- PLOTTING TERMINATED ----------')
         STOP
      ENDIF
      RETURN
      END
C
C
      SUBROUTINE HPCIRC (X,Y,ICLR,RAD)
C
C PLOT AND FILL A CIRCLE ON THE HP7475A
C
      CALL PLOT (X,Y,3)
      CALL NEWPEN (ICLR)
      IRAD = ABS(INT(RAD*1000.))
      WRITE (66,10) IRAD
   10 FORMAT (' FT;WG,',I5,',0,360;')
      RETURN
      END
