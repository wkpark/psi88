C
C       ***********************************************************
C       ********************  PSI/88 - PART 3  ********************
C       ***********************************************************
C
C       Version 1.0  Any questions to the author should specify
C                    the version being used.
C
      PROGRAM PSI2
C
C       PROGRAM FOR PLOTTING CHARGE DENSITIES OR ORBITAL VALUES
C       IN TWO OR THREE DIMENSIONS WITH OR WITHOUT HIDDEN LINE
C       ELIMINATION.
C
C       WILLIAM L. JORGENSEN
C       Daniel L. Severance
C       DEPARTMENT OF CHEMISTRY, Yale University
C       New Haven, CT 06511, USA
C
C         MODIFIED FOR PLOTTING INPUT SYMBOLS
C         JAN., 1986
C         JIALI GAO
C
C         Heavy Modifications of the Hidden Line Section to Increase
C         Speed by a Factor of 10 or More 8-87 to 3-88.  Introduction
C         of Code to Properly Handle Indentations and Doughnuts as Well.
C         Dan Severance, Purdue.
C
C Redistribution and use in source and binary forms are permitted
C provided that the above paragraphs and this one are duplicated in
C all such forms and that any documentation, advertising materials,
C and other materials related to such distribution and use acknowledge
C that the software was developed by William Jorgensen at Purdue University
C The name of the University or William Jorgensen may not be used to endorse
C or promote products derived from this software without specific prior
C written permission.  The author is now at Yale University.
C THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
C IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
C WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
C
C       AC IS THE ARRAY OF ALL POINTS IN THE CURVES
C       WHICH WERE GENERATED IN THE FIRST PART OF
C       THE PROGRAM. IT IS NECESSARY TO HAVE ALL OF
C       THEM IN CORE AT ONCE FOR PROPER HIDDEN LINE
C       ELIMINATION. NAC CONTAINS THE NUMBER OF POINTS
C       IN EACH CURVE. NBEG CONTAINS THE STARTING
C       SUBSCRIPT FOR EACH CURVE IN AC. ACMIN AND ACMAX
C       CONTAIN THE EXTREMA FOR EACH CURVE AFTER IT
C       HAS BEEN TRANSFORMED.
C       ** THE DIMENSION OF AC MUST BE CHANGED IF NC .GT.75000  **
C       **  ARRAYS DIMENSIONED 1024 MUST BE CHANGED IF NCURV .GT. 1024
C       THE, GAM AND PHI ARE THE ROTATION ANGLES
C       IN THE XY, YZ AND XZ PLANES RESPECTIVELY.
C       ONCE THE CURVES HAVE BEEN GENERATED IN PART ONE
C       MANY VALUES OF THE, GAM AND PHI MAY BE TRIED
C       UNTIL THE PLOT ADOPTS THE VIEW YOU DESIRE.
C       THE SIZE OF THE PLOT IS CONTROLLED BY SCALE
C       AS BEFORE. PERZ PROVIDES THE PLOT WITH PERSPECTIVE.
C       A LITTLE PERSPECTIVE IS ALMOST ALWAYS NEEDED
C       TO RELIEVE ANY AMBIGUITY IN THE STEREOCHEMISTRY.
C       THE DEFAULT VALUE OF 0.0 FOR PERZ WILL CAUSE
C       AN ADEQUATE DEGREE OF PERSPECTIVE TO BE PROVIDED.
C       PERSPECTIVE IS ELIMINATED BY TAKING PERZ VERY LARGE.
C
C       CARD INPUT -
C       CARD 1 - TITLE  (A120)
C       CARD 2 - SUBTITLE  (CENTERED A40)
C       CARD 3 - IRDXYZ = 01, IF COORDINATES ARE TO BE READ FROM CARDS
C                THIS OPTION NO LONGER DOES ANYTHING, COORDINATES ARE
C                ALWAYS READ FROM THE INPUT FILE (IRDXYZ ALWAYS SET TO 01
C                MOL = 01, FOR DRAWING OF STRUCTURE ONLY
C                NHL = 01 FOR NO HIDDEN LINE ELIMINATION  (3I2)
C       CARD 4 - NO. OF BONDS  (I2)  =0 FOR AUTOMATIC
C       CARD 5 - BOND PAIRS  (2I2) IF NO. OF BONDS SPECIFIED
C       CARD 6 - MOLECULAR TITLE AND COORDINATES, FOLLOWED BY 99 CARD
C
C       LAST CARD - THETA, GAMMA AND PHI ROTATION ANGLES, SCALE 4F10.6
C
C       LAST CARD + 1  - IF ANY ATOM HAS ATOMIC NUMBER GREATER THAN 18,
C                      THE ATOMIC SYMBOL IS READ IN. (A2,I1, # OF LETTER
C                      EVERY ATOM NEEDS ONE CARD!
C       LAST CARD + 2  - AN OPTION CARD IS NEEDED TO CONTROL COLOR PLOT
C                      OR DASHLINE PLOT. 0 FOR DASHLINE, 1 FOR COLOR,
C                      AND 2 FOR BOTH.
C                      DEFAULT -1- COLOR.
C
      COMMON /POINTS/ AC(75000,3)
      COMMON /HLCOM/ NAC(1024),NBEG(1024),ACMIN(1024,3),ACMAX(1024,3)
      COMMON /RPLOT/ N,CO(3),CM,THE,GAM,PHI,X(1024),Y(1024),Z(1024),
     *   IDASH,SCALE,PERZ
      COMMON /BONDS/ NBND,IB(35),IC(35)
C
      DIMENSION IAN(60),C(1024,3),IATSMN(60),CC(1024,3)
      CHARACTER TITLE*120,SUBT*40,IATSYM(60)*2
      EQUIVALENCE (C(1,1),X(1))
      DATA IRD,ILST/ 5,6 /
C
C     INITIALIZE PLOTTING
C
      CALL PLOTS (0)
C
      READ (IRD,10) TITLE,SUBT
   10 FORMAT (A/A)
      READ (IRD,20) IRDXYZ,MOL,NHL,FC
      IRDXYZ=1
C
C     THE DEFAULT FACTOR IS TO OPEN A 6.5"X6.5" VIEWPORT
C
      IF (FC.EQ.0.0E+0) FC = 0.650E+0
      CALL FACTOR (FC)
      READ (IRD,20) NBND
   20 FORMAT (3I2,F4.3)
      IF (NBND.NE.0) THEN
         DO 30 I = 1, NBND
            READ (IRD,20) IB(I),IC(I)
   30    CONTINUE
      ENDIF
C
C       THE COORDINATES ARE READ IN FROM THE PLOTTING FILE
C       THIS IS THE ONLY INFORMATION REQUIRED FROM THE PLOT
C       FILE FOR THIS PART OF THE PROGRAM.
C
      N = 1
      READ (IRD,20) JUNK
   40 READ (IRD,50) IAN(N),(C(N,J),J=1,3)
   50 FORMAT (I2,8X,3F10.6)
      IF (IAN(N).NE.99) THEN
         N = N+1
         GO TO 40
      ENDIF
      N = N-1
      READ (IRD,60) THE,GAM,PHI,SCALE
   60 FORMAT (4F10.4)
      IF (SCALE.EQ.0.0E+0) SCALE = 1.0E+0
      INIT = 1
C
C        READ IN ATOMIC LABELS FOR ATOMS WITH IAN > 18
C
      DO 70 I = 1, N
         IF (IAN(I).GT.18) READ (IRD,80) IATSYM(I),IATSMN(I)
   70 CONTINUE
C 
C  The field of IATSMN(I) was changed from 1 to 2 (I2) to allow
C  more than 9 atoms to be user-defined. Although it is unlikely
C  that this would happen with a typical organic molecule, the EHMO
C  program will undoubtedly need more at some point.
C
C                                             JJN  11-21-90
C
   80 FORMAT (A2,I2)
C
      READ (IRD,'(I2)',ERR=90) JLINES
      GO TO 100
   90 JLINES = 1
  100 CONTINUE
C
C     Save original positions
C
      Do I = 1, N
        CC(I,1) = C(I,1)
        CC(I,2) = C(I,2)
        CC(I,3) = C(I,3)
      End Do
      CALL DRAMOL (C,N,IAN,TITLE,CO,THE,GAM,PHI,CM,PERZ,SCALE,INIT,
     *   IATSYM,IATSMN)
      IF (MOL.NE.1) CALL HIDPLT (SUBT,NHL,JLINES)
C
C     IF PLOTTING TO A TERMINAL SCREEN, YOU MAY NEED A ROUTINE HERE
C     TO MAKE THE TERMINAL WAIT FOR YOU TO FINISH BEFORE CLEARING THE
C     SCREEN.  (WAIT FOR CARRIAGE-RETURN, OR MOUSE-CLICK, ETC. BEFORE
C     EXECUTING THE LAST CALL TO PLOT.
C
      IMOD = 999
      CALL PLOT (0.0E+0,0.0E+0,IMOD)
      WRITE (ILST,110) TITLE,SUBT
  110 FORMAT (1X,A/)
      WRITE (ILST,120) THE,GAM,PHI,SCALE
  120 FORMAT (' ','THETA = ',F6.1,2X,'GAMMA = ',F6.1,2X,'PHI = ',F6.1/1X
     *   ,'SCALE = ',F6.3/)
C
C     Restore
C
      Do I = 1, N
        C(I,1) = CC(I,1)
        C(I,2) = CC(I,2)
        C(I,3) = CC(I,3)
      End Do
      If (IMOD.NE.999) Goto 100
      STOP
      END
C
C
      SUBROUTINE DRAMOL (C,NAT,IAN,TITLE,CO,THE,GAM,PHI,CM,PERZ,SCALE,
     *   INIT,IATSYM,IATSMN)
      COMMON /MIN/ YMOLMN
      COMMON /BONDS/ NBND,IB,IC
      DIMENSION C(1024,3),IAN(60),IATSMN(60),CO(3),NCHAR1(18)
      DIMENSION IB(35),IC(35),BX(4),BY(4),COVRAD(18)
      CHARACTER*(*) TITLE
      CHARACTER*2 ATS(18),IATSYM(60)
C
C       ROUTINE TO DRAW MOLECULAR FRAMEWORK
C       INIT=1 TO INITIALIZE - CO,CM,PERZ RETURNED.
C       C IS DESTROYED. NAT = NO. OF ATOMS.
C
      DATA ATS / 'H ','HE','LI','BE','B ','C ','N ','O ','F ','NE','NA',
     *   'MG','AL','SI','P ','S ','CL','AR'/
C
C       THIS ARRAY CONTAINS APPROXIMATE COVALENT RADIUS DATA FOR
C       AUTOMATIC DETERMINATION OF BONDING.
C
      DATA COVRAD / 0.350E+0,0.0E+0,1.40E+0,1.060E+0,0.840E+0,0.770E+0,
     *   0.740E+0,0.740E+0,0.640E+0,0.0E+0,1.570E+0,1.450E+0,1.30E+0,
     *   1.220E+0,1.20E+0,1.140E+0,1.10E+0,0.0E+0 /
C
C       THIS ARRAY CONTAINS THE NUMBER OF LETTERS IN THE ATOMIC SYMBOL
C       FOR EACH ATOM IN ARRAY IATS.
C
      DATA NCHAR1 / 1,2,2,2,1,1,1,1,1,2,2,2,2,2,1,1,2,2 /
      IF (INIT.EQ.1) THEN
         CM = -100.0E+0
         DO 20 I = 1, 3
            CMI = 100.0E+0
            CMA = -100.0E+0
            DO 10 J = 1, NAT
               P = C(J,I)
               CMI = MIN(CMI,P)
               CMA = MAX(CMA,P)
   10       CONTINUE
            CO(I) = (CMA+CMI)*0.50E+0
            P = CMA-CMI
            CM = MAX(CM,P)
   20    CONTINUE
         IF (CM.LT.0.10E+0) CM = 2.50E+0
         PERZ = 10.0E+0*CM
      ENDIF
C
C       DETERMINE BONDS
C
      IF (NBND.EQ.0) THEN
         NMI = NAT-1
         IF (NMI.NE.0) THEN
            DO 40 I = 1, NMI
               IPI = I+1
               DO 30 J = IPI, NAT
                  P = SQRT((C(I,1)-C(J,1))**2+(C(I,2)-C(J,2))**2+(C(I,3)
     *               -C(J,3))**2)
C
C     CHECK TO SEE IF THE ATOMS ARE WITHIN 10 PERCENT OF THE COVALENT
C     BONDING DISTANCE, IF SO, DRAW THE BOND
C
                  COVBND = (COVRAD(IAN(I))+COVRAD(IAN(J)))*1.10E+0
                  IF (P.LE.COVBND) THEN
                     NBND = NBND+1
                     IB(NBND) = I
                     IC(NBND) = J
                  ENDIF
   30          CONTINUE
   40       CONTINUE
         ENDIF
      ENDIF
C
C       TRANSFORM C
C
      !CALL ROTCOR (C,NAT,THE,GAM,PHI,PERZ,CO)
      CALL ROTXYZ (C,NAT,THE,GAM,PHI,PERZ,CO)
      CL = CM*SCALE
      ADMI = -CL
      PII = CL*0.20E+0
C
C       MAKE TITLE
C
      CALL SYMBOL (0.10E+0,0.10E+0,0.300E+0,TITLE,0.00E+0,120)
C
C     THE NEXT LINE MAY NEED MODIFICATION - SHIFT THE ORIGIN TO CENTER T
C     PLOTS MORE, AS WELL AS MOVE AWAY FROM THE TITLE
C
      CALL PLOT (1.250E+0,1.250E+0,-3)
C
C       DETERMINE LOWEST POINT IN MOLECULE
C
      YMOLMN = 100.0E+0
      DO 50 I = 1, NAT
         YMOLMN = MIN(YMOLMN,C(I,2))
   50 CONTINUE
C
C       MAKE PLOTTING BOX
C
C       TO PUT A BOX AROUND THE PLOT
C       COMMENT OUT THE CALL PLOT STATMENTS
C
C      CALL PLOT (0.00E+0,0.00E+0,3)
C      CALL PLOT (10.00E+0,0.00E+0,2)
C      CALL PLOT (10.00E+0,10.00E+0,2)
C      CALL PLOT (0.00E+0,10.00E+0,2)
C      CALL PLOT (0.00E+0,0.00E+0,-2)
C
C       MAKE ATOM SYMBOLS
C
      DO 100 I = 1, NAT
C
C *******************************************
C
C  USE THIS SECTION TO PICK OUT THE ATOMS TO PLOT
C  FOR 2-D PLOTS
C
C       IF (I.EQ.1) GOTO 50
C       IF (I.EQ.2) GOTO 50
C       GOTO 11
C  50   CONTINUE
C
C ***************************************
C
         IANI = IAN(I)
         IF (IANI.LE.18) THEN
            NP = NCHAR1(IANI)
         ELSE
            NP = IATSMN(I)
         ENDIF
         X = ((C(I,1)+CL)/CL)*5.00E+0-0.0710E+0
         Y = ((C(I,2)+CL)/CL)*5.00E+0-0.0710E+0+0.01250E+0
C
C        X = ((C(I,1)+CL)/CL)*5.00E+0-0.0710E+0-0.0700E+0
C        Y = ((C(I,2)+CL)/CL)*5.00E+0-0.0710E+0-0.0500E+0
C
         IF (NP.EQ.2) X = X-0.0500E+0
C
C       DRAW ATOMS
C
         IF (IANI.GT.18) THEN
            CALL SYMBOL (X,Y,0.250E+0,IATSYM(I),0.00E+0,NP)
         ELSE
            CALL SYMBOL (X,Y,0.250E+0,ATS(IANI),0.00E+0,NP)
         ENDIF
  100 CONTINUE
C
C ***********************************************
C
C UNCOMMENTING THIS LINE WILL SKIP THE PLOTTING OF BONDS
C
C        GOTO 120
C
C ***********************************************
C
C       DRAW BONDS
C
      IF (NBND.NE.0) THEN
         CALL NEWPEN (1)
         BX(3) = ADMI
         BY(3) = ADMI
         BX(4) = PII
         BY(4) = PII
         DO 110 I = 1, NBND
            IBI = IB(I)
            ICI = IC(I)
            BX(1) = C(IBI,1)
            BX(2) = C(ICI,1)
            BY(1) = C(IBI,2)
            BY(2) = C(ICI,2)
            CALL LVGAP (BX(1),BY(1),BX(2),BY(2),PII,IER)
C
C       PLOT BONDS
C
            IF (IER.NE.1) THEN
               CALL DSHLIN (BX,BY,2,NULL,NULL,0)
            ENDIF
  110    CONTINUE
      ENDIF
      RETURN
      END
C
C
      SUBROUTINE LVGAP (X1,Y1,X2,Y2,PII,IER)
      DATA R / 0.280E+0 /
C
C       CALCULATE GAPS AROUND ATOM SYMBOLS
C
      DX = X2-X1
      DY = Y2-Y1
      DXY2 = DX**2+DY**2
      DIST = SQRT(DXY2)
      TH = ATAN(ABS(DY/DX))
      XP = R*PII*COS(TH)
      YP = R*PII*SIN(TH)
      IF (X1.GT.X2) XP = -XP
      IF (Y1.GT.Y2) YP = -YP
      X1 = X1+XP
      Y1 = Y1+YP
      X2 = X2-XP
      Y2 = Y2-YP
      PDIST = XP**2+YP**2
      PDIST = SQRT(PDIST)
      IF (DIST.LT.PDIST) THEN
         IER = 1
      ELSE
         IER = 0
      ENDIF
      RETURN
      END
C
C
      SUBROUTINE ROTCOR (A,N,THE,GAM,PHI,PERZ,CO)
      DIMENSION A(1024,3),CO(3)
      DATA PI / 3.141592653610E+0 /
      RPI = PI/180.0E+0
C
C       TRANSFORM TO CENTERED SYSTEM
C
      DO 20 I = 1, 3
         DO 10 J = 1, N
            A(J,I) = A(J,I)-CO(I)
   10    CONTINUE
   20 CONTINUE
C
C       XY ROTATION
C
      RT = THE*RPI
      CT = COS(RT)
      ST = SIN(RT)
      DO 30 I = 1, N
         RT = CT*A(I,1)-ST*A(I,2)
         A(I,2) = ST*A(I,1)+CT*A(I,2)
         A(I,1) = RT
   30 CONTINUE
C
C       ZX ROTATION
C
      RT = PHI*RPI
      CT = COS(RT)
      ST = SIN(RT)
      DO 40 I = 1, N
         RT = CT*A(I,3)-ST*A(I,1)
         A(I,1) = ST*A(I,3)+CT*A(I,1)
         A(I,3) = RT
   40 CONTINUE
C
C       YZ ROTATION
C
      RT = GAM*RPI
      CT = COS(RT)
      ST = SIN(RT)
      DO 50 I = 1, N
         RT = CT*A(I,2)-ST*A(I,3)
         A(I,3) = ST*A(I,2)+CT*A(I,3)
         A(I,2) = RT
   50 CONTINUE
C
C       GIVE PERSPECTIVE
C
      DO 60 I = 1, N
         RT = PERZ-A(I,3)
         IF (RT.NE.0.0E+0) THEN
            RT = PERZ/RT
            A(I,1) = RT*A(I,1)
            A(I,2) = RT*A(I,2)
         ENDIF
   60 CONTINUE
      RETURN
      END
C
C     Jmol compatible rotation rotateXYZ
C
      SUBROUTINE ROTXYZ (A,N,PX,PY,PZ,PERZ,CO)
      Implicit Real(A-H,O-Z)
      DIMENSION A(1024,3),CO(3),RM(3,3),B(3)
      DATA PI / 3.141592653610E+0 /
      RPI = PI/180.0E+0
C
C       TRANSFORM TO CENTERED SYSTEM
C
      DO I = 1, 3
         DO J = 1, N
            A(J,I) = A(J,I)-CO(I)
         End Do
      End Do
C
C       ROTATION
C
      cx = cos(pX*RPI)
      sx = sin(pX*RPI)
      cy = cos(pY*RPI)
      sy = sin(pY*RPI)
      cz = cos(pZ*RPI)
      sz = sin(pZ*RPI)
      RM(1,1) = cz*cy
      RM(2,1) = -sz*cx + cz*sy*sx
      RM(3,1) = sz*sx + cz*sy*cx
      RM(1,2) = sz*cy
      RM(2,2) = cz*cx + sx*sy*sz
      RM(3,2) = -cz*sx + sz*sy*cx
      RM(1,3) = -sy
      RM(2,3) = sx*cy
      RM(3,3) = cx*cy
      Do K = 1, N
        Do I = 1, 3
          S = 0.0
          Do J = 1, 3
            S = S + RM(J,I)*A(K,J)
          End Do
          B(I)=S
        End Do
        Do I = 1, 3
          A(K,I)=B(I)
        End Do
      End Do
C
C       GIVE PERSPECTIVE
C
      DO I = 1, N
         RT = PERZ-A(I,3)
         IF (RT.NE.0.0E+0) THEN
            RT = PERZ/RT
            A(I,1) = RT*A(I,1)
            A(I,2) = RT*A(I,2)
         ENDIF
      End Do
      RETURN
      END
C
C
      SUBROUTINE HIDPLT (SUBT,NHL,JLINES)
      IMPLICIT REAL (A-H,O-Z)
C
C     SUBROUTINE TO COMPUTE HIDDEN LINE ELIMINATION ON CONTOURS
C
      COMMON /DLT/ YINT(75000),SLPINV(75000),PLANES(1024,3),NCHID
      COMMON /POINTS/ AC(75000,3)
      COMMON /HLCOM/ NAC,NBEG,ACMIN,ACMAX
      COMMON /MIN/ YMOLMN
      COMMON /RPLOT/ N,CO(3),CM,THE,GAM,PHI,X(1024),Y(1024),Z(1024),
     *   IDASH,SCALE,PERZ
      CHARACTER*(*) SUBT
C
C     LOGICAL PARLEL - DESIGNATES IF TWO PLANES ARE PARALLEL
C
C     1/88 MODIFIED HIDDEN LINE ALGORITHM FOR SPEED AND CORRECTION
C     OF THE TREATMENT OF SOME SPECIAL CASES - DAN SEVERANCE, PURDUE
C
      DIMENSION ACMIN(1024,3),ACMAX(1024,3),C(1024,3),AMA(3),AMI(3)
      DIMENSION XPL(952),YPL(952),ACNMIN(1024,3),ACNMAX(1024,3)
      INTEGER NAC(1024),NCDASH(1024),PENCOD
      INTEGER IPOS(1024),NBEG(1024),NCHID(1024),IPARA(1024)
      LOGICAL HIDDEN,PARLEL,FIRST
      EQUIVALENCE (C(1,1),X(1))
      DATA ILST,IDSK1,IDSK2,IDSK3 / 6,22,23,24 /,PENCOD / 1 /
C
C       READ CURVE DATA FROM DISK FILES, TRANSFORM CURVES AND
C       DETERMINE EXTREMA FOR EACH CURVE
C
      REWIND(IDSK1)
      REWIND(IDSK2)
      REWIND(IDSK3)
      OPEN (IDSK1,FILE='FOR022',STATUS='OLD')
      OPEN (IDSK2,FILE='FOR023',STATUS='OLD')
      OPEN (IDSK3,FILE='FOR024',STATUS='OLD')
      READ (IDSK2,40) NC,NCURV,NC1,NCURV1,NC2,NCURV2,NC3,NCURV3
C
C       SET POINTER FOR PARALLEL PLANES, I.E. IF IPARA(I)=
C       IPARA(J), THEN PLANES I AND J ARE PARALLEL.  THIS IS USED
C       BY THE HIDDEN LINE ROUTINE.
C
      DO 10 I = 1, NCURV1
         IPARA(I) = 1
   10 CONTINUE
      DO 20 I = (NCURV1+1), NCURV2
         IPARA(I) = 2
   20 CONTINUE
      DO 30 I = (NCURV2+1), NCURV3
         IPARA(I) = 3
   30 CONTINUE
   40 FORMAT (10I6)
C
C       CHECK TO SEE IF ENOUGH ARRAY SPACE IS ALLOCATED FOR THE
C       CONTOURS..
C
      IF (NC.GT.75000) THEN
         WRITE (ILST,50) NC
   50    FORMAT (1X,'%%%%%% ARRAY AC MUST BE REDIMENSIONED   %%%%%%'/,1X
     *      ,'%%%%%% AC NEEDS',I7,' BUT HAS ONLY 75000 %%%%%%')
         CALL NEWPEN (1)
         CALL PLOT (0.0E+0,0.0E+0,999)
         STOP
      ENDIF
C
C       CHECK THAT THE DIMENSIONS OF INDIVIDUAL CONTOUR ARRAYS ARE LARGE
C       ENOUGH..
C
      IF (NCURV.GT.1024) THEN
         CALL NEWPEN (1)
         CALL PLOT (0.0E+0,0.0E+0,999)
         WRITE (ILST,60) NCURV
   60    FORMAT (1X,
     *      '%%%%%% ARRAYS DIMENSIONED AT 1024 NEED TO BE %%%%%%'/,1X,
     *      '%%%%%% AS',I6,'                             %%%%%%')
         STOP
      ENDIF
C
C       READ IN THE INDEXES TO THE CONTOUR FILE, I.E. NAC(I) IS THE
C       NUMBER OF POINTS IN CONTOUR (I), AND NCDASH(I) IS THE "COLOR"
C       OF CONTOUR (I)....
C
      READ (IDSK2,100) (NAC(I),NCDASH(I),I=1,NCURV)
      DO 80 I = 1, NCURV
C
C       PERHAPS THIS SHOULD BE DONE SOME WAY OTHER THAN HITTING EOF
C       THERE ARE AT MOST NCURV ELEMENTS IN FILE 24
C
         READ (IDSK3,70,END=90) NCHID(I)
   70    FORMAT (I6)
   80 CONTINUE
      READ (IDSK3,70,END=90) NCHID(NCURV+1)
      WRITE (ILST,*) 'ERROR -- ATTEMPTING TO READ MORE POINTS THAN'
      WRITE (ILST,*)
     *   'POSSIBLE FROM FILE 24, PLEASE CHECK FILE INTEGRITY'
      STOP
   90 NCHID2 = I-1
  100 FORMAT (20I4)
C
C       PLOTTING PARAMETERS
C
      DSH = 0.120E+0
      GAP = 0.100E+0
      HITE = 0.250E+0
      XSBT = 5.00E+0-15.0E+0*HITE
      KI = 0
C
      DO 150 I = 1, NCURV
         K = NAC(I)
         IF (K.GT.1024) THEN
            WRITE (ILST,110) K
  110       FORMAT (1X,'%%%%%% ARRAYS X,Y,Z DIMENSIONED AT 1024. %%%%%%'
     *         /,1X,'%%%%%% NEED ',I4,'... ALL 1024 AND 952   %%%%%%'/,
     *         1X,'%%%%%% ARRAYS NEED TO BE CHANGED        %%%%%%')
            CALL NEWPEN (1)
            CALL PLOT (0.00E+0,0.00E+0,999)
            STOP
         ENDIF
C
C       READ IN A CONTOUR
C
         READ (IDSK1,120) (X(M),Y(M),Z(M),M=1,K)
  120    FORMAT (8F10.6)
         AMA(1) = -100.0E+0
         AMA(2) = -100.0E+0
         AMA(3) = -100.0E+0
         AMI(1) = 100.0E+0
         AMI(2) = 100.0E+0
         AMI(3) = 100.0E+0
C
C       ROTATE THE CONTOUR USING THETA,PHI,GAMMA.....
C
         CALL ROTXYZ (C,K,THE,GAM,PHI,PERZ,CO)
         !CALL ROTCOR (C,K,THE,GAM,PHI,PERZ,CO)
C
C       DETERMINE THE PLANE EQUATION FOR EACH CONTOUR TO USE IN
C       THE HIDDEN LINE ROUTINE.
C
         CALL PLANEQ (K,I)
         NBEG(I) = KI+1
C
C        DETERMINE X,Y, AND Z MIN AND MAX FOR EACH CONTOUR
C
         DO 130 J = 1, K
            KI = KI+1
            P = C(J,1)
            AMA(1) = MAX(AMA(1),P)
            AMI(1) = MIN(AMI(1),P)
            AC(KI,1) = P
            P = C(J,2)
            AMA(2) = MAX(AMA(2),P)
            AMI(2) = MIN(AMI(2),P)
            AC(KI,2) = P
            P = C(J,3)
            AMA(3) = MAX(AMA(3),P)
            AMI(3) = MIN(AMI(3),P)
            AC(KI,3) = P
  130    CONTINUE
         DO 140 II = 1, 3
            ACMIN(I,II) = AMI(II)
            ACMAX(I,II) = AMA(II)
  140    CONTINUE
  150 CONTINUE
C
C       COMPUTE THE INVERSE OF THE SLOPE AND THE INTERCEPT
C       FOR EACH LINE SEGMENT FORMING THE CONTOURS.
C
      DO 160 IK = 2, NC
         DENOM = AC(IK,2)-AC((IK-1),2)
         IF (ABS(DENOM).GT.0.00010E+0) THEN
            SLPINV(IK) = (AC(IK,1)-AC((IK-1),1))/DENOM
         ELSE
            SLPINV(IK) = 1000.0E+0
         ENDIF
         IF (ABS(SLPINV(IK)).GT.0.00010E+0) THEN
            YINT(IK) = AC(IK,2)-(AC(IK,1)/SLPINV(IK))
         ELSE
            YINT(IK) = -1000.00E+0
         ENDIF
  160 CONTINUE
C
C       GENERATE ARRAY OF POINTERS TO THE START OF INDIVIDUAL CONTOURS.
C       THESE ARE USED TO KEEP TRACK OF ALL CONTOURS CONTAINED IN A
C       PLANE FOR THE HIDDEN LINE ELIMINATION.
C
      KI = NCURV+1
      NBEG(KI) = NBEG(NCURV)+NAC(NCURV)
      CL = CM*SCALE
      ADMI = -CL
      PII = 2.00E+0*CL*0.10E+0
C
C       HIDDEN LINE ELIMINATION
C
      NCCNT = 1
      DO 180 NK = 1, NCHID2
C
C       FIND TOTAL MIN AND MAX FOR EACH PLANE
C
         ACNMAX(NK,1) = -1000.00E+0
         ACNMAX(NK,2) = -1000.00E+0
         ACNMAX(NK,3) = -1000.00E+0
         ACNMIN(NK,1) = 1000.00E+0
         ACNMIN(NK,2) = 1000.00E+0
         ACNMIN(NK,3) = 1000.00E+0
C
C       REDUCE TO ONE PLANE EQUATION PER PLANE...
C
         PLANES(NK,1) = PLANES(NCCNT,1)
         PLANES(NK,2) = PLANES(NCCNT,2)
         PLANES(NK,3) = PLANES(NCCNT,3)
         IPARA(NK) = IPARA(NCHID(NK))
         DO 170 J = NCCNT, NCHID(NK)
            ACNMAX(NK,1) = MAX(ACNMAX(NK,1),ACMAX(J,1))
            ACNMAX(NK,2) = MAX(ACNMAX(NK,2),ACMAX(J,2))
            ACNMAX(NK,3) = MAX(ACNMAX(NK,3),ACMAX(J,3))
            ACNMIN(NK,1) = MIN(ACNMIN(NK,1),ACMIN(J,1))
            ACNMIN(NK,2) = MIN(ACNMIN(NK,2),ACMIN(J,2))
            ACNMIN(NK,3) = MIN(ACNMIN(NK,3),ACMIN(J,3))
  170    CONTINUE
         NCCNT = NCHID(NK)+1
  180 CONTINUE
C
      MCCNT = 1
      DO 350 MK = 1, NCHID2
C
C       COMPUTE THE Z VALUE OF THE PLANE AT X=1, Y=1 TO COMPARE PARALLEL
C       PLANES ( IF THIS ONE'S Z VALUE IS IN FRONT OF THE Z VALUE OF A
C       PLANE PARALLEL TO IT, ALL POINTS ON THE PLANE ARE IN FRONT OF
C       THE PARLEL PLANE, AND THUS CANNOT BE HIDDEN BY ANY SURFACE ON
C       THE PLANE.  PLANES(MK,1)=(A/C), PLANES(MK,2)=(B/C),
C       PLANES(MK,3)=(D/C), Z= - ( (A/C)X + (B/C)Y + (D/C) ), SO FOR X=1
C       AND Y=1, Z = - ( PLANES(MK,1)+PLANES(MK,2)+PLANES(MK,3) )
C
         ZTST = PLANES(MK,1)+PLANES(MK,2)+PLANES(MK,3)
         IPARA1 = IPARA(MK)
         DO 340 I = MCCNT, NCHID(MK)
            K = NAC(I)
            NUM = 0
            IS = 1
            IF = K
            IF (NHL.NE.1) THEN
               NCCNT = 1
               DO 200 NK = 1, NCHID2
                  ZNK = PLANES(NK,1)+PLANES(NK,2)+PLANES(NK,3)
                  PARLEL = IPARA1.EQ.IPARA(NK)
                  IF ((ZTST.LT.ZNK).OR.(.NOT.PARLEL)) THEN
                     FIRST = .TRUE.
                     DO 190 J = NCCNT, NCHID(NK)
                        IF (FIRST) THEN
                           IF (ACMIN(I,1).LT.ACMAX(J,1)) THEN
                              IF (ACMAX(I,1).GT.ACMIN(J,1)) THEN
                                 IF (ACMIN(I,2).LT.ACMAX(J,2)) THEN
                                    IF (ACMAX(I,2).GT.ACMIN(J,2)) THEN
                                    IF (ACMIN(I,3).LT.ACMAX(J,3)) THEN
                                    NUM = NUM+1
                                    IPOS(NUM) = NK
                                    FIRST = .FALSE.
                                    ENDIF
                                    ENDIF
                                 ENDIF
                              ENDIF
                           ENDIF
                        ENDIF
  190                CONTINUE
                  ENDIF
                  NCCNT = NCHID(NK)+1
  200          CONTINUE
            ENDIF
C
C       IPOS CONTAINS THE POSSIBLE COVERING CURVES, PLACE CURVE IN C
C
            NBE = NBEG(I)-1
            DO 210 L = 1, K
               NBE = NBE+1
               C(L,1) = AC(NBE,1)
               C(L,2) = AC(NBE,2)
               C(L,3) = AC(NBE,3)
  210       CONTINUE
C
C       TAKE EACH POINT AND SEE IF IT IS BEHIND A IPOS
C       IF IT IS, MARK IT WITH 1000 FOR ITS X.
C
            IF (NUM.NE.0) THEN
               DO 240 L = 1, K
                  X1 = X(L)
                  Y1 = Y(L)
                  Z1 = Z(L)
                  DO 220 J = 1, NUM
                     NCRV = IPOS(J)
                     Z2 = X1*PLANES(NCRV,1)+Y1*PLANES(NCRV,2)+
     *                  PLANES(NCRV,3)
                     IF (Z2.GT.(Z1+0.20)) THEN
C
C                    IF (Z1.LT.ACNMAX(NCRV,3)) THEN
C
                        IF (Z2.LT.ACNMAX(NCRV,3)) THEN
                           IF (Z2.GT.ACNMIN(NCRV,3)) THEN
                              IF (X1.LT.ACNMAX(NCRV,1)) THEN
                                 IF (X1.GT.ACNMIN(NCRV,1)) THEN
                                    IF (Y1.LT.ACNMAX(NCRV,2)) THEN
                                    IF (Y1.GT.ACNMIN(NCRV,2)) THEN
                                    IF (HIDDEN(X1,Y1,NCRV)) GO TO 230
                                    ENDIF
                                    ENDIF
                                 ENDIF
                              ENDIF
                           ENDIF
                        ENDIF
C
C                     ENDIF
C
                     ENDIF
  220             CONTINUE
                  GO TO 240
  230             X(L) = 1000.0E+0
  240          CONTINUE
            ENDIF
C
C       PLOT WHAT S LEFT OF THE CURVE
C
            NSEC = 1
            IF (NCDASH(I).EQ.0) LINCOL = 2
            IF (NCDASH(I).NE.0) LINCOL = 3
            IF (JLINES.GE.1.AND.LINCOL.NE.PENCOD) CALL NEWPEN (LINCOL)
            PENCOD = LINCOL
            NSEC = 1
            IF (NCDASH(I).NE.0) NSEC = 0
            IF (JLINES.EQ.1) NSEC = 0
            IF (NUM.EQ.0) GO TO 310
            K1 = 1
  250       DO 260 J = K1, K
               IF (X(J).NE.1000.0E+0) GO TO 270
  260       CONTINUE
            GO TO 340
  270       IS = J
            DO 280 J = IS, K
               IF (X(J).EQ.1000.0E+0) GO TO 290
  280       CONTINUE
            IF = K
            GO TO 300
  290       IF = J-1
  300       IF (IF.EQ.IS) GO TO 330
  310       JJ = 0
            DO 320 II = IS, IF
               JJ = JJ+1
               XPL(JJ) = X(II)
               YPL(JJ) = Y(II)
  320       CONTINUE
            XPL(JJ+1) = ADMI
            YPL(JJ+1) = ADMI
            XPL(JJ+2) = PII
            YPL(JJ+2) = PII
            CALL DSHLIN (XPL,YPL,JJ,DSH,GAP,NSEC)
  330       IF (IF.NE.K) THEN
               K1 = IF+1
               GO TO 250
            ENDIF
  340    CONTINUE
         MCCNT = NCHID(MK)+1
  350 CONTINUE
      CALL NEWPEN (1)
C
C       PLOT SUBTITLE
C       THE SUBTITLE IS PLACED SLIGHTLY BELOW THE MOLECULE
C       AND IS GENERALLY USED TO REPORT THE ENERGY OF THE
C       ORBITAL
C
      YMIN = 100.0E+0
      DO 360 I = 1, NCURV
         YMIN = MIN(YMIN,ACMIN(I,2))
  360 CONTINUE
      YMIN = MIN(YMIN,YMOLMN)
      YMIN = 5.00E+0*(YMIN+CL)/CL-0.800E+0
      CALL SYMBOL (XSBT,YMIN,HITE,SUBT,0.00E+0,40)
      CLOSE(IDSK1)
      CLOSE(IDSK2)
      CLOSE(IDSK3)
      RETURN
      END
C
C
      LOGICAL FUNCTION HIDDEN (X,Y,NCRV)
C
C       GIVEN THE POINT X,Y WHICH IS BEHIND THE PLANE CONTAINING
C       THE SURFACE NCRV, DETERMINE IF THE AREA OF THE SURFACE
C       COVERS THE POINT.
C
C       THIS ROUTINE IS THE GUTS OF THE HIDDEN LINE ELIMINATION.
C       THE ALGORITHM IT USES WAS DEVELOPED BY
C       W.L. JORGENSEN AND D.E. BARTH IN APRIL 1972.
C
C       MAJOR MODIFICATIONS TO THE ALGORITHM WERE MADE IN OCTOBER 1987
C       BY DANIEL L. SEVERANCE TO INCREASE THE EFFICIENCY.  THE HEIGHT
C       OF THE CURVE AT POINT X,Y IS CALCULATED BY SUBSTITUTING INTO
C       THE PLANE EQN. FOR THE CURVE.  THIS IS DONE OUTSIDE OF THE
C       ROUTINE TO SCREEN OUT CURVES WHICH WILL NOT BE HIDING THE POINT
C       ANYWAY.  THIS ROUTINE THEN DETERMINES FOR THOSE CURVES WITH
C       LARGER 'Z' VALUES, IF THE POINT IS ACTUALLY BEHIND A FILLED
C       PORTION OF THE CURVE, OR IN AN OPEN AREA.  THIS HAS NOW BEEN
C       MADE INTO A LOGICAL FUNCTION (TRUE/FALSE RETURNED).
C                            D.L. SEVERANCE
C
C       JAN. '88 MODIFIED TO IMPROVE PERFORMANCE, CLEANED UP "CLIPPING"
C       TO USE XOR (.NEQV.) LOGIC RATHER THAN IF STATEMENTS.  D.L.S.
C
      LOGICAL LT1,LT2,EVEN
      COMMON /DLT/ YINT(75000),SLPINV(75000),PLANES(1024,3),NCHID(1024)
      COMMON /POINTS/ AC(75000,3)
      COMMON /HLCOM/ NAC(1024),NNBEG(1024),ACMIN(1024,3),ACMAX(1024,3)
      IF (NCRV.EQ.1) THEN
         NBEG = 1
      ELSE
         NBEG = NCHID(NCRV-1)+1
      ENDIF
      EVEN = .TRUE.
      HIDDEN = .FALSE.
      DO 20 IK = NBEG, NCHID(NCRV)
         NBEG2 = NNBEG(IK)
         K = NAC(IK)-1
C
C        THIS SECTION DETERMINES WHICH LINES MAKE WINDOWS ABOUT THE POIN
C        USE A SINGLE LOGICAL ASSIGNMENT FOR EFFICIENCY.
C
         LT2 = (Y.LT.AC(NBEG2,2))
         DO 10 N2 = NBEG2+1, NBEG2+K
            LT1 = LT2
            LT2 = (Y.LT.AC(N2,2))
            IF (LT1.NEQV.LT2) THEN
               EVEN = .NOT.EVEN
               XP = (Y-YINT(N2))*SLPINV(N2)
C
C       XP IS THE X VALUE FOR THE
C       INTERSECTION POINT OF A LINE AT Y=Y WITH THE
C       CURVE IN QUESTION, NCRV. (THERE MAY BE ANY
C       NUMBER OF INTERSECTIONS.)
C
               IF (X.GE.XP) THEN
                  HIDDEN = .NOT.HIDDEN
               ENDIF
            ENDIF
   10    CONTINUE
   20 CONTINUE
      HIDDEN = HIDDEN.AND.EVEN
      RETURN
      END
C
C
      SUBROUTINE PLANEQ (IDIM,NCTR)
      COMMON /DLT/ YINT(75000),SLPINV(75000),PLANES(1024,3),NCH(1024)
      COMMON /RPLOT/ N,CO(3),CM,THE,GAM,PHI,X(1024),Y(1024),Z(1024),
     *   IDASH,SCALE,PERZ
      DIMENSION V(2,3),D(4)
C
C       GENERATE THE COEFFICIENTS OF THE PLANE EQUATION GIVEN 3 INPUT
C       POINTS KNOWN TO LIE IN THE PLANE
C
      I2 = IDIM/3
      I3 = I2*2+1
      I2 = I2+1
      V(1,1) = X(I2)-X(1)
      V(1,2) = Y(I2)-Y(1)
      V(1,3) = Z(I2)-Z(1)
      V(2,1) = X(I3)-X(1)
      V(2,2) = Y(I3)-Y(1)
      V(2,3) = Z(I3)-Z(1)
C
C       NOW WE HAVE THE TWO LINES CALC. CROSS PRODUCT
C
      D(1) = V(1,2)*V(2,3)-V(1,3)*V(2,2)
      D(2) = V(1,3)*V(2,1)-V(1,1)*V(2,3)
      D(3) = V(1,1)*V(2,2)-V(1,2)*V(2,1)
C
C       NOW THE VALUE OF D BY SUBSTITUTING VALUES FROM C(1)
C
      D(4) = -(D(1)*X(1)+D(2)*Y(1)+D(3)*Z(1))
C
C     TO FIND Z GIVEN X AND Y, THE EQUATION REARRANGES TO
C     Z = ( D(1)*X + D(2)*Y + D(4) ) / D(3)
C     REWRITING AS Z = (D(1)/D(3))*X + (D(2)/D(3))*Y + D(4)/D(3)
C     THIS NEXT CODE CALCS THE MULTIPLIERS ABOVE IN PARENTHESIS
C
      D3INV = -1.00E+0/D(3)
      PLANES(NCTR,1) = D(1)*D3INV
      PLANES(NCTR,2) = D(2)*D3INV
      PLANES(NCTR,3) = D(4)*D3INV
      RETURN
      END
