C
C
      PROGRAM PREPLOT
      PARAMETER (MAXAT=160)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C PROGRAM TO CONVERT THE FILE PRODUCED BY THE MOPAC 'GRAPH' KEYWORD
C TO A SET OF INPUT FILES FOR THE PSI/88 PROGRAM
C
C DERIVED FROM ROUTINES IN MOPAC - DAN SEVERANCE, PURDUE - 9/88
C
C       Version 1.0  Any questions to the author should specify
C                    the version being used.
C
C Redistribution and use in source and binary forms are permitted
C provided that the above paragraphs and this one are duplicated in
C all such forms and that any documentation, advertising materials,
C and other materials related to suchDanieldistribution and use acknowledge
C that the software was developed by Daniel Severance at Purdue University
C The name of the University or Daniel Severance may not be used to endorse
C or promote products derived from this software without specific prior
C written permission.  The author is now at Yale University.
C THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
C IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
C WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
C
C FILES USED:
C
C FILE 13 - INPUT MOPAC GRAPH (.GPT) FILE
C FILE 8  - OUTPUT PSI1   INPUT FILE
C FILE 9  - OUTPUT PSICON INPUT FILE
C FILE 10 - OUTPUT PSI2   INPUT FILE
C
      DIMENSION C(40000),H(40000),VECS(40000)
      DIMENSION ZS(MAXAT),ZP(MAXAT),ZD(MAXAT),XYZ(3,MAXAT)
      INTEGER TOTAL,NLAST(MAXAT),NFIRST(MAXAT),NAT(MAXAT),IAT(26)
      CHARACTER*10 SUBTIT
C
C INITIALIZE THE NUMBER OF VALENCE ELECTRONS PER ATOM
C
      DATA IAT / 1,2,1,2,3,4,5,6,7,8,1,2,3,4,5,6,7,8,1,2,3,4,5,6,7,8 /
C
C READ FROM DISK THE FOLLOWING DATA FOR GRAPHICS CALCULATION, IN ORDER:
C
C      NUMBER OF ATOMS, ORBITAL, ELECTRONS
C      ALL ATOMIC COORDINATES
C      ORBITAL COUNTERS
C      ORBITAL EXPONENTS, S, P, AND D, AND ATOMIC NUMBERS
C      EIGENVECTORS (M.O.S NOT RE-NORMALISED)
C      INVERSE-SQUARE ROOT OF THE OVERLAP MATRIX.
C
      READ (13) NUMAT,NORBS,NELECS,((XYZ(I,J),J=1,NUMAT),I=1,3)
      READ (13) (NLAST(I),NFIRST(I),I=1,NUMAT)
      READ (13) (ZS(I),I=1,NUMAT),(ZP(I),I=1,NUMAT),(ZD(I),I=1,NUMAT),
     *   (NAT(I),I=1,NUMAT)
C
      LINEAR = NORBS*NORBS
C
      READ (13) (C(I),I=1,LINEAR)
      READ (13) (H(I),I=1,LINEAR)
      CALL MULT (C,H,VECS,NORBS)
C
      TOTAL = 0
      DO 10 I = 1, NUMAT
         ITMP = NAT(I)
         TOTAL = TOTAL+IAT(ITMP)
C
C ZERO THE D ORBITAL ZETA VALUE. MOPAC WRITES OUT A ZETA (D) VALUE OF
C ONE FOR CHLORINE (AT LEAST) EVEN THOUGH IT HAS NO D ORBITALS.
C
         ZD(I) = 0.0D+0
   10 CONTINUE
C
      ICHRG = TOTAL-NELECS
      IHOMO = (NELECS+1)/2
      ILUMO = IHOMO+1
C
   20 WRITE(*,*)
      WRITE (*,'(A,I4,A,I4)')' The HOMO is MO number ',IHOMO,
     *  ' The LUMO is MO number ',ILUMO
      WRITE(*,*)
      WRITE (*,'(A,$)') ' WHICH MO DO YOU WISH TO PLOT? '
      READ (*,*) IMO
      IF (IMO.LT.0.OR.IMO.GT.TOTAL) GO TO 20
C
C WRITE PSI1/88 INPUT FILE
C
      SCALE = 1.4
      IONE = 1
      IBEG = (IMO-1)*NORBS+1
      IEND = IBEG+NORBS-1
C
      WRITE (8,'(A)') 'SEMI'
      WRITE (8,'(A5)') 'AUTO0'
      WRITE (8,'(I2,I2,1X,F4.2)') IMO,IMO,SCALE
      WRITE (8,50) ICHRG
      WRITE (8,60) (NAT(I),(XYZ(J,I),J=1,3),I=1,NUMAT)
      WRITE (8,'(A)') '99'
      WRITE (8,30) (VECS(I),I=1,LINEAR)
C
C WRITE OUT ZETA VALUES FOR EACH ATOM
C
   30 FORMAT (8F10.6)
      DO 40 I = 1, NUMAT
         WRITE (8,30) ZS(I),ZP(I),ZD(I)
   40 CONTINUE
C
C WRITE PSICON/88 INPUT FILE
C
      WRITE (9,'(A)') 'SEMI'
      WRITE (9,'(A)') '01010001'
      WRITE (9,'(A)') '0.075'
C
C DETERMINE LABEL TO USE FOR PLOT
C
      WRITE (SUBTIT,'(A,I4)') 'MO #',IMO
      IF ((IHOMO-2).EQ.IMO) SUBTIT = 'HOMO-2'
      IF ((IHOMO-1).EQ.IMO) SUBTIT = 'HOMO-1'
      IF (IHOMO.EQ.IMO) SUBTIT = 'HOMO'
      IF (ILUMO.EQ.IMO) SUBTIT = 'LUMO'
      IF ((ILUMO+1).EQ.IMO) SUBTIT = 'LUMO+1'
      IF ((ILUMO+2).EQ.IMO) SUBTIT = 'LUMO+2'
C
C WRITE PSI2/88 INPUT FILE
C
      WRITE (10,'(A)') ' '
      WRITE (10,'(A)') SUBTIT
      WRITE (10,'(A/A)') '01','00'
      WRITE (10,50) ICHRG
      WRITE (10,60) (NAT(I),(XYZ(J,I),J=1,3),I=1,NUMAT)
      WRITE (10,'(A)') '99'
      X = 10.0
      SCALE = 0.7
      WRITE (10,'(4F10.6)') X,X,X,SCALE
      WRITE (10,'(A)') '02'
   50 FORMAT (I2)
   60 FORMAT (I2,8X,3F10.6)
C
      STOP
      END
C
C
      SUBROUTINE MULT (C,S,VECS,N)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION C(N,*),S(N,*),VECS(N,*)
C
C**********************************************************************
C
C   THIS ROUTINE TAKEN FROM MOPAC BY J.S. STEWART
C
C   MULT IS USED IN THE MULLIKEN ANALYSIS ONLY. IT PERFORMS THE
C        OPERATION:-
C                                   VECS=BACK-TRANSFORMED EIGENVECTORS
C        VECS  =  C*S               C   =UN-BACK-TRANSFORMED VECTORS
C                                   S   =1/SQRT(OVERLAP MATRIX)
C
C**********************************************************************
C
      DO 30 I = 1, N
         DO 20 J = 1, N
C
C COMPUTE FIRST ELEMENT HERE (K=1) TO INITIALIZE SUM
C
            VECS(J,I) = C(1,I)*S(J,1)
            DO 10 K = 2, N
               VECS(J,I) = VECS(J,I)+C(K,I)*S(J,K)
   10       CONTINUE
            SUM = VECS(J,I)
   20    CONTINUE
   30 CONTINUE
      RETURN
      END
