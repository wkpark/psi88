      program chk2psi
      implicit real*8 (a-h,o-z)
      integer getarg
      logical exist
      integer unpred,junk
      character file*80,itype*20,icalc*4,irtcrd*4,alpha*1,ifile*80,     
     *   orbtyp*5,icard*80,clabel(2000)*16,unique*80
c
c  Program to retrieve the necessary information from the G90
c  checkpoint file for the psi programs. Three input files and one data
c  file are generated in the users current directory. vms 5.1
c                           James M. Briggs
c                           Purdue University
c                           chemistry department
c                           may 1988
c  Updated with modifications for UNIX/G90 Jim Blake
C
C Redistribution and use in source and binary forms are permitted
C provided that the above paragraph and this one are duplicated in 
C all such forms and that any documentation, C advertising materials,
C and other materials related to such distribution and use acknowledge 
C that the software was developed by James Briggs at Purdue University
C The name of the University or James Briggs may not be used to endorse 
C or promote products derived from this software without specific prior 
C written permission.
C THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
C IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
C WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
C
      parameter (idim=200)
      dimension eigvct(idim*idim),vct(idim,idim),vtmp(idim),eigval(idim)
      common /mol/ natoms,icharg,multip,nae,nbe,ne,nbasis,ian(401),     
     *   atmchg(400),c(3,400)
      common /route/ label(20*IDim),ititle(IDim/2),irtcrd(IDim*2)
      common /gtbasis/ itype,icard
      data ipsi1,ipsi2,ipsi3 / 7,8,9 /
      data icalc / 'auto'/
      data pinc,ionemo,nocut,ctrfact,mone / 0.2d+00,1,0,1.0d+00,1 /
      data molast,nct,iconn,iconz,norb / 1,1,1,0,1 /
      data ctr,scale / 0.1d+00,1.0d+0 /
      data npast,iuflag / 10,0 /
      data conver / 0.529177249d+00 /
c
      idim2 = idim*idim
c
c get checkpoint file name and add ".chk" if none supplied. also
c extract the unique file identifier for use with the naming of the
c data files.
c
      if (iargc() .ne. 1) then
         write (*,*) 'Usage: chk2psi filename[.chk]'
         stop
      endif
      I = getarg (1,ifile)
      call limits(ifile,jfbgn,jfend)
 
      if (ifile(jfend-3:jfend) .eq. '.chk') then
          kfend=jfend-4
      else
          kfend=jfend-jfbgn+1
          ifile(jfend+1:jfend+4) = '.chk'
          jfend = jfend + 4
      endif
      unique(1:kfend) = ifile(jfbgn:kfend)
c
c open checkpoint file and make sure that it exists before continuing.
c call fileio(11... are used to get the actual lengths of the files
c before retrieval.
c
      exist=.false.
      inquire(file=ifile,exist=exist)
      if(.not.exist)then
        write(*,*) 'Checkpoint file not found.'
        stop
      endif
c
      call fopen(1,4,ifile(jfbgn:jfend),0,junk)
c
c retrieve coordinates and other useful info. from the checkpoint file.
c
      call fileio(11,-997,lennatoms,natoms,0)
      if(lennatoms.eq.0) then
         write(*,*) 'Problem with the checkpoint file.'
         write(*,*) 'common block /mol/ not found.'
         stop
      else
         call fileio( 2,-997,lennatoms,natoms,0)
      endif
c
c retrieve route and other useful info. from the checkpoint file.
c
      call fileio(11,-502,lenlabel ,label ,0)
      if(lenlabel.eq.0) then
         write(*,*) 'Problem with the checkpoint file.'
         write(*,*) 'common block /route/ not found.'
         stop
      else
         call fileio( 2,-502,lenlabel ,label ,0)
      endif
      call stuflabl(clabel,nbasis)
c
c retrieve eigenvalues from the checkpoint file.
c
      call fileio(11,-522,leneigval ,eigval(1) ,0)
      if(leneigval.eq.0) then
         write(*,*)'Problem with the checkpoint file.'
         write(*,*)'eigenvalues not found...continuing.'
      else
         write(*,*) ' number of eigenvalues =',leneigval
         call fileio( 2,-522,leneigval ,eigval(1) ,0)
      endif
c
c retrieve real alpha eigenvectors from the checkpoint file.
c beta will be retreived later if necessary.
c
      call fileio(11,-524,leneigvct,eigvct(1),0)
      if(leneigvct.eq.0) then
         write(*,*) 'Problem with the checkpoint file.'
         write(*,*) 'real alpha eigenvectors not found.'
         stop
      else
         write(*,*) ' number of eigenvectors =',leneigvct
         call fileio(2,-524,leneigvct,eigvct(1),0)
      endif
c
c determine homo, lumo and other useful info.
c iuflag = 1 if there are unpaired electrons.
c norbs is the total number of orbitals that will be given in the
c       psi1 input file.
c npred is the number of paired electrons.
c
      nelec = ne
      unpred = multip-1
      if (unpred.ne.0) iuflag = 1
      npred = (nelec-unpred)/2
      nhomo = npred+unpred
      nlumo = nhomo+1
      norbs = nbasis
c
c     if(norbs.gt.nbasis)norbs=nbasis
c
      nbasis2 = nbasis*nbasis
      write (*,'(17a4)') (ititle(i),i=1,17)
   10 write (*,190) nhomo,nlumo
      write (*,200) nbasis
      write (*,'(''-1 for all mos ['',i2,'']:'',$)') nhomo
      read (*,'(i3)',err=10) jmo
      if (jmo.eq.0) jmo = nhomo
      if ((jmo.lt.-1).or.(jmo.gt.nbasis)) go to 10
      alpha = 'a'
      if (iuflag.eq.1) then
   20    write (*,*) 'you have chosen to plot mo # ',jmo
         write (*,*) 'molecule has multiplicity: ',multip
         write (*,'(''real alpha or beta coefficients (a/b): '',$)')
         read (*,'(a1)') alpha
         if ((alpha.ne.'a').and.(alpha.ne.'b')) go to 20
      endif
c
c open the various files associated with psi.
c
      file = unique(1:kfend)//'.psi1'
      call limits (file,jflbgn,jflend)
      open (unit=ipsi1,file=file(jflbgn:jflend),status='unknown',err=180
     *   ,iostat=ierror)
c
      file = unique(1:kfend)//'.psicon'
      call limits (file,jflbgn,jflend)
      open (unit=ipsi2,file=file(jflbgn:jflend),status='unknown',err=180
     *   ,iostat=ierror)
c
      file = unique(1:kfend)//'.psi2'
      call limits (file,jflbgn,jflend)
      open (unit=ipsi3,file=file(jflbgn:jflend),status='unknown',err=180
     *   ,iostat=ierror)
c
c set up information file from the run of psichk.
c this file contains all of the eigenvectors, eigenvalues, coordinates.
c from a gaussian job. it also contains some other interesting info.
c it could be useful in time of disaster.
c
      call getbasis
      call limits (icard,jrtbgn,jrtend)
c
c prepare input deck for psi1. lfc=7
c                        psi2. lfc=8
c                        psi3. lfc=9
c
      write (*,'(''contour level [0.1]:'',$)')
      read (*,'(f13.9)') ctr
      if (ctr.eq.0.0d+00) ctr = 0.1d+00
      if (jmo.lt.0) then
         ionemo = 0
         mone = nhomo
         molast = nhomo
      endif
      write (ipsi1,'(a20/,a4,i1/,2i2,f10.6)') itype,icalc,ionemo,mone,  
     *   molast,scale
      write (ipsi1,'(i2,5x,18a4)') icharg,(ititle(i),i=1,18)
      write (ipsi2,'(a20/4i2,1x,a4/,f10.6)') itype,nct,iconn,iconz,norb,
     *   'auto',ctr
      orbtyp = '     '
c
c having some fun here with orbital labels. ha
c s=second, t=third, f=fourth
c
      if(jmo.eq.(nhomo-3))then
         orbtyp='FHOMO'
      elseif(jmo.eq.(nhomo-2))then
         orbtyp='THOMO'
      elseif(jmo.eq.(nhomo-1))then
         orbtyp='SHOMO'
      elseif(jmo.eq.nhomo)then
         orbtyp=' HOMO'
      elseif(jmo.eq.nlumo)then
         orbtyp=' LUMO'
      elseif(jmo.eq.(nlumo+1))then
         orbtyp='SLUMO'
      elseif(jmo.eq.(nlumo+2))then
         orbtyp='TLUMO'
      elseif(jmo.eq.(nlumo+3))then
         orbtyp='FLUMO'
      endif
      write (ipsi3,'(17a4)') (ititle(i),i=1,17)
c
c do not print energy label if we did not get eigenvalues
c (leneigval.ne.0).
c
      if((iuflag.eq.1).and.(alpha.eq.'a').and.(leneigval.ne.0))then
        write(ipsi3,'(a5,2x,''Alpha: E = '',f9.5)') orbtyp,eigval(jmo)
      elseif((iuflag.eq.1).and.(alpha.eq.'b').and.(leneigval.ne.0)) then
        kmo=jmo+nbasis
        write(ipsi3,'(a5,2x,''Beta : E = '',f9.5)') orbtyp,eigval(kmo)
      elseif((iuflag.eq.0).and.(leneigval.ne.0)) then
        write(ipsi3,'(a5,2x,''E = '',f9.4)') orbtyp,eigval(jmo)
      else
         write (ipsi3,*) ' '
      endif
      write (ipsi3,'(''010000.000 auto'')')
      write (ipsi3,'(''00'')')
      write (ipsi3,'(i2,5x,18a4)') icharg,(ititle(i),i=1,18)
c
c  write the coordinates to the command files.
c
      do 40 i = 1, natoms
         do 30 j = 1, 3
            c(j,i) = c(j,i)*conver
   30    continue
   40 continue
      do 50 i = 1, natoms
         write (ipsi1,'(i2,8x,3f10.6)') ian(i),(c(j,i),j=1,3)
         write (ipsi3,'(i2,8x,3f10.6)') ian(i),(c(j,i),j=1,3)
   50 continue
      write (ipsi1,'(''99'')')
      write (ipsi3,'(''99'')')
c
   60 write (*,'(''theta(z)      [0.0]:'',$)')
      read (*,'(f10.4)',err=60) the
      if ((the.eq.0.0d+00).or.(the.eq.90.0d+00).or.(the.eq.180.0d+00)   
     *   .or.(the.eq.270.0d+00)) the = the+0.1d+00
c
   70 write (*,'(''gamma(x)      [0.0]:'',$)')
      read (*,'(f10.4)',err=70) gam
      if ((gam.eq.0.0d+00).or.(gam.eq.90.0d+00).or.(gam.eq.180.0d+00)   
     *   .or.(gam.eq.270.0d+00)) gam = gam+0.1d+00
c
   80 write (*,'(''phi(y)        [0.0]:'',$)')
      read (*,'(f10.4)',err=80) phi
      if ((phi.eq.0.0d+00).or.(phi.eq.90.0d+00).or.(phi.eq.180.0d+00)   
     *   .or.(phi.eq.270.0d+00)) phi = phi+0.1d+00
c
   90 write (*,'(''scale factor  [0.9]:'',$)')
      read (*,'(f10.4)',err=90) scale
      if (scale.eq.0.0d+00) scale = 0.9d+00
      write (ipsi3,'(4f10.4)') the,gam,phi,scale
      write (ipsi3,'(''02'')')
c
      icnt = 1
      do 110 i = 1, nbasis
         do 100 j = 1, nbasis
            vct(j,i) = eigvct(icnt)
            icnt = icnt+1
  100    continue
  110 continue
c
      nrow = nbasis
      ncol = norbs
      kite = 0
  120 low = kite+1
      kite = kite+5
      if (kite.gt.ncol) kite = ncol
c
      if (kite.lt.ncol) go to 120
      if (iuflag.eq.1) then
         if (jmo.gt.0) then
            do 130 i = 1, nbasis
               vtmp(i) = vct(i,jmo)
  130       continue
         endif
         if ((jmo.lt.0).and.(alpha.eq.'a')) then
            write (*,*) ' nbasis = ',nbasis
            write (ipsi1,'(8f10.6)') ((vct(i,j),i=1,nbasis),j=1,nbasis)
         endif
c
c  get real beta mo coeffs. for the checkpoint file.
c
         call fileio (11,-526,leneigvct,eigvct(1),0)
         if (leneigvct.eq.0) then
            write (*,*) 'problem with the checkpoint file.'
            write (*,*) 'real beta eigenvectors not found.'
            stop
         else
            call fileio (2,-526,leneigvct,eigvct(1),0)
         endif
         icnt = 1
         do 150 i = 1, nbasis
            do 140 j = 1, nbasis
               vct(j,i) = eigvct(icnt)
               icnt = icnt+1
  140       continue
  150    continue
         nrow = nbasis
         ncol = norbs
         kite = 0
  160    low = kite+1
         kite = kite+5
         if (kite.gt.ncol) kite = ncol
c
         if (kite.lt.ncol) go to 160
         if (jmo.gt.0) then
            if (alpha.eq.'a') write (ipsi1,'(8f10.6)') (vtmp(i),i=1,    
     *         nbasis)
            if (alpha.eq.'b') write (ipsi1,'(8f10.6)') (vct(i,jmo),i=1, 
     *         nbasis)
         elseif ((jmo.lt.0).and.(alpha.eq.'b')) then
            do 170 i = 1, nbasis
               write (ipsi1,'(8f10.6)') (vct(i,j),j=1,nbasis)
  170       continue
         endif
      else
         if (jmo.gt.0) then
            write (ipsi1,'(8f10.6)') (vct(i,jmo),i=1,nbasis)
         else
            write (ipsi1,210) ((vct(i,j),i=1,nbasis),j=1,nbasis)
         endif
      endif
      call fclose (1,0)
      stop
  180 write (*,*) 'iostat = ',ierror
      stop
  190 format ('note: homo =',i3,1x,'lumo =',i3)
  200 format ('mo to be plotted (1 to ',i3,')')
  210 format (8f10.6)
      end
c
c
      subroutine getbasis
      implicit integer (a-z)
      parameter (idim=200)
      common /route/ label(20*idim),ititle(idim/2) ,irtcrd(2*idim)
      common /gtbasis/ itype,icard
      character itype*20,icard*80,irtcrd*4
c
c  subroutine to parse the route card to determine the basis set used.
c
      itype = '                    '
      do 10 i = 1, 80
         icard(i:i) = ' '
   10 continue
      j = 0
      l = 0
      do 20 i = 1, 17
         j = l+1
         l = j+3
         icard(j:l) = irtcrd(i)
   20 continue
      j = 0
      kend = 0
      lend = 0
      do 50 i = 1, 63
         kend = i+1
         if ((icard(i:kend).eq.'ST').or.(icard(i:kend).eq.'3-').or.     
     *      (icard(i:kend).eq.'6-').or.(icard(i:kend).eq.'4-')) then
            kbgn = i
            kinc = 0
            do 30 j = (kbgn+1), (kbgn+20)
               if (icard(j:j).eq.' ') go to 40
               kinc = kinc+1
   30       continue
   40       kend = kbgn+kinc
            itype(1:(kinc+1)) = icard(kbgn:kend)
            return
         endif
   50 continue
      return
      end
c
      subroutine stuflabl(clabel,nbasis)
      implicit integer (a-z)
      Parameter (IDim=200)
      character*16 clabel(*)
      character*1 blabel(80*Idim),bslash
      common /route/ label(20*IDim),ititle(IDim/2),irtcrd(IDim*2)
      equivalence (label,blabel)
      bslash = char(92)
      k = 0
      icnt = 1
      do 30 i = 1, nbasis
         k = k+1
         clabel(i)(1:16) = '               '
   10    if (blabel(k).ne.bslash) then
            if (icnt.le.16) then
               clabel(i)(icnt:icnt) = blabel(k)
            endif
            k = k+1
            icnt = icnt+1
            if (k.gt.16000) then
               do 20 j = (i-1), nbasis
                  clabel(j)(1:16) = '               '
   20          continue
               return
            endif
            go to 10
         endif
         icnt = 1
   30 continue
      return
      end
c
c
      subroutine limits (str,first,last)
c
c     this subroutine finds the "first" and the "last" non-blank
c     characters in the string "str". the length of the string is not
c     numerically  limited, but its length is determined when called.
c     "i" and "ib" are the forward and backward counters.
c
      character str*(*)
      integer first,last,i,ib
c
      first = 0
      last = 0
      do 10 i = 1, len(str)
         if (first.eq.0) then
            if (str(i:i).ne.' ') first = i
         endif
c
         if (last.eq.0) then
            ib = (len(str)-i)+1
            if (str(ib:ib).ne.' ') last = ib
         endif
   10 continue
      return
      end
