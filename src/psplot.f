c
c    Fortran version of a set of C routines which handled the output for
c    Rex Saunders' Calcomp-like Postscript routines (written in Fortran)
c    Dan Severance, 5/89 Purdue University
c
c    The rest of the code is Rex Saunders' with some modifications
c
c    The original routines were very much UNIX bound, besides requiring
c    the user to have two compilers.
c
c    These routines should work on any computer including UNIX with a
c    fortran 77 compiler..
c
c    These routines do _not_ generate Encapsulated PostScript... just
c    the plain vanilla variety...
c
      subroutine plbegn
      character*7 fname
      character bigstr*256
      common /str/ bigstr,nstr
      data fname / 'psplot'/
      nstr = 1
      open (7,file=fname,status='unknown',iostat=l)
      if (l.ne.0) then
         write (*,*) 'plbegn: can''t open ',fname
         return
      endif
      write (7,*) '%!'
      write (7,*) '% postscript output from ccps library'
      return
      end
c
c
      subroutine pldone
c
      write (7,*) '% end of ccps output'
      close (7)
      return
      end
c
c
      subroutine plcout (c)
      character c*1,bigstr*256
      common /str/ bigstr,nstr
      data islash / 92 /
c
c    send one char to plot file
c
      ic = mod(ichar(c),127)
      bigstr(nstr:nstr) = c
      nstr = nstr+1
      return
      end
c
c
      subroutine pliout (i)
      integer i
      character bigstr*256,temp*80
      common /str/ bigstr,nstr
c
c    send one integer value to plot string
c
      write (temp,'(i20)') i
      call strbnd (temp,ibgn,iend)
      iadd = iend-ibgn
      bigstr(nstr:nstr+iadd) = temp(ibgn:iend)
      nstr = nstr+iadd+1
      return
      end
c
c
      subroutine plfout (f)
      real f
      character bigstr*256,temp*80
      common /str/ bigstr,nstr
c
c    send one float value to plot string
c
      write (temp,'(f10.3)') f
      call strbnd (temp,ibgn,iend)
      iadd = iend-ibgn
      bigstr(nstr:nstr+iadd) = temp(ibgn:iend)
      nstr = nstr+iadd+1
      return
      end
c
c
      subroutine plsout (s)
      character*(*)s
      character bigstr*256,blnk*1,newl*1,slshn*2
      common /str/ bigstr,nstr
      data blnk / ' '/,islsh / 92 /,slshn / ' n'/
c
c    send a string to plot file
c
      newl = char(10)
      slshn(1:1) = char(islsh)
      call strbnd (s,ibgn,iend)
      i = index(s(1:iend),newl)
      j = index(s(1:iend),slshn)
      if (j.ne.0) i = j
c
c    if i is not 0, then there was a newline imbedded, decrement the end
c    pointer to be just before the newline and write the string, along
c    with whatever happens to be in bigstr.
c
      if (i.ne.0) then
         iend = i-1
         if (nstr.eq.1) then
            write (7,*) s(1:iend)
         else
            nstr = nstr-1
            write (7,*) bigstr(1:nstr),s(1:iend)
            nstr = 1
         endif
      else
c
c    there was no newline, so add the text to whats in bigstr
c
         iadd = iend
c
c    add one to end in case a space was included in the incoming string
c
         iend = iend+1
         bigstr(nstr:(nstr+iadd)) = s(1:iend)
         nstr = nstr+iend
      endif
      return
      end
c
c
      subroutine strbnd (strng,ibgn,iend)
c
c    routine to return the bounds of a string
c
      character*(*) strng
      integer ibgn,iend
c
      l = len(strng)
      do 10 i = 1, l
         if (strng(i:i).ne.' ') go to 20
   10 continue
   20 ibgn = i
      do 30 i = l, 1, -1
         if (strng(i:i).ne.' ') go to 40
   30 continue
   40 iend = i
      return
      end
c
c end of rewritten C routines - what follows is partially modified code
c from Rex Saunders
c
      subroutine plots (idum)
      integer idum
c
c   plotter initialisation routine - must be called before any other plo
c     calls are made
c
c   where:
c     idum = dummy variable for compatibility
c

      common /cqpbnf/ xold,yold,fac,ires
      save /cqpbnf/
      real xold,yold,fac
      integer ires

      logical first
      save first
      data first / .true. /

c
c   initialise plot storage - if we haven't already
c
      if (idum.eq.1) then
         call pldone
         first = .true.
      endif
      if (first) then
         first = .false.
         call plbegn
c
c       scale, rotate and translate postscript output
c         units of pixels (300/inch for most laser printers)
c         origin in lower left corner, landscape mode
c
         call plsout ('72 300 div dup scale\n')
         call plsout ('90 rotate\n')
         call plsout ('75 -2460 translate\n')
         call plsout ('0 0 moveto\n')
c
c       set other line drawing parameters
c
         call plsout ('1 setlinewidth\n')
         call plsout ('1 setlinejoin\n')
         call plsout ('1 setlinecap\n')
c
c     speed up symbol font handling
c
         call plsout ('/sf /Times-Roman findfont def\n')
c
c       set up definitions for other routines:
c
c       move
c
         call plsout ('/m /moveto load def\n')
c
c       relative move
c
         call plsout ('/rm /rmoveto load def\n')
c
c       draw
c
         call plsout ('/d {lineto currentpoint stroke moveto} def\n')
c
         call plsout ('/l /lineto load def\n')
c
c       set new origin
c
         call plsout ('/o {currentpoint translate} def\n')
c
c       set new linewidth
c
         call plsout (
     *      '/w {currentpoint stroke moveto setlinewidth} def\n')
c
c       set character height
c
         call plsout ('/h {sf exch scalefont setfont} def\n')
c
c       show character string
c
         call plsout ('/s /show load def\n')
c
c       start and end rotated text
c
         call plsout ('/rs {currentpoint gsave translate rotate} def\n')
         call plsout ('/re /grestore load def\n')

c
c set a circle file macro.
c
         call plsout ('/cf {moveto setgray 0 360 arc fill} def\n')
         call plsout ('/ci {0 360 arc 0.0 setgray stroke} def\n')
         call plsout ('% \n')
      endif

c
c   initialise common variables
c
      fac = 1.0
      xold = 0.0
      yold = 0.0
      ires = 300
      return
      end
c
c
      subroutine plot (x,y,ipen)
      real x,y
      integer ipen
c
c  plotter driver conforming to:
c    'programming calcomp electromechanical plotters', calcomp, january
c    output for postscript printers like apple laserwriter plus
c
c  rex sanders, usgs, 2/87
c
c  where:
c    x,y = coordinates, in inches from the current origin, of the positi
c          to which the pen is to be moved
c
c    ipen = pen control, origin definition, and plot termiination such t
c      if ipen = 1, move with pen in present condition
c      if ipen = 2, move with pen down
c      if ipen = 3, move with pen up
c      if ipen = -1, move with no pen change, reset origin to terminal p
c      if ipen = -2, move with pen down, reset origin to terminal positi
c      if ipen = -3, move with pen up, reset origin to terminal position
c      if ipen = 666, see 999.
c      if ipen = 999, move with pen up, terminate plot, close plot file
c      if ipen = anything else, no action is taken
c

      common /cqpbnf/ xold,yold,fac,ires
      save /cqpbnf/
      real xold,yold,fac
      integer ires

      integer locpen
      logical penup
      save penup
      data penup / .true. /

      locpen = abs(ipen)

c
c   check pen for proper values
c
      if (locpen.ne.1.and.locpen.ne.2.and.locpen.ne.3.and.ipen.ne.
     *   666.and.ipen.ne.999) return

c
c   reset locpen to current pen status
c
      if (locpen.eq.1) then
         if (penup) then
            locpen = 3
         else
            locpen = 2
         endif
      elseif (ipen.eq.666) then
         locpen = 999
         ipen = 999
      endif

c
c   set up for move or draw
c     output 'x y'
c
      call pliout (nint(x*fac*ires))
      call plcout (char(32))
      call pliout (nint(y*fac*ires))

c
c   pen down - draw
c
      if (locpen.eq.2) then
         call plsout (' d\n')
         penup = .false.

c
c   pen up - move
c
      elseif (locpen.eq.3.or.locpen.eq.999) then
         call plsout (' m\n')
         penup = .true.
      endif

      if (ipen.ge.0) then
         xold = x
         yold = y
      else
c
c     set new origin
c
         call plsout ('o\n')
         xold = 0.0
         yold = 0.0
      endif

c
c   close and clean up plot file
c
      if (ipen.eq.999) then
         call plsout ('showpage\n')
         call pldone
      endif

      return
      end
c
c
      subroutine line (x,y,n,k,j,ocsym)
      character strocs
      integer ocsym
      dimension x(1),y(1)
c
c   plot pairs x,y scaled to minimum of
c   x(n*k+1),y(n*k+1), and increment per inch
c   of x(n*(k+1)),y(n*(k+1)).
c
c   x = horizontal array of points
c   y = vertical array of points
c   n = # of pairs of points to ploit in x and y
c   k = plot n points from the 1st, k+1st, 2,k+1st, etc..
c       positions of arrays x and y.
c   j = >0: plot symbol ocsym (hollerith) each jth point
c            with connected line
c   j =  0: plot only the line.
c   j = <0: plot only the symbols each jth point.
c
      n1 = n*k
      im = n1+1
      id = im+k
      if (x(id).eq.0.or.y(id).eq.0) return
      ipen = 2
      m = j
      if (j.lt.0) then
         m = -m
         ipen = 3
      endif
      x1 = (x(1)-x(im))/x(id)
      y1 = (y(1)-y(im))/y(id)
      call plot (x1,y1,3)
      jcnt = 0
      do 10 i = 1, n1, k
         jcnt = jcnt+1
         xp = (x(i)-x(im))/x(id)
         yp = (y(i)-y(im))/y(id)
         if (j.ne.0) then
            if (m.ne.jcnt) go to 10
            jcnt = 0
         endif
         call plot (xp,yp,ipen)
         if (j.ne.0) then
            strocs = char(ocsym)
            call symbol (xp,yp,.14,strocs,0.,-1)
            call plot (xp,yp,3)
         endif
   10 continue
      return
      end
c
c
      subroutine dshlin (x,y,n,dsh,gap,nsec)
      integer pen
      dimension x(1),y(1),dsh(1),gap(1)
c
      if (nsec.ne.0) then
c
c     initialize
c
         k = 1
         pen = 2
         s = 0.0
         t = dsh(1)
         xmin = x(n+1)
         xinc = x(n+2)
         ymin = y(n+1)
         yinc = y(n+2)
c
c     move to first point
c
         x2 = (x(1)-xmin)/xinc
         y2 = (y(1)-ymin)/yinc
         call plot (x2,y2,3)
c
c     plot dashed line curve
c
         do 20 i = 2, n
   10       x1 = x2
            y1 = y2
            x2 = (x(i)-xmin)/xinc
            y2 = (y(i)-ymin)/yinc
            d = sqrt((x2-x1)**2+(y2-y1)**2)
            s = s+d
            if (s.ge.t) then
               x2 = x2+(x1-x2)*(s-t)/d
               y2 = y2+(y1-y2)*(s-t)/d
               call plot (x2,y2,pen)
               pen = 5-pen
               s = 0.0
               t = gap(k)
               if (pen.eq.3) go to 10
               k = mod(k,nsec)+1
               t = dsh(k)
               go to 10
            endif
            call plot (x2,y2,pen)
   20    continue
         return
      endif
      call line (x,y,n,1,0,0)
      return
      end
c
c
      subroutine symbol (xin,yin,ht,cstr,ang,nchin)
      real xin,yin,ht,ang
      integer nchin
c
c   symbol subroutine conforming to
c     'programming calcomp electromechanical plotters', 1976
c   for postscript printers, using adobe courier font and font metrics
c
c   rex sanders, usgs, 3/87  jim blake 06/88
c

      common /cqpbnf/ xold,yold,fac,ires
      save /cqpbnf/
      real xold,yold,fac
      integer ires

      real x,y
      real cosang,sinang
      integer nch,i,ic,intang

c           cfudge - courier font fudge factor to get proper height
c
      real cfudge
      save cfudge

      character*(*) cstr


c     data cfudge / 1.66666666 / <-- Too big
      data cfudge / 1.20000000 /
c
c   bad input check
c
      if (nchin.lt.-2.or.ht.le.0.0) return
c
c check for calcomp on-center symbols.
c
      if (ichar(cstr(1:1)).ge.0.and.ichar(cstr(1:1)).le.25) then
         return
      endif
c
c   initialize lots of stuff
c
      x = xin-ht/4.0
      y = yin-ht/4.0
      nch = nchin
      call plot (x,y,3)
c
c   round angle to integer - good to 1 degree
c
      intang = nint(ang)
      cosang = cos(float(intang)*0.017453292519)
      sinang = sin(float(intang)*0.017453292519)
c
c   set char height
c
      call pliout (nint(ht*cfudge*fac*ires))
      call plsout (' h ')
c
c   plot a string of characters
c
      if (nch.gt.0) then
c
c     set char angle
c
         if (intang.ne.0) then
            call pliout (intang)
            call plsout (' rs ')
         endif

c
c     output '(string) s ', escape ( ) \
c
         call plcout (char(40))

         do 10 i = 1, nch
            ic = mod(ichar(cstr(i:i)),127)
            if (ic.eq.40.or.ic.eq.41.or.ic.eq.92) then
               call plcout (char(92))
            endif
            call plcout (cstr(i:i))
   10    continue

         call plsout (') s ')

c
c       update our idea of where the pen is.
c
         xold = x+(nch*ht*fac*cosang)
         yold = y+(nch*ht*fac*sinang)
c
c     undo character angle
c
         if (intang.ne.0) then
            call plsout ('re\n')
         else
            call plsout ('\n')
         endif

c
c   plot one char in strin
c
      elseif (nch.eq.0) then
c
c     set char angle
c
         if (intang.ne.0) then
            call pliout (intang)
            call plsout (' rs ')
         endif

c
c     output '(c) s ', escape '(' and ')' and '\'
c
         call plcout (char(40))
         ic = mod(ichar(cstr(1:1)),127)
         if (ic.eq.40.or.ic.eq.41.or.ic.eq.92) then
            call plcout (char(92))
         endif
         call plcout (cstr(1:1))
         call plsout (') s ')

c
c   update our idea of where the pen is.
c
         xold = x+(ht*fac*cosang)
         yold = y+(ht*fac*sinang)
c
c     undo character angle
c
         if (intang.ne.0) then
            call plsout ('re\n')
         else
            call plsout ('\n')
         endif
      endif

      return
      end
c
c
      subroutine factor (f)
      real f
c
c  sets plot sizing factor -
c    if f = 2.0 then all subsequent pen movements will be twice normal s
c    if f is reset to 1.0, all plotting returns to normal size
c

      common /cqpbnf/ xold,yold,fac,ires
      save /cqpbnf/
      real xold,yold,fac
      integer ires

      fac = f
      return
      end
c
c
      subroutine newpen (ipen)
      integer ipen
c
c  selects new pen as indicated by ipen
c  where:
c     ipen = 1..n indicating pen 1 - n
c  simulated by changing line width for postscript printers
c

      integer npen
      integer usepen
      integer savpen
      save savpen
      data savpen / 1 /

      npen = max(ipen,1)
      if (npen.ne.savpen) then
c
c     want pen width to be odd number of pixels wide
c
         usepen = ((npen-1)*2)+1
         call pliout (usepen)
         call plsout (' w\n')

         savpen = npen
      endif

      return
      end
