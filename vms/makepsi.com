$ set nover
$!
$! VMS dcl script to compile PSI/88.
$!
$! If you rename the fortran files from *.f to *.for, then remove the
$! .f from the lines below.  UNIX will not allow any other extension
$! than .f for fortran files, VMS will, so I named all of the source
$! files with the UNIX extensions.
$!
$! Rather than "hard-code" a directory to put the executables in,
$! when the compilation is complete, the executables will need to be 
$! moved to the desired directory (if different from the source).
$!
$! Usage: @makepsi [ psi1 | psicon | psi2 | all ] 
$!
$!        The default is all.
$!
$ p2=p1
$ if p1.eqs."" then p2="ALL"
$ if p2.eqs."ALL" then goto cmppreplot
$ goto cmp'p1
$ cmppreplot:
$ write sys$output "Compiling and linking Preplot."
$ fortran preplot.f
$ link preplot
$ write sys$output "Preplot made."
$ if p2.nes."ALL" then goto exit
$ cmppsi1:
$ write sys$output "Compiling and linking Psi1/88."
$ fortran psi1.f
$ link psi1
$ write sys$output "Psi1/88 made."
$ if p2.nes."ALL" then goto exit
$ cmppsicon:
$ write sys$output "Compiling and linking Psicon/88."
$ fortran psicon.f
$ link psicon
$ write sys$output "Psicon/88 made."
$ if p2.nes."ALL" then goto exit
$ cmppsi2:
$!
$! You will need to link in some sort of graphics library to use PSI2/88
$! I have included a GKS library using the GKS fortran binding.
$!
$! a routine to write out a Chemtext metafile
$! a routine to write out a postscript file and
$! a routine to write out a HPGL plot file
$!
$ write sys$output "Compiling and linking Psi2/88."
$ fortran psi2.f
$ fortran gksplot.f
$ fortran ctplot.f
$ fortran psplot.f
$ fortran hpplot.f
$!
$ link psi2,gksplot,sys$library:gksforbnd/lib   !psi2.exe   (GKS)
$ link psi2,ctplot/exe=psi2ct.exe               !psi2ct.exe (ChemText)
$ link psi2,psplot/exe=psi2ps.exe               !psi2ps.exe (PostScript)
$ link psi2,hpplot/exe=psi2hp.exe               !psi2hp.exe (HPGL)
$ write sys$output "Psi2/88 made."
$ delete/noconf *.obj*;*
$ exit:
$ exit
