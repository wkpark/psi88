$!
$! DCL script to run the the contour plotting program 
$! PSI2/88 using the filename.psi2 input file.
$!
$! This version of the program needs GKS to be run.
$!
$! Usage: psi2 filename (no extension)
$! It assumes the input file is named filename.psi2
$!
$! The following line should be in the system startup file
$! or placed in your startup file.
$! It can be uncommented here as well.
$! Replace with the disk and directory where the PSI/88 program will reside.
$!
$! define psidir dua0:[psi88]
$!
$! Check for input data
$!
$ if p2.eqs."" then goto noarg2
$ if p1.eqs."" then goto noargs
$ if p3.nes."" then set def 'p3'
$!
$! Define input files
$!
$ assign 'p1'.psi2  for005
$ assign psitmp:'p1'.f22   for022
$ assign psitmp:'p1'.f23   for023
$ assign psitmp:'p1'.f24   for024
$!
$! Define the gks symbols for the output device desired.
$! If you use another plotting package, you may need something
$! else here.
$!
$! Define an output device type in gks$wstype according to the GKS manual
$!
$ define gks$CONID psitmp:'p1'.gks
$ define gks$wstype 'p2'
$!
$! Run the program
$!
$ run psidir:psi2
$ sho status
$ deass for005
$ deass for022
$ deass for023
$ deass for024
$ if p2.eqs."53" then plot psitmp:'p1'.gks
$ if p2.eqs."38" then print psitmp:'p1'.gks
$ goto exit
$ noarg2:
$ write sys$output " "
$ write sys$output "You must supply the gks workstation type for the"
$ write sys$output "desired output device.  See the GKS documentation"
$ write sys$output " "
$ noargs:
$ write sys$output "Usage: psi2 filename (no extension) wstypenumber."
$ exit:
$ exit
