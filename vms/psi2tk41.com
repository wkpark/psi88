$!
$! DCL script to run the the contour plotting program 
$! PSI2/88 using the filename.psi2 input file.
$!
$! This version of the program needs GKS to be run.
$!
$! Usage: psi2tk41 filename (no extension)
$! It assumes the input file is named filename.psi2
$!
$! The following line should be in the system startup file
$! or placed in your startup file.
$! It can be uncommented here as well.
$! Replace with the disk and directory where the PSI/88 program will reside.
$!
$! define psidir dua0:[psi88]
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
$! These lines are for an attached Tektronix 4107 terminal
$!
$ define gks$CONID tt:
$ define gks$WSTYPE 80
$!
$! Run the program
$!
$ run psidir:psi2
$ sho status
$ deass for005
$ deass for022
$ deass for023
$ deass for024
$ goto exit
$ noargs:
$ write sys$output "Usage: psi2tk41 filename (no extension)."
$ exit:
$ exit
