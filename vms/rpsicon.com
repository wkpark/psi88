$!
$! DCL script to run the the contour generation program 
$! PSICON/88 using the filename.psicon input file.
$!
$! Usage: psicon filename (no extension)
$! It assumes the input file is named filename.psicon
$!
$ if p2.nes."" then set def 'p2'
$ if p1.eqs."" then goto noargs
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
$ assign 'p1'.psicon for005
$ assign psitmp:'p1'.f17 for017
$!
$! assign the output files
$!
$ assign psitmp:'p1'.f22 for022
$ assign psitmp:'p1'.f23 for023
$ assign psitmp:'p1'.f24 for024
$!
$! Run the program
$!
$ run psidir:psicon
$ sho status
$ deass for005
$ deass for017
$ deass for022
$ deass for023
$ deass for024
$ goto exit
$ noargs:
$ write sys$output "Usage: psicon filename (no extension)."
$ exit:
$ exit
