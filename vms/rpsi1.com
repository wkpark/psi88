$ set ver
$!
$! DCL script to run the PSI1/88 program using the input data
$! in filename.psi1.  
$!
$! When finished it runs the contour generation program 
$! PSICON/88 using the filename.psicon input file.
$!
$! Usage: psi1 filename (no extension)
$! It assumes the input file is named filename.psi1
$!
$ if p1.eqs."" then goto noargs
$ if p2.nes."" then set def 'p2'
$ assign 'p1'.psi1 for005
$!
$! assign the output files
$!
$ assign psitmp:'p1'.f17  for017
$!
$! The following line should be in the system startup file
$! or placed in your startup file.
$! It can be uncommented here as well.
$! Replace with the disk and directory where the PSI/88 program will reside.
$!
$! define psidir dua0:[psi88]
$!
$ sho status
$ run psidir:psi1
$ sho status
$!
$! PSI1/88 finished, now run the contouring program.
$!
$! Assign the new input files (besides the output from the PSI1/88
$! which is still assigned).
$!
$ assign 'p1'.psicon for005
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
$ write sys$output "Usage: psi1 filename (no extension)."
$ exit:
$ exit
