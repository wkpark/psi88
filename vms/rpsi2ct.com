$!
$! DCL script to run the the contour plotting program 
$! PSI2/88 using the filename.psi2 input file.
$!
$! Usage: psi2ct filename (no extension)
$! It assumes the input file is named filename.psi2
$!
$! The following line should be in the system startup file
$! or placed in your startup file.
$! It can be uncommented here as well.
$! Replace with the disk and directory where the PSI/88 program will reside.
$!
$! define psidir dua0:[psi88]
$!
$! Check for no arguments
$!
$ if p1.eqs."" then goto noargs
$ if p2.nes."" then set def 'p2'
$!
$! Define input files
$!
$ assign 'p1'.psi2  for005
$ assign psitmp:'p1'.f22   for022
$ assign psitmp:'p1'.f23   for023
$ assign psitmp:'p1'.f24   for024
$!
$! Define output file for the ChemText Metafile
$!
$ assign psitmp:'p1'.met for010
$!
$! Run the program
$!
$ sho status
$ run psidir:psi2ct.exe
$ sho status
$ deass for005
$ deass for010
$ deass for021
$ deass for022
$ deass for023
$ deass for024
$ goto exit
$ noargs:
$ write sys$output "Usage: psi2ct filename (no extension)."
$ exit:
$ exit