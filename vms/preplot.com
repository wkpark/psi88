$!
$! DCL script to run the PREPLOT program to convert
$! a MOPAC .gpt file produced with the graph keyword to
$! a set of PSI/88 input files.
$!
$! Usage: preplot filename (no extension)
$! It assumes the graph file is named filename.gpt
$!
$ if p1.eqs."" then goto noargs
$!
$ assign/nolog 'p1'.gpt for013
$ assign/nolog sys$command sys$input
$!
$! assign the output files
$!
$ assign/nolog 'p1'.psi1   for008
$ assign/nolog 'p1'.psicon for009
$ assign/nolog 'p1'.psi2   for010
$!
$! The following line should be in the system startup file
$! or placed in your startup file.
$! It can be uncommented here as well.
$! Replace with the disk and directory where the PSI/88 program will reside.
$!
$! define psidir dua0:[psi88]
$!
$ run psidir:preplot
$!
$ deass sys$input
$ deass for008
$ deass for009
$ deass for010
$ deass for013
$ write sys$output "PSI/88 input files have been generated."
$ goto exit
$ noargs:
$ write sys$output "Usage: preplot filename (no extension)."
$ exit:
$ exit
