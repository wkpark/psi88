$if p1.eqs."" then goto noargs
$if p2.eqs."" then goto noarg2
$defdir = f$environment("DEFAULT")
$submit /noprint/notify psidir:rpsi2.com /param=('p1','p2','defdir') -
  /que=psique/log='defdir'psi2.log
$goto exit
$noarg:
$write sys$output "Usage: psi2 filename (no extension) gksdevnumber."
$noarg2:
$write sys$output "You must supply the number for the output device."
$exit:
$exit
