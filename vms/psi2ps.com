$defdir = f$environment("DEFAULT")
$submit /noprint/notify psidir:rpsi2ps.com /param=('p1','defdir') -
   /que=psique/log='defdir'psi2ps.log
