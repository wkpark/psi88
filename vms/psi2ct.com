$defdir = f$environment("DEFAULT")
$submit /noprint/notify psidir:rpsi2ct.com /param=('p1','defdir') -
   /que=psique/log='defdir'psi2ct.log
