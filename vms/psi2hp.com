$defdir = f$environment("DEFAULT")
$submit /noprint/notify psidir:rpsi2hp.com /param=('p1','defdir') -
   /que=psique/log='defdir'psi2hp.log
