$defdir = f$environment("DEFAULT")
$submit /noprint/notify psidir:rpsi1.com /param=('p1','defdir') -
   /que=psique/log='defdir'psi1.log
