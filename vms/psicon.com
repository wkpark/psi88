$defdir = f$environment("DEFAULT")
$submit /noprint/notify psidir:rpsicon.com /param=('p1','defdir') -
    /que=psique/log='defdir'psicon.log
