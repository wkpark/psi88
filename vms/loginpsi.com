$!
$! The next line may be deleted from the user's login.com
$! if it has been installed in the system startup file.
$! When installing in the system startup file, specify the /system 
$! qualifier on the line.
$!
$ define psidir  dua0:[psi88]
$ define psique  sys$fast
$ define psitmp  dua1:[scratch]
$!
$! Copy these lines to the user's login.com file, as well as any others
$! that you may have added.  Optionally they can be placed in the 
$! sys$manager:sylogin.com file.
$!
$ psi1       :== @psidir:psi1
$ psicon     :== @psidir:psicon
$ psi2       :== @psidir:psi2
$ psi2ps     :== @psidir:psi2ps
$ psi2hp     :== @psidir:psi2hp
$ psi2ct     :== @psidir:psi2ct
$ psi2tk41   :== @psidir:psi2tk41
$ preplot    :== @psidir:preplot
$ rpsi1      :== @psidir:rpsi1
$ rpsicon    :== @psidir:rpsicon
$ rpsi2hp    :== @psidir:rpsi2hp
$ rpsi2ct    :== @psidir:rpsi2ct
$ rpsi2ps    :== @psidir:rpsi2ps
$ rpsi2      :== @psidir:rpsi2
$ exit
