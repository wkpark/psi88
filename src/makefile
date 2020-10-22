#
# This is a makefile for the psi/88 program.  Uncomment the Optimization
#      flags for your particular machine below.
#
# It appears to be somewhat machine dependant. If you have any trouble
# You may reach me by E-mail at:
#
#      dan@rani.chem.yale.edu  (internet)
#
#  USnail:
#      Dan Severance
#      Chemistry Dept.
#      P.O. Box 6666
#      Yale University
#      225 Prospect St.
#      New Haven, CT
#      06511
#
#  Voice:(203) 432-6288 (lab)  or (203) 498-7287 (home)
#
#  FAX:  (203) 432-6144
#
#########################################################################
SYSTEM := $(shell uname)
#
# You may add other optimization flags specific to your machine here
# It is always a good idea to test the program with a lower optimization 
# level and when it is working, increase the optimization level.  So far
# optimization has never caused problems with this program.
#
# SGI 4D series flags
#
ifeq ($(SYSTEM),IRIX)
 FC = f77
 FFLAGS = -w0 -O4 -G 64
 LFLAGS = -lfastm -lfpe -s
endif
#
# SUN Sparc series flags version 1.3 or later compiler
#
ifeq ($(SYSTEM),SunOS)
 FC = f77
 FFLAGS = -fast -O3
 LDFLAGS = -Bstatic
endif
#
# SUN Sparc series flags version 1.2 compiler
#
#FC = f77
#FFLAGS = -dalign -O3 /usr/lib/libm.il
#LDFLAGS =
#
# CRAY Unicos compilation flags
#
#FC = cf77
#FFLAGS = -Zv -Wf" -a static -m 0 -d p"
#LFLAGS = -Wl" -lnet" 
#
# IBM RS/6000 Series compilation flags (tentative)
#
#FC = xlf
#FFLAGS = -O 
#LDFLAGS = 
#
# Linux
#
ifeq ($(SYSTEM),Linux)
 FC = f77
 FFLAGS = -std=legacy -g
 LDFLAGS =
 GKSLIB = -lxgks
endif
#
#  Place your G88/G90 util library definition here if you wish to 
#  compile chk2psi
#
#G90LIB = /usr/people/frisch/g90/libg90.a
#
# define the directory where the executables will reside
# usually /usr/local/bin or ~/bin to put it in your own directory
#
BIN = /usr/local/bin
#  
#**** Add PSI2 to this and the install lists if you have a user defined
#     Plotting library to plot to the screen, also remove any which you 
#     do not have available.
#
#**** Add PSI2GKS and define GKSLIB if you have GKS installed or a PHIGS
#     GKS Compatibility Option.
#
#GKSLIB = 
#
# User defined library for the screen
#GRLIBS =
#
EXTRAS = PSI2GKS
# EXTRAS = PSI2 PSI2GKS
#######################################################################
#

 all:  PSI1 PSICON PSI2CT PSI2HP PSI2PS PREPLOT psi1cube $(EXTRAS)

#
# Install in the executable directory - may need to be superuser
# depending on where you decide to put it (/usr/local/bin for instance)
#
install: PSI1 PSICON PSI2CT PSI2HP PSI2PS PREPLOT chk2psi $(EXTRAS)
	mv PSI1 $(BIN)
	chmod +rx $(BIN)/PSI1
	mv PSICON $(BIN)
	chmod +rx $(BIN)/PSICON
	mv PREPLOT $(BIN)
	chmod +rx $(BIN)/PREPLOT
	mv PSI2* $(BIN)
	chmod +rx $(BIN)/PSI2*
	mv chk2psi $(BIN)
	chmod +rx $(BIN)/chk2psi

#
# This is the Density matrix generation program (PSI1/88)
#
 PSI1: psi1.f 
	$(FC) $(FFLAGS) psi1.f -o PSI1 $(LFLAGS)
#
# This is the Cube file generation program
#
 psi1cube: psi1cube.f
	$(FC) $(FFLAGS) psi1cube.f -o psi1cube

#
# This is the contour generation program (PSICON/88)
# 
 PSICON: psicon.f
	$(FC) $(FFLAGS) psicon.f -o PSICON $(LFLAGS)

#
# This is for the plotting routine (PSI2/88) to a user defined graphics lib
#
 PSI2: psi2.f
	$(FC) $(FFLAGS) psi2.f -o PSI2 $(GRLIBS) $(LFLAGS)

#
#  This is for the plotting routine set up to plot to a GKS device
#  gksplot.f is a gks library that SHOULD work on any machine.....
#
 PSI2GKS: psi2.f gksplot.f
	$(FC) $(FFLAGS) psi2.f gksplot.f -o PSI2GKS $(LFLAGS) $(GKSLIB)

#
#  This is for the plotting routine set up to plot to a ChemText Metafile
#
 PSI2CT: psi2.f ctplot.f
	$(FC) $(FFLAGS) psi2.f ctplot.f -o PSI2CT $(LFLAGS)

#
#  This is for the plotting routine set up to plot to a PostScript device
#  psplot.f is a postscript library that SHOULD work on any machine.....
#
 PSI2PS: psi2.f psplot.f
	$(FC) $(FFLAGS) psi2.f psplot.f -o PSI2PS $(LFLAGS)

#
#  This is for the plotting routine set up to plot to a HP plotter
#  hpplot.f is a HP plotter library that SHOULD work on any machine.....
#
 PSI2HP: psi2.f hpplot.f
	$(FC) $(FFLAGS) psi2.f hpplot.f -o PSI2HP $(LFLAGS)

#
# This is a program to convert MOPAC .GPT files (using the graph keyword)
# to PSI/88 input files.
#
 PREPLOT: preplot.f 
	$(FC) $(FFLAGS) preplot.f -o PREPLOT $(LFLAGS)

#
# This is a program to read checkpoint files - 
#
 chk2psi: chk2psi.f $(G90LIB)
	$(FC) $(FFLAGS) chk2psi.f $(G90LIB) -o chk2psi $(LFLAGS)

#
# Clean up when done
#
 clean :
	touch a.out foo.o core foo.u foo.f17
	rm -f a.out *.o core *.u *.f17 *.f2*
	rm -f PSI1 PSICON PSI2CT PSI2HP PSI2PS PREPLOT chk2psi
