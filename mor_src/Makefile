########################################################################
#       Makefile for Topdrawer on a generic Unix system.
#
#       make                    # make everything
#
#   This product uses :
#      mortran - for compilaton
#
#   Change *IRIX to *AIX/*IRIX/*OSF1/*SunOS/*ULTRIX at the end of the
#   Installation Parameters section
#
########################################################################

########## Installation parameters #####################################

SHELL = /bin/csh#                Use C shell per ISAJET Makefile

#       Fortran with options.
F77AIX     =  f77  -NQ20000  -qextname -qrndsngl -qcharlen=2048
F77IRIX    =  f77  -O0   -nocpp -backslash -g1 -trapuv -static
F77OSF1    =  f77  -O4  -nocpp -assume backslash
F77SunOS   =  f77  -O1  -silent -Nl99  -libmil -xl
F77ULTRIX  =  f77  -O1  -w  -Olimit 1500 -nocpp -static -assume backslash

FFLAGS     =

#       ar s qualifier, only ULTRIX differs
ARSAIX     =
ARSIRIX    =
ARSOSF1    =
ARSSunOS   =
ARSULTRIX  = s

#       Ranlib command (if required, only on SUN)
RANAIX     =
RANIRIX    =
RANOSF1    =
RANSunOS   = ranlib ../td.a
RANULTRIX  =

#       SYSTEM DEPENDENT CHANGES     #

# AIX - t2seth4.mor, token.mor - change '\' to '\\'

F77 = $(F77OSF1)
ARS = $(ARSOSF1)
RAN = $(RANOSF1)

#--------- End of installation parameters ------------------------------

.SUFFIXES: .mor

MORT      = $(MORTRAN_DIR)/mortran

########## Make everything #############################################

#all:  td.a

########## Mortran #####################################################

.mor.o:;\
  $(MORT)             $*     ;\
  rm                  $*.mst ;\
  $(F77) -c           $*.f   ;\
  ar rv$(ARS) ../td.a $*.o   ;\
  $(RAN)

td.a: \
hb_search_id.o \
help.o     \
smctrl.o   \
t2axes.o   \
t2cntr.o   \
t2del.o    \
t2fft.o    \
t2fill.o   \
t2fit.o    \
t2main.o   \
t2mesh.o   \
t2plot.o   \
t2savd.o   \
t2set.o    \
t2setc.o   \
t2seth4.o  \
t2show.o   \
t2smooth.o \
t2spline.o \
t2tabl.o   \
t2timf.o   \
t2xfrm.o   \
td.o       \
tdend.o    \
tdtext.o   \
token.o    \
txline.o   \
txxug77.o  \
; \
   ls -lF  ../td.a
