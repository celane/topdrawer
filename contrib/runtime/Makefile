FC = g77
FFLAGS = -O2

TDR = /home/common/topdrawer/td_rtl.a /home/common/topdrawer/misc.a
UGS = /home/common/ugs/ugs.a
FORLIB = /home/common/forlib/*.a
XLIB = -L/usr/X11/lib -lXt -lX11


LIB = $(TDR) $(UGS) $(FORLIB) $(XLIB)

all: test05
# all: test05 test04 test03 test02 test01

.f:
	$(FC) -o $@ $< $(LIB)

clean:
	rm *.o
