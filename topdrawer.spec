Summary: TopDrawer graphing and fitting program
Name: topdrawer
Version: 5.12
Release: 7%{?dist}
License: GPL
Group: Graphics/Utilities
Source: ftp://iris.riken.go.jp/pub/topdrawer/topdrawer-all.tar.gz
Source1: ftp://iris.riken.go.jp/pub/ugs/ugs.tar.gz
Source2: topdrawer-update.tar.gz
BuildRoot: %{_tmppath}/%{name}-%{version}-root
URL: http://duphy4.physics.drexel.edu/utils/topdrawer.html
Patch0: topdrawer-5.12-rpm.patch
Patch1: ugs-2.10d-rpm.patch
Patch2: topdrawer-5.12-fitfix.patch
Patch3: topdrawer-5.12-fix200502.patch
Patch4: ugs-xwin.patch
Patch5: topdrawer-gcc3.patch
Patch6: miscfix.patch
Patch7: ugs-hollorith.patch
Patch8: ugs-loc.patch
Patch9: td-char.patch
Patch10: kill_warnings.patch
Patch11: topdrawer-5.12-fix201403.patch
Prefix: /usr


%description

TopDrawer graphing and fitting program

%changelog

* Tue Feb 27 2007 Charles Lane <lane@duphy4.physics.drexel.edu>
- fix xwin color code
- fix g77 .F include
- usual mods to use system include files

* Tue Feb 15 2005 Charles Lane <lane@duphy4.physics.drexel.edu>
- fix fitting stuff

* Wed Feb 18 2004 Charles Lane <lane@duphy4.physics.drexel.edu>
- create spec file



%prep
%setup -n topdrawer
%setup -T -n ugs -b 1
%setup -D -T -n topdrawer -b 2
cd $RPM_BUILD_DIR
%patch -P 0 -p0
%patch -P 1 -p0
%patch -P 2 -p0
%patch -P 3 -p0
%patch -P 4 -p0
%patch -P 5 -p0
%patch -P 6 -p0
%patch -P 7 -p0
%patch -P 8 -p0
%patch -P 9 -p0
%patch -P 10 -p0
%patch -P 11 -p0

%build
cd ../ugs
xmkmf
make Makefiles
make clean
make all FC='gfortran -std=legacy -fallow-invalid-boz'
cd ../topdrawer
xmkmf
make Makefiles
make clean
make all FC='gfortran -std=legacy -fallow-invalid-boz'


%install
[ "$RPM_BUILD_ROOT" != "/" ] && rm -rf $RPM_BUILD_ROOT
docdir="$RPM_BUILD_ROOT/usr/share/doc/topdrawer"
install -D td $RPM_BUILD_ROOT/usr/bin/td
install -D doc/topdrawer4.0.gih $docdir/topdrawer.gih
install -d $docdir/html/case
install -D doc/html/* $docdir/html/
install -D doc/case/* $docdir/html/case/
install -d $docdir/examples
install -D examples/* $docdir/examples/
install    README*    $docdir/
install    INSTALL*   $docdir/
install	   HINTS      $docdir/ 
install    contrib/elisp/td.el $docdir/
install    contrib/elisp/README.elisp $docdir/
install	-D doc/td.1   $RPM_BUILD_ROOT/usr/share/man/man1/td.1
%clean
[ "$RPM_BUILD_ROOT" != "/" ] && rm -rf $RPM_BUILD_ROOT

%files
%defattr(-, root, root)
/usr/bin/td
/usr/share/doc/topdrawer/*
/usr/share/man/man1/*


%post
if [ -d /usr/share/emacs/site-lisp ] ; then
   if [ -h /usr/share/emacs/site-lisp/td.el ] ; then
	rm -f /usr/share/emacs/site-lisp/td.el
   fi
   if [ ! -e /usr/share/emacs/site-lisp/td.el ]; then
     ln -s /usr/share/doc/topdrawer/td.el /usr/share/emacs/site-lisp/td.el
   fi
fi

%preun
if [ -h /usr/share/emacs/site-lisp/td.el ] ; then
   rm -f /usr/share/emacs/site-lisp/td.el
fi
