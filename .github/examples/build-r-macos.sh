#!/bin/sh
# This is the quick build.  Use full build to update everything.
# brew install gcc gettext gmp isl jpeg libmpc libpng mpfr pcre2 pkg-config readline xz texinfo wget

set -e # stop on error

# Set some variables, not all strictly needed.
export FC="/usr/local/bin/gfortran"
export PATH="/Library/TeX/texbin:/usr/local/opt/texinfo/bin:$PATH"
export PKG_CONFIG_PATH="/opt/X11/lib/pkgconfig"
export R_CRAN_WEB="https://cran.rstudio.com"
export CRAN_RSYNC="mirrors.nic.cz::CRAN"
export R_TEXI2DVICMD="emulation"

# Prep source code
cd ~/R/R-devel-src
sed -i.bak 's|$(GIT) svn info|./.github/workflows/svn-info.sh|' Makefile.in

# Maybe update recommended packages
# ./.github/workflows/wget-recommended.sh
./.github/workflows/svn-info.sh

# (Maybe) configure and build
export PDFLATEX="${PWD}/.github/workflows/dummy"
cd ../R-devel
# CC=clang ../R-devel-src/configure --disable-java --without-cairo --without-tcltk --without-x --with-aqua --with-lapack --with-blas --enable-R-shlib SED=/usr/bin/sed
make all -j8

# Run all checks (takes long)
# make check-all