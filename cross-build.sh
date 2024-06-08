#!/bin/bash

export TARGET=x86_64-w64-mingw32
export AR=$TARGET-ar
export AS=$TARGET-as
export CC=$TARGET-gcc
export CXX=$TARGET-g++
export LD=$TARGET-ld
export STRIP=$TARGET-strip
export NM=$TARGET-nm
export RANLIB=$TARGET-ranlib
export OBJDUMP=$TARGET-objdump

# Set GHC to use the cross-compiler
export GHC_CC=$CC
export GHC_LD=$LD

# Ensure MinGW is in the path
export PATH=/usr/lib/ghc/bin:/usr/lib/cabal/bin:$PATH

# Build the project without static linking
cabal build --builddir=dist-windows --ghc-options="-optl=-no-pie"
