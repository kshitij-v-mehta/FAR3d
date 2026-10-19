#!/bin/bash
# Builds init_value_plotter (FAR3d restart .bp -> 2D Silo/VisIt files).
#   SILO_DIR=/path/to/silo-4.10.2 ./build_plotter.sh [output-binary]
# Needs: gfortran, ADIOS2 (Fortran bindings, serial lib), Silo (with Fortran interface;
# the plotter writes Silo's PDB format, so Silo does not need HDF5).
#   SILO_DIR   Silo prefix: include/silo.inc and lib/libsilo      (required)
#   ADIOS2_DIR ADIOS2 prefix                       (default /Users/kpu/vshare/sw/adios/ADIOS2-2.12.1/installation)
#   SILO_LIBS  extra link flags if your Silo build needs them (e.g. "-lhdf5 -lz")
set -e
HERE=$(cd "$(dirname "$0")" && pwd)
OUT=${1:-$HERE/xplot_init_value}
ADIOS2_DIR=${ADIOS2_DIR:-/Users/kpu/vshare/sw/adios/ADIOS2-2.12.1/installation}
[ -n "$SILO_DIR" ] || { echo "set SILO_DIR to your Silo install prefix"; exit 1; }
B=$(mktemp -d)
cd "$B"
# Silo >= 4.11 ships a free-form include (silo_f9x.inc); older versions only the fixed-form silo.inc
if [ -f "$SILO_DIR/include/silo_f9x.inc" ]; then echo 'include "silo_f9x.inc"' > silo_include.inc
elif [ -f "$SILO_DIR/include/silo.inc" ]; then echo 'include "silo.inc"' > silo_include.inc
else echo "no silo.inc / silo_f9x.inc under $SILO_DIR/include (Silo built without Fortran?)"; exit 1; fi
gfortran -O -ffree-line-length-none -fallow-argument-mismatch \
    -I. -I"$SILO_DIR/include" -I"$ADIOS2_DIR/include/adios2/fortran" \
    -o "$OUT" "$HERE/bp_read_mod.f90" "$HERE/init_value_plotter.f90" \
    -L"$SILO_DIR/lib" -lsilo $SILO_LIBS \
    -L"$ADIOS2_DIR/lib" -ladios2_fortran -Wl,-rpath,"$ADIOS2_DIR/lib"
rm -rf "$B"
echo "built $OUT"
