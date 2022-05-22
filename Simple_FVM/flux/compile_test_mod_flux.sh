#!/bin/bash
ParentDirectory=$(cd ../../ && pwd)

ConfigSrc="$ParentDirectory"/Simple_FVM/config.f95

gfortran -c $ConfigSrc
gfortran -c mod_flux.f95
gfortran -c test_mod_flux.f95
gfortran test_mod_flux.o mod_flux.o config.o -o ./test_mod_flux
rm *.o
rm *.mod
