#!/bin/bash

gfortran -c mod_flux.f95
gfortran -c test_mod_flux.f95
gfortran test_mod_flux.o mod_flux.o -o ./test_mod_flux
rm *.o
rm *.mod
