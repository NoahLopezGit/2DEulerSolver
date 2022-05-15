#!/bin/bash

gfortran -c ./mod_read_grid.f95 -o ./mod_read_grid.o
gfortran -c ./use_read_grid.f95 -o ./use_read_grid.o
gfortran ./mod_read_grid.o ./use_read_grid.o -o ./use_read_grid
rm mod_read_grid.o
rm use_read_grid.o
rm mod_read_grid.mod
