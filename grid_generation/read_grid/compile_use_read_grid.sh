#!/bin/bash

gfortran -c ./module_read_grid.f95 -o ./module_read_grid.o
gfortran -c ./use_read_grid.f95 -o ./use_read_grid.o
gfortran ./module_read_grid.o ./use_read_grid.o -o ./use_read_grid
rm module_read_grid.o
rm use_read_grid.o
rm module_read_grid.mod
