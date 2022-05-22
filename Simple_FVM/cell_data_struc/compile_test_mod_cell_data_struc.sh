ConfigSrc=/home/frosty/projects/2DEulerSolver/Simple_FVM/config.f95
ModCellGeometrySRC=/home/frosty/projects/2DEulerSolver/Simple_FVM/cell_geometry/mod_cell_geometry.f95

gfortran -c $ConfigSrc
gfortran -c $ModCellGeometrySRC
gfortran -c ./mod_cell_data_struc.f95
gfortran -c ./test_mod_cell_data_struc.f95
gfortran ./test_mod_cell_data_struc.o ./mod_cell_data_struc.o config.o -o test_mod_cell_data_struc
rm *.o
rm *.mod
