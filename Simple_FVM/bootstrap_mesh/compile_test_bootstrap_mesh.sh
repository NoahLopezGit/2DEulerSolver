ConfigSrc=/home/frosty/projects/2DEulerSolver/Simple_FVM/config.f95
ModReadGridSrc=/home/frosty/projects/2DEulerSolver/grid_generation/read_grid/mod_read_grid.f95
ModCellDataStrucSrc=/home/frosty/projects/2DEulerSolver/Simple_FVM/cell_data_struc/mod_cell_data_struc.f95
ModCellGeometrySrc=/home/frosty/projects/2DEulerSolver/Simple_FVM/cell_geometry/mod_cell_geometry.f95

gfortran -c $ConfigSrc
gfortran -c $ModReadGridSrc
gfortran -c $ModCellGeometrySrc
gfortran -c $ModCellDataStrucSrc
gfortran -c ./bootstrap_mesh.f95
gfortran -c ./test_bootstrap_mesh.f95
gfortran test_bootstrap_mesh.o bootstrap_mesh.o mod_cell_data_struc.o mod_read_grid.o mod_cell_geometry.o config.o -o test_bootstrap_mesh
rm *.o
rm *.mod
