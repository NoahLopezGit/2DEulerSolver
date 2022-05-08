gfortran -J./main_subdir -c cell_geometry.f95 -o ./main_subdir/cell_geometry.o
gfortran -I./main_subdir -c main.f95 -o ./main_subdir/main.o
gfortran ./main_subdir/cell_geometry.o ./main_subdir/main.o -o run_main
