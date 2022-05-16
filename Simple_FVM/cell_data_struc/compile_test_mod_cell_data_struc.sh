gfortran -c ./mod_cell_data_struc.f95
gfortran -c ./test_mod_cell_data_struc.f95
gfortran ./test_mod_cell_data_struc.o ./mod_cell_data_struc.o -o test_mod_cell_data_struc
rm *.o
rm *.mod
