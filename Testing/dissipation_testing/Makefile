TARGET  = dissipation_testing
MODULES = CellType.f95 update.f95 dissipation_mod.f95
MODOUT  = CellType.o update.f95 dissipation_mod.o
OBJ     = $(MODOUT) dissipation_testing.o

$(TARGET): $(OBJ)
	    gfortran -o dissipation_testing $(OBJ)

dissipation_testing.o:	    dissipation_testing.f95
	    gfortran -c dissipation_testing.f95

$(MODOUT): $(MODULES)
	    gfortran -c $(MODULES)

clean:
	rm *.o
