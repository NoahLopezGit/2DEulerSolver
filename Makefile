TARGET  = main
MODULES = readGrid.f95 CellType.f95 update.f95 variables.f95
MODOUT  = readGrid.o CellType.o update.o variables.o
OBJ     = $(MODOUT) main.o

$(TARGET): $(OBJ)
	    gfortran -o main $(OBJ)

main.o:	    main.f95
	    gfortran -c main.f95

$(MODOUT): $(MODULES)
	    gfortran -c $(MODULES)

clean:
	rm *.o
