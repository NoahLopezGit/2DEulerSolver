# 2DEulerSolver
This is a project to numerically solve the euler equations over a channel with a bump using jameson scheme finite volume analysis. The program is written in fortran as many CFD solvers are supercomputers which commonly use fortran. 

To install project: $ git clone https://github.com/FrostyNip/2DEulerSolver

Note: this project is not finished... It will give a solution but not the best solution.
I need to verify if my implementation is correct now. It seems to be giving correct results

Going to clean up the project to make the structure more clear. Also going to go back and fix dissipation scheme so the solver is stable.

To build the executable:
1. open terminal in 2DEulerSolver ("root") directory
2. type and enter : $ make
3. type and enter : $ make clean 
    will clean up extra files
4. type and enter : $ ./main 
    will run the main file
