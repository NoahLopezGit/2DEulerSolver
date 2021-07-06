program main
  use readGrid !module containing loading grid
  use CellType !module containing cell data type
  use update
  use variables !module containing used variables
  use dissipation_mod
  
  implicit none

  
  !printing configuration
  P0 = rho0*Rc*T0 !presure calculation of state
  print *, "2D Euler Solver Configuration - FrostyNips"
  print *, " "
  print *, "Initial Conditions"
  print *, "Density (kg/m^3): ",rho0
  print *, "velocities (u and v) (m/s): ",u0," ",v0
  print *, "Pressure (Pa): ",P0
  print *, "Temperature (k): ",T0
  print *, " " 
  print *, "Inlet Conditions"
  print *, "Pressure (Pa): ",P_i
  print *, "Temperature (K): ",T_i
  print *, " "
  print *, "Outlet Conditions"
  print *, "Pressure (Pa): ",P_out
  print *, " "
  
  !getting laplace grid data
  grid = readnstore()
  dimi = size(grid, 1)
  dimj = size(grid, 2)

  !allocations
  allocate(Cell_Mat(dimi+3,dimj+3)) !allocate space for all cells and 4 ghost cells in each dim
  !allocate(dissipation_mat(timesteps,4,dimi-1,dimj-1)) !allocate for dissipation matrix

  
  !set initial conditions over whole domain
  E0 = (P0)/((gamma-1.0)*rho0) + 0.5*(u0**2.0 + v0**2.0) !energy calculation
  do i=1,dimi+3
     do j=1,dimj+3
        Cell_Mat(i,j)%states(:) = (/rho0,rho0*u0,rho0*v0,rho0*E0/)
     end do
  end do

  
  !building cell nodes
  do i = 3,dimi+1
     do j = 3,dimj+1
        ![BL,BR,TR,TL] order
        !converting to grid coordinates
        r = i-2 
        s = j-2
        Cell_Mat(i,j)%nodes(:,1) = (/grid(r,s,1), grid(r+1,s,1), grid(r+1,s+1,1), grid(r,s+1,1)/)
        Cell_Mat(i,j)%nodes(:,2) = (/grid(r,s,2), grid(r+1,s,2), grid(r+1,s+1,2), grid(r,s+1,2)/)
     end do
  end do

  
  !solver
  do step=1,timesteps !time steps
     do i=3,dimi+1
        do j=3,dimj+1
           
           A = get_area(Cell_Mat(i,j)%nodes(:,:))
           
           !boundary conditions with ghost cells
           !walls
           if (j==3) then !bottom wall
              !set ghost cells to reflected velocity
              Cell_Mat(i,j-1)%states(:) = get_reflect(Cell_Mat(i,j)%states(:),&
                   Cell_Mat(i,j)%nodes(:,:),0)
              Cell_Mat(i,j-2)%states(:) = get_reflect(Cell_Mat(i,j+1)%states(:),&
                   Cell_Mat(i,j)%nodes(:,:),0)
           else if (j==dimj+1) then !top wall
              Cell_Mat(i,j+1)%states(:) = get_reflect(Cell_Mat(i,j)%states(:),&
                   Cell_Mat(i,j)%nodes(:,:),1)
              Cell_Mat(i,j+2)%states(:) = get_reflect(Cell_Mat(i,j-1)%states(:),&
                   Cell_Mat(i,j)%nodes(:,:),1)
           end if
           !inlet
           if (i==3) then
              !get internal values
              rho_in = Cell_Mat(i,j)%states(1)
              u_in = Cell_Mat(i,j)%states(2)/rho_in
              v_in = Cell_Mat(i,j)%states(3)/rho_in
              !calculate boundary values
              T_bc = T_i - (u_in**2.0 + v_in **2.0)/(2.0*Cp)
              P_bc = P_i*(T_bc/T_i)**(gamma/(gamma - 1.0))
              u_bc = u_in 
              v_bc = v_in
              rho_bc = P_bc/(Rc*T_bc)
              rhoE_bc = P_bc/(gamma - 1.0) + 0.5*rho_bc*(u_bc**2.0 + v_bc**2.0)
              !set bondary states
              Cell_Mat(i-1,j)%states(:) = (/rho_bc,rho_bc*u_bc,rho_bc*v_bc,rhoE_bc/)
              Cell_Mat(i-2,j)%states(:) = (/rho_bc,rho_bc*u_bc,rho_bc*v_bc,rhoE_bc/)
           end if
           
           !outlet 
           if(i==dimi+1) then
              !getting internal values
              rho_in = Cell_Mat(i+1,j)%states(1) 
              u_in = Cell_Mat(i+1,j)%states(2)/rho_in
              v_in = Cell_Mat(i+1,j)%states(3)/rho_in
              !calculate bounary values
              u_bc = u_in
              v_bc = v_in
              rho_bc = rho_in
              rhoE_bc = P_out/(gamma - 1.0) + 0.5*rho_bc*(u_in**2.0+v_in**2.0)
              !set boundary states
              Cell_Mat(i+1,j)%states(:) = (/rho_bc,rho_bc*u_bc,rho_bc*v_bc,rhoE_bc/)
              Cell_Mat(i+2,j)%states(:) = (/rho_bc,rho_bc*u_bc,rho_bc*v_bc,rhoE_bc/)
           end if
           

           !update
           do iter=1,3
              CellVect_H(:,iter) = Cell_Mat(i+iter-2,j)%states(:)
              CellVect_V(:,iter) = Cell_Mat(i,j+iter-2)%states(:)
           end do
           Cell_Temp = Cell_Mat(i,j)%states(:)

           !dissipation not work atm
           dissipation(:) = 0.0 !get_dissipation(Cell_Mat(i-2:i+2,j-2:j+2))/dt
           !dissipation_mat(step,:,i-2,j-2) = dissipation(:)
           
           !Runge Kutta timestepping
           do iter=1,4
              CellVect_H(:,2) = Cell_Mat(i,j)%states(:)
              CellVect_V(:,2) = Cell_Mat(i,j)%states(:)
              stenH = stenflux(CellVect_H)
              stenV = stenflux(CellVect_V)

              residual = get_residual(stenH,stenV,Cell_Mat(i,j)%nodes(:,:))

              Cell_Mat(i,j)%states(:) = Cell_Temp(:) - alpha(iter)*dt/A*(residual(:) - dissipation(:))
           end do

        end do
     end do
  end do

  
  !writing dissipation to file (over iterations)
  !open(4,file="dissipation.txt",status="replace")
  !do step=1,timesteps !over ten steps
  !   write(4, *)  "     "
  !   write(4, *)  "Time Step = ", step
  !   write(4, *)  "     "
  !   do iter=1,4 !4 variables
  !      do j=1,dimj-1
  !         write(4,"(104F10.3)") dissipation_mat(step,iter,:,dimj-j)
  !         !print *, dissipation_mat(step,iter,:,dimj+3-j+1)
  !      end do
  !      write(4, *)  "     " 
  !   end do
  !end do
  !close(4)

  
  !writing final cells state
  open(3,file="results.txt",status="replace")
  do i=1,2
     do j=1,dimj+3
        write(3,"(104F10.3)") Cell_Mat(:,dimj+3-j+1)%nodes(1,i)
     end do
     write(3,*) "  "
  end do
  
  do i=1,4
     do j=1,dimj+3!all cells including ghost cells
        write(3,"(104F15.3)") Cell_Mat(:,dimj+3-j+1)%states(i)
     end do
     write(3,*) "    "
  end do
  close(3)
  
  deallocate(Cell_Mat)
  !deallocate(dissipation_mat)
  
end program main
