program main
  use readGrid !module containing loading grid
  use CellType !module containing cell data type
  use update
  use variables !module containing used variables
  use dissipation_mod
  
  implicit none

  !config
  timesteps = 1000
  !getting laplace grid data
  
  grid = readnstore()
  dimi = size(grid, 1)
  dimj = size(grid, 2)

  allocate(Cell_Mat(dimi+3,dimj+3)) !allocate space for all cells and 4 ghost cells in each dim
  allocate(dissipation_mat(timesteps,4,dimi-1,dimj-1)) !allocate for dissipation matrix

  !iterate and set zeros for whole domain
  Pa = 100*10**3.0
  RhoA = 1.225
  Ua = 0.3*sqrt(1.4*(Pa/RhoA))
  Va = 0.0
  Ea = (Pa)/(0.4*RhoA) + 0.5*(Ua**2.0 + Va**2.0)
  do i=1,dimi+3
     do j=1,dimj+3
        Cell_Mat(i,j)%states(:) = (/1.225,1.225*0.3*sqrt(1.4*(100*10**3.0/1.225)),0.0,RhoA*Ea/)
     end do
  end do
  
  !building cell matrix
  !iterate over interior cells and set grid points
  do i = 3,dimi+1
     do j = 3,dimj+1
        ![BL,BR,TR,TL] order
        !converting to grid coordinates
        r = i-2 
        s = j-2
        Cell_Mat(i,j)%nodes(:,1) = (/grid(r,s,1), grid(r+1,s,1), grid(r+1,s+1,1), grid(r,s+1,1)/)
        Cell_Mat(i,j)%nodes(:,2) = (/grid(r,s,2), grid(r+1,s,2), grid(r+1,s+1,2), grid(r,s+1,2)/)

        !setting state (initial conditions)
        !Cell_Mat(i,j)%states(:) = (/1.0, 1.0, 1.0, 1.0/) !dummy for now ; Change
     end do
  end do
  
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
           !inlet (TODO)
           !outlet (TODO)
           

           !update
           do iter=1,3
              CellVect_H(:,iter) = Cell_Mat(i+iter-2,j)%states(:)
              CellVect_V(:,iter) = Cell_Mat(i,j+iter-2)%states(:)
           end do
           
           Cell_Temp = Cell_Mat(i,j)%states(:)
           dissipation(:) = get_dissipation(Cell_Mat(i-2:i+2,j-2:j+2))/dt
           dissipation_mat(step,:,i-2,j-2) = dissipation(:)
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
  open(4,file="dissipation.txt",status="replace")
  do step=1,timesteps !over ten steps
     write(4, *)  "     "
     write(4, *)  "Time Step = ", step
     write(4, *)  "     "
     do iter=1,4 !4 variables
        do j=1,dimj-1
           write(4,"(104F10.3)") dissipation_mat(step,iter,:,dimj-j)
           !print *, dissipation_mat(step,iter,:,dimj+3-j+1)
        end do
        write(4, *)  "     " 
     end do
  end do
  close(4)
  !writing the cell states to grid
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
  
end program main
