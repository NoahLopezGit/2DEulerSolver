program main
  use readGrid !module containing loading grid
  use CellType !module containing cell data type
  use update
  
  implicit none

  integer :: dimi,dimj,i,j,r,s,iter,step
  real, dimension(:,:,:), allocatable     :: grid
  type(Cell), dimension(:,:), allocatable :: Cell_Mat
  real, dimension(4)     :: residual
  real, dimension(2,4,3) :: stenH,stenV
  real, dimension(4,3)   :: CellVect_H, CellVect_V
  real, dimension(4)     :: alpha,Cell_Temp
  real                   :: dt,A,RhoA,Pa,Ua,Va,Ea
  alpha(:) = (/0.25,0.33,0.5,1.0/)
  dt = .000001
  !getting laplace grid data

  grid = readnstore()
  dimi = size(grid, 1)
  dimj = size(grid, 2)

  allocate(Cell_Mat(dimi+3,dimj+3)) !allocate space for all cells and 4 ghost cells in each dim

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
  
  do step=1,500 !time steps
     do i=3,dimi+1
        do j=3,dimj+1
           
           A = get_area(Cell_Mat(i,j)%nodes(:,:))
           
           !boundary conditions with ghost cells
           if (j==3) then !bottom wall
              !set ghost cell to reflected velocity
              Cell_Mat(i,j-1)%states(:) = get_reflect(Cell_Mat(i,j)%states(:),&
                   Cell_Mat(i,j)%nodes(:,:),0)
           else if (j==dimj+1) then !top wall
              Cell_Mat(i,j+1)%states(:) = get_reflect(Cell_Mat(i,j)%states(:),&
                   Cell_Mat(i,j)%nodes(:,:),1)
           end if


           !update
           do iter=1,3
              CellVect_H(:,iter) = Cell_Mat(i+iter-2,j)%states(:)
              CellVect_V(:,iter) = Cell_Mat(i,j+iter-2)%states(:)
           end do
           
           Cell_Temp = Cell_Mat(i,j)%states(:)
           do iter=1,4
              CellVect_H(:,2) = Cell_Mat(i,j)%states(:)
              CellVect_V(:,2) = Cell_Mat(i,j)%states(:)
              stenH = stenflux(CellVect_H)
              stenV = stenflux(CellVect_V)

              residual = get_residual(stenH,stenV,Cell_Mat(i,j)%nodes(:,:))

              Cell_Mat(i,j)%states(:) = Cell_Temp(:) - alpha(iter)*dt/A*residual(:)
           end do

        end do
     end do
  end do

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
