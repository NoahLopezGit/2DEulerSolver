module readGrid
  implicit none

contains
  function readnstore()result(grid)
    implicit none
    real, dimension(:,:,:), allocatable :: grid
    integer :: dimi,dimj,i,j
    
    open(1,file='smoothGrid.dat',status='old')
    read(1,*) dimi,dimj
    allocate(grid(dimi,dimj,2))
    
    do i=1,dimi
       do j=1,dimj
          read(1,*) grid(i,j,1), grid(i,j,2)
       end do
    end do
  end function readnstore
end module readGrid
