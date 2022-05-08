module grid_storage
  implicit none
  !this program will handle the data organization for the mesh data
  !will read from .dat file? and store mesh in array... no ghost cells for now
contains
  subroutine readnstore()
    implicit none
    !real, dimension(:,:,:), allocatable :: grid
    character*20 :: filename !character length 20
    character*50 :: output
    integer :: width,height
    integer :: i,j


    print *, "Enter Grid Filename "
    read *, filename
    open(1,file=filename,status='old')

    read(1,*) width,height
    allocate(grid(width,height,2))

    do i=1,width
       do j=1,height
          read(1,*) grid(i,j,1), grid(i,j,2)
       end do
    end do

  end subroutine readnstore
end module grid_storage
