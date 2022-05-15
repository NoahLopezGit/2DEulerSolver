program use_read_grid
  use mod_read_grid
  implicit none
  integer :: width,height
  integer :: i,j,itr
  real, dimension(:,:,:), allocatable :: grid
  character*50 :: filename

  print *, "Enter grid filename"
  read *, filename

  call get_grid_info(filename,height,width)
  print *, "dimensions are ",width,"x",height
  allocate(grid(width,height,2))

  call read_grid(filename,grid)
  print *, grid

  print *, "Writing results to file"
  open(1,file='use_read_grid_output.dat',status='replace')
  write(1,*) width,height
  do j=1,height
        !this uses an implied do loop to write x and y row in one line
        write(1,*) (grid(itr,j,1), grid(itr,j,2), itr=1,width)
  end do
  close(1)

  deallocate(grid)

end program use_read_grid
