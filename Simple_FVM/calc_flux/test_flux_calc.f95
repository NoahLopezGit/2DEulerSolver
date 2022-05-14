program test_flux_calc
  use module_read_grid
  use module_flux_calculation
  implicit none
  !testing outward normal calculations
  !loading square grid into grid
  integer :: width,height
  integer :: i,j
  real, dimension(:,:,:), allocatable :: grid
  character*50 :: filename

  print *, "Enter grid filename"
  read *, filename

  call get_grid_info(filename,height,width)
  print *, "dimensions are ",width,"x",height
  allocate(grid(width,height,2))

  call read_grid(filename,grid)

  !need to test outward normal going counter clockwise from bot left node
  call select_side(1,i,j)
  print *, i,j
  call select_side(3,i,j)
  print *, i,j
  call select_side(5,i,j)
  print *, i,j

end program test_flux_calc
