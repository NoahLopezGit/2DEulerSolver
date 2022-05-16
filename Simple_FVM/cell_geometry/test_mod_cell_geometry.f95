program test_mod_cell_geometry
  use mod_read_grid
  use mod_cell_geometry
  use mod_cell_data_struc
  implicit none
  !testing outward normal calculations
  !loading square grid into grid
  integer :: width,height
  integer :: i,j
  integer :: side_num
  integer, dimension(2) :: cell_index
  real, dimension(:,:,:), allocatable :: grid
  real, dimension(2) :: outward_normal
  character*50 :: filename

  print *, "Enter grid filename"
  read *, filename

  call get_grid_info(filename,height,width)
  print *, "dimensions are ",width,"x",height
  allocate(grid(width,height,2))

  call read_grid(filename,grid)


  cell_index = (/2,2/)
  side_num = 2
  outward_normal = calc_outward_normal(grid,cell_index,side_num)
  print *, outward_normal

end program test_mod_cell_geometry
