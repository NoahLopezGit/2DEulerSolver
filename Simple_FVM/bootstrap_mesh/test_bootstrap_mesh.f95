program test_bootstrap_mesh
  use bootstrap_mesh
  use mod_cell_data_struc
  use mod_cell_geometry
  implicit none
  real, dimension(:,:,:), allocatable :: grid
  type(Cell), dimension(:), allocatable :: cell_array
  real, dimension(2) :: nodepair1, nodepair2
  integer :: cell_id_length, cell_id
  character*50 :: filename
  integer :: height, width
  integer :: i
  real, dimension(5,2) :: node_pair_array


  !this is intended to convert my existing grids into the unstructured mesh form which
    !my Simple_FVM code is now using
  !will need to get better solution to this for more flexibility
  print *, "(test_bootstrap_mesh) Enter grid filename"
  read *, filename

  call get_grid_info(filename,height,width)
  print *, "(test_bootstrap_mesh) dimensions are ",width,"x",height
  allocate(grid(width,height,2))
  cell_id_length=(width-1)*(height-1)
  allocate(cell_array(cell_id))

  call read_grid(filename,grid)
  print *, "(test_bootstrap_mesh) grid loaded"
  cell_array=init_cell_array(grid)
  print *, "(test_bootstrap_mesh) cell_array initialized"


  cell_id = 1
  do i=1,9
    print *, "(test_bootstrap_mesh) Cell id for ",i," is ",cell_array(i)%cell_id
  end do

  print *, "(test_bootstrap_mesh) test Cell id ",cell_array(cell_id)%cell_id
  print *, "(test_bootstrap_mesh) test cell quantity ",cell_array(cell_id)%cell_quantities(:)
  print *, "(test_bootstrap_mesh) test cell area",cell_array(cell_id)%cell_area
  print *, "(test_bootstrap_mesh) test cell centroid",cell_array(cell_id)%cell_centroid(:)
  print *, "(test_bootstrap_mesh) test cell faces"
  do i=1,4
    print *, "(test_bootstrap_mesh) Neighbor id ",cell_array(cell_id)%cell_faces(i)%neighbor_cell_id
    nodepair1=cell_array(cell_id)%cell_faces(i)%face_node_pair(1,:)
    nodepair2=cell_array(cell_id)%cell_faces(i)%face_node_pair(2,:)
    print *, "(test_bootstrap_mesh) Face nodes (",nodepair1,"),(",nodepair2,")"
    print *, "(test_bootstrap_mesh) Outward norm ",cell_array(cell_id)%cell_faces(i)%face_outward_normal
    print *, "(test_bootstrap_mesh) intercept ", cell_array(cell_id)%cell_faces(i)%face_intercept
  end do

  node_pair_array = gen_node_pair_array(1,1,grid)
  do i=1,4
    print *, "(test_bootstrap_mesh) Node pair ","(",(node_pair_array(i,:)),"),(",(node_pair_array(i+1,:)),")"
    print *, "(test_bootstrap_mesh) Outward norm ",calc_outward_normal(node_pair_array(i:i+1,:))
  end do

end program test_bootstrap_mesh
