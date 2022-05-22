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
  print *, "Enter grid filename"
  read *, filename

  call get_grid_info(filename,height,width)
  print *, "dimensions are ",width,"x",height
  allocate(grid(width,height,2))
  cell_id_length=(width-1)*(height-1)
  allocate(cell_array(cell_id))

  call read_grid(filename,grid)

  cell_array=init_cell_array(grid)

  cell_id = 1

  print *, "test Cell id ",cell_array(cell_id)%cell_id
  print *, "test cell quantity ",cell_array(cell_id)%cell_quantities(1)
  print *, "test cell area",cell_array(cell_id)%cell_area
  print *, "test cell centroid",cell_array(cell_id)%cell_centroid(:)
  print *, "test cell faces"
  do i=1,4
    print *, "Neighbor id ",cell_array(cell_id)%cell_faces(i)%neighbor_cell_id
    nodepair1=cell_array(cell_id)%cell_faces(i)%face_node_pair(1,:)
    nodepair2=cell_array(cell_id)%cell_faces(i)%face_node_pair(2,:)
    print *, "Face nodes (",nodepair1,"),(",nodepair2,")"
    print *, "Outward norm ",calc_outward_normal(cell_array(cell_id)%cell_faces(i)%face_node_pair(:,:))
    print *, " "
  end do

  node_pair_array = gen_node_pair_array(1,1,grid)
  do i=1,4
    print *, "Node pair ","(",(node_pair_array(i,:)),"),(",(node_pair_array(i+1,:)),")"
    print *, "Outward norm ",calc_outward_normal(node_pair_array(i:i+1,:))
    print *, " "
  end do

end program test_bootstrap_mesh
