module bootstrap_mesh
  use config
  use mod_read_grid
  use mod_cell_data_struc
  use mod_cell_geometry
  implicit none

contains
  function init_cell_array(grid)result(cell_array)
    real, dimension(:,:,:) :: grid
    integer, dimension(:,:), allocatable :: cell_map
    type(Cell), dimension(:), allocatable :: cell_array
    real, dimension(3) :: cell_quantities
    type(Cell_Face), dimension(4) :: tmp_face_array
    integer :: i,j,itr
    integer, dimension(4) :: cell_map_array
    real, dimension(5,2) :: node_pair_array
    real, dimension(2,2) :: tmp_node_pair
    real :: x,y,u_vec,v_vec

    allocate(cell_map(size(grid,1)-1,size(grid,2)-1)) !cells are one less than grid dims
    !mapping cell id to i,j index
    itr=0
    do i=1,size(grid,1)-1
      do j=1,size(grid,2)-1
        !assign nodes to idsget_nodes
        itr=itr+1
        cell_map(i,j)=itr
      end do
    end do
    allocate(cell_array(itr))


    !cells with node associated with boundaries will have negative ids
    !for now the first and second quantity will hold the u and v field vectors

    if (bootstrap_debug .eqv. .true.) then
      print *, "(bootstrap_mesh) assigning cell quantities"
    end if

    cell_quantities=(/0.0,0.0,0.0/) !dummy holder for quanties
    do i=1,size(grid,1)-1
      do j=1,size(grid,2)-1
        if (bootstrap_debug .eqv. .true.) then
          print *, "(bootstrap_debug) Initializing cell ",cell_map(i,j)
        end if
        cell_map_array = gen_cell_map_array(i,j,cell_map)
        node_pair_array = gen_node_pair_array(i,j,grid)
        do itr=1,4
          tmp_node_pair(1,:) = node_pair_array(itr,:)
          tmp_node_pair(2,:) = node_pair_array(itr+1,:)
          tmp_face_array(itr) = init_cell_face(cell_map_array(itr),tmp_node_pair)
          if (bootstrap_debug .eqv. .true.) then
            print *, "(bootstrap_debug) tmp cell face ",itr
            print *, "(bootstrap_debug) neighbor cell id ",tmp_face_array(itr)%neighbor_cell_id
            print *, "(bootstrap_debug) face_node_pair ",tmp_face_array(itr)%face_node_pair
            print *, "(bootstrap_debug) face_interept ",tmp_face_array(itr)%face_intercept
            print *, "(bootstrap_debug) face_outward_normal ",tmp_face_array(itr)%face_outward_normal
          end if
        end do
        cell_array(cell_map(i,j))= init_cell(cell_map(i,j),cell_quantities,tmp_face_array)
        !x = cell_array(cell_map(i,j))%cell_centroid(1)
        !y = cell_array(cell_map(i,j))%cell_centroid(2)
        !u_vec = x+y
        !v_vec = y
        !cell_array(cell_map(i,j))%cell_quantities(:)=(/u_vec,v_vec,0.0/)
      end do
    end do

    if (bootstrap_debug .eqv. .true.) then
      print *, "(bootstrap_mesh) calculating cell area and centroid"
      print *, "(bootstrap_mesh) size of cell_array ", size(cell_array)
    end if
    !calculates each cell centroid and area
    do i=1,size(cell_array)
      call calc_cell_geometry(i,cell_array)
    end do

    if (bootstrap_debug .eqv. .true.) then
      print *, "(bootstrap_mesh) calculating cell faces normal and intercept"
    end if
    !calculating each face outward normal and intercept
    do i=1,size(cell_array)
      call calc_cell_face_geometry(i,cell_array)
    end do

  end function init_cell_array


  function gen_cell_map_array(i,j,cell_map)result(cell_map_array)
    implicit none
    !this is really bad but i can't think of a cleaner way of implementing it
    integer :: i,j
    integer, dimension(:,:) :: cell_map
    integer, dimension(4) :: cell_map_array
    integer :: cell_index
    integer :: width, height

    width=size(cell_map,1)
    height=size(cell_map,2)

    if (j==1) then
      cell_map_array(1)=-1
    else
      cell_index = cell_map(i,j-1)
      cell_map_array(1)=cell_index
    end if

    if (i==width) then
      cell_map_array(2)=-2
    else
      cell_index = cell_map(i+1,j)
      cell_map_array(2)=cell_index
    end if

    if (j==height) then
      cell_map_array(3)=-3
    else
      cell_index = cell_map(i,j+1)
      cell_map_array(3)=cell_index
    end if

    if (i==1) then
      cell_map_array(4)=-4
    else
      cell_index = cell_map(i-1,j)
      cell_map_array(4)=cell_index
    end if
  end function gen_cell_map_array


  function gen_node_pair_array(i,j,grid)result(node_pair_array)
    implicit none
    integer :: i,j
    real, dimension(:,:,:) :: grid
    real, dimension(5,2) :: node_pair_array

    node_pair_array(1,:) = grid(i,j,:)
    node_pair_array(2,:) = grid(i+1,j,:)
    node_pair_array(3,:) = grid(i+1,j+1,:)
    node_pair_array(4,:) = grid(i,j+1,:)
    node_pair_array(5,:) = grid(i,j,:) !include first cell again
  end function gen_node_pair_array
end module bootstrap_mesh
