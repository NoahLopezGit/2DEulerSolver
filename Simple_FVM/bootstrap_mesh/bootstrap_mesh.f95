module bootstrap_mesh
  use mod_read_grid
  use mod_cell_data_struc
  implicit none

contains
  function init_cell_array(grid)result(cell_array)
    real, dimension(:,:,:) :: grid
    integer, dimension(:,:), allocatable :: cell_map
    type(Cell), dimension(:), allocatable :: cell_array
    real, dimension(1) :: cell_quantities
    type(Cell_Face), dimension(4) :: tmp_face_array
    integer :: i,j,itr
    integer, dimension(4) :: cell_map_array
    real, dimension(5,2) :: node_pair_array
    real, dimension(2,2) :: tmp_node_pair

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
    cell_quantities=(/0/) !dummy holder for quanties
    do i=1,size(grid,1)-1
      do j=1,size(grid,2)-1
        cell_map_array = gen_cell_map_array(i,j,cell_map)
        node_pair_array = gen_node_pair_array(i,j,grid)
        do itr=1,4
          tmp_node_pair(1,:) = node_pair_array(itr,:)
          tmp_node_pair(2,:) = node_pair_array(itr+1,:)
          tmp_face_array(itr) = init_cell_face(cell_map_array(itr),tmp_node_pair)
        end do
        cell_array(cell_map(i,j))= init_cell(cell_map(i,j),cell_quantities,tmp_face_array)
      end do
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
