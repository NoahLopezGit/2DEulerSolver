module mod_cell_data_struc
  use mod_cell_geometry
  implicit none
  type Cell_Face
    integer :: neighbor_cell_id
    real,dimension(2,2) :: face_node_pair !in counterclockwise order
  end type Cell_Face

  type Cell
    !needs to store all the indexexs of its neighboring cells going in a counterclockwise order
    !for now I will need the meshes to be loaded using this data structure
    !will need to worry about how to generate such a data structure later.
    !will write a sort of bootstrap to get existing grids into this structure form
    integer :: cell_id
    type(Cell_Face), dimension(:), allocatable :: cell_faces !TODO: how to allocate?
    real :: cell_area
    real, dimension(2) :: cell_centroid
    real, dimension(:), allocatable :: cell_quantities
  end type Cell

contains
  function init_cell(cell_id,cell_quantities,cell_faces)result(myCell)
    implicit none
    integer :: cell_id
    real, dimension(:) :: cell_quantities
    type(Cell_Face), dimension(:) :: cell_faces
    type(Cell) :: myCell

    !local variables
    real, dimension(:,:), allocatable :: nodes
    integer :: num_faces,i

    myCell%cell_id=cell_id
    allocate(myCell%cell_quantities(size(cell_quantities)))
    myCell%cell_quantities=cell_quantities
    num_faces = size(cell_faces)
    allocate(myCell%cell_faces(num_faces))
    myCell%cell_faces=cell_faces

    !construct nodes for geometry calclulations
    allocate(nodes(num_faces+1,2))
    do i=1,num_faces
      nodes(i,:) = myCell%cell_faces(i)%face_node_pair(1,:)
    end do
    !first node must be repeated for area and centroid functions
    nodes(num_faces+1,:) = myCell%cell_faces(1)%face_node_pair(1,:)

    myCell%cell_area = calc_cell_area(nodes)
    myCell%cell_centroid = calc_cell_centroid(nodes)

  end function init_cell


  function init_cell_face(neighbor_cell_id,face_node_pair)result(myCell_Face)
    implicit none
    integer :: neighbor_cell_id
    real,dimension(2,2) :: face_node_pair !in counterclockwise order
    type(Cell_Face) :: myCell_Face

    myCell_Face%neighbor_cell_id=neighbor_cell_id
    myCell_Face%face_node_pair=face_node_pair
  end function init_cell_face

end module mod_cell_data_struc
