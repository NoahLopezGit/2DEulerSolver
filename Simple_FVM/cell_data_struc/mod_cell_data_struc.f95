module mod_cell_data_struc
  use config
  implicit none
  type Cell_Face
    integer :: neighbor_cell_id
    real,dimension(2,2) :: face_node_pair !in counterclockwise order
    !intercept between parent cell/neighbor cell line and cell face line
    real, dimension(2) :: face_intercept
    real, dimension(2) :: face_outward_normal
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

contains !should separate cell data type and methods b/c don't want to need mod_cell_geometry to compile this mod
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
    if (mod_cell_data_struc_debug .eqv. .true.) then
      print *, "(mod_cell_data_struc) Number of cell faces ", num_faces
    end if
    allocate(myCell%cell_faces(num_faces))
    myCell%cell_faces=cell_faces

    !geometric values in each cell must be calculated with mod_cell_geometry
  end function init_cell


  function init_cell_face(neighbor_cell_id,face_node_pair)result(myCell_Face)
    implicit none
    integer :: neighbor_cell_id
    real,dimension(2,2) :: face_node_pair !in counterclockwise order
    type(Cell_Face) :: myCell_Face

    myCell_Face%neighbor_cell_id=neighbor_cell_id
    myCell_Face%face_node_pair=face_node_pair

    !dummy values.. to be calculated later with cell geometry
    myCell_Face%face_intercept = (/0.0,0.0/)
    myCell_Face%face_outward_normal = (/0.0,0.0/)
  end function init_cell_face

end module mod_cell_data_struc
