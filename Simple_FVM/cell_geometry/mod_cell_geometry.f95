module mod_cell_geometry
  use config
  use mod_math
  use mod_cell_data_struc
  implicit none

contains

  subroutine calc_cell_geometry(cell_id,cell_array)
    implicit none
    real, dimension(:,:), allocatable :: nodes
    type(Cell), dimension(:) :: cell_array
    integer :: cell_id

    if (mod_cell_geometry_debug .eqv. .true.) then
      print *, "(mod_cell_geometry) getting nodes for cell ", cell_id
    end if

    nodes = get_nodes(cell_array(cell_id)) !is this passing a new pointer or generating a new value?

    if (mod_cell_geometry_debug .eqv. .true.) then
      print *, "(mod_cell_geometry) getting area for cell ", cell_id
    end if

    cell_array(cell_id)%cell_area=calc_cell_area(nodes)

    if (mod_cell_geometry_debug .eqv. .true.) then
      print *, "(mod_cell_geometry) getting centroid for cell", cell_id
    end if

    cell_array(cell_id)%cell_centroid=calc_cell_centroid(nodes)

  end subroutine calc_cell_geometry


  subroutine calc_cell_face_geometry(cell_id,cell_array)
    !call this after using calc_cell_geometry on all cells
    implicit none

    integer, intent(in) :: cell_id
    type(Cell), dimension(:), intent(inout):: cell_array !both taking in and returnin out so inout

    !local variables
    integer :: neighbor_cell_id,itr
    real, dimension(2) :: cell_centroid, neighbor_cell_centroid
    real, dimension(2,2) :: face_node_pair

    if (mod_cell_geometry_debug .eqv. .true.) then
      print *, "(mod_cell_geometry) Cell ",cell_id,"faces ",size(cell_array(cell_id)%cell_faces)
    end if
    do itr=1,size(cell_array(cell_id)%cell_faces) !iterate through all cell faces

      if (mod_cell_geometry_debug .eqv. .true.) then
        print *, "(mod_cell_geometry) Calculating geometry for cell ",cell_id," Face ", itr
      end if

      if (mod_cell_geometry_debug .eqv. .true.) then
        print *, "(mod_cell_geometry) cell_array? ",cell_array(cell_id)%cell_faces(itr)%face_node_pair
      end if
      face_node_pair=cell_array(cell_id)%cell_faces(itr)%face_node_pair
      if (mod_cell_geometry_debug .eqv. .true.) then
        print *, "(mod_cell_geometry) after face_node_pair"
      end if
      cell_array(cell_id)%cell_faces(itr)%face_outward_normal=&
        calc_outward_normal(face_node_pair)
      if (mod_cell_geometry_debug .eqv. .true.) then
        print *, "(mod_cell_geometry) after outward normal"
      end if

      !this means all cell must already be calculated
      neighbor_cell_id=cell_array(cell_id)%cell_faces(itr)%neighbor_cell_id
      if (neighbor_cell_id < 0) then
        !ghost cell case
        !face intercept should be average of y1,y2 and x1,x2

        cell_array(cell_id)%cell_faces(itr)%face_intercept=calc_ghost_face_intercept(face_node_pair)
      else
        neighbor_cell_centroid=cell_array(neighbor_cell_id)%cell_centroid
        cell_centroid=cell_array(cell_id)%cell_centroid
        cell_array(cell_id)%cell_faces(itr)%face_intercept=&
          calc_face_intercept(cell_centroid,neighbor_cell_centroid,face_node_pair)
      end if
    end do

  end subroutine calc_cell_face_geometry


  function get_nodes(myCell)result(nodes)
    implicit none
    !in
    type(Cell) :: myCell

    !out
    real, dimension(:,:), allocatable :: nodes

    !local variables
    integer :: i, num_faces

    !construct nodes for geometry calclulations
    num_faces=size(myCell%cell_faces)
    if (mod_cell_geometry_debug .eqv. .true.) then
      print *, "(mod_cell_geometry) cell has ", num_faces, " faces"
    end if
    allocate(nodes(num_faces+1,2))
    do i=1,num_faces
      nodes(i,:) = myCell%cell_faces(i)%face_node_pair(1,:)
    end do
    !first node must be repeated for area and centroid functions
    nodes(num_faces+1,:) = myCell%cell_faces(1)%face_node_pair(1,:)

  end function get_nodes


  function calc_ghost_face_intercept(face_node_pair)result(ghost_face_intercept)
    implicit none
    !in
    real, dimension(2,2) :: face_node_pair

    !out
    real, dimension(2) :: ghost_face_intercept

    !local variables
    real :: x_intercept,y_intercept

    !taking average x and y between face node pair
    x_intercept = (face_node_pair(1,1)+face_node_pair(2,1))/2.0
    y_intercept = (face_node_pair(1,2)+face_node_pair(2,2))/2.0
    ghost_face_intercept = (/x_intercept,y_intercept/)

  end function calc_ghost_face_intercept


  function calc_face_intercept(cell_centroid,neighbor_cell_centroid,face_node_pair)result(face_intercept)
    implicit none
    !in
    real, dimension(2) :: cell_centroid, neighbor_cell_centroid
    real, dimension(2,2) :: face_node_pair

    !out
    real, dimension(2) :: face_intercept

    !local variables
    real :: x1,x2,x3,x4,y1,y2,y3,y4
    real, dimension(3) :: cell_cf,face_cf
    real :: y_intercept, x_intercept

    !it is not safe to use slope intercept to find the point of intersection
      !due to the possibility of vertical lines

    !must solve for intersection by soliving a system of eqns. from the homogeneous
      !form of the line equations

    !first need to find the coefficients for the homogeneous line equations

    !cell_line homogeneous coordinates (this is a cross product)
    !cell and face homogeneous coefficients
    x1=cell_centroid(1)
    y1=cell_centroid(2)
    x2=neighbor_cell_centroid(1)
    y2=neighbor_cell_centroid(2)
    x3=face_node_pair(1,1)
    y3=face_node_pair(1,2)
    x4=face_node_pair(2,1)
    y4=face_node_pair(2,2)
    cell_cf= cross((/1.0,x1,y1/),(/1.0,x2,y2/))
    face_cf = cross((/1.0,x3,y3/),(/1.0,x4,y4/))
    !now solve system of equations
    !https://math.stackexchange.com/questions/1798037/find-intersection-point-of-two-straight-lines
    !had to rearrange the values to fit with my convention order
    x_intercept=(cell_cf(3)*face_cf(1)-face_cf(3)*cell_cf(1))/(cell_cf(2)*face_cf(3)-face_cf(2)*cell_cf(3))
    y_intercept=(cell_cf(1)*face_cf(2)-cell_cf(2)*face_cf(1))/(cell_cf(2)*face_cf(3)-face_cf(2)*cell_cf(3))

    face_intercept=(/x_intercept,y_intercept/)
    if (mod_cell_geometry_debug .eqv. .true.) then
      print *, "(mod_cell_geometry) Intercept calculation"
      print *, "(mod_cell_geometry) face intercept ",face_intercept
      print *, "(mod_cell_geometry) x1,y1",x1,y1
      print *, "(mod_cell_geometry) x2,y2",x2,y2
      print *, "(mod_cell_geometry) x3,y3",x3,y3
      print *, "(mod_cell_geometry) x4,y4",x4,y4
    end if
  end function calc_face_intercept


  function calc_outward_normal(face_node_pair)result(outward_normal) !TODO: change this to take cell_array,cell_id,face_id instead
    !cell indexes are associated with the bottom left node
    !by convention we are traversing the nodes counterclockwise
      !so our outward normal will be on the "right"
    implicit none
    real, dimension(2,2) :: face_node_pair !how to import variables of unknown size into function
    real, dimension(2) :: node1,node2 !going from node1 to node2
    real, dimension(2) :: outward_normal,aligned_vector !vector of outward normal


    !first need to get starting and ending node.
    !direction is counterclockwise, this must be ensured in the init_cell node order
    node1 = face_node_pair(1,:)
    node2 = face_node_pair(2,:)

    !calculate vector going from starting to ending node (sub end from start)
    aligned_vector = (/node2(1)-node1(1),node2(2)-node1(2)/)
    !this convetion ensures perpindicular vector is going right as we travel along the aligned_vector
    !by convention we are going counterclockwise around the sides.
      !right side direction will always face outwards
    outward_normal = (/aligned_vector(2),-aligned_vector(1)/)
    if (mod_cell_geometry_debug .eqv. .true.) then
      print *, "(mod_cell_geometry) node 1 ", node1
      print *, "(mod_cell_geometry) node 2 ", node2
      print *, "(mod_cell_geometry) X diff", node2(1)-node1(1)
      print *, "(mod_cell_geometry) Y diff", node2(2)-node1(2)
      print *, "(mod_cell_geometry) Aligned vector ", aligned_vector
    end if
  end function calc_outward_normal


  function calc_cell_area(nodes)result(area) !TODO: change this to take cell_array,cell_id instead
    implicit none
    !arrays with unknown size use assumed shape arguements
    !this must be used w/in module (which generates implicit interface) or have an explicit interface
    real, dimension(:,:) :: nodes
    real :: area

    !local variables
    integer :: num_nodes
    integer :: i  !iterables



    num_nodes = size(nodes,1)
    if (mod_cell_geometry_debug .eqv. .true.) then
      print *, "(mod_cell_geometry) calculating cell area"
      print *, "(mod_cell_geometry) Nodes"
      do i=1,num_nodes
        print *, nodes(i,:)
      end do
    end if

    area = 0.0
    do i=1,num_nodes-1
          area = area + nodes(i,1)*nodes(i+1,2) - nodes(i+1,1)*nodes(i,2)
    end do
    area = 0.5 * area

    if (mod_cell_geometry_debug .eqv. .true.) then
      print *, "(mod_cell_geometry) Cell area ", area
    end if
  end function calc_cell_area


  function calc_cell_centroid(nodes)result(centroid) !TODO: change this to take cell_array,cell_id instead
    implicit none

    !in
    real, dimension(:,:) :: nodes

    !out
    real, dimension(2) :: centroid !x and y coordinate in array

    !local variables
    integer :: num_nodes, num_coords
    integer :: i    !construct nodes for geometry calclulation

    real    :: area
    real :: Cx, Cy
    real :: x1, x2, y1, y2

    num_nodes = size(nodes,1)
    !must first calculate the area
    area = calc_cell_area(nodes)

    Cx = 0.0
    Cy = 0.0
    do i=1,num_nodes-1
      x1 = nodes(i,1)
      x2 = nodes(i+1,1)
      y1 = nodes(i,2)
      y2 = nodes(i+1,2)

      Cx = Cx + (x1+x2)*(x1*y2-x2*y1)
      Cy = Cy + (y1+y2)*(x1*y2-x2*y1)
    end do
    Cx = 1/(6*area)*Cx
    Cy = 1/(6*area)*Cy
    centroid(:) = (/Cx,Cy/)
    if (mod_cell_geometry_debug .eqv. .true.) then
      print *, "(mod_cell_geometry) Cell centroid", centroid
    end if
  end function calc_cell_centroid

end module mod_cell_geometry
