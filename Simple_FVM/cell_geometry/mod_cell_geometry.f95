module mod_cell_geometry
  use config
  implicit none

contains
  function calc_outward_normal(face_nodes)result(outward_normal)
    !apparently fortran just passes the location of the variable and not the value itself
      !so passing this function the entire grid shouldn't affect the efficiency
    !cell indexes are associated with the bottom left node
    !by convention we are traversing the nodes counterclockwise
      !so our outward normal will be on the "right"
    implicit none
    real, dimension(2,2) :: face_nodes !how to import variables of unknown size into function
    real, dimension(2) :: node1,node2 !going from node1 to node2
    real, dimension(2) :: outward_normal,aligned_vector !vector of outward normal


    !first need to get starting and ending node.
    !direction is counterclockwise, this must be ensured in the init_cell node order
    node1 = face_nodes(1,:)
    node2 = face_nodes(2,:)

    !calculate vector going from starting to ending node (sub end from start)
    aligned_vector = (/node2(1)-node1(1),node2(2)-node1(2)/)
    !this convetion ensures perpindicular vector is going right as we travel along the aligned_vector
    !by convention we are going counterclockwise around the sides.
      !right side direction will always face outwards
    outward_normal = (/aligned_vector(2),-aligned_vector(1)/)
    if (mod_cell_geometry_debug .eqv. .true.) then
      print *, "node 1 ", node1
      print *, "node 2 ", node2
      print *, "X diff", node2(1)-node1(1)
      print *, "Y diff", node2(2)-node1(2)
      print *, "Aligned vector ", aligned_vector
    end if
  end function calc_outward_normal


  function calc_cell_area(nodes)result(area)
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
      print *, "calculating cell area"
      print *, "Nodes"
      do i=1,num_nodes
        print *, nodes(i,:)
      end do
    end if

    area = 0.0
    do i=1,num_nodes-1
      if (mod_cell_geometry_debug .eqv. .true.) then
        print *, "node ", i," ",nodes(i,:)
      end if
      area = area + nodes(i,1)*nodes(i+1,2) - nodes(i+1,1)*nodes(i,2)
    end do
    area = 0.5 * area

    if (mod_cell_geometry_debug .eqv. .true.) then
      print *, "Cell area ", area
    end if
  end function calc_cell_area


  function calc_cell_centroid(nodes)result(centroid)
    implicit none

    real, dimension(:,:) :: nodes
    real, dimension(2) :: centroid !x and y coordinate in array

    !local variables
    integer :: num_nodes, num_coords
    integer :: i
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
      print *, "Cell centroid", centroid
    end if
  end function calc_cell_centroid

end module mod_cell_geometry
