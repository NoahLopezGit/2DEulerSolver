module mod_cell_geometry
  use mod_cell_data_struc
  implicit none
  logical :: debug=.false.

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
    if (debug .eqv. .true.) then
      print *, "node 1 ", node1
      print *, "node 2 ", node2
      print *, "X diff", node2(1)-node1(1)
      print *, "Y diff", node2(2)-node1(2)
      print *, "Aligned vector ", aligned_vector
    end if
  end function calc_outward_normal

end module mod_cell_geometry
