module mod_cell_geometry
  implicit none
  logical :: debug=.true.

contains
  function calc_outward_normal(grid,cell_index,side_num)result(outward_normal)
    !apparently fortran just passes the location of the variable and not the value itself
      !so passing this function the entire grid shouldn't affect the efficiency
    !cell indexes are associated with the bottom left node
    !by convention we are traversing the nodes counterclockwise
      !so our outward normal will be on the "right"
    implicit none
    real, dimension(:,:,:) :: grid !how to import variables of unknown size into function
    integer :: i,j
    integer, dimension(2) :: cell_index
    integer :: side_num
    real, dimension(2) :: node1,node2 !going from node1 to node2
    real, dimension(2) :: outward_normal,aligned_vector !vector of outward normal

    !first need to get starting and ending node.
    !we are going counterclockwise starting at bot left (cell_index) node
    call select_node(side_num,i,j) !starting node index
    node1 = grid(cell_index(1)+i,cell_index(2)+j,:)
    if (debug .eqv. .true.) then
      print *, "node 1 index",cell_index(1)+i,cell_index(2)+j
    end if
    call select_node(side_num+1,i,j) !ending node index
    node2 = grid(cell_index(1)+i,cell_index(2)+j,:)
    if (debug .eqv. .true.) then
      print *, "node 2 index",cell_index(1)+i,cell_index(2)+j
    end if

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


  subroutine select_node(node_num,i,j)
    implicit none
    integer, intent(in) :: node_num
    integer, intent(out):: i,j

    !node order is 1 to 4 going counterclockwise; starting with bottom left node (cell index)
    Select case (node_num)
      case(1)
        i=0
        j=0
      case(2)
        i=1
        j=0
      case(3)
        i=1
        j=1
      case(4)
        i=0
        j=1
      case(5)
        i=0
        j=0
    end select
  end subroutine select_node
end module mod_cell_geometry
