module module_flux_calculation
  implicit none

contains
  function calc_flux(cell_index,node_pair)result(cell_flux)
    implicit none
    integer, dimension(2) :: cell_index
    real, dimension(2,2) :: node_pair
    real :: cell_flux
    !this will calculate the flux at the middle of one cell wall
    !depending on the flux terms in the PDE you are trying to solve, the
      !code here might change
    !the PDE I am trying to solve is du/dt + div(k*divu) = 0
    !meaning over each side our sum is: ((K*du/dx+K*du/dy).n)*Side_length
    !Where n is the normal outward vector
    !to find du/dx, du/dy is found by the change in value over distance at
      !the two respective cell centers

    !lets start by finding the normal outward vector
  end function calc_flux

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
    real, dimension(2) :: outward_normal !vector of outward normal

    !first need to get starting and ending node.
    !we are going counterclockwise starting at bot left (cell_index) node
    call select_side(side_num,i,j) !starting node index
    node1 = grid(i,j,:)
    call select_side(side_num+1,i,j) !ending node index
    node2 = grid(i,j,:)

    !calculate vector going from starting to ending node (sub end from start)
    aligned_vector = (/node2(1)-node1(1),node2(2)-node1(2)/)
    !this convetion ensures perpindicular vector is going right as we travel along the aligned_vector
    !by convention we are going counterclockwise around the sides.
      !right side direction will always face outwards
    outward_normal = (/aligned_vector(2),-aligned_vector(1)/)
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
end module module_flux_calculation
