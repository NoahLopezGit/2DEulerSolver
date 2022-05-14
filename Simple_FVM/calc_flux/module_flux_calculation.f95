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

  function calc_outward_normal(grid,cell_index,side_num)result(side_flux)
    !apparently fortran just passes the location of the variable and not the value itself
      !so passing this function the entire grid shouldn't affect the efficiency
    !cell indexes are associated with the bottom left node
    !by convention we are traversing the nodes counterclockwise
      !so our outward normal will be on the "right"
    implicit none
    real, dimension(:,:,:) :: grid
    integer, dimension(2) :: cell_index
    real, dimension(2) :: node1,node2 !going from node1 to node2
    integer :: side_num
    real, dimension(2) :: side_flux

  end function calc_outward_normal

  subroutine select_side(side_num,i,j)
    implicit none
    integer, intent(in) :: side_num
    integer, intent(out):: i,j

    !node order is 1 to 4 going counterclockwise; starting with bottom left node (cell index)
    Select case (side_num)
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
  end subroutine select_side
end module module_flux_calculation
