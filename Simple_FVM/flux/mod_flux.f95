module mod_flux
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
end module mod_flux
