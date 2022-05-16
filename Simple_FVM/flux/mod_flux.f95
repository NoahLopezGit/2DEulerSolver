module mod_flux
implicit none

contains
  function calc_con_flux(grid,cell_index,side_num)result(cell_flux)
    implicit none
    real, dimension(:,:,:) :: grid
    integer, dimension(2) :: cell_index
    real, dimension(2) :: field_p, field_n
    real, dimension(2,2) :: node_pair
    real :: cell_flux
    !calculates the flux of a convective quantity at one face
    !using upwind differencing because that is what CFD codes mostly use

    !need to calculate the field at the face
      !will use linear diff for field at face
      !need to get field at P and N cell
    grid()
  end function calc_con_flux


  function calc_diff_flux(grid,cell_index,node_pair)result(cell_flux)
    implicit none
    real, dimension(:,:,:) :: grid
    integer, dimension(2) :: cell_index
    real, dimension(2,2) :: node_pair
    real :: cell_flux
    !Calculates flux of a diffusive term at one face.


  end function calc_diff_flux
end module mod_flux
