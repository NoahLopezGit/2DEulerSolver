module mod_flux
implicit none

contains
  function calc_con_flux(cell_array,cell_id,cell_face)result(convective_flux)
    implicit none
    integer :: cell_id, cell_face
    type(Cell) :: cell_array !this will be the cell array that is loaded by bootstrap_mesh
    real, dimension(:) :: convective_flux
    !Calculates convective flux at one face
    !using the upwind scheme; as this is most "physically realistic"
    !cells must store value of field at center


  end function calc_con_flux


  function calc_diff_flux(cell_array,cell_id,cell_face)result(diffusive_flux)
    implicit none
    integer :: cell_id, cell_face
    type(Cell) :: cell_array
    real, dimension(:) :: diffisive_flux
    !Calculates flux of a diffusive term at one face.


  end function calc_diff_flux
end module mod_flux
