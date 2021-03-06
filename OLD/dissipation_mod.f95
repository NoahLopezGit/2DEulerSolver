module dissipation_mod
  use CellType
  use update
  implicit none

contains
  
  function get_dissipation(cell_stencil)result(dissipation_times_dt)
    implicit none
    !input uses cell data type
    type(Cell), dimension(5,5) :: cell_stencil !(3,3) is middle
    !output
    real, dimension(4)         :: dissipation_times_dt
    !local variables
    real, dimension(4)         :: Dx,Dy
    real, dimension(4,2)       :: nodes
    real                       :: right,left,top,bot,mid_area !cell area averages TODO: not sure if this is correct 


    !getting average areas
    mid_area = get_area(cell_stencil(3,3)%nodes(:,:))
    right = 0.5 * (get_area(cell_stencil(3,4)%nodes(:,:)) + mid_area)
    left = 0.5 * (get_area(cell_stencil(3,2)%nodes(:,:)) + mid_area)
    top = 0.5 * (get_area(cell_stencil(4,3)%nodes(:,:)) + mid_area)
    bot = 0.5 * (get_area(cell_stencil(2,3)%nodes(:,:)) + mid_area)

    Dx = right * diff_op(cell_stencil(2:5,3)) - left * diff_op(cell_stencil(1:4,3))
    Dy = top * diff_op(cell_stencil(3,2:5)) - bot * diff_op(cell_stencil(3,1:4))
    dissipation_times_dt = Dx(:) + Dy(:)

  contains

    function diff_op(cell_line)
      !input
      type(Cell), dimension(4) :: cell_line
      !output
      real, dimension(4)       :: diff_op
      !local variables
      integer                  :: i
      real, parameter          :: K2 = 0.25, K4 = 0.00390625 !1/256
      real                     :: Vplus0,Vplus1,E2,E4
      real                     :: rho,u,v,E
      real, dimension(4)       :: P
      real, dimension(4)       :: D2,D4

      do i=1,4
         rho = cell_line(i)%states(1)
         u = cell_line(i)%states(2)/rho
         v = cell_line(i)%states(3)/rho
         E = cell_line(i)%states(4)/rho
         P(i) = (1.4-1.0)*rho*(E - 0.5*(u**2.0+v**2.0))
      end do

      Vplus0 = abs(P(3) - 2.0 * P(2) + P(1)) / (abs(P(3)) + 2.0 * abs(P(2)) + abs(P(1)))
      Vplus1 = abs(P(4) - 2.0 * P(3) + P(2)) / (abs(P(4)) + 2.0 * abs(P(3)) + abs(P(2)))

      E2 = K2 * max(Vplus0,Vplus1)
      E4 = max(0.0,(K4 - E2)) 
      D2 =  E2 * (cell_line(3)%states(:) - cell_line(2)%states(:))
      D4 = E4 * (cell_line(4)%states(:) - 3.0 * cell_line(3)%states(:) &
           + 3.0 * cell_line(2)%states(:) - cell_line(1)%states(:))
      diff_op = (D2(:) + D4(:))
    end function diff_op

  end function get_dissipation

end module dissipation_mod
