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
    real                       :: right,left,top,bot !cell area averages TODO: not sure if this is correct
    
    !face lengths
    nodes(:,:) = cell_stencil(3,3)%nodes(:,:)
    right = sqrt( (nodes(3,1)-nodes(2,1))**2.0 + (nodes(3,2)-nodes(2,2))**2.0 )
    left = sqrt( (nodes(1,1)-nodes(4,1))**2.0 + (nodes(1,2)-nodes(4,2))**2.0 )
    top = sqrt( (nodes(4,1)-nodes(3,1))**2.0 + (nodes(4,2)-nodes(3,2))**2.0 )
    bot = sqrt( (nodes(2,1)-nodes(1,1))**2.0 + (nodes(2,2)-nodes(1,2))**2.0 )
    

    Dx = right * diff_op(cell_stencil(2:5,3)) - left * diff_op(cell_stencil(1:4,3))
    
    Dy = top * diff_op(cell_stencil(3,2:5)) - bot * diff_op(cell_stencil(3,1:4))
    
    dissipation_times_dt = Dx(:) + Dy(:)

    right*diff2_op() !right
    left*diff2_op() !left
    top*diff2_op() !top
    bot*diff2_op() !bot

    right*diff4_op() !right
    left*diff4_op() !left
    top*diff4_op() !top
    bot*diff4_op() !bot

    Dx = 
    Dy = 
  contains

    function diff2_op(cell_line)
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

      E2 = K2 * 0.5 * (Vplus0 + Vplus1)
      
      diff2_op =  E2 * (cell_line(3)%states(:) - cell_line(2)%states(:))
      
    end function diff2_op

    
    function diff4_op(cell_line)
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

      E2 = K2 * 0.5 * (Vplus0 + Vplus1)
      
      E4 = max(0.0,(K4 - E2))
      
      diff4_op = E4 * (cell_line(4)%states(:) - 3.0 * cell_line(3)%states(:) &
           + 3.0 * cell_line(2)%states(:) - cell_line(1)%states(:))
      
    end function diff4_op


    function lambda()

      dx_p = node_p(1,1) - node_p(2,1) !x value of plus node 1 and 2 
      dy_p = node_p(1,2) - node_p(2,2) !y value of plus node 1 and 2 
      dx_m = node_m
      dy_m =
      
      u_p =
      v_p =
      u_m =
      v_m =
      
      Vmag_p = (u_p * dx_p + v_p * dy_p)/(dx_p**2.0+dy_p**2.0)
      Vmag_m = (u_m * dx_m + v_m * dy_m)/(dx_m**2.0+dy_m**2.0)
      lambda_p = c + Vmag_p
      lambda_m = c + Vmag_m
      lambda=0.5*(lamba_p+lambda_m)
    end function lambda
    
  end function get_dissipation

end module dissipation_mod
