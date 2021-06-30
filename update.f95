module update
  use CellType
  implicit none
  
contains
  
  function get_dissipation(cell_stencil)result(dissipation_times_dt)
    implicit none
    !input uses cell data type
    type(Cell), dimension(5,5) :: cell_stencil 
    !output
    real, dimension(4)         :: dissipation_times_dt
    !local variables
    real, dimension(4)         :: Dx,Dy
    real, dimension(4,2)       :: nodes
    real                       :: right,left,top,bot !sides
    

    nodes = cell_stencil(3,3)%nodes(:,:)
    right = sqrt((nodes(3,1)-nodes(2,1)) ** 2.0 + (nodes(3,2) - nodes(2,2)) ** 2.0)
    left  = sqrt((nodes(1,1)-nodes(4,1)) ** 2.0 + (nodes(1,2) - nodes(4,2)) ** 2.0)
    top   = sqrt((nodes(4,1)-nodes(3,1)) ** 2.0 + (nodes(4,2) - nodes(3,2)) ** 2.0)
    bot   = sqrt((nodes(2,1)-nodes(1,1)) ** 2.0 + (nodes(2,2) - nodes(1,2)) ** 2.0)
    
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

  
  function get_area(nodes)result(Area)
    implicit none
    !output
    real :: Area
    !input
    real, dimension(4,2) :: nodes
    !local variables
    Area = 0.5*( (nodes(3,1)-nodes(1,1))*(nodes(4,2)-nodes(2,2)) &
         -(nodes(3,2)-nodes(1,2))*(nodes(4,1)-nodes(2,1)))
  end function get_area
  
  
  function get_reflect(CellState,Nodes,case)result(cell_reflect)
    implicit none
    !output
    real, dimension(4)   :: cell_reflect
    !input
    real, dimension(4)   :: CellState
    real, dimension(4,2) :: Nodes
    integer :: case
    !local variables
    real :: u,v,dx,dy,ur,vr
    
    u= CellState(2)/CellState(1)
    v= CellState(3)/CellState(1)

    if (case==1) then !top wall
       dx = Nodes(4,1)-Nodes(3,1) !TR-TL
       dy = Nodes(3,2)-Nodes(3,2)
    else if (case==0) then !bottom wall
       dx = Nodes(2,1)-Nodes(1,1) !BR-BL
       dy = Nodes(2,2)-Nodes(1,2)
    end if

    ur = 2.0*(u*dx+v*dy)/(dx**2.0+dy**2.0)*dx-u
    vr = 2.0*(u*dx+v*dy)/(dx**2.0+dy**2.0)*dy-v

    cell_reflect = (/CellState(1),CellState(1)*ur,CellState(1)*vr,CellState(4)/)
  end function get_reflect
  
    
  function stenflux(CellVect)result(flux)
    implicit none
    !output line vector of fluxs in cells
    real, dimension(2,4,3) :: flux
    !input line of cell states
    real, dimension(4,3) :: CellVect
    !local variables
    real    :: rho,u,v,E,P,rhoH
    integer :: itr
    do itr = 1,3
       rho = CellVect(1,itr)
       u = CellVect(2,itr)/rho
       v = CellVect(3,itr)/rho
       E = CellVect(4,itr)/rho
       P = (1.4-1.0)*rho*(E - 0.5*(u**2.0+v**2.0))!P vector for 3 cells
       rhoH = rho*E+P

       !settting flux
       flux(1,:,itr) = (/rho*u,rho*u**2.0+P,rho*v*u,rhoH*u/) !1 is f (x-direction)
       flux(2,:,itr) = (/rho*v,rho*u*v,rho*v**2.0+P,rhoH*v/) !2 is g (y-direction)
    end do
  end function stenflux

  
  function get_residual(StenH,StenV,Nodes)result(residual)
    implicit none
    !output 4 vector residual (for each state)
    real, dimension(4) :: residual
    !inputs; need fluxes of 5 cell stencil and nodes of center cell
    real, dimension(4,2) :: Nodes !node values of center cell: [BL,BR,TR,TL]
    real, dimension(2,4,3) :: StenH,StenV ![-1,0,+1] order
    !local variables
    real, dimension(4) :: f_bot,f_right,f_top,f_left,g_bot,g_right,g_top,g_left
    real :: dx_bot,dx_right,dx_top,dx_left,dy_bot,dy_right,dy_top,dy_left
    
    f_bot   = 0.5*(StenV(1,:,1) + StenV(1,:,2)) 
    f_right = 0.5*(StenH(1,:,2) + StenH(1,:,3)) 
    f_top   = 0.5*(StenV(1,:,2) + StenV(1,:,3)) 
    f_left  = 0.5*(StenH(1,:,1) + StenH(1,:,2)) 

    g_bot   = 0.5*(StenV(2,:,1) + StenV(2,:,2)) 
    g_right = 0.5*(StenH(2,:,2) + StenH(2,:,3)) 
    g_top   = 0.5*(StenV(2,:,2) + StenV(2,:,3)) 
    g_left  = 0.5*(StenH(2,:,1) + StenH(2,:,2)) 
    
    !delta x/y calculations
    dx_bot   = Nodes(2,1) - Nodes(1,1)
    dx_right = Nodes(3,1) - Nodes(2,1)
    dx_top   = Nodes(4,1) - Nodes(3,1)
    dx_left  = Nodes(1,1) - Nodes(4,1)
    
    dy_bot   = Nodes(2,2) - Nodes(1,2)
    dy_right = Nodes(3,2) - Nodes(2,2)
    dy_top   = Nodes(4,2) - Nodes(3,2)
    dy_left  = Nodes(1,2) - Nodes(4,2)
    
    !residual calculation here
    residual = (f_bot*dy_bot - g_bot*dx_bot)& !bottom 
         + (f_right*dy_right - g_right*dx_right)& !right
         + (f_top*dy_top - g_top*dx_top)& !top
         + (f_left*dy_left - g_left*dx_left)  !left
  end function get_residual
  
end module update
