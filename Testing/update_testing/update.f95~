module update
  
contains
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
    
    !local variables
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

  function get_dissipation()result(dissipation)
    implicit none
    real, dimension(4) :: dissipation
    !dissipation calculation here
  end function get_dissipation
  
end module update
