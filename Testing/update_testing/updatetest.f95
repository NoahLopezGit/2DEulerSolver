program updatetest
  
  use update
  
  implicit none
  !create cellVect
  real, dimension(4,3) :: CellVect
  !fluxvect
  real, dimension(2,4,3) :: fluxvect
  real, dimension(4,2)   :: Nodes
  real, dimension(4)     :: residual

  CellVect(:,1) = (/1.0,2.0,3.0,4.0/)
  CellVect(:,2) = (/5.0,6.0,7.0,8.0/)
  CellVect(:,3) = (/9.0,10.0,11.0,12.0/)
  fluxvect = stenflux(CellVect)
  print "(2F7.3)", fluxvect(:,:,1)
  print "(2F7.3)", fluxvect(:,:,2)
  print "(2F7.3)", fluxvect(:,:,3)

  Nodes(:,1) = (/0.1,0.7,0.6,0.2/)
  Nodes(:,2) = (/0.0,0.1,0.5,0.4/)

  residual = get_residual(fluxvect,fluxvect,Nodes)
  print *, residual
end program updatetest
