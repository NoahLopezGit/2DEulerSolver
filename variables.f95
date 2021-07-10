module variables
  use CellType
  implicit none

  !---------------------config---------------------------!
  
  !timestepping
  real, parameter :: dt = 0.000001 !timestep value (s)
  integer, parameter :: timesteps=15000 !amt of timesteps
  
  !initial conditions
  real, parameter :: rho0=1.225 !density (kg/m^3)
  real, parameter :: u0=0.0,v0=0.0 !velocities (m/s)
  real, parameter :: T0 = 273.15 !temp (K)
  real :: P0 !pressure (Pa)
  real :: E0 !energy (?)
  
  !inlet conditions
  real, parameter :: T_i=273.15 !temp (K)
  real, parameter :: P_i=100*10**3.0 !pressure (Pa)
  
  !outlet conditions
  real, parameter :: P_out = 95*10**3.0 !pressure (Pa)
  
  !constants
  real, parameter :: gamma=1.4 !gamma air
  real, parameter :: Rc=287 !gas constant air
  real, parameter :: Cp=1.004 !specific heat air

  !result control
  logical, parameter :: write_convergence = .true.
  logical, parameter :: write_dissipation = .false.
  logical, parameter :: write_states = .true.
  
  !-------------------endconfig--------------------------!

  !convergence holder
  real, dimension(:,:), allocatable :: delta_vec

  !Initial State Placeholder
  real, dimension(4) :: initial_state

  !Norm State Placeholder
  real, dimension(4) :: norm_state
  
  !max lengths for j and i dimensions
  integer :: dimi,dimj
  
  !iteration variables
  integer :: i,j,r,s,iter,step

  !x/y laplace smoothd grid data [i,j,x or y]
  real, dimension(:,:,:), allocatable     :: grid

  !Cell data object (has node and state data)
  type(Cell), dimension(:,:), allocatable :: Cell_Mat

  !dissipation matrix for writing data
  real, dimension(:,:,:,:), allocatable :: dissipation_mat

  !residual and dissipation for updating
  real, dimension(4)     :: residual,dissipation

  !data for residual calculations
  real, dimension(2,4,3) :: stenH,stenV
  real, dimension(4,3)   :: CellVect_H, CellVect_V
  real, dimension(4)     :: Cell_Temp
  
  !runge kutta time stepping constants
  real, parameter, dimension(4) :: alpha = (/0.25,0.33,0.5,1.0/)

  !area placeholder
  real :: A
  
  !boundary cell variables
  real :: rho_in !internal density
  real :: u_in,v_in !internal velocities
  real :: rho_bc !density (kg/m^3)
  real :: T_bc !temp (K)
  real :: P_bc !pressure (K)
  real :: u_bc,v_bc !velocities
  real :: rhoE_bc !density x energy
  
  
  


end module variables
