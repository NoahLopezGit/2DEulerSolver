program laplaceGrid
  implicit none
  !TODO need to adapt this to new grid scheme
  !will generate the direchlet bcs for laplace smoothing
  integer  :: Imax,Jmax,i,j
  real, dimension(:,:), allocatable :: x,y

  integer            :: dimi, dimj
  real               :: de, dn  !delta used in gauss sidel
  integer            :: itr,xint2,xint3

  real                              :: x_prev,y_prev,err_sum1,err_sum2,err_tot
  real, dimension(:,:), allocatable :: a,b,g,c,a1,a2,a3,a4,a5,a6,a7,a8
  real, dimension(:), allocatable   :: slope

  !setting up variables
  !============================================================================================!
  print *, "Enter Imax (columns) (evenly divisible by 5)"
  read *, Imax
  print *, "Enter Jmax (rows)"
  read *, Jmax

  allocate(x(Imax+1,Jmax+1)) !(row,col)
  allocate(y(Imax+1,Jmax+1))
  dimi = Imax+1
  dimj = Jmax+1

  allocate(a(dimi-2,dimj-2))
  allocate(b(dimi-2,dimj-2))
  allocate(g(dimi-2,dimj-2))
  allocate(c(dimi-2,dimj-2))
  allocate(a1(dimi-2,dimj-2))
  allocate(a2(dimi-2,dimj-2))
  allocate(a3(dimi-2,dimj-2))
  allocate(a4(dimi-2,dimj-2))
  allocate(a5(dimi-2,dimj-2))
  allocate(a6(dimi-2,dimj-2))
  allocate(a7(dimi-2,dimj-2))
  allocate(a8(dimi-2,dimj-2))

  allocate(slope(dimi))

  de=1.0/(real(dimj-1))
  dn=1.0/(real(dimi-1))

  !creating algebraic grid
  y = 0.0
  x = 0.0

  print *, "Creating algebraic grid"
  do i=1,Imax+1
     do j=1,Jmax+1
        !bottom
        x(i,j) = 5*real(i-1)/real(Imax)

        if (i <= 2*Imax/5+1) then
           !first flat
           y(i,j) = real(j-1)/real(Jmax)
        else if ( i > 2*Imax/5+1 .and. i < 3*Imax/5+1) then
           !middle hump
           y(i,j) = (1-0.1*sin(3.1415928*(x(i,1)-x(2*Imax/5+1,1))))*(j-1)/Jmax&
                +0.1*sin(3.1415928*(x(i,1)-x(2*Imax/5+1,1)))
        else if ( i >= 3*Imax/5+1) then
           !last flat
           y(i,j) = real(j-1)/real(Jmax)
        end if

     end do
  end do

  print *, x
  print *, y

  !smoothing algebraic grid
  !=====================================================================================!
  !code to adjust (x,y)[l] by laplace smoothing
  print *, "Starting laplace smoothing"

  do itr=1,5000
     err_sum1 = 0
     err_sum2 = 0
     err_tot  = 0


     !WHILE LOOP for every iteration here
     !constants are only calculated for the interior nodes (range from 2->dimi-1, 2->dimj-1)
     !print *, "Calculating constants"
     do i=2,dimi-1
        do j=2,dimj-1
           a(i-1,j-1)=1/(4*dn**2)*((x(i,j+1)-x(i,j-1))**2+(y(i,j+1)-y(i,j-1))**2)&
                +10.0**(-9.0)
           b(i-1,j-1)=1/(4*de*dn)*((x(i+1,j)-x(i-1,j))*(x(i,j+1)-x(i,j-1))+(y(i+1,j)-y(i-1,j))*(y(i,j+1)-y(i,j-1)))&
                +10.0**(-9.0)
           g(i-1,j-1)=1/(4*de**2)*((x(i+1,j)-x(i-1,j))**2+(y(i+1,j)-y(i-1,j))**2)&
                +10.0**(-9.0)
           c(i-1,j-1)=2*a(i-1,j-1)/de**2+2*g(i-1,j-1)/dn**2


           a1(i-1,j-1)= a(i-1,j-1)  / (de**2*c(i-1,j-1))
           a2(i-1,j-1)= a(i-1,j-1)  / (de**2*c(i-1,j-1))
           a3(i-1,j-1)= -b(i-1,j-1) / (2*de*dn*c(i-1,j-1))
           a4(i-1,j-1)= b(i-1,j-1)  / (2*de*dn*c(i-1,j-1))
           a5(i-1,j-1)= b(i-1,j-1)  / (2*de*dn*c(i-1,j-1))
           a6(i-1,j-1)= -b(i-1,j-1) / (2*de*dn*c(i-1,j-1))
           a7(i-1,j-1)= g(i-1,j-1)  / (dn**2*c(i-1,j-1))
           a8(i-1,j-1)= g(i-1,j-1)  / (dn**2*c(i-1,j-1))
        end do
     end do

     !now that all the constants for this iteration have been calculated... commence with gauss sidel
     !iterate over all interior points (exterior are the BC and will not be changed)
     !print *, "Updating x"
     do i=2,dimi-1
        do j=2,dimj-1
           x_prev = x(i,j)
           x(i,j) = a1(i-1,j-1)*x(i+1,j) &
                +a2(i-1,j-1)*x(i-1,j)    &
                +a3(i-1,j-1)*x(i+1,j+1)  &
                +a4(i-1,j-1)*x(i-1,j+1)  &
                +a5(i-1,j-1)*x(i+1,j-1)  &
                +a6(i-1,j-1)*x(i-1,j-1)  &
                +a7(i-1,j-1)*x(i,j+1)    &
                +a8(i-1,j-1)*x(i,j-1)
           err_sum1 = err_sum1 + x(i,j)-x_prev
        end do
     end do

     !print *, "Updating y"
     do i=2,dimi-1
        do j=2,dimj-1
           y_prev = y(i,j)
           y(i,j) = a1(i-1,j-1)*y(i+1,j) &
                +a2(i-1,j-1)*y(i-1,j)    &
                +a3(i-1,j-1)*y(i+1,j+1)  &
                +a4(i-1,j-1)*y(i-1,j+1)  &
                +a5(i-1,j-1)*y(i+1,j-1)  &
                +a6(i-1,j-1)*y(i-1,j-1)  &
                +a7(i-1,j-1)*y(i,j+1)    &
                +a8(i-1,j-1)*y(i,j-1)
           err_sum2 = err_sum2 + y(i,j)-y_prev
        end do
     end do

     !impose bc neumann on top (perpindicular) this will allow for adjustable spacing in x on top
     !do i=1,dimi
     !   x(i,dimj) = x(i,dimj-1)
     !end do

     !impose neumann bc... only really need along bottom (perpindicular to the slope of bottom line)
     !adjust internal x and y so that [y(2,i)-y(1,i)]/[x(2,i)-x(1,i)] = slope
     if (itr >= 500) then
        xint2 = 1
        xint3 = 1
        do i=1,dimi

           !slope fisrt interior line
           slope(i) = (y(i+1,2)-y(i-1,2))/(x(i+1,2)-x(i-1,2))

           !enforcing neumann perpindilularity
           if (slope(i) /= 0) then
              x(i,1) = (x(i,1)*slope(i)+x(i,2)*(1/slope(i))+y(i,2)-y(i,1)) / (slope(i)+(1/slope(i)))
              y(i,1) = y(i,1) + (x(i,2)-x(i,1))*slope(i)
              !else if (slope(i)==0) then
              !x(i,2) =  x(i,1)
           end if

           !enforcing direchlet boundaries
           if (x(i,1) <= 2.0 .or. x(i,1) >= 3.0) then
              y(i,1) = 0.0
           else if (x(i,1) > 2.0 .and. x(i,1) < 3.0) then
              y(i,1) = 0.1*sin(3.14159*(x(i,1)-2.0))
           end if

           !finding points closets to (2.0,0) and (3.0,0)
           if (abs(x(i,1)-2.0) < abs(x(xint2,1)-2.0)) then
              xint2 = i
           end if

           if (abs(x(i,1)-3.0) < abs(x(xint3,1)-3.0)) then
              xint3 = i
           end if
        end do

        !setting edge points for clean piecewise function
        x(xint2,1) = 2.0
        y(xint2,1) = 0.0
        x(xint3,1) = 3.0
        y(xint3,1) = 0.0
        x(1,1)     = 0.0
     end if

     !create mesure for error (avg change for all points)
     err_tot = err_sum1 + err_sum2
     !print "(16F7.3)", x
     !print "(16F7.3)", y
     !print *, err_tot
  end do


  print *, "Writing results to file"
  open(3,file="smoothgrid.txt",status="replace")
  do i = 1, dimi-1
     do j = 1, dimj-1 !each i,j combo will produce the line path of a cell
        write(3,*) x(i,j), y(i,j)
        write(3,*) x(i+1,j), y(i+1,j)
        write(3,*) x(i+1,j+1), y(i+1,j+1)
        write(3,*) x(i,j+1), y(i,j+1)
        write(3,*) x(i,j), y(i,j)
        write(3,*)
     end do
  end do
  close(3)

  open(4,file='smoothgrid.dat',status='replace')
  write(4,*) dimi, dimj
  do i=1,dimi
     do j=1,dimj
        write(4,*) x(i,j), y(i,j)
     end do
  end do
  close(4)

  !print *, "X matrix"
  !print "(21F7.3)", x
  !print *, "Y matrix"
  !print "(21F7.3)", y


  deallocate(x) !(row,col)
  deallocate(y)

  deallocate(a,b,g,c,a1,a2,a3,a4,a5,a6,a7,a8)
  deallocate(slope)

  call execute_command_line('gnuplot -p plotsmooth.plt')


end program laplaceGrid
