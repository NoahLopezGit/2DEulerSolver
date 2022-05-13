program generate_square_grid
  implicit none
  !this will generate a small square grid to be used for testing
  !should convert this to a module
  integer :: i,j,itr
  integer :: width, height
  real, dimension(:,:,:), allocatable :: grid

  width=4 !adjust size of square grid here
  height=4
  allocate(grid(height,width,2))

  do j=1,height
    do i=1,width
      !auto converts to int to float; b/c specified data type as real
      grid(j,i,1)=j !first index is x val
      grid(j,i,2)=i !y val
    end do
  end do

  print *, "Writing results to file"
  open(2,file='squaregrid.dat',status='replace')
  write(2,*) width,height
  do j=1,height
        !this uses an implied do loop to write x and y row in one line
        write(2,*) (grid(itr,j,1), grid(itr,j,2), itr=1,width)
  end do
  close(2)

end program generate_square_grid
