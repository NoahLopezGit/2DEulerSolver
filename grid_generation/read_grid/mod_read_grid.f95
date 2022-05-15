module mod_read_grid
  implicit none

contains

  subroutine get_grid_info(filename,height,width)
    implicit none
    !this is to get grid dimensions so programs using read_grid subroutine
      !can define variable before gettting grid data returned

    character*50, intent(in) :: filename
    integer, intent(out) :: height, width
    open(1,file=filename,status='old')
    read(1,*) width,height
    close(1)

  end subroutine get_grid_info

  subroutine read_grid(filename,grid)
    !module to read grid data
    integer :: height, width
    integer :: i,j,itr
    character*50, intent(in) :: filename
    real, dimension(:,:,:), intent(out) :: grid !stored heightxwidth index and x,y data

    open(1,file=filename,status='old')
    read(1,*) width,height

    do j=1,height
      !uses implied do loop to read each line
      read(1,*) (grid(itr,j,1), grid(itr,j,2), itr=1,width)
    end do
    close(1)
  end subroutine read_grid

end module mod_read_grid
